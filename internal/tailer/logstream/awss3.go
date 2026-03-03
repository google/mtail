package logstream

import (
	"compress/gzip"
	"context"
	"errors"
	"io"
	"net/url"
	"regexp"
	"strings"
	"sync"
	"time"

	"github.com/aws/aws-sdk-go-v2/aws"
	cfg "github.com/aws/aws-sdk-go-v2/config"
	"github.com/aws/aws-sdk-go-v2/service/s3"
	"github.com/aws/smithy-go"
	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/waker"
)

const (
	AWS3Scheme = "s3"
)

func IsAWSExitableError(err error) bool {
	if err == nil {
		return false
	}

	var ae smithy.APIError

	if errors.As(err, &ae) {
		switch ae.ErrorCode() {
		case "AccessDenied", "UnauthorizedOperation":
			return true
		default:
			return false
		}
	}

	return false
}

const (
	PLAIN = "plain"
	GZIP  = "gzip"
)

type s3BucketConfig struct {
	config aws.Config

	bucket string

	prefix string

	pattern string
	re      *regexp.Regexp

	// init time
	lastModified time.Time

	lastKey string

	format string
}

type s3Stream struct {
	streamBase
	s3BucketConfig

	cancel context.CancelFunc
}

//go:generate go run ./../../../cmd/config-gen/main.go -type S3Config -file s3_config_generated.go -module logstream

func parseS3URL(u *url.URL) (s3BucketConfig, error) {
	config := s3BucketConfig{}

	if u.Host == "" {
		return config, errors.New("S3 URL must contain a host as bucket")
	}
	config.bucket = u.Host
	config.prefix = strings.TrimPrefix(u.Path, "/")

	if pattern := u.Query().Get("Pattern"); pattern != "" {
		config.pattern = pattern
	} else {
		config.pattern = ".*"
	}

	re, err := regexp.Compile(config.pattern)
	if err != nil {
		return config, err
	}
	config.re = re

	if lastModified := u.Query().Get("LastModified"); lastModified != "" {
		t, err := time.Parse(time.RFC3339, lastModified)
		if err != nil {
			return config, err
		}
		config.lastModified = t
	} else {
		config.lastModified = time.Now()
	}

	if lastKey := u.Query().Get("LastKey"); lastKey != "" {
		config.lastKey = lastKey
	}

	if format := u.Query().Get("Format"); format != "" {
		config.format = format
	} else {
		config.format = PLAIN
	}

	config.config, _ = cfg.LoadDefaultConfig(context.TODO())

	if err := parseS3Config(u, &config.config); err != nil {
		return config, err
	}

	return config, nil
}

func newAWSS3Stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, u *url.URL, oneShot OneShotMode) (LogStream, error) {
	glog.V(2).Infof("newAWSS3Stream(%s): config", u.String())
	config, err := parseS3URL(u)
	if err != nil {
		return nil, err
	}

	ctx, cancel := context.WithCancel(ctx)
	fs := &s3Stream{
		s3BucketConfig: config,
		cancel:         cancel,
		streamBase: streamBase{
			sourcename: u.Host,
			lines:      make(chan *logline.LogLine),
		},
	}

	if err := fs.stream(ctx, wg, waker, oneShot); err != nil {
		return nil, err
	}

	glog.V(2).Infof("newAWSStream(%s): started stream", fs.sourcename)

	return fs, nil
}

func (fs *s3Stream) stream(ctx context.Context, wg *sync.WaitGroup, waker waker.Waker, oneShot OneShotMode) error {
	client := s3.NewFromConfig(fs.config)

	var total int
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("stream(%s): read total %d bytes", fs.sourcename, total)
			glog.Info("stream(%s): last key %s", fs.lastKey)
			close(fs.lines)
			fs.cancel()
		}()

		for {
			input := &s3.ListObjectsV2Input{
				Bucket: &fs.bucket,
			}

			if fs.prefix != "" {
				input.Prefix = &fs.prefix
			}

			if fs.lastKey != "" {
				input.StartAfter = &fs.lastKey
			}

			paginator := s3.NewListObjectsV2Paginator(client, input)

			for paginator.HasMorePages() {
				page, err := paginator.NextPage(ctx)
				if err != nil {
					if IsAWSExitableError(err) {
						glog.V(2).Infof("stream(%s): exiting, conn has AWS error %s", fs.sourcename, err)
						return
					}

					logErrors.Add(fs.sourcename, 1)
					glog.Infof("stream(%s): error listing objects: %v", fs.sourcename, err)
					break
				}
				for _, obj := range page.Contents {
					fs.lastKey = *obj.Key

					// Skip files that are older than the time or do not match the pattern.
					if obj.LastModified.Before(fs.lastModified) || !fs.re.MatchString(*obj.Key) {
						glog.V(2).Infof("stream(%s): skipping file (%s) (%v)", fs.sourcename, *obj.Key, obj.LastModified)
						continue
					}

					out, err := client.GetObject(ctx, &s3.GetObjectInput{
						Bucket: &fs.bucket,
						Key:    obj.Key,
					})

					if err != nil {
						if IsAWSExitableError(err) {
							glog.V(2).Infof("stream(%s): exiting, conn has AWS error %s", fs.sourcename, err)
							return
						}

						if ctx.Err() != nil {
							// The context has been cancelled, so exit directly.
							glog.V(2).Infof("stream(%s): context cancelled, exiting directly", fs.sourcename)
							return
						}

						logErrors.Add(fs.sourcename, 1)
						glog.Infof("stream(%s): error getting object: %v", fs.sourcename, err)
						continue
					}

					logOpens.Add(fs.sourcename, 1)
					glog.V(2).Infof("stream(%s): opened new file (%s)", fs.sourcename, *obj.Key)

					defer out.Body.Close()

					var lr *LineReader

					switch fs.format {
					case GZIP:
						gr, err := gzip.NewReader(out.Body)
						if err != nil {
							logErrors.Add(fs.sourcename, 1)
							glog.Infof("stream(%s): error creating gzip reader: %v", fs.sourcename, err)
							continue
						}

						defer gr.Close()

						lr = NewLineReader(*obj.Key, fs.lines, gr, defaultReadBufferSize, fs.cancel)
					default:
						lr = NewLineReader(*obj.Key, fs.lines, out.Body, defaultReadBufferSize, fs.cancel)
					}

					for {
						n, err := lr.ReadAndSend(ctx)

						glog.V(2).Infof("stream(%s): read %d bytes, err is %v", fs.sourcename, n, err)

						if n > 0 {
							total += n

							// No error implies there is more to read so restart the loop.
							if err == nil && ctx.Err() == nil {
								continue
							}
						} else if n == 0 && total > 0 {
							// `pipe(7)` tells us "If all file descriptors referring to the
							// write end of a fifo have been closed, then an attempt to
							// read(2) from the fifo will see end-of-file (read(2) will
							// return 0)."  To avoid shutting down the stream at startup
							// before any writer has connected to the fifo, condition on
							// having read any bytes previously.
							glog.V(2).Infof("stream(%s): exiting, 0 bytes read", fs.sourcename)
							break
						}

						// Test to see if we should exit.
						if IsExitableError(err) {
							// Because we've opened in nonblocking mode, this Read can return
							// straight away.  If there are no writers, it'll return EOF (per
							// `pipe(7)` and `read(2)`.)  This is expected when `mtail` is
							// starting at system init as the writer may not be ready yet.
							if !(errors.Is(err, io.EOF) && total == 0) {
								glog.V(2).Infof("stream(%s): exiting, stream has error %s", fs.sourcename, err)
								break
							}
						}
					}

					lr.Finish(ctx)

					if oneShot == OneShotEnabled {
						glog.Infof("stream(%s): oneshot mode, exiting", fs.sourcename)
						return
					}
				}
			}

			// Wait for wakeup or termination.
			glog.V(2).Infof("stream(%s): waiting", fs.sourcename)
			select {
			case <-ctx.Done():
				// Exit directly.
				glog.V(2).Infof("stream(%s): context cancelled, exiting directly", fs.sourcename)
				return
			case <-waker.Wake():
				// sleep until next Wake()
				glog.V(2).Infof("stream(%s): Wake received", fs.sourcename)
			}
		}
	}()

	return nil
}

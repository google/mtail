package logstream

import (
	"context"
	"errors"
	"net/url"
	"strings"
	"sync"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"github.com/segmentio/kafka-go"
)

const (
	KafkaScheme = "kafka"
)

type kafkaStream struct {
	streamBase
	config kafka.ReaderConfig

	cancel context.CancelFunc
}

//go:generate go run ./../../../cmd/config-gen/main.go -type KafkaConfig -file kafka_config_generated.go -module logstream

func parseKafkaURL(u *url.URL) (kafka.ReaderConfig, error) {
	config := kafka.ReaderConfig{
		Brokers: []string{u.Host},
		Topic:   strings.TrimPrefix(u.Path, "/"),
	}

	if u.User.Username() != "" {
		config.GroupID = u.User.Username()
	}

	if err := parseKafkaConfig(u, &config); err != nil {
		return config, err
	}

	return config, config.Validate()
}

func newKafkaStream(ctx context.Context, wg *sync.WaitGroup, u *url.URL, oneShot OneShotMode) (LogStream, error) {
	glog.V(2).Infof("newKafkaStream(%s): config", u.String())
	config, err := parseKafkaURL(u)
	if err != nil {
		return nil, err
	}

	ctx, cancel := context.WithCancel(ctx)
	ks := &kafkaStream{
		cancel: cancel,
		config: config,
		streamBase: streamBase{
			sourcename: u.String(),
			lines:      make(chan *logline.LogLine),
		},
	}

	if err := ks.stream(ctx, wg, oneShot); err != nil {
		return nil, err
	}

	glog.V(2).Infof("newKafkaStream(%s): started stream", ks.sourcename)

	return ks, nil
}

func (ks *kafkaStream) stream(ctx context.Context, wg *sync.WaitGroup, oneShot OneShotMode) error {
	r := kafka.NewReader(ks.config)

	glog.V(2).Infof("stream(%s): opened new reader", ks.sourcename)

	var total int
	wg.Add(1)
	go func() {
		defer wg.Done()
		defer func() {
			glog.V(2).Infof("stream(%s): read total %d bytes", ks.sourcename, total)
			glog.V(2).Infof("stream(%s): closing kafka connection", ks.sourcename)
			if err := r.Close(); err != nil {
				logErrors.Add(ks.sourcename, 1)
				glog.Infof("stream(%s): closing connection: %v", ks.sourcename, err)
			}
			logCloses.Add(ks.sourcename, 1)
		}()

		for {

			m, err := r.ReadMessage(ctx)

			if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
				glog.V(2).Infof("stream(%s): context cancelled or deadline exceeded", ks.sourcename)
				break
			}

			if IsExitableError(err) {
				glog.V(2).Infof("stream(%s): exiting, conn has error %s", ks.sourcename, err)
				break
			}

			if err != nil {
				logErrors.Add(ks.sourcename, 1)
				glog.V(2).Infof("stream(%s): read error: %v", ks.sourcename, err)
			}

			logLines.Add(ks.sourcename, 1)
			ks.lines <- logline.New(ctx, ks.sourcename, string(m.Value))

			if oneShot == OneShotEnabled {
				glog.Infof("stream(%s): read one in one shot mode, exiting. Sample data: %s = %s", ks.sourcename, string(m.Key), string(m.Value))
				break
			}
		}
		close(ks.lines)
	}()

	return nil
}

package logstream_test

import (
	"bytes"
	"compress/gzip"
	"context"
	"fmt"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/aws/aws-sdk-go-v2/aws"
	"github.com/aws/aws-sdk-go-v2/config"
	"github.com/aws/aws-sdk-go-v2/service/s3"
	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
)

func TestS3StreamRead(t *testing.T) {
	var wg sync.WaitGroup

	bucket := os.Getenv("MTAIL_AWSS3_TEST_BUCKET")
	if bucket == "" {
		t.Skip("MTAIL_AWSS3_TEST_BUCKET not set")
	}

	region := "us-east-1"

	key := "testdata/yo.txt"

	sourcename := fmt.Sprintf("s3://%s/%s/?Region=%s", bucket, "testdata", region)

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")
	fs, err := logstream.New(ctx, &wg, waker, sourcename, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: key, Line: "yo"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	time.Sleep(time.Second * 3)

	awaken(1, 1) // synchronise past first read

	cfg, _ := config.LoadDefaultConfig(context.Background())
	client := s3.NewFromConfig(cfg)
	_, err = client.PutObject(context.Background(), &s3.PutObjectInput{
		Bucket: &bucket,
		Key:    aws.String(key),
		Body:   strings.NewReader("yo\n"),
	})
	testutil.FatalIfErr(t, err)

	defer func() {
		client.DeleteObject(context.Background(), &s3.DeleteObjectInput{
			Bucket: &bucket,
			Key:    aws.String(key),
		})
	}()

	awaken(1, 1)

	time.Sleep(time.Second * 1)

	cancel()
	wg.Wait()

	checkLineDiff()

	if v := <-fs.Lines(); v != nil {
		t.Errorf("expecting filestream to be complete because stopped")
	}
}

func TestS3StreamReadGzip(t *testing.T) {
	var wg sync.WaitGroup

	bucket := os.Getenv("MTAIL_AWSS3_TEST_BUCKET")
	if bucket == "" {
		t.Skip("MTAIL_AWSS3_TEST_BUCKET not set")
	}

	region := "us-east-1"

	key := "testdata/yo.txt.gz"

	sourcename := fmt.Sprintf("s3://%s/%s/?Region=%s&Format=gzip", bucket, "testdata", region)

	ctx, cancel := context.WithCancel(context.Background())
	waker, awaken := waker.NewTest(ctx, 1, "stream")
	fs, err := logstream.New(ctx, &wg, waker, sourcename, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.TODO(), Filename: key, Line: "yo"},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, fs.Lines())

	time.Sleep(time.Second * 3)

	awaken(1, 1) // synchronise past first read

	cfg, _ := config.LoadDefaultConfig(context.Background())
	client := s3.NewFromConfig(cfg)
	var buf bytes.Buffer
	gz := gzip.NewWriter(&buf)
	gz.Write([]byte("yo\n"))
	gz.Close()

	_, err = client.PutObject(context.Background(), &s3.PutObjectInput{
		Bucket: &bucket,
		Key:    aws.String(key),
		Body:   bytes.NewReader(buf.Bytes()),
	})

	testutil.FatalIfErr(t, err)

	defer func() {
		client.DeleteObject(context.Background(), &s3.DeleteObjectInput{
			Bucket: &bucket,
			Key:    aws.String(key),
		})
	}()

	awaken(1, 1)

	time.Sleep(time.Second * 1)

	cancel()
	wg.Wait()

	checkLineDiff()

	if v := <-fs.Lines(); v != nil {
		t.Errorf("expecting filestream to be complete because stopped")
	}
}

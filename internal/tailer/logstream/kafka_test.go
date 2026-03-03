package logstream_test

import (
	"context"
	"fmt"
	"math/rand"
	"os"
	"sync"
	"testing"
	"time"

	"github.com/google/mtail/internal/logline"
	"github.com/google/mtail/internal/tailer/logstream"
	"github.com/google/mtail/internal/testutil"
	"github.com/google/mtail/internal/waker"
	"github.com/segmentio/kafka-go"
)

func TestKafkaStreamRead(t *testing.T) {
	var wg sync.WaitGroup

	ctx, cancel := context.WithCancel(context.Background())
	waker, _ := waker.NewTest(ctx, 1, "stream")

	// start kafka test server with docker
	// refer to https://hub.docker.com/r/apache/kafka
	host := os.Getenv("MTAIL_KAFKA_TEST_HOST")
	if host == "" {
		// t.Log("use default kafka host")
		// host = "localhost:9092"
		t.Skip("MTAIL_KAFKA_TEST_HOST not set")
	}

	topic := fmt.Sprintf("test-%d", rand.Intn(100))

	conn, err := kafka.DialLeader(ctx, "tcp", host, topic, 0)
	if err != nil {
		testutil.FatalIfErr(t, err)
	}
	defer conn.Close()

	// err = testutil.CreateTopic(conn, topic)
	// if err != nil {
	// 	testutil.FatalIfErr(t, err)
	// }

	consumerGroup := fmt.Sprintf("mtail-test-%d", rand.Intn(100))

	msg := "yo"

	sourcename := fmt.Sprintf("%s://%s@%s/%s", logstream.KafkaScheme, consumerGroup, host, topic)

	t.Log("sourcename", sourcename)

	ks, err := logstream.New(ctx, &wg, waker, sourcename, logstream.OneShotDisabled)
	testutil.FatalIfErr(t, err)

	expected := []*logline.LogLine{
		{Context: context.Background(), Filename: sourcename, Line: msg},
	}
	checkLineDiff := testutil.ExpectLinesReceivedNoDiff(t, expected, ks.Lines())

	// write to kafka
	n, err := conn.WriteMessages(kafka.Message{Topic: topic, Value: []byte(msg)})
	testutil.FatalIfErr(t, err)
	t.Log(n)

	time.Sleep(time.Second * 1)
	cancel()
	wg.Wait()

	checkLineDiff()

	if v := <-ks.Lines(); v != nil {
		t.Errorf("expecting filestream to be complete because stopped")
	}
}

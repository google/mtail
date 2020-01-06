// Copyright 2019 Google, Inc.  All Rights Reserved.
// This file is available under the Apache License.

package tailer

import (
	"bytes"
	"context"
	"net"
	"time"
	"unicode/utf8"

	"github.com/golang/glog"
	"github.com/google/mtail/internal/logline"
	"go.opencensus.io/trace"
)

// Socket provides an abstraction over unix sockets being tailed by `mtail'.
type Socket struct {
	name     string
	pathname string
	lastRead time.Time
	sock     net.Conn
	partial  *bytes.Buffer
	llp      logline.Processor
}

// NewSocket returns a new Socket named by the given pathname.
// `llp' is a logline Processor that receivres the bytes when read by Read().
func NewSocket(pathname, absPath string, llp logline.Processor) (*Socket, error) {
	glog.V(2).Infof("tailer.NewSocket(%s)", absPath)
	c, err := net.ListenUnixgram("unixgram", &net.UnixAddr{absPath, "unixgram"})
	if err != nil {
		return nil, err
	}
	return &Socket{pathname, absPath, time.Now(), c, bytes.NewBufferString(""), llp}, nil
}

func (s *Socket) LastReadTime() time.Time {
	return s.lastRead
}

func (s *Socket) Name() string {
	return s.name
}

func (s *Socket) Pathname() string {
	return s.pathname
}

func (s *Socket) Close(ctx context.Context) error {
	ctx, span := trace.StartSpan(ctx, "Socket.Close")
	defer span.End()
	if s.partial.Len() > 0 {
		s.sendLine(ctx)
	}
	return s.sock.Close()
}

func (s *Socket) Read(ctx context.Context) error {
	ctx, span := trace.StartSpan(ctx, "Socket.Read")
	defer span.End()

	b := make([]byte, 0, 4096)
	totalBytes := 0
	for {
		if err := s.sock.SetReadDeadline(time.Now().Add(1 * time.Second)); err != nil {
			glog.V(2).Infof("%s: %s", s.pathname, err)
		}
		n, err := s.sock.Read(b[:cap(b)])
		glog.V(2).Infof("read count %v err %v", n, err)
		totalBytes += n
		b = b[:n]

		if err, ok := err.(net.Error); ok && err.Timeout() {
			glog.Info("timeout, returning")
			return nil
		}

		var (
			rune  rune
			width int
		)
		for i := 0; i < len(b) && i < n; i += width {
			rune, width = utf8.DecodeRune(b[i:])
			switch {
			case rune != '\n':
				s.partial.WriteRune(rune)
			default:
				glog.Infof("sendline")
				s.sendLine(ctx)
			}
		}
		if err != nil {
			if totalBytes > 0 {
				s.lastRead = time.Now()
			}
			return err
		}
	}
}

func (s *Socket) sendLine(ctx context.Context) {
	ctx, span := trace.StartSpan(ctx, "Socket.sendLine")
	defer span.End()
	glog.Infof("Sending a line %q", s.partial.String())
	s.llp.ProcessLogLine(ctx, logline.New(ctx, s.name, s.partial.String()))
	lineCount.Add(s.name, 1)
	s.partial.Reset()
}

func (s *Socket) Follow(ctx context.Context) error {
	ctx, span := trace.StartSpan(ctx, "Socket.Follow")
	defer span.End()
	return s.Read(ctx)
}

// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	"html/template"
	"net/http"

	"github.com/golang/glog"
)

const statusTemplate = `
<html>
<head>
<title>mtail on {{.BindAddress}}</title>
</head>
<body>
<h1>mtail on {{.BindAddress}}</h1>
<p>Build: {{.BuildInfo}}</p>
<p>Metrics: <a href="/json">json</a>, <a href="/graphite">graphite</a>, <a href="/metrics">prometheus</a>, <a href="/varz">varz</a></p>
<p>Debug: <a href="/debug/pprof">debug/pprof</a>, <a href="/debug/vars">debug/vars</a>, <a href="/tracez">tracez</a>, <a href="/progz">progz</a></p>
`

// ServeHTTP satisfies the http.Handler interface, and is used to serve the
// root page of mtail for online status reporting.
func (m *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	t, err := template.New("status").Parse(statusTemplate)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	data := struct {
		BindAddress string
		BuildInfo   string
	}{
		m.listener.Addr().String(),
		m.buildInfo.String(),
	}
	w.Header().Add("Content-type", "text/html")
	w.WriteHeader(http.StatusOK)
	if err = t.Execute(w, data); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
	err = m.l.WriteStatusHTML(w)
	if err != nil {
		glog.Warningf("Error while writing loader status: %s", err)
	}
	err = m.t.WriteStatusHTML(w)
	if err != nil {
		glog.Warningf("Error while writing tailer status: %s", err)
	}
}

// FaviconHandler is used to serve up the favicon.ico for mtail's http server.
func FaviconHandler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "image/x-icon")
	w.Header().Set("Cache-Control", "public, max-age=7776000")
	if _, err := w.Write(logoFavicon); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

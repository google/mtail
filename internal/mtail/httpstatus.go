// Copyright 2020 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package mtail

import (
	_ "embed"
	"html/template"
	"net/http"

	"github.com/golang/glog"
)

const statusTemplate = `
<!DOCTYPE html>
<html>
<head>
<title>mtail on {{.BindAddress}}</title>
</head>
<body>
<h1>mtail on {{.BindAddress}}</h1>
<p>Build: {{.BuildInfo}}</p>
<p>Metrics: <a href="/json">json</a>, <a href="/graphite">graphite</a>, <a href="/metrics">prometheus</a></p>
<p>Info: {{ if .HTTPInfoEndpoints }}<a href="/varz">varz</a>, <a href="/progz">progz</a> <a href="/tracez">tracez</a></p>{{ else }} disabled {{ end }}</p>
<p>Debug: {{ if .HTTPDebugEndpoints }}<a href="/debug/pprof">debug/pprof</a>, <a href="/debug/vars">debug/vars</a>{{ else }} disabled {{ end }}</p>
`

const statusTemplateEnd = `
</body>
</html>
`

// ServeHTTP satisfies the http.Handler interface, and is used to serve the
// root page of mtail for online status reporting.
func (m *Server) ServeHTTP(w http.ResponseWriter, _ *http.Request) {
	t, err := template.New("status").Parse(statusTemplate)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	te, err := template.New("statusend").Parse(statusTemplateEnd)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	data := struct {
		BindAddress        string
		BuildInfo          string
		HTTPDebugEndpoints bool
		HTTPInfoEndpoints  bool
	}{
		m.listener.Addr().String(),
		m.buildInfo.String(),
		m.httpDebugEndpoints,
		m.httpInfoEndpoints,
	}
	w.Header().Add("Content-type", "text/html")
	w.WriteHeader(http.StatusOK)
	if err = t.Execute(w, data); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
	if m.httpInfoEndpoints {
		err = m.r.WriteStatusHTML(w)
		if err != nil {
			glog.Warningf("Error while writing loader status: %s", err)
		}
		err = m.t.WriteStatusHTML(w)
		if err != nil {
			glog.Warningf("Error while writing tailer status: %s", err)
		}
	}

	if err = te.Execute(w, data); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

//go:embed logo.ico
var logoFavicon []byte

// FaviconHandler is used to serve up the favicon.ico for mtail's http server.
func FaviconHandler(w http.ResponseWriter, _ *http.Request) {
	w.Header().Set("Content-Type", "image/x-icon")
	w.Header().Set("Cache-Control", "public, max-age=7776000")
	if _, err := w.Write(logoFavicon); err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"expvar"
	"html/template"
	"io"

	"github.com/google/mtail/internal/tailer/logstream"
)

const tailerTemplate = `
<h2 id="tailer">Log Tailer</h2>
<h3>Patterns</h3>
<ul>
{{range $name, $val := $.Patterns}}
<li><pre>{{$name}}</pre></li>
{{end}}
</ul>
<h3>Log files watched</h3>
<table border="1">
<tr>
<th>pathname</th>
<th>errors</th>
<th>opens</th>
<th>truncations</th>
<th>lines read</th>
</tr>
{{range $name, $val := $.LogStreams}}
<tr>
<td><pre>{{$name}}</pre></td>
<td>{{index $.Errors $name}}</td>
<td>{{index $.Opens $name}}</td>
<td>{{index $.Truncs $name}}</td>
<td>{{index $.Lines $name}}</td>
</tr>
{{end}}
</table>
`

// WriteStatusHTML emits the Tailer's state in HTML format to the io.Writer w.
func (t *Tailer) WriteStatusHTML(w io.Writer) error {
	tpl, err := template.New("tailer").Parse(tailerTemplate)
	if err != nil {
		return err
	}
	t.logstreamsMu.RLock()
	defer t.logstreamsMu.RUnlock()
	t.globPatternsMu.RLock()
	defer t.globPatternsMu.RUnlock()
	data := struct {
		LogStreams map[string]logstream.LogStream
		Patterns   map[string]struct{}
		Opens      map[string]string
		Lines      map[string]string
		Errors     map[string]string
		Truncs     map[string]string
	}{
		t.logstreams,
		t.globPatterns,
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
	}
	for _, pair := range []struct {
		k string
		m map[string]string
	}{
		{"log_errors_total", data.Errors},
		{"log_opens_total", data.Opens},
		{"file_truncates_total", data.Truncs},
		{"log_lines_total", data.Lines},
	} {
		pair := pair
		v := expvar.Get(pair.k).(*expvar.Map)
		v.Do(func(kv expvar.KeyValue) {
			pair.m[kv.Key] = kv.Value.String()
		})
	}
	return tpl.Execute(w, data)
}

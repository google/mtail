// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package tailer

import (
	"expvar"
	"html/template"
	"io"
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
<table border=1>
<tr>
<th>pathname</th>
<th>errors</th>
<th>rotations</th>
<th>truncations</th>
<th>lines read</th>
</tr>
{{range $name, $val := $.Handles}}
<tr>
<td><pre>{{$name}}</pre></td>
<td>{{index $.Errors $name}}</td>
<td>{{index $.Rotations $name}}</td>
<td>{{index $.Truncs $name}}</td>
<td>{{index $.Lines $name}}</td>
</tr>
{{end}}
</table>
</ul>
`

// WriteStatusHTML emits the Tailer's state in HTML format to the io.Writer w.
func (t *Tailer) WriteStatusHTML(w io.Writer) error {
	tpl, err := template.New("tailer").Parse(tailerTemplate)
	if err != nil {
		return err
	}
	t.handlesMu.RLock()
	defer t.handlesMu.RUnlock()
	t.globPatternsMu.RLock()
	defer t.globPatternsMu.RUnlock()
	data := struct {
		Handles   map[string]Log
		Patterns  map[string]struct{}
		Rotations map[string]string
		Lines     map[string]string
		Errors    map[string]string
		Truncs    map[string]string
	}{
		t.handles,
		t.globPatterns,
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
	}
	for _, pair := range []struct {
		v *expvar.Map
		m map[string]string
	}{
		{logErrors, data.Errors},
		{logRotations, data.Rotations},
		{logTruncs, data.Truncs},
		{lineCount, data.Lines},
	} {
		pair.v.Do(func(kv expvar.KeyValue) {
			pair.m[kv.Key] = kv.Value.String()
		})
	}
	return tpl.Execute(w, data)
}

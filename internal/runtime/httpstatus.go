// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package runtime

import (
	"fmt"
	"html/template"
	"io"
	"net/http"

	"github.com/google/mtail/internal/runtime/vm"
)

const loaderTemplate = `
<h2 id="loader">Program Loader</h2>
<table border="1">
<tr>
<th>program name</th>
<th>errors</th>
<th>load errors</th>
<th>load successes</th>
<th>unloads</th>
<th>runtime errors</th>
<th>last runtime error</th>
</tr>
<tr>
{{range $name, $errors := $.Errors}}
<td>{{ if index $.ProgLoaded $name}}<a href="/progz?prog={{$name}}">{{$name}}</a>{{else}}{{$name}}{{end}}</td>
<td>
{{if $errors}}
{{$errors}}
{{else}}
No compile errors
{{end}}
</td>
<td>{{index $.Loaderrors $name}}</td>
<td>{{index $.Loadsuccess $name}}</td>
<td>{{index $.Unloads $name}}</td>
<td>{{index $.RuntimeErrors $name}}</td>
<td><pre>{{index $.RuntimeErrorString $name}}</pre></td>
</tr>
{{end}}
</table>
`

// WriteStatusHTML writes the current state of the loader as HTML to the given writer w.
func (r *Runtime) WriteStatusHTML(w io.Writer) error {
	t, err := template.New("loader").Parse(loaderTemplate)
	if err != nil {
		return err
	}
	r.programErrorMu.RLock()
	defer r.programErrorMu.RUnlock()
	data := struct {
		ProgLoaded         map[string]bool
		Errors             map[string]error
		Loaderrors         map[string]string
		Loadsuccess        map[string]string
		Unloads            map[string]string
		RuntimeErrors      map[string]string
		RuntimeErrorString map[string]string
	}{
		make(map[string]bool),
		r.programErrors,
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
	}
	for name := range r.programErrors {
		if ProgLoadErrors.Get(name) != nil {
			data.Loaderrors[name] = ProgLoadErrors.Get(name).String()
		}
		if ProgLoads.Get(name) != nil {
			data.Loadsuccess[name] = ProgLoads.Get(name).String()
		}
		if ProgUnloads.Get(name) != nil {
			data.Unloads[name] = ProgUnloads.Get(name).String()
		}
		if vm.ProgRuntimeErrors.Get(name) != nil {
			data.RuntimeErrors[name] = vm.ProgRuntimeErrors.Get(name).String()
		}
		r.handleMu.RLock()
		if h, ok := r.handles[name]; ok {
			data.ProgLoaded[name] = true
			data.RuntimeErrorString[name] = h.vm.RuntimeErrorString()
		}
		r.handleMu.RUnlock()
	}
	return t.Execute(w, data)
}

func (r *Runtime) ProgzHandler(w http.ResponseWriter, req *http.Request) {
	prog := req.URL.Query().Get("prog")
	if prog != "" {
		r.handleMu.RLock()
		handle, ok := r.handles[prog]
		r.handleMu.RUnlock()
		if !ok {
			http.Error(w, "No program found", http.StatusNotFound)
			return
		}
		fmt.Fprint(w, handle.vm.DumpByteCode())
		fmt.Fprintf(w, "\nLast runtime error:\n%s", handle.vm.RuntimeErrorString())
		return
	}
	r.handleMu.RLock()
	defer r.handleMu.RUnlock()
	w.Header().Add("Content-type", "text/html")
	fmt.Fprintf(w, "<ul>")
	for prog := range r.handles {
		fmt.Fprintf(w, "<li><a href=\"?prog=%s\">%s</a></li>", prog, prog)
	}
	fmt.Fprintf(w, "</ul>")
}

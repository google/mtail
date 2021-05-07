// Copyright 2021 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package runtime

import (
	"html/template"
	"io"

	"github.com/google/mtail/internal/runtime/vm"
)

const loaderTemplate = `
<h2 id="loader">Program Loader</h2>
<table border=1>
<tr>
<th>program name</th>
<th>errors</th>
<th>load errors</th>
<th>load successes</th>
<th>runtime errors</th>
<th>last runtime error</th>
</tr>
<tr>
{{range $name, $errors := $.Errors}}
<td><a href="/progz?prog={{$name}}">{{$name}}</a></td>
<td>
{{if $errors}}
{{$errors}}
{{else}}
No compile errors
{{end}}
</td>
<td>{{index $.Loaderrors $name}}</td>
<td>{{index $.Loadsuccess $name}}</td>
<td>{{index $.RuntimeErrors $name}}</td>
<td><pre>{{index $.RuntimeErrorString $name}}</pre></td>
</tr>
{{end}}
</table>
`

// WriteStatusHTML writes the current state of the loader as HTML to the given writer w.
func (l *Runtime) WriteStatusHTML(w io.Writer) error {
	t, err := template.New("loader").Parse(loaderTemplate)
	if err != nil {
		return err
	}
	l.programErrorMu.RLock()
	defer l.programErrorMu.RUnlock()
	data := struct {
		Errors             map[string]error
		Loaderrors         map[string]string
		Loadsuccess        map[string]string
		RuntimeErrors      map[string]string
		RuntimeErrorString map[string]string
	}{
		l.programErrors,
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
		make(map[string]string),
	}
	for name := range l.programErrors {
		if ProgLoadErrors.Get(name) != nil {
			data.Loaderrors[name] = ProgLoadErrors.Get(name).String()
		}
		if ProgLoads.Get(name) != nil {
			data.Loadsuccess[name] = ProgLoads.Get(name).String()
		}
		if vm.ProgRuntimeErrors.Get(name) != nil {
			data.RuntimeErrors[name] = vm.ProgRuntimeErrors.Get(name).String()
		}
		data.RuntimeErrorString[name] = l.handles[name].vm.RuntimeErrorString()
	}
	return t.Execute(w, data)
}

// Copyright 2011 Google Inc. All Rights Reserved.
// This file is available under the Apache license.

package exporter

import (
	"encoding/json"
	"net/http"

	"github.com/golang/glog"
)

// HandleJSON exports the metrics in JSON format via HTTP.
func (e *Exporter) HandleJSON(w http.ResponseWriter, r *http.Request) {
	e.store.RLock()
	defer e.store.RUnlock()

	b, err := json.MarshalIndent(e.store.Metrics, "", "  ")
	if err != nil {
		glog.Info("error marshalling metrics into json:", err.Error())
	}
	w.Write(b)
}

# mtail agent guide

## Build & test

- **Only Bazel** — all development iteration uses `bazel test //...`.
- **Always `gofmt` your code** before submitting.

## Project structure

- `cmd/{mtail,mdot,mgen,mfmt}/main.go` — four binaries: `mtail` and `mfmt` are the ones that matter.
- `internal/` — core packages: `logline`, `metrics`, `runtime` (compiler + vm), `tailer`, `exporter`, `testutil`.
- `internal/testutil/` — shared test helpers.
- `examples/*.mtail` — sample mtail programs, also used as test fixtures.

## Documentation

- `docs` has extensive instructions for users and developers, written in Markdown.  Keep these up to date.

## Testing conventions

- Table-driven tests with `t.Run` subtest form.
- Comparison: `deep.Equal(expected, observed)` from `github.com/google/go-cmp`. Order is always expected first.
- `testutil` package provides common helpers (e.g., `testutil.TestServer`).

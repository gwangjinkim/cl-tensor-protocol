# cl-tensor-protocol

Tiny, stable CLOS-based tensor protocol (ABI) for Common Lisp with a capability registry so backends can be partial while higher-level code stays backend-agnostic. Ships a CPU reference backend; GPU adapters live in separate systems.

- Protocol types + generics in `src/`
- Backend registry + capability model
- Tiered conformance tests in `test/`
- CPU reference backend in `backends/cpu/`

See AGENTS.md for the full spec and milestones (M0–M8).

## Quick start (Roswell)

- Run tests: `make test`
- REPL: `make repl` then `(asdf:test-system :cl-tensor-protocol/test)`

## Protocol ABI (v0.1)

Package `cl-tensor-protocol` (nickname `ctp`). Core exports (minimal set):

- Classes: `backend`, `tensor`
- Conditions: `error`, `shape-error`, `dtype-error`, `backend-error`, `not-implemented`
- Registry: `register-backend`, `available-backends`, `find-backend`, `default-backend`
- Capabilities API: `capabilities`, `supports?`, `missing-capabilities`, `require-capabilities`
- Tensor introspection: `backend-of`, `dtype`, `shape`, `rank`, `size`
- Creation/conversion: `tensor`, `to-array`
- Shape ops: `reshape`, `transpose`, `slice`
- Math ops: `add`, `mul`, `mm`
- Dtype ops (Tier D): `copy`, `as-dtype`

Notes on semantics (high-level):
- `tensor`: Create a tensor from a scalar or CL array on a backend; stores dtype and shape.
- `to-array`: Returns a CL array or scalar; `:copy` can force materialization.
- `reshape`/`transpose`/`slice`: Shape transforms; signal `shape-error` on mismatch/invalid specs.
- `add`/`mul`: Elementwise; supports tensor⊗tensor (same backend) and tensor⊗scalar/scalar⊗tensor; cross-backend signals `backend-error`.
- `mm`: Matrix multiply for rank-2 tensors (m×k)·(k×n) -> (m×n); shape mismatch signals `shape-error`.
- `copy`: Deep copy of a tensor on the same backend with same dtype/shape.
- `as-dtype`: Convert tensor elements to a new dtype (backend-defined coercions).

## Capability model

Backends declare a set of capability keywords (e.g., `:tensor-from`, `:to-array`, `:mm`).

- Check support: `(supports? backend cap)`
- Enforce requirements: `(require-capabilities backend '(:mm :add) :context ...)`

## Registry and selection

- Register: `(register-backend backend &key priority)`
- Discover: `(available-backends)`, `(find-backend :cpu)`
- Default selection: `(default-backend &key prefer require)` picks the first backend in `prefer` order that satisfies `require` capabilities; otherwise signals `not-implemented` with missing capabilities.

## Conformance runner (Tiered tests)

The test suite contains a conformance runner that only exercises capabilities the backend declares.

- From a REPL: `(cl-tensor-protocol/test:run-conformance (ctp:default-backend) :tiers '(:A :B :C :D))`
- Tiers:
  - A: `:tensor-from :to-array :shape`
  - B: `:reshape :transpose :slice`
  - C: `:add :mul :mm`
  - D: `:copy :as-dtype`

Partial backends can participate truthfully by declaring only implemented caps.

## CPU reference backend

`backends/cpu/` implements a simple in-memory backend that passes tiers A–D. It registers at load, so `(default-backend)` yields CPU.

## Adapter templates (separate systems)

- mlx-cl: `docs/adapter-mlx-cl.md` (wraps `mlx-cl`; do NOT `:use :mlx-cl`; use local-nicknames; start with Tier A, then `:mm`, then `:add/:mul`, then shape ops.)
- cl-cuda: `docs/adapter-cl-cuda.md` (uses `cl-cuda`; start with Tier A, then `:add/:mul`, then `:mm`; consider cuBLAS as an optional add-on.)

## MLX backend (local system)

This repo ships an optional MLX backend in `backends/mlx/` (depends on `mlx-cl`). It auto-registers if `mlx-cl` is available.

- Load: `(asdf:load-system :cl-tensor-backend-mlx)`
- Tests: `(asdf:test-system :cl-tensor-backend-mlx/test)` (or `make test` to run all)
- Select MLX by preference: `(ctp:default-backend :prefer '(:mlx :cpu))`

## Status (v0.1 Definition of Done)

- CPU backend passes tiers A–D
- Registry selection with `:require` works
- Tiered conformance supports partial backends (fake :mm-only backend included in tests)
- Adapter template docs exist
- Roswell workflow is stable (`make test` is canonical)

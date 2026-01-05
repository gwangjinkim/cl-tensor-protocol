# AGENTS.md — cl-tensor-protocol

## 0) Mission
Build a **tiny, stable** CLOS-based tensor protocol (“ABI”) for Common Lisp, with a **capability registry** so backends can be partial (e.g., only `mm`) while higher-level code remains platform-agnostic.

This repo ships:
- protocol types + generic functions
- backend registry + capability model
- tiered conformance tests
- **CPU reference backend** (always available)

GPU backends live in separate systems/repos:
- `cl-tensor-backend-mlx` (wraps `mlx-cl`)
- `cl-tensor-backend-cuda` (wraps `cl-cuda`)
- optional `cl-tensor-backend-metal` (may wrap `metal-matmul`, likely partial)

---

## 1) Non-negotiable rules (for agents)

### 1.1 Micro-milestones only
Implement exactly one milestone (M0, M1, …) per run.
Do not start the next milestone unless the user asks.

### 1.2 Test-first
For each milestone:
1) Add/extend tests first (FiveAM)
2) Implement minimal code
3) Run `make test` and fix failures
4) Summarize:
   - what changed
   - which files
   - which acceptance criteria are met

### 1.3 No API creep
Every exported generic is forever.
If you think you need a new generic, first try to express it via existing ones.
If unavoidable:
- add it to `src/protocol.lisp`
- add conformance tests
- document semantics in README

### 1.4 Protocol stays clean
This repo must NOT depend on `mlx-cl`, `cl-cuda`, Metal libraries, CUDA drivers, etc.
All GPU functionality is in optional backend systems.

---

## 2) Roswell workflow (canonical)

### Run tests (required)
```bash
make test
```

### Start a REPL (optional)
```bash
make repl
```

If `make test` fails because scripts are missing, create them exactly as specified in `ROSWELL.md`,
then re-run `make test`.

Agents must not invent new commands: always use the Makefile targets.

---

## 3) Repo layout (required)

### Top-level
- `cl-tensor-protocol.asd`
- `AGENTS.md`
- `README.md`
- `ROSWELL.md`
- `Makefile`
- `scripts/test.ros`
- `scripts/repl.ros`

### Source
```
src/
  package.lisp
  conditions.lisp
  dtype.lisp
  capability.lisp
  backend.lisp
  tensor.lisp
  protocol.lisp
  registry.lisp
  utils.lisp
```

### CPU backend (reference)
```
backends/cpu/
  cl-tensor-backend-cpu.asd
  package.lisp
  cpu-backend.lisp
  cpu-tensor.lisp
  cpu-ops.lisp
```

### Tests
```
test/
  package.lisp
  conformance.lisp
  cpu-backend.lisp
  capability-tests.lisp
```

---

## 4) Protocol specification (the ABI)

### 4.1 Core exported names (keep minimal)
Package: `CL-TENSOR-PROTOCOL` (nickname suggested: `CTP`)

Exports:
- Classes: `backend`, `tensor`
- Conditions: `error`, `shape-error`, `dtype-error`, `backend-error`, `not-implemented`
- Registry: `register-backend`, `available-backends`, `find-backend`, `default-backend`
- Capabilities: `capabilities`, `supports?`, `missing-capabilities`, `require-capabilities`
- Tensor introspection: `backend-of`, `dtype`, `shape`, `rank`, `size`
- Creation/conversion: `tensor`, `to-array`, `copy`, `as-dtype`
- Shape ops: `reshape`, `transpose`, `slice`
- Math ops: `add`, `mul`, `mm`

### 4.2 Base classes

#### `backend` (class)
Required accessors:
- `(backend-name backend) -> string`
- `(device-type backend) -> keyword` ; :cpu :mlx :cuda :metal (extensible)
- `(features backend) -> plist|hash-table` ; optional

#### `tensor` (class)
Required generics:
- `(backend-of tensor) -> backend`
- `(dtype tensor) -> keyword`
- `(shape tensor) -> simple-vector` ; #(d1 d2 ...)

Derived helpers in `utils.lisp`:
- `(rank tensor)`  ; length of shape
- `(size tensor)`  ; product of dims

### 4.3 Capabilities (partial backends allowed)

Capabilities are keywords defined in `src/capability.lisp`.

Core capabilities (v0.1):
- `:tensor-from`
- `:to-array`
- `:copy`
- `:as-dtype`
- `:shape`
- `:reshape`
- `:transpose`
- `:slice`
- `:add`
- `:mul`
- `:mm`

Backend capability API:
- `(capabilities backend) -> list-of-keywords`
- `(supports? backend cap) -> boolean`
- `(missing-capabilities backend caps) -> list`
- `(require-capabilities backend caps &key context) -> t | signals not-implemented`

### 4.4 Registry + selection
- `(register-backend backend &key priority)` registers a backend instance
- `(available-backends)` returns all registered backends
- `(default-backend &key prefer require)`
  - `prefer`: ordered list like `(:cuda :mlx :metal :cpu)`
  - `require`: list of capabilities required
  - selects first backend in preference order that satisfies requirements
  - otherwise signals `not-implemented` with missing-capabilities

### 4.5 Creation / conversion generics
1) `(tensor backend object &key dtype order) -> tensor`
Capability: `:tensor-from`
- object: scalar or CL array
- dtype: keyword, must be stored and queryable
- order: :c :f :auto (backend may ignore)

2) `(to-array tensor &key copy element-type) -> CL array|scalar`
Capability: `:to-array`

3) `(copy tensor) -> tensor`
Capability: `:copy`

4) `(as-dtype tensor dtype) -> tensor`
Capability: `:as-dtype`

### 4.6 Shape ops generics
5) `(reshape tensor new-shape &key copy) -> tensor`   (`:reshape`)
6) `(transpose tensor axes) -> tensor`                (`:transpose`)
7) `(slice tensor spec) -> tensor`                    (`:slice`)

Slice spec (v0.1):
- list of per-axis specs, length == rank
- axis spec:
  - `(:all)`
  - `(:range start stop step)` where any may be NIL, `step` defaults to 1

### 4.7 Math ops generics (v0.1 minimal)
8) `(add x y) -> tensor`  (`:add`)
9) `(mul x y) -> tensor`  (`:mul`)
10) `(mm a b) -> tensor`  (`:mm`)
Rules:
- Support tensor⊗tensor (same backend)
- Support tensor⊗scalar and scalar⊗tensor
- Cross-backend ops: signal `backend-error`

---

## 5) Error model (conditions)
Conditions in `src/conditions.lisp`:
- `ctp:error` base
- `ctp:shape-error`
- `ctp:dtype-error`
- `ctp:backend-error`
- `ctp:not-implemented` with slots:
  - `backend`
  - `missing-capabilities`
  - `context`

Protocol code must signal these, not plain `error`.

---

## 6) Conformance tests (tiered for partial backends)

### 6.1 Tiers
- Tier A (minimal): `:tensor-from :to-array :shape`
- Tier B (shape):   `:reshape :transpose :slice`
- Tier C (math):    `:add :mul :mm`
- Tier D (dtype):   `:copy :as-dtype`

### 6.2 Conformance runner
In `test/conformance.lisp` implement:
- `(run-conformance backend &key tiers)` which runs only tests the backend *claims* to support
- verify truthfulness: if backend declares `:mm`, mm tests must run and pass

CPU backend must pass tiers A–D.

---

## 7) Backend adapter templates (docs + design contract)

### 7.1 Adapter template: mlx-cl (separate repo/system)
Goal: `cl-tensor-backend-mlx` wraps `mlx-cl` without symbol collisions.

**IMPORTANT:** Do NOT `:use :mlx-cl`. `mlx-cl` overwrites CL functions; avoid `(:use :cl :mlx-cl)`.
Use local nicknames or selective imports.

Template package:
```lisp
(defpackage #:cl-tensor-backend-mlx
  (:use #:cl)
  (:local-nicknames
   (:mx #:mlx-cl)
   (:mx-linalg #:mlx-cl.linalg))
  (:import-from #:cl-tensor-protocol
    #:backend #:tensor #:backend-of #:dtype #:shape
    #:to-array #:copy #:as-dtype
    #:add #:mul #:mm
    #:capabilities #:supports?
    #:register-backend #:device-type #:backend-name
    #:backend-error #:not-implemented #:shape-error))
```

Suggested classes:
- `mlx-backend` (subclass of `backend`)
- `mlx-tensor`  (subclass of `tensor`) with slot `arr` (mlx array)

Suggested mapping:
- `(tensor (bk mlx-backend) obj ...)` -> `mx:mlx-array`
- `(to-array (t mlx-tensor) ...)`     -> `mx:lisp<-`
- `(mm (a mlx-tensor) (b mlx-tensor))` -> `mx-linalg:matmul` (or mlx equivalent)
Declare capabilities only for what is implemented.

Start with Tier A, then add `:mm`, then add `:add/:mul`, then shape ops.

### 7.2 Adapter template: cl-cuda (separate repo/system)
Goal: `cl-tensor-backend-cuda` uses `cl-cuda` to implement device tensors.

Template package:
```lisp
(defpackage #:cl-tensor-backend-cuda
  (:use #:cl)
  (:local-nicknames (:cu #:cl-cuda))
  (:import-from #:cl-tensor-protocol
    #:backend #:tensor #:backend-of #:dtype #:shape
    #:to-array #:copy #:as-dtype
    #:add #:mul #:mm
    #:capabilities #:supports?
    #:register-backend #:device-type #:backend-name
    #:backend-error #:not-implemented #:shape-error))
```

Suggested classes:
- `cuda-backend` (subclass of `backend`) holds device/context
- `cuda-tensor`  (subclass of `tensor`) holds device memory block + shape + dtype

Suggested minimal path:
- Tier A first: create/copy to device + to-array (device->host)
- Add `:add/:mul` via simple kernels
- Add `:mm` initially naive, later via cuBLAS in a separate optional package

Declare only implemented capabilities; conformance suite will run accordingly.

---

## 8) Milestones (do in order)

### M0 — Skeleton + Roswell scripts
Acceptance:
- `make test` works
- one trivial FiveAM test passes

### M1 — conditions + dtype + base classes
Acceptance:
- `backend`, `tensor` classes exist
- dtype set documented
- conditions signal correctly

### M2 — capability model + helpers
Acceptance:
- `capabilities/supports?/missing/require-capabilities` implemented
- `not-implemented` includes missing caps + context

### M3 — registry + CPU backend registration
Acceptance:
- `default-backend` returns CPU
- `default-backend :require '(:mm)` succeeds (CPU has it)

### M4 — CPU tensor creation + to-array + shape
Acceptance:
- scalar and 2D array roundtrip tests
- shape returns simple-vector of fixnums

### M5 — CPU shape ops
Acceptance:
- reshape/transpose/slice correctness
- mismatch signals `shape-error`

### M6 — CPU math ops
Acceptance:
- add/mul scalar + tensor
- mm correctness on small matrices
- mismatch signals `shape-error`

### M7 — tiered conformance runner
Acceptance:
- `run-conformance` runs only declared tiers
- CPU passes tiers A–D
- a fake partial backend declaring only `:mm` runs only mm tests

### M8 — docs for adapter templates
Acceptance:
- `docs/adapter-mlx-cl.md` and `docs/adapter-cl-cuda.md` exist (can be minimal)
- explain symbol hygiene + minimal method set

---

## 9) Definition of Done (v0.1)
v0.1 is done when:
- CPU backend passes tiers A–D
- registry selection with `:require` works
- tiered conformance suite supports partial backends
- adapter templates exist
- Roswell workflow is stable (`make test` is the canonical command)

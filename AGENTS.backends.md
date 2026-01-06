# AGENTS.backends.md — cl-tensor-protocol backends (MLX + CUDA)

This document extends the main `AGENTS.md` for `cl-tensor-protocol` with backend-specific milestones,
repo layout, and adapter rules for:

- `cl-tensor-backend-mlx` (Apple MLX via `mlx-cl`)
- `cl-tensor-backend-cuda` (NVIDIA CUDA via `cl-cuda`)

`cl-tensor-protocol` remains dependency-free w.r.t. GPU libraries. Backends are separate ASDF systems.

---

## 10) Backend adapter policy

Backends are implemented as **separate ASDF systems** that depend on `cl-tensor-protocol` plus the backend library:

- `cl-tensor-backend-mlx` depends on `mlx-cl`
- `cl-tensor-backend-cuda` depends on `cl-cuda`

`cl-tensor-protocol` must **not** depend on either.

Backends must:
- implement protocol generics via CLOS methods
- declare capabilities honestly
- **auto-register** themselves on load, but only if usable on the current machine
- ship their own tests and (optionally) conformance execution

---

## 11) Repo layout for backend systems

Add these directories (or separate repos if you prefer):

```
backends/mlx/
  cl-tensor-backend-mlx.asd
  src/
    package.lisp
    backend.lisp
    tensor.lisp
    ops.lisp
  test/
    package.lisp
    smoke.lisp

backends/cuda/
  cl-tensor-backend-cuda.asd
  src/
    package.lisp
    backend.lisp
    tensor.lisp
    ops.lisp
  test/
    package.lisp
    smoke.lisp
```

If you keep them in the same repo, your root can also contain an umbrella `.asd` that defines the two backend systems.

---

## 12) Symbol hygiene rules (mlx-cl)

**Do not `:use :mlx-cl`.** It may shadow common symbols.

Instead, use either:
- `:local-nicknames` (preferred), or
- `:import-from` selective imports

Example package skeleton (required):

```lisp
(defpackage #:cl-tensor-backend-mlx
  (:use #:cl)
  (:local-nicknames
   (:ctp #:cl-tensor-protocol)
   (:mx  #:mlx-cl)
   (:mxla #:mlx-cl.linalg))
  (:export
   #:register-mlx-backend
   #:mlx-available-p
   #:*mlx-backend*))
```

---

## 13) CUDA backend rules

CUDA availability is not guaranteed. The CUDA backend must:
- provide `(cuda-available-p)` (best-effort)
- register itself only if available
- gracefully skip tests on machines without CUDA

Initially, CUDA tensors may be **host-backed + GPU-accelerated per op** (simple + robust).
Later milestones can switch to persistent device storage.

---

## Backend milestones

### M9 — MLX backend skeleton + Tier A
**Goal:** `cl-tensor-backend-mlx` provides Tier A protocol support on macOS.

Capabilities required:
- `:tensor-from`, `:to-array`, `:shape`

Acceptance criteria:
- System loads: `(asdf:load-system :cl-tensor-backend-mlx)`
- Backend registers itself:
  - `(ctp:default-backend :prefer '(:mlx :cpu) :require '(:tensor-from :to-array :shape))` returns MLX backend
- Roundtrip test:
  - `tensor -> to-array` preserves numeric values and shape for 1D and 2D input arrays
- `make test` runs MLX tests (or skips cleanly if MLX unavailable)

Files:
- `backends/mlx/src/{package,backend,tensor}.lisp`
- `backends/mlx/test/smoke.lisp`

---

### M10 — MLX elementwise ops
**Goal:** MLX backend supports basic elementwise arithmetic.

Capabilities required:
- add `:add`, `:mul`

Semantics:
- tensor ⊕ tensor (same backend) OK
- tensor ⊕ scalar and scalar ⊕ tensor OK
- cross-backend ops signal `ctp:backend-error`

Acceptance criteria:
- `ctp:add` and `ctp:mul` work for vector + scalar, vector + vector
- Tests cover scalar + tensor, tensor + tensor

---

### M11 — MLX matmul (`mm`) (TransformerBlocks relevance)
**Goal:** add `:mm` capability for 2D float32 matrices.

Acceptance criteria:
- `(ctp:mm A B)` works for compatible shapes
- shape mismatch signals `ctp:shape-error`
- tests verify correctness on small fixed matrices (2x3 · 3x2)

Notes:
- Choose the correct MLX matmul entrypoint (often in `mlx-cl.linalg`)
- If MLX only supports certain dtypes, declare dtype limitations and test them.

---

### M12 — CUDA backend skeleton + Tier A (graceful)
**Goal:** `cl-tensor-backend-cuda` exists and does *something useful* on CUDA machines.

Capabilities required:
- `:tensor-from`, `:to-array`, `:shape`

Acceptance criteria:
- System loads even without CUDA drivers present
- `(cuda-available-p)` returns NIL on non-CUDA machines without erroring
- backend registers only if available
- tests skip (not fail) when CUDA unavailable

---

### M13 — CUDA elementwise ops (GPU accelerated)
**Goal:** add `:add`, `:mul` using `cl-cuda` kernels.

Acceptance criteria:
- On CUDA machines, `add/mul` use GPU kernels (device buffers + sync)
- On non-CUDA machines, tests skip cleanly
- correctness tests for vector add/mul

Implementation constraints (v0):
- host-backed tensors are allowed
- per-op device buffers are allowed
- must not leak memory blocks (use `unwind-protect`)

---

### M14 — CUDA matmul (`mm`) (phase 1)
**Goal:** add `:mm` minimally.

Acceptance criteria:
- `mm` works for small float32 matrices
- can be naïve kernel first
- later improvement milestone can integrate cuBLAS (optional)

---

### M15 — Cross-backend selection + conformance integration
**Goal:** verify registry selection and partial-backend conformance behavior.

Acceptance criteria:
- with both backends installed, `default-backend` chooses by `:prefer`
- conformance runner runs only supported tiers for each backend
- MLX passes Tier A + Tier C partial, CPU passes all tiers

---

## Adapter-specific documentation

Add docs pages:
- `docs/backend-mlx.md`
- `docs/backend-cuda.md`

Each must include:
- How to install dependencies
- How to run tests
- What capabilities are implemented
- Known limitations (dtype support, shape support, etc.)

---

## Commit messages for these milestones

- `M9: add mlx backend skeleton + Tier A roundtrip`
- `M10: mlx backend add/mul + tests`
- `M11: mlx backend mm (2D matmul) + tests`
- `M12: add cuda backend skeleton + availability gating`
- `M13: cuda backend add/mul kernels + tests`
- `M14: cuda backend mm (phase 1) + tests`
- `M15: backend selection + conformance integration`

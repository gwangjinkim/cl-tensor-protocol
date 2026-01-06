# MLX Backend — cl-tensor-backend-mlx

This backend wraps Apple’s MLX via the `mlx-cl` library to implement the cl-tensor-protocol on macOS Apple Silicon.

System name: `cl-tensor-backend-mlx`

- Depends on: `cl-tensor-protocol`, `mlx-cl`
- Auto-registers on load if `mlx-cl` is available
- Device type reported: `:mlx`

## Install

- Install `mlx-cl` per its documentation.
- Ensure ASDF can find `mlx-cl` and this backend.

## Usage

- Load: `(asdf:load-system :cl-tensor-backend-mlx)`
- Select MLX by preference: `(ctp:default-backend :prefer '(:mlx :cpu))`
- Capability-aware selection, e.g.: `(ctp:default-backend :prefer '(:mlx :cpu) :require '(:tensor-from :to-array :shape))`

## Capabilities (M9–M11)

- M9 (Tier A): `:tensor-from`, `:to-array`, `:shape`
- M10 (Elementwise): `:add`, `:mul`
- M11 (Matmul): `:mm` (2D float32)

Only declared capabilities are implemented; others are omitted.

## Symbol hygiene

Do NOT `:use :mlx-cl`. The backend package uses local nicknames:

```
(defpackage #:cl-tensor-backend-mlx
  (:use #:cl)
  (:local-nicknames
   (:ctp #:cl-tensor-protocol)
   (:mx  #:mlx-cl)
   (:mxla #:mlx-cl.linalg)))
```

## Tests

- Run the repo tests: `make test` (MLX tests skip if `mlx-cl` is not available).
- MLX-only tests live in `backends/mlx/test/`.

## Notes

- `copy` and `as-dtype` are not implemented in this milestone set; CPU backend provides full Tier D.
- Future milestones can extend dtype handling and shape ops as needed.


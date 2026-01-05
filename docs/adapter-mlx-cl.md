# Adapter Template: cl-tensor-backend-mlx (wraps mlx-cl)

Goal: wrap `mlx-cl` as a backend without polluting the global namespace and only declare capabilities you actually implement.

Key rules (from AGENTS.md):
- Do NOT `:use :mlx-cl`. `mlx-cl` overwrites CL functions; avoid `(:use :cl :mlx-cl)`.
- Use local nicknames or selective imports.
- Start with Tier A, then add `:mm`, then `:add/:mul`, then shape ops.

Suggested package:
```
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

Classes:
- `mlx-backend` (subclass of `ctp:backend`).
- `mlx-tensor`  (subclass of `ctp:tensor`) with slot `arr` holding the `mlx` array.

Minimal methods to start (Tier A):
- `(ctp:tensor (bk mlx-backend) obj &key dtype order) -> mlx array`
- `(ctp:to-array (t mlx-tensor) &key copy element-type) -> CL array`
- `(ctp:shape (t mlx-tensor)) -> simple-vector`

Matrix multiply (Tier C):
- `(ctp:mm (a mlx-tensor) (b mlx-tensor))` using `mx-linalg:matmul` or equivalent.

Capabilities: only advertise what works, e.g. `(:tensor-from :to-array :shape :mm)` initially.

Notes:
- For cross-backend ops, signal `ctp:backend-error`.
- Keep dtype keywords; conversions can be incremental.


# Adapter Template: cl-tensor-backend-cuda (uses cl-cuda)

Goal: Implement a CUDA backend with minimal surface, declaring only the capabilities supported.

Key guidance (from AGENTS.md):
- Keep `cl-tensor-protocol` as the only hard dependency; CUDA/driver-specific libs live here.
- Start with Tier A: device tensor creation and host conversion.
- Add `:add/:mul` via simple kernels.
- Add `:mm` naive first; later can integrate cuBLAS via an optional package.

Suggested package:
```
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

Classes:
- `cuda-backend` (subclass of `ctp:backend`) holds device/context.
- `cuda-tensor`  (subclass of `ctp:tensor`) holds device memory + shape + dtype.

Minimal path:
- Tier A: `(ctp:tensor (bk cuda-backend) obj ...)` to upload to device; `(ctp:to-array (t cuda-tensor))` to download.
- Tier C: `:add/:mul` via simple kernels; `:mm` naive or via cuBLAS in an optional add-on.

Capabilities:
- Begin with `(:tensor-from :to-array :shape)` and extend only when implemented.

Notes:
- Signal `ctp:backend-error` for cross-device ops.
- Keep symbol hygiene; avoid `:use` of vendor packages; prefer local nicknames.


# Roswell workflow for this repo

This repo runs Common Lisp tasks via **Roswell**. Most of the time you should use the **Makefile**
targets, which wrap the Roswell commands in a consistent way.

## Canonical commands

From the repo root:

### Run tests (preferred)

```bash
make test
```

This runs a clean Roswell session:

```bash
ros -Q script scripts/test.ros
```

### Start a REPL (optional)

```bash
make repl
```

which runs:

```bash
ros -Q script scripts/repl.ros
```

### Bootstrap fallback (only if scripts are missing)

```bash
ros -Q run \
  --eval '(require :asdf)' \
  --load 'cl-tensor-protocol.asd' \
  --eval '(asdf:test-system :cl-tensor-protocol/test)' \
  --eval '(uiop:quit 0)' \
  --quit
```


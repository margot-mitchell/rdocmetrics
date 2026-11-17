# tools/: the identifier guard

Nothing here is installed: `R CMD build` packages only `r/`. Installing, using or
testing `rdocmetrics` never needs Python.

| File | Job | Run by |
|---|---|---|
| `scan_repo.py` | scans files for participant identifiers and secrets | `make scan`, the pre-commit hook, and the `no-pii` CI job |
| `pre-commit` | git hook that runs the scanner on staged files | installed by `make hooks` |

The scanner uses only the Python standard library. Values it may flag that are
known to be fine (the maintainer's email) are listed in `.publicscanignore`.

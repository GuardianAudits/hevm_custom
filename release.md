2.0 Latest

This version adds the following cheatcodes (with behavior as implemented in `src/EVM.hs`):

- `readFile(string)`: Reads a file from disk and returns its contents as `string`. Requires `--ffi` (`allowFFI`). The resolved path must stay under `HEVM_FS_ROOT` (fallback: `ECHIDNA_FS_ROOT`, else current working directory). File size is limited by `HEVM_FS_MAX_BYTES` (fallback: `ECHIDNA_FS_MAX_BYTES`, default `5 MiB`).
- `getCode(string)`: Resolves contract bytecode from JSON artifacts and returns `bytes`. Requires `--ffi` (`allowFFI`). Accepts either direct `.json` paths or selectors (`file.sol`, `Contract`, `file.sol:Contract`, `file.sol:<version>`, `Contract:<version>`). If multiple artifacts match, selection uses explicit or preferred compiler version env (`HEVM_SOLC_VERSION`/fallbacks); otherwise it errors as ambiguous.
- `parseJsonBytes(string,string)`: Parses JSON and returns `bytes` extracted from a path expression. Supports dot/bracket navigation (including quoted keys and array indices). The selected JSON value must be either a `0x`-prefixed hex string or an array of byte values `[0..255]`; otherwise it reverts with an error.
- `snapshot()`: Captures a full VM snapshot (env, block, tx substate, logs, traces, forks, current fork) and returns a new snapshot id (`uint256`). In symbolic mode this errors.
- `revertTo(uint256)`: Restores the VM to a previously captured snapshot id and returns `true`. Errors if the id is unknown or symbolic. In symbolic mode this errors.

Breaking change:

- `doFunctionCall(address,bytes,address)`: Removed in `2.0`.

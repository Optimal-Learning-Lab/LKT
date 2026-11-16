# Exact native Linux semantic runtime

This is a source-only setup workflow for the already selected MPNet encoder. It
uses the explicit managed CPython **3.12.14**, the unchanged helper, all **34 original**
distribution versions unchanged plus the explicitly pinned **hf-xet1.8.0 Linux
dependency**, and the original model artifact. It never copies the
Windows venv, changes the package lock, downloads another model revision or
substitutes an encoder. It does not submit jobs or fit research models.

## Source identities and configuration

The managed interpreter is the x86_64 glibc installation from the official
[immutable 20260901 python-build-standalone release](https://github.com/astral-sh/python-build-standalone/releases/tag/20260901):

```text
cpython-3.12.14+20260901-x86_64-unknown-linux-gnu-install_only_stripped.tar.gz
SHA256 72748da13197c1fb161e3afeef20a6a385ff24f2165e6e2758e47008e7faba4c
```

The hash was read directly from the release's published
[SHA256SUMS](https://github.com/astral-sh/python-build-standalone/releases/download/20260901/SHA256SUMS).
The config explicitly selects this provider, URL and hash; no provider selection
or alternate interpreter search occurs after a failure.

The root coordinator has a prepared configuration template and authentic four-text
probe in `C:/dev/LKT-outputs/pfa-mechanism-review-20261109/execution/semantic-linux-preparation`.
The probe contains exact members of the existing corpus (a question, a response,
an empty response and a KC statement); its source receipt records the original
corpus hash and member indices. No synthetic encoder is used.

Required JSON fields are exactly:

```text
schema, provider, workspace, container, container_sha256,
original_package_lock, original_package_lock_sha256,
linux_dependency_contract, linux_dependency_contract_sha256,
r_dependency_receipt, r_dependency_receipt_sha256, controller, controller_sha256,
worker, worker_sha256, runtime_dir, runtime_limit_bytes, model_dir,
manifest_sha256, package_lock, package_lock_sha256, helper, helper_sha256,
probe_request, probe_request_sha256, reports_dir, python_url, python_sha256,
storage_roots, persistent_limit_bytes, required_free_bytes, locale
```

The current configuration schema is **2**. All artifact paths are explicit
absolute workspace paths. The R dependency receipt
must confirm successful R4.5.3 setup in the exact same SIF. Hashes bind the
controller/worker, package lock, installed package helper, probe and model manifest.
The model is the already staged **438,673,469-byte** directory, with manifest hash
`e70b5539c9d6a4716c167afd1529a2652b7cf7852d0bd16f81ae3f6ac770453e`
and revision `e8c3b32edf5434bc2275fc9bab85f82640a19130`. Every listed model file
is checked before setup, and the model is mounted read-only during execution.
The unchanged helper hash is
`d17a0f4d34797fa394475ed079b4e8d446baad013199ea91b1fe39a55cb15db4`.
The immutable original34 version-record hash is
`bbb795b9c1f42aae978a4c637bbf42f96ecfbbf93d7448bbb94d9c7a58558f9c`.
The Linux35 lock hash is
`038e5dff07f32e8a3a77f93dfb84300787e1b0888e0091372a242dab259702ae`.
The explicit Linux dependency-contract hash is
`4b6f50f262fe900afd87ff0528a6dc02173405921c3290649487705de0c1578e`.

The official huggingface-hub0.36.2 wheel METADATA declares
`hf-xet<2.0.0,>=1.1.3` for `platform_machine == "x86_64"` (and three other
architecture strings), so this dependency is mandatory in the actual Linux
runtime. This is separate from the package's optional `hf-xet` extra. The exact
[official hf-xet1.8.0 release metadata](https://pypi.org/pypi/hf-xet/1.8.0/json)
provides a non-yanked cp38-abi3 manylinux2.17 x86_64 wheel for Python>=3.8,
published2026-10-30. It satisfies the Hub range and managed Python3.12.14, and
its only declared dependency is pytest for the unrequested tests extra.
The approved wheel SHA256 is
`29dab7c904cd1914a0439c6e1306c12c7497c8bd32290fbe563ebf51ca4b6d6d`.

The original34 record is retained unchanged. The derived Linux lock must preserve
every original version and add exactly hf-xet1.8.0; other additions or changes
fail. The contract freezes the exact official Hub and hf-xet wheel URLs/hashes,
metadata evidence, mandatory marker and version-selection rationale. After
installation, the worker checks that actual wheel metadata activates and satisfies
that marker, checks that hf-xet adds no further mandatory dependency, and still
requires `pip check`. It does not suppress the dependency check or install an
unversioned package into a failed environment.

The Linux template uses the unchanged hash-bound source helper at
`source/inst/python/lkt_semantic_encode.py`, allowing runtime setup to run alongside
the frozen package build. An installed helper path with the same verified hash is
also a valid explicit selection. Replace only the template's
SIF-hash placeholder with the live verified value, and review all actual paths and
hashes before staging. No historical source/protocol/model is overwritten.

## Execution boundary

The host controller is compatible with Python3.6 and imports only its standard
library. It hashes inputs and downloads/extracts the verified CPython archive.
It never executes that interpreter on the CentOS host. Every interpreter version
check, venv operation, pip resolution/installation, ML import and encoder call
runs inside the verified R4.5.3 SIF with Singularity `--cleanenv --no-home` and
absolute workspace/job-scratch binds. The worker rejects a different Python
version, prefix, architecture or job context.

Stage the independent runtime setup sources under `runtime/setup-sources`, both
original34 and Linux35 lock JSON files, the explicit Linux dependency contract,
authentic probe and reviewed configuration; create Slurm's `logs/` before submission. The coordinator's reviewed
command is:

```bash
cd /project/ppavlik/pfa-mechanism-study-20261110
sbatch runtime/setup-sources/setup-pfa-semantic-runtime.slurm \
  "$PWD/runtime/setup-sources/setup-pfa-semantic-runtime.py" \
  "$PWD/runtime/semantic-runtime-config.json"
```

The wrapper requests one CPU, 8 GB memory and one hour in `acomputeq`. This
example is not a submission performed by preparing the source.

The runtime directory must be fresh. It retains only the managed `python/`
interpreter and one `venv/`. All downloaded archives, wheels, pip resolver reports,
compiler/cache paths, temporary HOME, Hugging Face caches and probe outputs live
under `/tmp/pfa-semantic-JOBID-*` and are removed on exit. Pip uses `--no-compile`
and Python uses `-B`/`PYTHONDONTWRITEBYTECODE=1`, so bytecode caches are not retained.
No source builds are permitted: every locked distribution must have a compatible
Linux wheel or setup fails without changing its requested version.

The included bootstrap pip is used solely to install exact pip25.0.1. That pinned
pip resolves the remaining locked artifacts using **only PyPI** for non-Torch
packages and **only the official PyTorch CPU index** for torch2.6.0+cpu. There is
no extra-index lookup or transitive version selection. Resolved official wheel
URLs and published SHA256 hashes are frozen, downloaded, hash-checked and installed
with `--no-index --no-deps --require-hashes`. The exact distribution set and
versions must match the exact derived Linux35 lock, preserving all original34
versions, and `pip check` must pass.

The total persistent home+project ceiling remains **10,000,000,000 bytes**. The
managed runtime ceiling is **2,000,000,000 bytes**; the template requires
3,000,000,000 bytes of free persistent headroom before starting. The controller
checks storage every15 seconds during its owned job and terminates only that
installer process group if either ceiling is exceeded. A failed directory is not
accepted or partially resumed. Retain failed logs/metadata; cleanup/retry belongs
to the coordinator. At the reported5,931,556,864-byte snapshot, the model is already
included and this headroom is below the4,068,443,136 bytes remaining. The actual
Linux installed size is measured, never inferred from the Windows venv size.

## Actual encoder and later native package tests

After verifying Python and all35 Linux distributions, the worker imports real CPU
Torch2.6.0+cpu and numpy2.5.4. It calls the unchanged hash-bound helper twice on the
frozen authentic probe with the already staged MPNet weights. The helper enforces
CPU float32, one thread, seed1, deterministic algorithms, local-files-only,
no remote code, normalized768-coordinate output and maximum length384. Both
outputs must be byte-identical, finite, correctly shaped and normalized within
1e-5. The empty response must produce a valid finite normalized vector.

Runtime setup sources are independent of the already frozen LKT package/archive
and its source manifest. Deploy updated installers to `runtime/setup-sources`,
bind their actual maintained source hashes in the reviewed runtime configuration,
and record those separately. Do not rewrite the active package manifest, frozen
source tree, original34 lock, previous failure logs or receipts.

Small per-job receipts record the official interpreter and wheel provenance,
complete installed versions, Python executable/prefix/hash, SIF/lock/helper/model
identities, probe norms, exact repeated-output hash, measured storage and scratch
removal. A generated `semantic-test-config-JOBID.json` points native R tests at
the verified Linux environment and installed helper. This verifies the real
encoder; it does not claim bitwise parity with historical Windows embeddings.

Before research fits, run the actual maintained package integration test inside
the same verified SIF/Slurm runtime, with the verified R library and:

```text
LKT_SEMANTIC_TEST_CONFIG=/absolute/runtime/provenance/semantic-test-config-JOBID.json
PYTHONDONTWRITEBYTECODE=1
PYTHONNOUSERSITE=1
HF_HUB_OFFLINE=1
TRANSFORMERS_OFFLINE=1
TOKENIZERS_PARALLELISM=false
OMP_NUM_THREADS=1
MKL_NUM_THREADS=1
OPENBLAS_NUM_THREADS=1
Rscript --vanilla /absolute/source/LKT/tests/gru-semantic-input.R
```

Keep those cache/isolation/thread environment variables in later Slurm jobs too.
The semantic package test must use real MPNet encoding; no fake helper or cached
replacement demonstrates native capability. Source/AST parsing alone does not
establish installation, encoder execution, package integration or fit readiness.

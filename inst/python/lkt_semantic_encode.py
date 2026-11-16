"""Offline, hash-bound sentence encoding for LKT's semantic response inputs.

This executable never downloads a model or substitutes another implementation.
The caller binds both this script's hash and the model manifest's hash.
"""
import argparse
import hashlib
import importlib.metadata
import json
import os
from pathlib import Path
import sys

def sha256_file(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--model-dir", required=True, type=Path)
    parser.add_argument("--manifest-sha256", required=True)
    parser.add_argument("--request", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    root = args.model_dir.resolve(strict=True)
    manifest_path = root / "lkt-semantic-manifest.json"
    digest = sha256_file(manifest_path)
    if digest != args.manifest_sha256.lower():
        raise ValueError("Semantic model manifest SHA256 differs from the saved contract.")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    if manifest.get("schema") != 1 or manifest.get("embedding_dim") != 768:
        raise ValueError("Unsupported semantic encoder manifest schema or width.")
    for record in manifest["files"]:
        path = (root / record["path"]).resolve(strict=True)
        if not path.is_relative_to(root):
            raise ValueError("Semantic artifact path escapes the selected model directory.")
        if path.stat().st_size != record["bytes"] or sha256_file(path) != record["sha256"]:
            raise ValueError("Semantic artifact hash or length differs: " + record["path"])
    required_packages = manifest["package_versions"]
    if not required_packages:
        raise ValueError("The semantic runtime versions must be explicitly pinned.")
    versions = {name: importlib.metadata.version(name) for name in required_packages}
    if versions != required_packages:
        raise ValueError("Semantic runtime package versions differ from the saved contract.")
    request = json.loads(args.request.read_text(encoding="utf-8-sig"))
    texts = request.get("texts")
    if not isinstance(texts, list) or any(not isinstance(text, str) for text in texts):
        raise ValueError("texts must be an array of authentic strings; null is not a string.")
    settings = manifest["encoding"]
    expected = {"device": "cpu", "dtype": "float32", "normalize_embeddings": True,
                "batch_size": 32, "seed": 1, "num_threads": 1, "trust_remote_code": False}
    if settings != expected or manifest["max_seq_length"] != 384:
        raise ValueError("Unsupported semantic encoding settings.")
    os.environ["HF_HUB_OFFLINE"] = "1"
    os.environ["TRANSFORMERS_OFFLINE"] = "1"
    os.environ["TOKENIZERS_PARALLELISM"] = "false"
    import numpy as np
    import torch
    from sentence_transformers import SentenceTransformer
    torch.set_num_threads(1)
    torch.manual_seed(1)
    torch.use_deterministic_algorithms(True)
    model = SentenceTransformer(str(root), device="cpu", local_files_only=True,
                                trust_remote_code=False, model_kwargs={"use_safetensors": True})
    model.max_seq_length = 384
    model.eval()
    with torch.inference_mode():
        vectors = model.encode(texts, batch_size=32, show_progress_bar=False,
                               convert_to_numpy=True, normalize_embeddings=True) if texts else np.empty((0, 768), dtype=np.float32)
    if vectors.shape != (len(texts), 768) or not np.isfinite(vectors).all():
        raise ValueError("Semantic encoder returned the wrong shape or nonfinite values.")
    payload = {"embeddings": vectors.tolist(), "embedding_dim": 768,
               "model_id": manifest["model_id"], "revision": manifest["revision"],
               "manifest_sha256": digest, "package_versions": versions}
    # Commit only complete, finite responses. An exception never leaves partial JSON.
    temporary = args.output.with_name(args.output.name + ".partial")
    temporary.write_text(json.dumps(payload, ensure_ascii=False, allow_nan=False), encoding="utf-8")
    temporary.replace(args.output)

if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        print(type(error).__name__ + ": " + str(error), file=sys.stderr)
        raise SystemExit(1)

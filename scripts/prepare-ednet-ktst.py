"""Run the published KTST EdNet cleaning and student-split functions unchanged.

The neural model remains in LKT. This adapter preserves one row per original
question response and exports the authors' five validation folds and fixed test
students, rather than expanding a response into repeated skill-level targets.
"""
from pathlib import Path
import argparse
import hashlib
import json
import random
import sys
import zipfile


def digest(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(8 * 1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


args = argparse.ArgumentParser()
args.add_argument("output", type=Path)
args.add_argument("reference", type=Path)
options = args.parse_args()
output = options.output.resolve()
reference = options.reference.resolve()
raw = output / "raw" / "ednet"
assert not raw.exists(), "A prior raw selection must not be overwritten."
raw.mkdir(parents=True)

# The authors shuffle the same integer student range with seed 2, taking the
# first 5,000 files that exist. Select those exact archive members before
# extraction so we do not create 784,309 unused files on the user's computer.
with zipfile.ZipFile(output / "downloads/EdNet-KT1.zip") as archive:
    members = {m.filename: m for m in archive.infolist()}
    assert "KT1/u1.csv" in members
    candidates = list(range(840473))
    random.seed(2)
    random.shuffle(candidates)
    selected = [u for u in candidates if f"KT1/u{u}.csv" in members][:5000]
    assert len(selected) == 5000
    (raw / "KT1").mkdir()
    for uid in selected:
        name = f"KT1/u{uid}.csv"
        (raw / name).write_bytes(archive.read(name))
with zipfile.ZipFile(output / "downloads/EdNet-Contents.zip") as archive:
    (raw / "contents").mkdir()
    for name in archive.namelist():
        if name.startswith("contents/") and name.endswith(".csv"):
            assert len(Path(name).parts) == 2
            (raw / name).write_bytes(archive.read(name))

# Import the intact preprocessing package directly. Its functions do not need
# the unrelated neural-model dependencies imported by the top-level pyKT package.
# No author source, sampling rule, filter or split function is patched.
sys.path.insert(0, str(reference / "pykt-toolkit/pykt"))
from preprocess.ednet_preprocess import read_data_from_csv
from preprocess.split_datasets import read_data, train_test_split, KFold_split
import numpy as np
import pandas as pd
import tqdm

prepared = output / "prepared"
assert not prepared.exists()
prepared.mkdir()
author_files = [
    "pykt-toolkit/pykt/preprocess/__init__.py",
    "pykt-toolkit/pykt/preprocess/data_proprocess.py",
    "pykt-toolkit/pykt/preprocess/ednet_preprocess.py",
    "pykt-toolkit/pykt/preprocess/split_datasets.py",
    "pykt-toolkit/pykt/preprocess/utils.py",
]
source_hashes = {name: digest(reference / name) for name in author_files}
read_data_from_csv(str(raw).replace("\\", "/"),
                   str(raw / "data.txt").replace("\\", "/"), dataset_name="ednet")
sequences, keys = read_data(str(raw / "data.txt"))
assert set(sequences["uid"].astype(int)).issubset(selected)
train_valid, test = train_test_split(sequences, 0.2)
folds = KFold_split(train_valid, 5)
assignments = folds[["uid", "fold"]].copy()
assignments = pd.concat([assignments, test[["uid"]].assign(fold=-1)], ignore_index=True)
assert assignments["uid"].is_unique
assignments["uid"] = assignments["uid"].astype(int)
assignments = assignments.rename(columns={"uid": "participant", "fold": "ktst_validation_fold"})
assignments.to_csv(prepared / "participant-assignments.csv", index=False)

# Export only histories accepted by the authors' minimum-length and response
# checks. Independently compare every exported sequence against their parser,
# including original tag order, timestamps, response order and correctness.
data = pd.read_csv(raw / "ednet_sample_process.csv")
assert list(pd.read_csv(raw / "ednet_sample.csv")["user_id"].drop_duplicates()) == selected
data = data[data.user_id.isin(assignments.participant)].copy()
data = data.sort_values(["user_id", "timestamp", "index"], kind="stable")
lookup = sequences.set_index(sequences.uid.astype(int))
for uid, rows in data.groupby("user_id", sort=False):
    expected = lookup.loc[uid]
    for source, field in [("question_id", "questions"), ("tags", "concepts"),
                          ("correct", "responses"), ("timestamp", "timestamps")]:
        assert ",".join(rows[source].astype(str)) == expected[field], (uid, field)
data = data.merge(assignments, left_on="user_id", right_on="participant", how="left", validate="many_to_one", sort=False)
data["ktst_position"] = data.groupby("user_id", sort=False).cumcount() + 1
export = pd.DataFrame({
    "Anon.Student.Id": data.user_id.astype(str), "KC..Default.": data.question_id,
    "KC..Cluster.": data.tags, "CF..ansbin.": data.correct.astype(int),
    "CF..Time.": data.timestamp / 1000, "comparison_source_row": data["index"] + 1,
    "ktst_validation_fold": data.ktst_validation_fold.astype(int),
    "ktst_position": data.ktst_position,
    "original_timestamp_ms": data.timestamp,
})
assert export.comparison_source_row.is_unique and not export.isna().any().any()
assert set(export["CF..ansbin."]) == {0, 1}
export.to_csv(prepared / "ednet-lkt.csv", index=False)
record = dict(ktst_commit=reference.name.removeprefix("kt_set_transformers-"),
              python=sys.version, numpy=np.__version__, pandas=pd.__version__, tqdm=tqdm.__version__,
              initial_students=len(selected), retained_students=len(assignments), responses=len(export),
              items=export["KC..Default."].nunique(), skill_combinations=export["KC..Cluster."].nunique(),
              test_students=len(test), test_responses=int((export.ktst_validation_fold == -1).sum()),
              validation_students={str(k): int((assignments.ktst_validation_fold == k).sum()) for k in range(5)},
              author_source_sha256=source_hashes,
              archive_sha256={name: digest(output / "downloads" / name) for name in ["EdNet-KT1.zip", "EdNet-Contents.zip", "ktst-source.tar.gz"]},
              exported_data_sha256=digest(prepared / "ednet-lkt.csv"),
              participant_assignments_sha256=digest(prepared / "participant-assignments.csv"),
              first_response="Retained for causal history; available as a separate evaluation exclusion.",
              skill_representation="Original full skill-tag combination is one category for the unchanged GRU skill input.",
              cleaning="Unchanged author functions; no rare-item filter, deduplication or student-history cap added.")
assert {name: digest(reference / name) for name in author_files} == source_hashes
(prepared / "provenance.json").write_text(json.dumps(record, indent=2) + "\n")
print(json.dumps({k: v for k, v in record.items() if k not in ["author_source_sha256", "archive_sha256"]}, indent=2))

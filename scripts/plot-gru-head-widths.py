"""Plot completed validation results; arguments: verified results directory."""

import csv
from pathlib import Path
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt


output = Path(sys.argv[1])
assert (output / "verification.txt").is_file()
with (output / "validation_width_summary.csv").open(newline="") as stream:
    rows = list(csv.DictReader(stream))
with (output / "selected_widths.csv").open(newline="") as stream:
    chosen = {row["family"]: int(row["head_width"]) for row in csv.DictReader(stream)}

# Plot initialization variability explicitly as a standard deviation, rather than
# suggesting that three seeds measure uncertainty over new student samples.
fig, axes = plt.subplots(1, 3, figsize=(11, 4.3), layout="constrained")
for ax, (family, label) in zip(
    axes,
    [("matched", "Matched engineered features"), ("raw", "Raw item / skill / time"), ("augmented", "Augmented")],
):
    values = sorted((row for row in rows if row["family"] == family), key=lambda row: int(row["head_width"]))
    assert len(values) == 4
    widths = [int(row["head_width"]) for row in values]
    means = [float(row["validation_log_loss"]) for row in values]
    deviations = [float(row["validation_seed_sd"]) for row in values]
    ax.errorbar(widths, means, yerr=deviations, fmt="o-", capsize=4, color="#245b83", linewidth=1.8)
    selected_index = widths.index(chosen[family])
    ax.scatter([chosen[family]], [means[selected_index]], color="#bd6700", s=90, zorder=5,
               label=f"Selected: {chosen[family]} units")
    ax.set_xscale("log", base=2)
    ax.set_xticks(widths, labels=[str(width) for width in widths])
    ax.set_xlabel("Head hidden units")
    ax.set_ylabel("Validation log loss (lower is better)")
    ax.set_title(label, fontsize=11)
    ax.ticklabel_format(axis="y", style="plain", useOffset=False)
    ax.grid(axis="y", alpha=0.2)
    ax.spines[["top", "right"]].set_visible(False)
    ax.legend(loc="best", frameon=False, fontsize=9)
fig.suptitle("Prediction-head width: mean validation loss across three seeds", fontsize=13)
fig.supxlabel("Error bars: ±1 initialization-seed SD. Each panel has its own vertical scale. Same participant split throughout.", fontsize=9)
fig.savefig(output / "validation_head_widths.png", dpi=180)
fig.savefig(output / "validation_head_widths.pdf")
plt.close(fig)

# Add the locally rendered figure to the verified report without changing any
# scores or selection records. Re-running this renderer must not duplicate it.
report = output / "report.html"
document = report.read_text(encoding="utf-8")
if 'id="validation-width-figure"' not in document:
    figure = ('<figure id="validation-width-figure"><img src="validation_head_widths.png" '
              'alt="Validation log loss versus head width for all three input versions" '
              'style="width:100%;height:auto"></figure>')
    document = document.replace("<h2>Selected widths</h2>", figure + "<h2>Selected widths</h2>")
    report.write_text(document, encoding="utf-8")
print(output / "validation_head_widths.png")

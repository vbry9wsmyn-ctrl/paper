from __future__ import annotations

import json
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np


ROOT = Path(__file__).resolve().parents[1]
DATA = json.loads((ROOT / "mathematica/out/strategy_data.json").read_text())
BLUE = "#5e81b5"
SHADE = "#ccd6e8"

LABELS = {
    "fig1a": [
        ("RR", 0.13, 0.50, 11), ("RA", 0.37, 0.52, 11),
        ("AA", 0.66, 0.50, 11), ("AR", 0.970, 0.52, 10),
        ("RR", 0.10, 0.95, 11), ("RA", 0.37, 0.95, 11),
        ("AA", 0.70, 0.95, 11), ("AR", 0.970, 0.95, 10),
    ],
    "fig1b": [
        ("RR", 0.12, 0.43, 11), ("RA", 0.39, 0.44, 11),
        ("AA", 0.66, 0.44, 11), ("AR", 0.92, 0.43, 11),
        ("RR", 0.10, 0.95, 11), ("RA", 0.43, 0.95, 11),
        ("AA", 0.75, 0.95, 11), ("AR", 0.94, 0.95, 11),
    ],
    "fig2": [
        ("RR", 0.08, 0.45, 11), ("RA", 0.30, 0.45, 11),
        ("AA", 0.65, 0.45, 11), ("RR", 0.06, 0.91, 11),
        ("RA", 0.36, 0.91, 11), ("AA", 0.78, 0.96, 11),
    ],
}


def render(name: str) -> None:
    region = DATA[name]
    adoption = np.asarray(region["adoption"], dtype=float)
    polygon = np.vstack(([adoption[0, 0], 0], adoption, [adoption[-1, 0], 0]))

    fig, ax = plt.subplots(figsize=(4.2, 4.2))
    fig.subplots_adjust(left=0.15, right=0.97, bottom=0.14, top=0.97)
    ax.fill(polygon[:, 0], polygon[:, 1], facecolor=SHADE, edgecolor="none", zorder=0)

    for points in region["boundaries"]:
        line = np.asarray(points, dtype=float)
        ax.plot(line[:, 0], line[:, 1], color=BLUE, linewidth=0.85, zorder=2)

    if name == "fig2":
        cutoff = float(DATA["gammaHat"])
        ax.axhline(cutoff, color="#555555", linewidth=0.8,
                   linestyle=(0, (2, 2)), zorder=1)
        ax.text(0.68, cutoff + 0.025, r"$\hat{\gamma}\;(c_g=0)$",
                ha="left", va="bottom", fontsize=11)

    for label, x, y, size in LABELS[name]:
        ax.text(x, y, label, ha="center", va="center", fontsize=size, color="black")

    ax.set(xlim=(0, 1), ylim=(0, 1), xticks=np.linspace(0, 1, 6),
           yticks=np.linspace(0, 1, 6))
    ax.set_aspect("equal")
    ax.set_xlabel(r"$\lambda$", fontsize=15)
    ax.set_ylabel(r"$\gamma$", fontsize=15)
    ax.tick_params(direction="in", top=True, right=True, labelsize=10.5, length=3)
    for spine in ax.spines.values():
        spine.set_linewidth(0.8)

    output = ROOT / {"fig1a": "Fig1a", "fig1b": "Fig1b", "fig2": "Fig2"}[name]
    fig.savefig(output.with_suffix(".pdf"), format="pdf")
    plt.close(fig)
    print(output.with_suffix(".pdf"))


for figure_name in ("fig1a", "fig1b", "fig2"):
    render(figure_name)


def render_high_commission_zoom() -> None:
    fig, axes = plt.subplots(1, 2, figsize=(8.4, 3.6), sharey=True)
    for ax, key, xlim, title, label_x in (
        (axes[0], "fig1a", (0.925, 0.945), r"$g=0.3$", (0.930, 0.935, 0.941)),
        (axes[1], "fig1b", (0.825, 0.865), r"$g=0.6$", (0.832, 0.843, 0.855)),
    ):
        region = DATA[key]
        adoption = np.asarray(region["adoption"], dtype=float)
        ax.fill_between(adoption[:, 0], 0, adoption[:, 1], color=SHADE, zorder=0)
        for points in region["boundaries"]:
            line = np.asarray(points, dtype=float)
            ax.plot(line[:, 0], line[:, 1], color=BLUE, linewidth=1, zorder=2)
        for label, x in zip(("AA", "RA", "AR"), label_x):
            ax.text(x, 0.55, label, ha="center", va="center", fontsize=10)
        ax.set(xlim=xlim, ylim=(0, 1), title=title,
               xticks=np.linspace(xlim[0], xlim[1], 5))
        ax.set_xlabel(r"$\lambda$", fontsize=13)
        ax.tick_params(direction="in", top=True, right=True, labelsize=9, length=3)
        for spine in ax.spines.values():
            spine.set_linewidth(0.8)
    axes[0].set_ylabel(r"$\gamma$", fontsize=13)
    fig.subplots_adjust(left=0.08, right=0.98, bottom=0.16, top=0.90, wspace=0.14)
    output = ROOT / "high_commission_zoom.pdf"
    fig.savefig(output, format="pdf")
    plt.close(fig)
    print(output)


render_high_commission_zoom()

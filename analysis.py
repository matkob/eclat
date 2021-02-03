import pathlib
import re

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

DATA_DIR = "data"
GRAPH_DIR = "graph"
pathlib.Path(GRAPH_DIR).mkdir(parents=True, exist_ok=True)
mpl.rcParams['agg.path.chunksize'] = 10000


def read_results(path):
    with open(path) as file:
        lines = file.readlines()
    all_metrics = []
    for line in lines:
        metrics = re.findall("metrics: .*", line)[0][9:]
        all_metrics.append(parse_metrics(metrics))
    return pd.DataFrame(data=all_metrics)


def parse_metrics(metrics):
    split = [y.split("=") for y in metrics.split(" ")]
    return {x[0]: float(x[1]) for x in split}


def create_plots(data: pd.DataFrame, metric: str):
    data[metric].hist()
    save_plt_fig(f"{GRAPH_DIR}/hist_{metric}.png")
    data.plot.scatter(x="LiftIndex", y=metric)
    save_plt_fig(f"{GRAPH_DIR}/plot_{metric}.png")


def save_plt_fig(path):
    plt.savefig(path)
    plt.close()


def plot_corr_mtx(data: pd.DataFrame):
    corr = data.corr()
    plt.subplots(figsize=(11, 9))
    sns.heatmap(
        corr,
        mask=np.triu(np.ones_like(corr, dtype=bool)),
        cmap=sns.diverging_palette(230, 20, as_cmap=True),
        vmax=.3,
        center=0,
        square=True,
        linewidths=.5,
        cbar_kws={"shrink": .5}
    )


rules = [
    read_results(f"{DATA_DIR}/agaricus-lepiota-out_3k_0.5.data"),
    read_results(f"{DATA_DIR}/car-out_80_0.2.data"),
    read_results(f"{DATA_DIR}/tic-tac-toe-out_20_0.2.data")
]
results = pd.concat(rules, ignore_index=True)
plot_corr_mtx(results)
save_plt_fig(f"{GRAPH_DIR}/correlation.png")
print(results["LiftIndex"].describe())
for metric in [m for m in results.columns if m != "LiftIndex"]:
    print(f"analysing {metric}")
    df = results[["LiftIndex", metric]]
    print(df[metric].describe())
    create_plots(df, metric)

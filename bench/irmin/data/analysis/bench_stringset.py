#!/usr/bin/env python3

import sys
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

if len(sys.argv) != 3:
    sys.exit("usage: ./bench_stringset.py <dataset.csv> <output.svg>")

df = pd.read_csv(sys.argv[1])
figure_file = sys.argv[2]

fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(13, 5))

sns.lineplot(data = df, ax = ax1, x = 'entries', y = 'reachable_words', hue = 'implementation')
sns.lineplot(data = df, ax = ax2, x = 'entries', y = 'allocated_words', hue = 'implementation')
sns.boxplot(data = df, ax = ax3, x = 'implementation', y = 'time(ns)')

ax3.set_yscale('log')
ax1.grid(which ='major', color='gray', linewidth=0.2)
ax2.grid(which ='major', color='gray', linewidth=0.2)
ax3.grid(which ='minor', color='gray', linewidth=0.2)

fig.suptitle('Relative performance of implementations')
fig.tight_layout()

print('Saving to \'%s\'' % figure_file)
fig.savefig(figure_file)

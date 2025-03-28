import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Read the CSV data
data = pd.read_csv('data.csv')
data2 = pd.read_csv('data2.csv')

ind = np.arange(len(data))
width = 0.3

plt.figure(figsize=(10,5))

# Specify the values of blue bars (height)
blue_bar = (23, 25, 17)
# Specify the values of orange bars (height)
orange_bar = (19, 18, 14)

combined = data.merge(data2, on="Category", how='left')
print (combined)
# Plot the bar chart
plt.bar(ind, combined['Value_x'], width, label = 'callcc 0.3')
plt.bar(ind+width,combined['Value_y'], width, label = 'chez 9.5.8')

# Add labels and title
plt.xlabel('Benchmark')
plt.ylabel('runtime (seconds)')
plt.xticks(ind + width/2, combined['Category'], rotation=90, ha='right')
plt.title('Callcc vs. Chez runtime (lower is better)')
plt.tight_layout()
plt.legend(loc='best')

# Show the plot
#plt.show()
plt.savefig('bar_chart.png')


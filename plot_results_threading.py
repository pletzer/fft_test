import pandas
from matplotlib import pylab

df = pandas.read_csv('results_threading.csv')
print(df)

styles = ['r--', 'c--', 'g--',]
i = 0
legs = []
for colname in df.columns:
	if colname == 'nth':
		continue
	print(colname)
	print(df['nth'])
	print(df[colname])
	print(i)
	print(styles[i])
	legs.append(colname)
	pylab.loglog(df['nth'], df[colname], styles[i])
	i += 1
pylab.title('sine expansion 20000')
pylab.legend(legs)
pylab.xlabel('OMP_NUM_THREADS')
pylab.ylabel('time [s]')
pylab.show()
import pandas
from matplotlib import pylab

df = pandas.read_csv('results.csv')
print(df)

styles = ['r-', 'r--', 'c-', 'c--', 'g-', 'g--', 'b--', 'k--', 'm--']
i = 0
legs = []
for colname in df.columns:
	if colname == 'n':
		continue
	print(colname)
	print(df['n'])
	print(df[colname])
	print(i)
	print(styles[i])
	legs.append(colname)
	pylab.loglog(df['n'], df[colname], styles[i])
	i += 1
pylab.title('sine expansion')
pylab.legend(legs)
pylab.xlabel('n')
pylab.ylabel('time [s]')
pylab.show()
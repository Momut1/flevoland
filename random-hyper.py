import numpy as np
import matplotlib.pyplot as plt

np.random.seed(9)

rl = np.random.uniform(low=4.0000, high=0.0000,size =(100,))
print(len(rl))
print(rl)

rl2 = np.around(rl,decimals=5)
print(len(np.unique(rl2)))
print(rl2)

rm = np.random.uniform(low=0.9, high=1.0000,size =(100,))
print(rm)

rm2 = np.around(rm,decimals=5)
print(len(np.unique(rm2)))
print(rm2)

lr = 10**(-rl2)
m = rm2

[print("({}, {})".format(lr[i], m[i]))for i in range(len(rl2))]
[print(lr[i]) for i in range(len(rl2))]
[print(m[i])for i in range(len(rl2))]



    

fig = plt.figure()  
ax = fig.add_subplot(111)
s = ax.scatter(m, lr)
ax.set_xlabel('Momentum')
ax.set_ylabel('Learning rate')
ax.set_yscale('log')
#ax.set_xscale('log')
plt.show()
print(3)
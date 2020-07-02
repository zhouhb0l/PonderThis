import numpy as np
import scipy.linalg as la

F=[0,1]
k=0
while(k<50):
    k=k+1
    n=len(F)
    F.append(F[n-1]+F[n-2])
print(F)


#X=np.array([[1,1,1],[0,1,0],[0,2,1]])
#Y=np.array([1,1,1])

#ALFMDTV
X=np.array([[1,1,1,0,0,0,0],[0,1,0,1,1,0,0],[1,0,1,0,0,0,0],[0,0,0,2,0,0,0],[0,1,0,0,1,1,0],[1,0,0,0,0,1,1],[1,0,0,0,0,0,2]])
Y=np.array([1,1,1,1,1,1,1])
print("-------F(n)F(n+1)------")
a=[F[n]*F[n+1] for n in range(50)]
print(a)

X=np.array([[0,0,1,1,0,0,0],[0,1,0,1,0,0,0],[0,0,1,1,0,0,0],[2,0,1,0,0,0,1],[0,1,0,1,0,0,0],[1,0,0,0,1,1,0],[1,0,1,0,0,1,0]])
Y=[1,1,1,1,1,1,1]
print(X)

b=[0]
for i in range(50):
    b.append(Y[1])
    Y=X@Y

print('-----b----')
print(b)





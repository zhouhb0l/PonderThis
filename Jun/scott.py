import sympy as sy
tg=3031634148236289733373855928919180891127808

m=576
m=tg
f=sy.factorint(m)
print(m,"factorint of m",f)

div=sy.divisors(m);
divp=[x+1 for x in div]
for x in divp.copy():
  if(not sy.isprime(x)):
     divp.remove(x)
divp.sort()
#print(divp)
#print("p+1",divp)
fdiv={}
for x in divp:
    fdiv[x]=sy.factorint(x-1)
#print("fdiv",fdiv)
print("Start to find solution")

def value(pf):
    v=1
    for x in pf:
      v=v*pow(x,pf[x])
    return v



def get_n0(f,fdiv,pf):
   divisible=True 
   #calculate e1=l^(y-1)*(l-1) in factorial form
   e1={}
   for x in pf:
       y=pf[x]
       if x in e1.keys() and y>1:
           e1[x]=e1[x]+y-1
       if x not in e1.keys() and y>1:
           e1[x]=y-1
       z=fdiv[x]
       for s in z:
           if s in e1.keys():
               e1[s]=e1[s]+z[s]
           if s not in e1.keys():
               e1[s]=z[s]
#   print("e1",e1)
#   calculation of n0
   n0=f.copy()
   for x in e1:
       if x not in f:
           return (False,{})
       else:
           if e1[x]>f[x]:
               return (False,{})
           else:
               n0[x]=f[x]-e1[x]
   for x in n0.copy():
       if n0[x]==0:
           n0.pop(x)
   return (True,n0)

#-----------------
def IsDivisible(a,b):
    if len(a)==0:
        return True
    for x in a:
        if x not in b.keys():
            return False
        elif a[x]>b[x]:
            return False
    return True

def get_prime_set(n0,fdiv,pf):
    S=[]
    if len(pf)==0:
        pmax=1
    else:
        pmax=max(pf.keys())
    for x in fdiv:
      if x>pmax and IsDivisible(fdiv[x],n0):
          S.append(x)
    return S


def nextG(path,fdiv):
 #print("Investigating path",path[0],"in direction of ",path[1])
 pf=path[0]
 di=path[1]
 if len(pf)==0 and di=='b':
     return (pf,di)
 if len(pf)==0:
     pmax=1
 else:
     pmax=max(pf.keys())

 if di=='p':  #go to parent vertex
     nf=pf.copy()
     nf.pop(pmax)
     return (nf,'b') #go to parent vertex then should looking for brother vertex

 if di=='c': #go to child vertex
     remain=get_n0(f,fdiv,pf)
     if remain[0]: #divisible
         n0=remain[1]
         if len(n0)==0:
             vv=value(pf)
             #if vv>9099200014783811252697811978568673336955092 and vv<9099400014783811252697811978568673336955092:
             print("solution found",pf)
             print("value:",vv)
              #   print("sigma:",sy.divisor_sigma(vv,1)-vv)
             nf=pf.copy()
             return (nf,'b')
         else:
             S=get_prime_set(n0,fdiv,pf)
             if len(S)>0: #non-empty set
                 nf=pf.copy()
                 nf[S[0]]=1
                 return (nf,'c') #continuous go to its child vertex
             else:
                 nf=pf.copy()
                 return (nf,'b') #empty set, then go to brother vertex
     else: #in case remain[0]=False
         nf=pf.copy()
         return (nf,'p') #not divisible, so go back to parent vertex

 if di=='b':#go to brother vertex
     #first try y->y+1
     pff=pf.copy()
     pff[pmax]=pff[pmax]+1
     remain=get_n0(f,fdiv,pff)
     if remain[0]: #can so just find the brother
         nf=pff.copy()
         return (nf,'c')#find the brother already
     else: #go back to parent to find brothers
         pff=pf.copy() 
         pff.pop(pmax)#pff is the parent of pf
         n0=get_n0(f,fdiv,pff)[1]
         S=get_prime_set(n0,fdiv,pff)
    #     print("S",S)
         Sind=S.index(pmax)
         if Sind==len(S)-1: #do not have such S
             nf=pff.copy()
             return (nf,'b')#then go to brother of parent vertex instead
         else:
             nf=pff.copy()
             nf[S[Sind+1]]=1
             return (nf,'c') #find a brother vertex

while (True):
 print("###################")
 print("###################")
 print("###################")
 print("###################")
 i=int(input("initial path: "))
 j=int(input("cycles:"))
 if(i==0):
     exit()
 pf=sy.factorint(i)
 print("initial path:",pf)
 path=(pf,'c')
 for i in range(j):
   path=nextG(path,fdiv)
 print("Final path",path[0])
 print(value(path[0]))






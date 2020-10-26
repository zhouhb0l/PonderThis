import numpy as np
import random
def getNumberOfPath(graph,length):
    result=[]
    exitNode=len(graph)-1
    pan=[0] # store the possible arrived nodes (pan) after step n. it start with node 0. 
    for step in range(length):
        #panadd=[y+1 for y in pan]
        #print(step,panadd)
        nextPan=[]
        for node in pan:
            for nextNode,i in enumerate(graph[node]):
                if i==1:
                    nextPan.append(nextNode)
        pan=nextPan.copy()
        result.append(pan.count(exitNode))
    return result
Nnode=13
graph=np.zeros((Nnode,Nnode),dtype=int)
f=open("answer.txt","r")
for i in range(Nnode):
    line=f.readline().replace(" ",'')
    for j,value in enumerate(line):
        if j<Nnode:
          #print(i,j,value)
          graph[i][j]=value
f.close
print(graph)
resultList=getNumberOfPath(graph,18)
print(resultList)











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

def compare(listA,listB):
    for i in range(len(listA)):
        if listA[i]>listB[i] :
            return (1,i)
        if listA[i]<listB[i] :
            return (-1,i)
    return (0,i)

targetList=[0,2,2,5,8,17,32,65,128,257,512,1025,2048]
Nnode=15

graph=np.zeros((Nnode,Nnode),dtype=int)
f=open("graph.txt","r")
for i in range(Nnode):
    line=f.readline().replace(" ",'')
    for j,value in enumerate(line):
        if j<Nnode:
          #print(i,j,value)
          graph[i][j]=value
f.close
print(graph)
resultList=getNumberOfPath(graph,13)
print(resultList)

#graph=np.zeros((Nnode,Nnode),dtype=int)

graphTmp=graph.copy()
preGraph=graph.copy()
preDepth=0
for x in range(200000):
    if x%10000==0:
        graph=graphTmp.copy()
        preGraph=graph.copy()
        preDepth=0
    resultList=getNumberOfPath(graph,preDepth+2)
    com=compare(resultList,targetList)
    depth=com[1]
    #print(x,resultList,depth,preDepth)
    if depth>=preDepth:
        preGraph=graph.copy()
        if depth>preDepth:
          preDepth=depth
          print("depth",depth,x)
          if depth>=11:
              print(resultList)
              print(preGraph)
    if depth<preDepth:
        graph=preGraph.copy()

    if com[0]==0 and depth>15:
           print("Result found!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
           print(graph)
           break
    else:
           i=random.randint(0,Nnode-2)
           j=random.randint(0,Nnode-1)
        
    if com[0]==1:
        while graph[i][j]==0:
           i=random.randint(0,Nnode-2)
           j=random.randint(0,Nnode-1)
        if not (i,j) in [(0,1),(0,2),(1,Nnode-1),(2,Nnode-1)]:
                graph[i][j]=0
    if com[0]==-1:
         while graph[i][j]==1:
           i=random.randint(0,Nnode-2)
           j=random.randint(0,Nnode-1)
         if  (i,j) not in [(0,Nnode-1)] and (i!=1 or np.sum(graph[1])<2) and (i!=2 or np.sum(graph[2])<3):
                graph[i][j]=1

print(graph)











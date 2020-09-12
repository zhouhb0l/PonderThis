import numpy as np
from itertools import permutations

f=open("data11.txt")
dataset=[]
for x in f:
    dataset.append(int(x))

#print(dataset)
#print(dataset[1][1])

def generateRelationMap(data,dimension):
    dataStr=str(data)
    index=0
    RelationMap = [[0 for x in range(dimension)] for x in range(dimension)] 
    for i in range(dimension):
        for j in range(i+1,dimension):
            entry=int(dataStr[index])
            #print(index,entry)
            RelationMap[i][j]=entry
            RelationMap[j][i]=1-entry
            index+=1
    return RelationMap

def generate3cycleSet(RelationMap):
    dimension=len(RelationMap)
    cycleSet=[]
    for i in range(dimension):
        for j in range(i):
            for k in range(j):
                if RelationMap[i][j]==1 and RelationMap[j][k]==1 and RelationMap[k][i]==1:
                    cycleSet.append([i,j,k])
                if RelationMap[i][j]==0 and RelationMap[j][k]==0 and RelationMap[k][i]==0:
                    cycleSet.append([k,j,i])
    return cycleSet


def generateEdgeProfile(cycleSet,dimension):
    edgeProfile=[[0 for x in range(dimension)] for x in range(dimension)]
    for cycle in cycleSet:
        edgeProfile[cycle[1]][cycle[0]]+=1
        edgeProfile[cycle[2]][cycle[1]]+=1
        edgeProfile[cycle[2]][cycle[0]]+=1
        
        edgeProfile[cycle[0]][cycle[1]]+=1
        edgeProfile[cycle[1]][cycle[2]]+=1
        edgeProfile[cycle[0]][cycle[2]]+=1
    return edgeProfile

def generateProfile(cycleSet,edgeProfile):
    profile=[]
    dimension=len(edgeProfile)
    for i in range(dimension):
        profileOfVertice={}
        for cycle in cycleSet:
            if i in cycle:
                ind=cycle.index(i)
                np=cycle[(ind+1)%3]
                pp=cycle[(ind+2)%3]
                triple=(edgeProfile[i][np],edgeProfile[np][pp],edgeProfile[pp][i])
                if triple in profileOfVertice:
                    profileOfVertice[triple]+=1
                else:
                    profileOfVertice[triple]=1
        profile.append(profileOfVertice)
    return profile

def generatePointProfile(profile):
    pointProfile=[]
    evaluated=[]
    for i, p in enumerate(profile):
        if not i in evaluated:
         groupi=[i]
         evaluated.append(i)
         for j,q in enumerate(profile):
            if(p==q and i!=j):
                groupi.append(j)
                evaluated.append(j)
         pointProfile.append(groupi)
    return pointProfile

def searchAm(groupIndex,pointProfile,RelationMap,permIndex,amCount):
    if groupIndex >= len(pointProfile):
        dimension=len(RelationMap)
        permMap=[[0 for x in range(dimension)] for x in range(dimension)]
        for i in range(dimension):
            for j in range(dimension):
                permMap[permIndex[i]][permIndex[j]]=RelationMap[i][j]
        if permMap==RelationMap:
            amCount+=1
            #print("AM:",amCount,permIndex)
        return amCount
    else:
        group=pointProfile[groupIndex]
        permList=list(permutations(group))
        for perm in permList:
            for i,point in enumerate(group):
                permIndex[point]=perm[i]
            amCount=searchAm(groupIndex+1,pointProfile,RelationMap,permIndex,amCount)
        return amCount


for i in range(len(dataset)):
  RelationMap=generateRelationMap(dataset[i],11)
  cycleSet=generate3cycleSet(RelationMap)
  #print(i,len(cycleSet))
  edgeProfile=generateEdgeProfile(cycleSet,len(RelationMap))
  profile=generateProfile(cycleSet,edgeProfile)
  pointProfile=generatePointProfile(profile)
  permIndex=list(range(len(RelationMap)))
  amCount=searchAm(0,pointProfile,RelationMap,permIndex,0)
  if(amCount>1):
      print("for data",i,"In total",amCount)
  if(amCount>50):
      print(RelationMap)
      print(pointProfile)
 



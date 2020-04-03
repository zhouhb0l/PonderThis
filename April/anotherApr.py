import numpy as np

def calEvolutionMatrix(AdjacentMatrix):
  dimension=len(AdjacentMatrix)
  EvolutionMatrix=np.zeros([2**dimension,2**dimension])+1
  for i in range(2**dimension):
      for j in range(2**dimension):
          row=[(i//2**k)%2 for k in range(dimension)]    
          col=[(j//2**k)%2 for k in range(dimension)]    
          for k in range(dimension):
              factor=col[k]-(1-row[k])*(2*col[k]-1)*(1-p)**(np.dot(AdjacentMatrix[k],row))
              EvolutionMatrix[j][i]=EvolutionMatrix[j][i]*factor

  return EvolutionMatrix
                
AdjacentMatrix=[[0,1,1,1,0],[1,0,0,0,1],[1,0,0,1,0],[1,0,1,0,1],[0,1,0,1,0]]
p=0.1
evolutionMatrix=calEvolutionMatrix(AdjacentMatrix)

InitialState=np.zeros((2**len(AdjacentMatrix)))
InitialState[1]=1

state=InitialState
for i in range(10):
    state=np.dot(evolutionMatrix,state)

print(state)


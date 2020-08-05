#This program is to solve the IBM ponder this challenge, Aug 2020.


def GenerateTriplets():
    AllTriplets=[];
    for ThirdPile in range(101):
        for SecondPile in range(ThirdPile+1):
            for FirstPile in range(SecondPile+1):
                AllTriplets.append((FirstPile,SecondPile,ThirdPile))
    AllTriplets.sort(key=sum)
    return AllTriplets

def MoveIsPossible(start,end):
    PermutationOfThree=[(0,1,2),(0,2,1),(1,0,2),(1,2,0),(2,0,1),(2,1,0)]
    for permutation in PermutationOfThree:
      a=start[0]-end[permutation[0]]
      b=start[1]-end[permutation[1]]
      c=start[2]-end[permutation[2]]
      if(a>=0 and b>=0 and c>=0 and a+b+c>0):
       IsPossible=(a*b*(a-b)+b*c*(b-c)+a*c*(a-c)==0)
       if IsPossible:
           return IsPossible
    return False
def nextPossibleMove(currentMove,position):
    piles=currentMove[0]
    amount=currentMove[1]
    leastNumber=position[piles[0]]
    for pile in piles:
        if(position[pile]<leastNumber):
            leastNumber=position(pile)
    if(amount<leastNumber):
        amount=amount+1
        return (piles,amount)
    else:
        amount=0
        if piles==[0]:
            piles=[1]
            return (piles,amount)
        if piles==[1]:
            piles=[2]
            return (piles,amount)
        if piles==[2]:
            piles=[0,1]
            return (piles,amount)
        if piles==[0,1]:
            piles=[0,2]
            return (piles,amount)
        if piles==[0,2]:
            piles=[1,2]
            return (piles,amount)
        if piles==[1,2]:
            piles=[0,1,2]
            return(piles,amount)
        if piles==[0,1,2]:
            return "none"


def GenerateMoveMatrix(AllTriplets):
    AllTriplets.sort(key=sum)
    AlltripletsIndex={}
    index=0
    for triplet in AllTriplets:
        AlltripletsIndex[triplet]=index
        index+=1
    MoveMatrix=[]
    tripletIndex=0
    for triplet in AllTriplets:
        print(tripletIndex)
        move=([0],0)
        possibleMove=[]
        while move!="none":
          positionAfterMove=list(triplet)
          movingPiles=move[0]
          movingAmount=move[1]
          for pile in movingPiles:
             positionAfterMove[pile]=triplet[pile]-movingAmount
          positionAfterMove.sort()
          paf=tuple(positionAfterMove) #position after move
          if paf not in possibleMove and paf!=triplet:
              possibleMove.append(AlltripletsIndex[paf])
          move=nextPossibleMove(move,triplet)
        MoveMatrix.append(possibleMove)
        tripletIndex+=1
    return MoveMatrix

def getTripletsIndex(AllTriplets):
    index=0
    AllTripletsIndex={}
    for triplet in AllTriplets:
        AllTripletsIndex[triplet]=index
        index+=1
    return AllTripletsIndex

def positionTOindex(positions,AllTripletsIndex):
    index=[]
    for triplet in positions:
        index.append(AllTripletsIndex[triplet])
    return index

def indexTOposition(indices,AllTriplets):
    positions=[]
    for index in indices:
        positions.append(AllTriplets[index])
    return positions

def getLoseIndices_v1(moveMatrix,loseIndices,winIndices):
    startIndex=0
    for endIndices in moveMatrix:
        print(startIndex)
        for index in endIndices:
            if index in loseIndices:
                winIndices.append(startIndex)
                break
        else:
            if startIndex not in loseIndices:
               loseIndices.append(startIndex)
        startIndex+=1
    return loseIndices

def getLoseIndices_v2(LOSErules,WINrules,AllTriplets,moveMatrix):
    winPositions=WINrules.copy()
    losePositions=LOSErules.copy()
    AllTripletsIndex=getTripletsIndex(AllTriplets)

    winIndices=positionTOindex(winPositions,AllTripletsIndex)
    loseIndices=positionTOindex(losePositions,AllTripletsIndex)

    loseIndices=findLoseIndices(moveMatrix,loseIndices,winIndices)
    
    return loseIndices

def getRMM(moveMatrix):#get reversed move matrix
    RMM=[]
    for index in range(len(moveMatrix)):
       RMM.append([])
    rowindex=0
    for row in moveMatrix:
        for endindex in row:
            RMM[endindex].append(rowindex)
        rowindex+=1
    return RMM



def getLoseIndices(LOSErules,WINrules,AllTriplets,RMM):
    winPositions=WINrules.copy()
    losePositions=LOSErules.copy()
    AllTripletsIndex=getTripletsIndex(AllTriplets)

    winIndices=positionTOindex(winPositions,AllTripletsIndex)
    loseIndices=positionTOindex(losePositions,AllTripletsIndex)
    
    positions=[]
    for triplet in AllTriplets:
        positions.append('unknown')
    for index in winIndices:
        positions[index]='win'
    for index in loseIndices:
        positions[index]='lose'
    
    loseP=[]
    for index in range(len(positions)):
        if positions[index]!= 'win':
            positions[index]='lose'
            loseP.append(index)
            for ind in RMM[index]:
                if(positions[ind]=='unknown'):
                  positions[ind]='win'
        index+=1
    return loseP










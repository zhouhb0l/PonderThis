#This program is to solve the IBM ponder this challenge, Aug 2020.


def GenerateTriplets():
    AllTriplets=[];
    for ThirdPile in range(101):
        for SecondPile in range(ThirdPile+1):
            for FirstPile in range(SecondPile+1):
                AllTriplets.append((FirstPile,SecondPile,ThirdPile))

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

   
LOSErules=[(0,0,0)];
WINrules=[];

AllTriplets=GenerateTriplets()
AllTriplets.sort(key=sum)
#print(AllTriplets)
print("There are ",len(AllTriplets), " triplets in total")

losePositions=LOSErules
winPositions=WINrules

winCount=len(winPositions)
loseCount=len(losePositions)

for triplet in AllTriplets:
      print(winCount,loseCount,"Now for triplet:",triplet) 
      if triplet in winPositions or triplet in losePositions:
          continue
      move=([0],0)
      winningMoveExists=False
      while move!="none" and (not winningMoveExists):
          positionAfterMove=list(triplet)
          movingPiles=move[0]
          movingAmount=move[1]
          for pile in movingPiles:
              positionAfterMove[pile]=triplet[pile]-movingAmount
          positionAfterMove.sort()
          TpositionAfterMove=tuple(positionAfterMove)
          if TpositionAfterMove != triplet:
             if TpositionAfterMove in losePositions:
                winningMoveExists=True
                winPositions.append(triplet)
                winCount+=1
          move=nextPossibleMove(move,triplet)
      if not winningMoveExists:
              losePositions.append(triplet)
              loseCount+=1
      
print(losePositions)
print(len(losePositions))





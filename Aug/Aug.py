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
    
def updateWinPosition(winPositions,losePositions,AllTriplets):
 for start in AllTriplets:
    for end in losePositions.copy():
        if(MoveIsPossible(start,end)) and not start in winPositions:
            winPositions.append(start)
 return winPositions

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

def updateLosePosition(winPositions,losePositions,AllTriplets):
  for start in AllTriplets:
      move=([0],0)
      nonlossMoveExists=False
      while move!="none" and (not nonlossMoveExists):
          positionAfterMove=list(start)
          movingPiles=move[0]
          movingAmount=move[1]
          for pile in movingPiles:
              positionAfterMove[pile]=start[pile]-movingAmount
          positionAfterMove.sort()
          TpositionAfterMove=tuple(positionAfterMove)
          if not TpositionAfterMove in winPositions and movingAmount!=0:
              nonlossMoveExists=True
          else:
              move=nextPossibleMove(move,start)
      if move=="none" and not start in losePositions:
          losePositions.append(start)
  return losePositions

LOSErules=[(0,0,0)];
WINrules=[];

AllTriplets=GenerateTriplets()
print("There are ",len(AllTriplets), " triplets in total")

losePositions=LOSErules
winPositions=WINrules

for cycles in range(10):
 winPosition=updateWinPosition(winPositions,losePositions,AllTriplets)
 losePosition=updateLosePosition(winPositions,losePositions,AllTriplets)
 print("After cycle ",cycles,"No of lose position:",len(losePositions),"No of win positions",len(winPositions))
#print("lose positions:",losePositions)
#print("win positions:",winPositions)






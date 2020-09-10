dataset = [
    111100001110001101011101110111000000, 111100001011010110101001111011100101,
    111100001110010101101110110111100101, 111100001111000101011101110111100101,
    111100001110010011101010110111100101, 111100001010011110011011011110100010,
    111100001010011110101011101011001000, 111100001010101110011010111110001000,
    111100001010110110011101011111000101, 111100001010110110101100111111000101,
    111100001110100101011101110111000101, 111100001111000100111101110111000101,
    111100001010110110101010111011001000, 111100001010101110110010111011001000,
    111100001110001110011101111111000000
]

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

print(dataset[1])
print(generateRelationMap(dataset[1],9))

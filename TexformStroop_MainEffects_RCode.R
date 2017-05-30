#########################################################
# A familiar-size Stroop effect in the absence of basic-level recognition.
# Main Analyses 
# Bria Long, brialorelle@gmail.com, May 2017
#########################################################

library(trimr)
library(ez)
library(plyr)

setwd("/Users/Bria/Dropbox (Personal)/Projects/TexformSizeStroop/Outputs/Manuscript/0-DataArchive/1-forGitHub")
dataFileName_E1="0-StroopData/E1GroupData.csv" 
dataFileName_E2="0-StroopData/E2GroupData.csv" 

# load the data
Data_E1 <- read.csv(dataFileName_E1)
Data_E2 <- read.csv(dataFileName_E2)

# trim data
DataTrimmed_E1<-Data_E1[Data_E1$RT>200 & Data_E1$RT<1500 & Data_E1$isCorrect==1,]
DataTrimmed_E2<-Data_E2[Data_E2$RT>200 & Data_E2$RT<1500 & Data_E2$isCorrect==1,]

# convert sid to factor
DataTrimmed_E1$sid=factor(DataTrimmed_E1$sid)
DataTrimmed_E2$sid=factor(DataTrimmed_E2$sid)

# main effects for experiment 1
aov.out = ezANOVA(data = DataTrimmed_E1, dv=.(RT), wid=.(sid), within=.(condLabel,taskLabel), type=3)
print(aov.out)

## main effects for experiment 2
aov.out = ezANOVA(data = DataTrimmed_E2, dv=.(RT), wid=.(sid), within=.(condLabel,taskLabel), type=3)
print(aov.out)


### ITEM EFFECTS
## item effects in experiment 2
itemBySub <- aggregate(RT ~ sid + condLabel + imRightName, DataTrimmed_E2, mean)
itemByCond <- aggregate(RT ~ condLabel + imRightName, itemBySub, mean)
stroopByItem=itemByCond$RT[itemByCond$condLabel=='incongruent']-itemByCond$RT[itemByCond$condLabel=='congruent']

# Load the size rank data
sizeRankData="1-TurkJudgements/SizeRankData.csv" 
sizeRanks <- read.csv(sizeRankData)
sizeRankbyItem <- aggregate(sizeRank ~ imName, sizeRanks, mean) # aggregate by item mean

# load pairing index
pairIndexFile="1-TurkJudgements/PairingIndexes.csv" 
pairIndex <- read.csv(pairIndexFile)
#sizeRankOrdered <- join(sizeRankbyItem, pairIndex) # use join rather than merge because it doesn't sort

# make binary classifiation scores
sizeRanks$BigBinar<-as.numeric(sizeRanks$sizeRank>4)
sizeRanks$SmallBinar<-as.numeric(sizeRanks$sizeRank<=4)

# for texforms that are big (or small) average size rank and binary classification scores by item. then sort.
bigBinarbyItem <- aggregate(BigBinar ~ imName, sizeRanks, mean)
bigBinarbyItem <- join(bigBinarbyItem, pairIndex) # use join rather than merge because it doesn't sort
bigTexClass=bigBinarbyItem[bigBinarbyItem$ActualSize=='Big',]
bigTexClass = bigTexClass[order(bigTexClass$SortIndex),]

smallBinarbyItem <- aggregate(SmallBinar ~ imName, sizeRanks, mean) # aggregate by item mean
smallBinarbyItem <- join(smallBinarbyItem, pairIndex) # use join rather than merge because it doesn't sort
smallTexClass = smallBinarbyItem[smallBinarbyItem$ActualSize=='Small',]
smallTexClass = smallTexClass[order(smallTexClass$SortIndex),]

# average classification score for each pair of items
sortedPairsAcc<-(smallTexClass$SmallBinar +  bigTexClass$BigBinar)/2; 
cor(sortedPairsAcc,stroopByItem)

# Load the turk judgement depth data
depthDataFile="1-TurkJudgements/DepthJudgements.csv" 
depthData <- read.csv(depthDataFile)
depthRankbyItem <- aggregate(Depth ~ pairNum + objectCategory + imageType, depthData, mean) # aggregate by item x big/small
SmallDepth=depthRankbyItem$Depth[depthRankbyItem$objectCategory=='SmallObjects' & depthRankbyItem$imageType=='Texforms']
BigDepth=depthRankbyItem$Depth[depthRankbyItem$objectCategory=='BigObjects' & depthRankbyItem$imageType=='Texforms']
DepthDiff=BigDepth-SmallDepth;

distDataFile="1-TurkJudgements/DistanceJudgements.csv" 
distData <- read.csv(distDataFile)
distRankbyItem <- aggregate(Distance ~ pairNum + objectCategory + imageType, distData, mean) # aggregate by item x big/small
SmallDist=distRankbyItem$Distance[distRankbyItem$objectCategory=='SmallObjects' & distRankbyItem$imageType=='Texforms']
BigDist=distRankbyItem$Distance[distRankbyItem$objectCategory=='BigObjects' & distRankbyItem$imageType=='Texforms']
DistDiff=BigDist-SmallDist;

CurvDataFile="1-TurkJudgements/CurvatureJudgements.csv" 
CurvData <- read.csv(CurvDataFile)
CurvRankbyItem <- aggregate(BoxyCurvy ~ pairNum + objectCategory + imageType, CurvData, mean) # aggregate by item x big/small
SmallCurv=CurvRankbyItem$BoxyCurvy[CurvRankbyItem$objectCategory=='SmallObjects' & CurvRankbyItem$imageType=='Texforms']
BigCurv=CurvRankbyItem$BoxyCurvy[CurvRankbyItem$objectCategory=='BigObjects' & CurvRankbyItem$imageType=='Texforms']
CurvDiff=BigCurv-SmallCurv;

# now do regression with all of these together
allTogether=data.frame(stroopByItem,CurvDiff,DepthDiff,DistDiff)
out=lm(stroopByItem ~ CurvDiff + DepthDiff + DistDiff, data = allTogether)

# now just curvature by itself
out=lm(stroopByItem ~ CurvDiff, data = allTogether)


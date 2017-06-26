#########################################################
# A familiar-size Stroop effect in the absence of basic-level recognition.
# Main Analyses 
# Bria Long, brialorelle@gmail.com, 
# Created in May 2017
# Updated in more detail, June 26th, 2017

# Please email me for experiment code as well as stimuli.
# Analyses were conducted in Matlab and in R; analyses were redone here for ease of sharing.
#########################################################


library(trimr)
library(ez)
library(plyr)

## CHANGE HERE to the working directory where you download the repository
setwd("/Users/Bria/Dropbox (Personal)/Projects/TexformSizeStroop/Outputs/Manuscript/0-DataArchive/1-forGitHub")
dataFileName_E1="0-StroopData/E1GroupData.csv" 
dataFileName_E2="0-StroopData/E2GroupData.csv" 

# load the data
Data_E1 <- read.csv(dataFileName_E1)
Data_E2 <- read.csv(dataFileName_E2)

# trim data
DataTrimmed_E1<-Data_E1[Data_E1$RT>200 & Data_E1$RT<1500 & Data_E1$isCorrect==1,]
DataTrimmed_E2<-Data_E2[Data_E2$RT>200 & Data_E2$RT<1500 & Data_E2$isCorrect==1,]

# convert subject id number to factor
DataTrimmed_E1$sid=factor(DataTrimmed_E1$sid)
DataTrimmed_E2$sid=factor(DataTrimmed_E2$sid)

####  Experiment 1  
#  Main ANOVA effects for Experiment 1
aov.out = ezANOVA(data = DataTrimmed_E1, dv=.(RT), wid=.(sid), within=.(condLabel,taskLabel), type=3)
print(aov.out)

# Look at data by individual subjects and plot (Figure 1c)
meanRTbySubject <- aggregate(RT ~ sid + condLabel, DataTrimmed_E1, mean)
congRT=meanRTbySubject$RT[meanRTbySubject$condLabel=='congruent']
incongRT=meanRTbySubject$RT[meanRTbySubject$condLabel=='incongruent']
barplot(sort(incongRT-congRT, decreasing = TRUE)) # individual subject effects
mean(incongRT-congRT)/sd(incongRT-congRT) # cohens D

#### Experiment 2: Replication with paired items
# Main ANOVA effects for Experiment 2
aov.out = ezANOVA(data = DataTrimmed_E2, dv=.(RT), wid=.(sid), within=.(condLabel,taskLabel), type=3)
print(aov.out)

## Item effects in Experiment 2 
# Step 1: Get Stroop item effects : Here, images are presented in specific pairs, indexed by the first two numbers (i.e., "01_XX.jpg') of the image. So, we can average
# the data according to just the image name presented on the right side.
itemBySub <- aggregate(RT ~ sid + condLabel + imRightName, DataTrimmed_E2, mean) # average subject x condition x image
itemByCond <- aggregate(RT ~ condLabel + imRightName, itemBySub, mean) # average across subjects to get item means
stroopByItem=itemByCond$RT[itemByCond$condLabel=='incongruent']-itemByCond$RT[itemByCond$condLabel=='congruent'] # stroop pairs

## Step 2: Load rating data
# Load the size rank data used to pair the Texforms for Experiment 2
sizeRankData="1-TurkJudgements/SizeRankData.csv" 
sizeRanks <- read.csv(sizeRankData)
sizeRankbyItem <- aggregate(sizeRank ~ imName, sizeRanks, mean) # aggregate by item mean

# Load pairing index - indexes used to pair texforms in RT experiment
pairIndexFile="1-TurkJudgements/PairingIndexes.csv" # used to pair the items in experiment 2
pairIndex <- read.csv(pairIndexFile)

# Compute raw binary classifiation scores (see Konkle & Oliva, 2011 and Long et al., 2016 for a visualization of this sizeRank scale)
sizeRanks$BigBinar<-as.numeric(sizeRanks$sizeRank>4)
sizeRanks$SmallBinar<-as.numeric(sizeRanks$sizeRank<=4)

# Compute the average size rank and binary classification scores by item separately for texforms that are big (or small); then sort.
bigBinarbyItem <- aggregate(BigBinar ~ imName, sizeRanks, mean)
bigBinarbyItem <- join(bigBinarbyItem, pairIndex) # 
bigTexClass=bigBinarbyItem[bigBinarbyItem$ActualSize=='Big',]
bigTexClass = bigTexClass[order(bigTexClass$SortIndex),]

smallBinarbyItem <- aggregate(SmallBinar ~ imName, sizeRanks, mean) # aggregate by item mean
smallBinarbyItem <- join(smallBinarbyItem, pairIndex) # 
smallTexClass = smallBinarbyItem[smallBinarbyItem$ActualSize=='Small',]
smallTexClass = smallTexClass[order(smallTexClass$SortIndex),]

# Compute average classification score for each pair of items
sortedPairsAcc<-(smallTexClass$SmallBinar +  bigTexClass$BigBinar)/2; 

# perceived size for each texform 
sizeRankByItem <- join(sizeRankbyItem, pairIndex) # use join rather than merge because it doesn't sort
sizeRankByItemSorted <- sizeRankByItem[order(sizeRankByItem$SortIndex),] 

# correlate this average classification score with the Stroop item effects
cor(sortedPairsAcc,stroopByItem)

# regression with size classifiability by itself
thisData=data.frame(stroopByItem,sortedPairsAcc)
out=lm(stroopByItem ~ sortedPairsAcc, data = thisData)
summary(out)

# make a quick plot - Figure 2c
plot(sortedPairsAcc,stroopByItem)
abline(lm(stroopByItem ~ sortedPairsAcc, data = thisData))

### Experiment 3: Which mid-level features activate size information?
########## Perceived Viewing Distance ######### 
distDataFile="1-TurkJudgements/DistanceJudgements.csv" 
distData <- read.csv(distDataFile) # Load the turk judgement distance data
distRankbyItem <- aggregate(Distance ~ pairNum + objectCategory + imageType, distData, mean) # aggregate by item x big/small

# Do big and small objects differ in perceived distance? (Yes, somewhat)
SmallDistOrig=distRankbyItem$Distance[distRankbyItem$objectCategory=='SmallObjects' & distRankbyItem$imageType=='Originals']
BigDistOrig=distRankbyItem$Distance[distRankbyItem$objectCategory=='BigObjects' & distRankbyItem$imageType=='Originals']
t.test(BigDistOrig,SmallDistOrig,var.equal=TRUE) 

# Do big and small objects differ in perceived distance? (No at p<.05)
SmallDistTex=distRankbyItem$Distance[distRankbyItem$objectCategory=='SmallObjects' & distRankbyItem$imageType=='Texforms']
BigDistTex=distRankbyItem$Distance[distRankbyItem$objectCategory=='BigObjects' & distRankbyItem$imageType=='Texforms']
t.test(BigDistTex,SmallDistTex,var.equal=TRUE) 

# Does perceived distance predict perceived real-world size?
distRankbyTexforms<-distRankbyItem[distRankbyItem$imageType=='Texforms',]
distRankbyItemSorted=distRankbyTexforms[order(distRankbyTexforms$pairNum),]
plot(sizeRankByItemSorted$sizeRank,distRankbyItemSorted$Dist)
cor.test(sizeRankByItemSorted$sizeRank, distRankbyItemSorted$Dist)

# Do any differences in perceived distance predict stroop effects?
DistDiff=BigDistTex-SmallDistTex; # differences in perceived distance per pair of texforms
cor.test(DistDiff,stroopByItem) # differences in perceived distance do not predict the stroop effect

########## Depicted Depth ######### 
depthDataFile="1-TurkJudgements/DepthJudgements.csv" 
depthData <- read.csv(depthDataFile) # Load the turk judgement depth data
depthRankbyItem <- aggregate(Depth ~ pairNum + objectCategory + imageType, depthData, mean) # aggregate by item x big/small

# Do big and small objects differ in depicted depth? (Yes, somewhat)
SmallDepthOrig=depthRankbyItem$Depth[depthRankbyItem$objectCategory=='SmallObjects' & depthRankbyItem$imageType=='Originals']
BigDepthOrig=depthRankbyItem$Depth[depthRankbyItem$objectCategory=='BigObjects' & depthRankbyItem$imageType=='Originals']
t.test(BigDepthOrig,SmallDepthOrig,var.equal=TRUE) # for originals - difference.

# Do big and small object texforms differ in depicted depth? (No)
SmallDepthTex=depthRankbyItem$Depth[depthRankbyItem$objectCategory=='SmallObjects' & depthRankbyItem$imageType=='Texforms']
BigDepthTex=depthRankbyItem$Depth[depthRankbyItem$objectCategory=='BigObjects' & depthRankbyItem$imageType=='Texforms']
t.test(BigDepthTex,SmallDepthTex,var.equal=TRUE) # for texforms - no difference.

# Does depicted depth predict perceived real-world size?
depthRankTexforms<-depthRankbyItem[depthRankbyItem$imageType=='Texforms',]
depthRankByItemSorted=depthRankTexforms[order(depthRankTexforms$pairNum),]
plot(sizeRankByItemSorted$sizeRank,depthRankByItemSorted$Depth)
cor.test(sizeRankByItemSorted$sizeRank, depthRankByItemSorted$Depth)

# Do any differences in depicted depth predict stroop effects?
DepthDiff=BigDepthTex-SmallDepthTex; 
cor.test(DepthDiff,stroopByItem) 

########## Perceived Curvature ######### 
CurvDataFile="1-TurkJudgements/CurvatureJudgements.csv" 
CurvData <- read.csv(CurvDataFile) # Load the curvatre judgementdata
CurvRankbyItem <- aggregate(BoxyCurvy ~ pairNum + objectCategory + imageType, CurvData, mean) # aggregate by item x big/small

# Do big and small objects differ in curvature?
SmallCurvOrig=CurvRankbyItem$BoxyCurvy[CurvRankbyItem$objectCategory=='SmallObjects' & CurvRankbyItem$imageType=='Originals']
BigCurvOrig=CurvRankbyItem$BoxyCurvy[CurvRankbyItem$objectCategory=='BigObjects' & CurvRankbyItem$imageType=='Originals']
t.test(BigCurvOrig,SmallCurvOrig,var.equal=TRUE) 

# Do big and small texforms differ in curvature?
SmallCurvTex=CurvRankbyItem$BoxyCurvy[CurvRankbyItem$objectCategory=='SmallObjects' & CurvRankbyItem$imageType=='Texforms']
BigCurvTex=CurvRankbyItem$BoxyCurvy[CurvRankbyItem$objectCategory=='BigObjects' & CurvRankbyItem$imageType=='Texforms']
t.test(BigCurvTex,SmallCurvTex,var.equal=TRUE) 

# Does perceived curvature predict perceived real-world size? (yes)
CurvRankTexforms<-CurvRankbyItem[CurvRankbyItem$imageType=='Texforms',]
CurvRankByItemSorted=CurvRankTexforms[order(CurvRankTexforms$pairNum),]
plot(CurvRankByItemSorted$BoxyCurvy,sizeRankByItemSorted$sizeRank)
cor.test(CurvRankByItemSorted$BoxyCurvy,sizeRankByItemSorted$sizeRank)

# Do curvature differences predict the size stroop effect?
CurvDiff=BigCurvTex-SmallCurvTex;
cor.test(CurvDiff,stroopByItem) 

## Regression analysis
# Now do a regression with all of these as predictors, and stroop displays as 
allTogether=data.frame(stroopByItem,CurvDiff,DepthDiff,DistDiff)
out=lm(stroopByItem ~ CurvDiff + DepthDiff + DistDiff, data = allTogether)
summary(out)



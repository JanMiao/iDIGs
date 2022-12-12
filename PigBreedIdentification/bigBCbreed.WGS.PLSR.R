
library(caret)
library(data.table)
library(GBC)
source("sourceCode.R")

train_plinkA_file="merge11.raw"
test_plinkA_file="WGSused.raw"
breedInfo_file = "breedusedQC.txt"

# training data
plinkA = fread(plinkA_file)
sampleID = plinkA[[2]]
plinkA = as.matrix(plinkA[,7:ncol(plinkA)])
rownames(plinkA) = sampleID
breedDF = read.table(breedInfo_file, head=F)
breedInfo = breedDF[,2]
names(breedInfo)  = breedDF[,1]
# check
breedInfo = breedInfo[rownames(plinkA)]
identical(names(breedInfo), rownames(plinkA))
trainX.raw = plinkA
trainy = breedInfo

# test data
plinkA = fread(test_plinkA_file)
sampleID = plinkA[[2]]
plinkA = as.matrix(plinkA[,7:ncol(plinkA)])
rownames(plinkA) = sampleID
testX = plinkA
WGSbreed =read.table("WGSbreed.txt")
rownames(WGSbreed) = WGSbreed[,1]
testy = WGSbreed[rownames(testX),2]
names(testy) = WGSbreed[rownames(testX),1]

# extract same marker of train and test
trainX = trainX.raw[,colnames(testX)]
identical(colnames(trainX), colnames(testX))

# 开始预测
bins = seq(1000,15000,1000)
breedFq = breedFre(trainX, trainy)
res_file = "WGS.PCA.csv"
cat("nmarkers accuracy\n", file=res_file, append=T)
set.seed(1)
for (nmarker in bins){
#nmarker=5000
Fq.selected = pcaLoads(breedFq, Num=nmarker, npca=10)
trainX.tmp = trainX[,Fq.selected]
testX.tmp = testX[,Fq.selected]
model.plsda <- plsda(trainX.tmp, factor(as.character(trainy)), ncomp = 150)
pred.plsda = predict(model.plsda, testX.tmp)
cat(nmarker, sum(testy == pred.plsda)/length(testy), "\n", file=res_file, append=T )
errTrue = testy[testy != pred.plsda]
errPred = pred.plsda[testy != pred.plsda]
err = data.frame(True=errTrue, Predict=errPred)
write.csv(err, paste0("PCA_err_",nmarker,".csv"), quote=F)
}
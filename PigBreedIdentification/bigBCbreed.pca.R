### marker selection by eu

rm(list=ls());gc()
library(data.table)
library(caret)
library(e1071)
library(class)
#library(rpart)
library(randomForest)
library(GBC)
source("sourceCode.R")

plinkA_file="merge10.raw"
breedInfo_file = "breedusedQC.txt"

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

res_file = "NEW.RES.PCA.txt"
cat ("seed nmarkers accuracy method\n", file=res_file, append=T)

for(seed in seq(5)){
  set.seed(seed)
  foldInfo = makeFold(breedInfo, nfold=5)
  for( nmarker in c(200, 500, 800, 1000, 3000, 5000, 7000, 10000,12000)){
    # one 5-fold running
    count.SVM.r = 0
    count.SVM.p = 0
    count.plsda = 0
    count.knn = 0
    count.NSC = 0
	count.RF = 0
    for(groupID in 1:5){
      testID = names(foldInfo[foldInfo == groupID])
      trainID = setdiff(names(foldInfo), testID)
      trainX = plinkA[trainID,]
      testX = plinkA[testID,]
      trainy = breedInfo[trainID]
      testy = breedInfo[testID]
      # marker selection
      breedFq = breedFre(trainX, trainy)
      Fq.selected = pcaLoads(breedFq, Num=nmarker, npca=10)
      trainX.tmp = trainX[,Fq.selected]
      testX.tmp = testX[,Fq.selected]
      # SVM-radial
      model.svm <- svm(x=trainX.tmp, y = factor(as.character(trainy)), kernel ="radial",scale=F)
      pred.svm = predict(model.svm, testX.tmp)
      count.SVM.r = count.SVM.r + sum(testy == pred.svm)
      # SVM-polynomial
      model.svm <- svm(x=trainX.tmp, y = factor(as.character(trainy)), kernel ="polynomial",scale=F)
      pred.svm = predict(model.svm, testX.tmp)
      count.SVM.p = count.SVM.p + sum(testy == pred.svm)
      # PLSDA
	  model.plsda <- plsda(trainX.tmp, factor(as.character(trainy)), ncomp = 150)
      pred.plsda = predict(model.plsda, testX.tmp)
      count.plsda = count.plsda + sum(testy == pred.plsda)
      # NSC
      model.NSC<-train(x=trainX.tmp, y=factor(as.character(trainy)), method="pam")
      pred.NSC = predict(model.NSC, testX.tmp)
      count.NSC = count.NSC + sum(testy == pred.NSC)
      # knn
      pred.knn = knn(trainX.tmp, testX.tmp, cl=factor(as.character(trainy)), k=5)
      count.knn = count.knn + sum(testy == pred.knn)
	  # RF
	  model = randomForest(x=trainX.tmp, y = factor(as.character(trainy)))
      pred = predict(model, testX.tmp)
      count.RF = count.RF + sum(testy == pred)
    }
    cat(seed, nmarker, count.SVM.r/length(foldInfo), "SVM.r\n", file=res_file, append=T )
    cat(seed, nmarker, count.SVM.p/length(foldInfo), "SVM.p\n", file=res_file, append=T )
    cat(seed, nmarker, count.plsda/length(foldInfo), "plsda\n", file=res_file, append=T )
    cat(seed, nmarker, count.knn/length(foldInfo),   "knn\n",   file=res_file, append=T )
    cat(seed, nmarker, count.NSC/length(foldInfo),   "NSC\n",   file=res_file, append=T )
	cat(seed, nmarker, count.RF/length(foldInfo),   "RF\n",   file=res_file, append=T )
  }
}
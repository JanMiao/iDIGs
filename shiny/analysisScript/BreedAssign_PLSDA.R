##### mian function

### 1. breed identify 
PCA_PLSDA <- function(REF="REF_data.rds", test_prefix, PLINK, breedUsed=NULL, WHATEVER=T, ifAnomaly="no", threshold=0.02){
  REF = readRDS(REF)
  # choose breed
  if (!is.null(breedUsed)){
    indx = REF$trainy %in% breedUsed
    REF$trainy = REF$trainy[indx]
    REF$trainX = REF$trainX[indx,]
  }
  # normalize of snpID in test set
  test_prefix_tmp <- paste0(test_prefix, "_tmp")
  # CPP exe
  system(paste0("/disk191/miaoj/software/MakeSNPid ", paste0(test_prefix, ".bim"), " " ,paste0(test_prefix_tmp, ".bim")))
  system(paste0("cp ", test_prefix, ".bed ", test_prefix_tmp, ".bed"))
  system(paste0("cp ", test_prefix, ".fam ", test_prefix_tmp, ".fam"))
  # extract same SNP for train data and test data
  SNPtestPos <- fread(paste0(test_prefix, "_tmp.bim"))[[2]]
  sameSNP <- intersect(REF$RefAllele[,1], SNPtestPos)
  # warning & stop
  if(WHATEVER == F){
  if(1000 <= length(sameSNP) &&  length(sameSNP) < 5000 ){
    warning("Less than 5000 markers are used, marker selection will not be conducted!")
    } else if(length(sameSNP) < 1000){
      stop("Less than 1000 markers are used!")}
	  }
  write.table(sameSNP, "SNPused.txt", row.names = F, col.names = F, quote = F)
  # recode A for test data
  write.table(REF$RefAllele, "recode.txt", row.names = F, col.names = F, quote = F)
  system(paste0(PLINK, " --silent --bfile ", test_prefix_tmp, " --extract SNPused.txt", " --recodeA --recode-allele recode.txt"))
  # generate test genotype
  testX <- data.table::fread("plink.raw")
  sampleID <- testX[[2]]
  testX <- as.matrix(testX[, 7:ncol(testX)])
  rownames(testX) <- sampleID
  trainX = REF$trainX[,colnames(testX)]
  if (length(sameSNP) >= 5000){
    # breed frequency
    breedFq = breedFre(trainX, REF$trainy)
    # marker selection
    if (is.null(breedUsed)){
      marker.selected = pcaLoads(breedFq, Num=5000, npca=10)
    } else {
      if (length(breedUsed) < 10) {
        marker.selected = pcaLoads(breedFq, Num=5000, npca=length(breedUsed))
      } else{
        marker.selected = pcaLoads(breedFq, Num=5000, npca=10)
      }
    }
    trainX = trainX[, marker.selected]
    testX = testX[, marker.selected]}
  # prediction
  nsample = dim(trainX)[1]
  if (nsample <= 100) {
   ncomps=ceiling(nsample/10)} else {
    ncomps=150}
  model.plsda <- plsda(trainX, factor(as.character(REF$trainy)), ncomp = ncomps)
  if (ifAnomaly == "no"){
  pred.plsda = predict(model.plsda, testX)
  res = data.frame(sampleID=sampleID, predict=pred.plsda)
  } else {
  mat = predict(model.plsda, testX, type="prob")[,,1]
  threshold = 0.013
  predictLable = c()
  labels = colnames(mat)
  for(i in 1:nrow(mat)){
   line = mat[i,]
   if (max(line) >= threshold){
    plabel = labels[which(line == max(line))]
    predictLable = c(predictLable, plabel)
   } else {
    predictLable = c(predictLable, "unknown")
   }
  }
  res = data.frame(sampleID=sampleID, predict=predictLable)
  }
  # output
  return(res)
}

### 2. panel design
Panel <- function(breed, nSNP=100,
                  REF="/disk192/miaoj/GBC/PLSDA/data/REF_data11.rds"){
  # load reference data
  ref <- readRDS(REF)
  # check input
  breed.in.ref <-unique(ref$trainy)
  if (!all(breed %in% breed.in.ref)){
    stop("Some input breeds are not in our data.")
  }
  # Two different methods to design panel
  if (length(breed) == 1){
    # one VS others
    trainX = ref$trainX
    trainy = ref$trainy
    trainy[trainy != breed[1]] = "Others"
    breedFq = breedFre(trainX, trainy)
    marker.selected = delta(breedFq, Num=nSNP)
  } else{
    # among specific breeds
    trainy = ref$trainy[ref$trainy %in% breed]
    trainX = ref$trainX[names(trainy),]
    breedFq = breedFre(trainX, trainy)
	nsample = dim(trainX)[1]
	if (nsample <= 100) {
	 ncomps=ceiling(nsample/10)} else {
	  ncomps=100}
    marker.selected = plsda.select(trainX, trainy, Num=nSNP, ncomp=ncomps)
  }
  acc = CVtest(trainX[,marker.selected], trainy, marker.selected, nfold=5)
  return(list(marker = marker.selected, accuracy=acc))
}


##### other functions

# allele frequency of allele1
breedFre <- function(plinkAmatrix, breedInfo) {
  # calculate SNP Frequency for each breed
  freqMatrix <- c()
  breedList <- c()
  for (breed in unique(breedInfo)) {
    breedList <- c(breedList, breed)
    sampleUsed <- names(breedInfo[breedInfo == breed])
    plinkAmatrix_breed <- plinkAmatrix[sampleUsed, ]
    freqMatrix <- rbind(freqMatrix, apply(plinkAmatrix_breed, 2, mean) / 2)
  }
  freqMatrix[freqMatrix == 1] <- 0.99999
  freqMatrix[freqMatrix == 0] <- 0.00001
  rownames(freqMatrix) <- breedList
  return(freqMatrix = freqMatrix)
}

# marker selection by PCA loadings 
pcaLoads = function(freqMatrix, Num=5000, npca=10){
    pca = prcomp(freqMatrix)
    pca$rotation = pca$rotation[,1:npca]
    markerScore = apply(pca$rotation, 1, function(x){sum(x^2)})
    Fq.selected = names(sort(markerScore, decreasing = T))[1:Num]
    return(Fq.selected)
  }

# marker selection by PLSDA
plsda.select = function(trainX, trainy, Num=5000, ncomp=100){
  model.plsda <- plsda(trainX, factor(as.character(trainy)), ncomp = ncomp)
  coefs = model.plsda$coefficients[,,ncomp]
  markerScore = apply(coefs, 1, function(x){sum(x^2)})
  Fq.selected = names(sort(markerScore, decreasing = T))[1:Num]
  return(Fq.selected)
}

# marker selection by DELTA
delta <- function(AFMatrix, Num=5000) {
  distance = abs(AFMatrix[1,] - AFMatrix[2,])
  Fq.selected = names(sort(distance, decreasing = T))[1:Num]
  return(Fq.selected)
}

# reliable of prediction
add.reliable = function(pred.raw){
  pred = c()
  res = pred.raw[,,1]
  for(i in 1:nrow(res)){
    line = res[i,]
    line = sort(line, decreasing=T)
    if(line[1] >= 2 * line[2]){
      pred = c(pred, 2)
    } else if (line[1] >= 1.5 * line[2]) {
      pred = c(pred, 1) } else {
        pred = c(pred, 0)
      }
  }
  return(pred)
}

# generate 5 fold CV groupID
makeFold = function(breedInfo, nfold=5){
  breeds = unique(breedInfo)
  foldID = c()
  for (breed in breeds){
    breedTmp = breedInfo[breedInfo == breed]
    n = length(breedTmp)
    baseValue = floor(n/nfold)
    numPlus1 = n - baseValue * nfold
    foldnum = rep(baseValue, nfold-numPlus1)
    foldnum = c(foldnum, rep(baseValue+1, numPlus1))
    fold = rep(seq(nfold), times = foldnum)
    foldID.tmp = sample(fold, length(fold))
    foldID = c(foldID, foldID.tmp)
  }
  foldInfo = foldID
  names(foldInfo) = names(breedInfo)
  return(foldInfo)
}

# evaluate panel performance use 5-fold CV
CVtest = function(X, y, markers, nfold=5){
  set.seed=1
  # group information
  foldInfo = makeFold(y, nfold=nfold)
  # CV
  count = 0
  for(groupID in 1:nfold){
    testID = names(foldInfo[foldInfo == groupID])
    trainID = setdiff(names(foldInfo), testID)
    trainX = X[trainID,]
    testX = X[testID,]
    trainy = y[trainID]
    testy = y[testID]
    # PLSDA prediction
    if(length(markers) < 500){
	  model.fit <- svm(x=trainX, y = factor(as.character(trainy)), kernel ="radial",scale=F)
	  } else{
        model.fit <- plsda(trainX, factor(as.character(trainy)), ncomp = 150)
      }
    model.pred = predict(model.fit, testX)
    count = count + sum(testy == model.pred)
  }
  accuracy = count/length(foldInfo)
  return(accuracy)
}

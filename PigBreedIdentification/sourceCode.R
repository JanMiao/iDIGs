markerSelection2 = function(freqMatrix, Num=5000){
  MarkerDistance <- eu_distance(freqMatrix)
  markerSelected_I <- order(MarkerDistance, decreasing = T)[1:Num]
  return(markerSelected_I)
}

### Euclidean distance
eu_distance <- function(AFMatrix, breedused = NULL) {
  if (!is.null(breedused)) {
    AFMatrix <- AFMatrix[breedused, ]
  }
  nmarker <- dim(AFMatrix)[2]
  # npairs: number of comparison
  nbreed <- dim(AFMatrix)[1]
  npairs <- nbreed * (nbreed - 1) / 2
  Dmatrix <- matrix(0, nrow = npairs, ncol = nmarker)
  # calculate pair-wise Delta value
  D_rowIndex <- 1
  for (i in seq(nbreed - 1)) {
    for (j in seq(i + 1, nbreed)) {
      Dmatrix[D_rowIndex, ] <- (AFMatrix[i, ] - AFMatrix[j, ])^2
      D_rowIndex <- D_rowIndex + 1
    }
  }
  # Euclidean distance
  distance <- sqrt(apply(Dmatrix, 2, sum)) / npairs
  return(distance)
}


GetSNPinfo <- function(freqMatrix) {
  SNP_ref <- colnames(freqMatrix)
  SNP_breaks <- strsplit(SNP_ref, "_")
  SNPpos <- c()
  RefAllele <- c()
  for (i in 1:length(SNP_breaks)) {
    SNPpos <- c(SNPpos, SNP_breaks[[i]][1])
    RefAllele <- c(RefAllele, SNP_breaks[[i]][2])
  }
  return(list(SNPpos = SNPpos, RefAllele = RefAllele))
}

markerSelection3 = function(freqMatrix, breed, Num=5000){
  breedinFq = freqMatrix[breed,]
  breedinFq.matrix = matrix(rep(breedinFq,each=dim(breedFq)[1]-1), dim(breedFq)[1]-1, dim(breedFq)[2])
  a = abs(freqMatrix[-which(rownames(freqMatrix) == breed),] - breedinFq.matrix)
  b = apply(a,1,sum)
  c = order(b)
  nbreed = length(c)
  near.breed = names(b[1:floor(nbreed/2)])
  far.breed = names(b[(floor(nbreed/2)+1): nbreed])
  near.mean = apply(freqMatrix[near.breed,], 2, mean)
  far.mean = apply(freqMatrix[far.breed,], 2, mean)
  near.dis = abs(breedinFq - near.mean)
  far.dis = abs(breedinFq - far.mean)
  near.indx =  order(near.dis, decreasing = T)[1: (Num/2)]
  far.indx =  order(far.dis, decreasing = T)[(Num/2 + 1): Num]
  markerSelected_I = c(near.indx, far.indx)
  return(markerSelected_I)
}

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

preFST = function(mergeFam, trainID, nmarker){
  keep = mergeFam[mergeFam[,2] %in% trainID,]
  write.table(keep, "keep.txt", row.names = F, col.names = F, sep=" ", quote=F)
  system("/disk191/zzy/software/plink --bfile merge11 --keep keep.txt --make-bed --out FSTtmp")
  within = cbind(keep[,1:2], breedInfo[keep[,2]])
  write.table(within, "cluster.txt", row.names = F, col.names = F, sep=" ", quote=F)
  system("/disk191/zzy/software/plink --bfile merge11 --fst --within cluster.txt")
  df = read.table("plink.fst",head=T)
  Fq.selected = df[order(df$FST, decreasing = T),2][1:nmarker]
  return(Fq.selected)
}

pcaLoads = function(freqMatrix, Num=5000, npca=10){
  pca = prcomp(freqMatrix)
  pca$rotation = pca$rotation[,1:npca]
  markerScore = apply(pca$rotation, 1, function(x){sum(x^2)})
  Fq.selected = names(sort(markerScore, decreasing = T))[1:Num]
  return(Fq.selected)
}

plsda.select = function(trainX, trainy, Num=5000, ncomp=100){
  model.plsda <- plsda(trainX, factor(as.character(trainy)), ncomp = ncomp)
  coefs = model.plsda$coefficients[,,ncomp]
  markerScore = apply(coefs, 1, function(x){sum(x^2)})
  Fq.selected = names(sort(markerScore, decreasing = T))[1:Num]
  return(Fq.selected)
}

### delta distance
delta <- function(AFMatrix, Num=5000) {
  nmarker <- dim(AFMatrix)[2]
  # npairs: number of comparison
  nbreed <- dim(AFMatrix)[1]
  npairs <- nbreed * (nbreed - 1) / 2
  Dmatrix <- matrix(0, nrow = npairs, ncol = nmarker)
  # calculate pair-wise Delta value
  D_rowIndex <- 1
  for (i in seq(nbreed - 1)) {
    for (j in seq(i + 1, nbreed)) {
      Dmatrix[D_rowIndex, ] <- abs(AFMatrix[i, ] - AFMatrix[j, ])
      D_rowIndex <- D_rowIndex + 1
    }
  }
  # Euclidean distance
  distance <- apply(Dmatrix, 2, sum) / npairs
  Fq.selected = names(sort(distance, decreasing = T))[1:Num]
  return(Fq.selected)
}

tech <- read.csv(file.choose(),header=T)
View(tech)

#function for calculating Cramer's v
cramer <- function(y,x){
  K <- nlevels(y)
  L <- nlevels(x)
  n <- length(y)
  chi2 <- chisq.test(y,x,correct=F)
  print(chi2$statistic)
  v <- sqrt(chi2$statistic/(n*min(K-1,L-1)))
  return(v)
}

#similarity matrix
sim <- matrix(1,nrow=ncol(tech),ncol=ncol(tech))
rownames(sim) <- colnames(tech)
colnames(sim) <- colnames(tech)
for (i in 1:(nrow(sim)-1)){
  for (j in (i+1):ncol(sim)){
    y <- tech[,i]
    x <- tech[,j]
    sim[i,j] <- cramer(y,x)
    sim[j,i] <- sim[i,j]
  }
}
#distance matrix
dissim <- as.dist(1-sim)
#clustering
tree <- hclust(dissim,method="ward.D")
plot(tree, hang=-1) 

vote <- read.csv(file.choose(),header=T)
View(vote)

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
sim <- matrix(1,nrow=ncol(vote),ncol=ncol(vote))
rownames(sim) <- colnames(vote)
colnames(sim) <- colnames(vote)
for (i in 1:(nrow(sim)-1)){
  for (j in (i+1):ncol(sim)){
    y <- vote[,i]
    x <- vote[,j]
    sim[i,j] <- cramer(y,x)
    sim[j,i] <- sim[i,j]
  }
}
#distance matrix
dissim <- as.dist(1-sim)
#clustering
tree <- hclust(dissim,method="ward.D")
plot(tree,hang=-1) 
#plot(mydata.hclust,hang=-1)

library(ClustOfVar)
arbre <- hclustvar(X.quali=vote.active)
plot(arbre)
mgroups <- kmeansvar(X.quali=vote.active,init=2,nstart=10)
print(summary(mgroups)) 

#2 subgroups
groups <- cutree(tree,k=2)
print(groups)
#Cramer's v : affiliation vs. attributes
cv <- sapply(vote.active,cramer,x=vote.data$affiliation)
print(cv)
#mean of v for each group
m <- tapply(X=cv,INDEX=groups,FUN=mean)
print(m)


#Dice's index
dice <- function(m1,m2){
  return(0.5*sum((m1-m2)^2))
}
#Dice's index matrix
d2 <- matrix(0,ncol(disj),ncol(disj))
for (j in 1:ncol(disj)){
  for (jprim in 1:ncol(disj)){
    d2[j,jprim] <- dice(disj[,j],disj[,jprim])
  }
}
colnames(d2) <- colnames(disj)
rownames(d2) <- colnames(disj)
#transform the matrix in a R 'dist' class
d <- as.dist(sqrt(d2))

#cluster analysis on indicator variables
arbre.moda <- hclust(d,method="ward.D2")
plot(arbre.moda)


#create 3 groups
dgroups <- cutree(arbre.moda,k=3)
#illustrative variable - dummy coding scheme
illus <- acm.disjonctif(as.data.frame(vote.data$affiliation))
colnames(illus) <- c("democrat","republican")
#distance to illustrative levels
dice.democrat <- sapply(disj,dice,m2=illus$democrat)
tapply(dice.democrat,dgroups,mean)
dice.republican <- sapply(disj,dice,m2=illus$republican)
tapply(dice.republican,dgroups,mean)


# loading the package
library(Hmisc)
# calling the "varclus" function
# see the help file for the parameters
v <-
  varclus(as.matrix(disj),type="data.matrix",
          similarity="bothpos",method="ward.D")
plot(v)

#MCA with the ade4 package
acm <- dudi.coa(disj,scannf=F,nf=2)
#factorial coordinates of the levels
acm.coord <- data.frame(acm$co)
rownames(acm.coord) <- colnames(disj)
#distance matrix
m.acm <- dist(acm.coord,method="euclidian")
#cluster analysis from the distance matrix m.acm
arbre.acm <- hclust(m.acm,method="ward.D")
plot(arbre.acm)


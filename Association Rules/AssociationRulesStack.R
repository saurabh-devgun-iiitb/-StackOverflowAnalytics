#adjust this line to the folder where you will be saving this code and the data file
#load("~/R_code_&_data/titanic.raw.rdata")
stackOver = NULL
stackOver = read.csv("C:\\Users\\Saurabh\\Desktop\\Project Docs\\Association Rules\\AssociationRulesStack.csv",header=T)


head(stackOver)
attach(stackOver)

#install.packages("Matrix")
#install.packages("arules")
library(arules)
# find association rules with default settings

class(stackOver$Occupation)

stackOver$Age = as.factor(stackOver$Age)
stackOver$Exp = as.factor(stackOver$Exp)
stackOver$sibsp = as.factor(stackOver$sibsp)
stackOver$parch = as.factor(stackOver$parch)
rules = apriori(stackOver, parameter = list(minlen=2, supp=0.005, conf=0.8))
inspect(rules)


rules <- apriori(stackOver, parameter = list(supp=0.001, conf=0.8), appearance = list(rhs=c("Compensation=Less than $20,000","Compensation=$20,000 - $40,000","Compensation=$40,000 - $60,000","Compensation=$60,000 - $80,000","Compensation=$80,000 - $100,000","Compensation=$100,000 - $120,000","Compensation=$120,000 - $140,000","Compensation=$140,000 - $160,000","Compensation=More than $160,000","Compensation=Unemployed"), default="lhs"))
rules <- apriori(stackOver, parameter = list(minlen=2, supp=0.001, conf=0.6), appearance = list(rhs=c("Compensation=$20,000 - $40,000","Compensation=$80,000 - $100,000","Compensation=$100,000 - $120,000","Compensation=$120,000 - $140,000","Compensation=More than $160,000","Compensation=Unemployed"), default="lhs"))
rules <- apriori(stackOver, parameter = list(minlen=2, supp=0.005, conf=0.6), appearance = list(rhs=c("Open.to.new.job.opportunities=I am actively looking for a new job","Open.to.new.job.opportunities=I am not interested in other job opportunities"), default="lhs"))
rules <- apriori(stackOver, parameter = list( supp=0.0004, conf=0.1), appearance = list(rhs=c("Job.Satisfaction=I hate my job"), default="lhs"))
rules <- apriori(stackOver, parameter = list( supp=0.00001), appearance = list(lhs=c("Country=India","Industry=Consulting"), default="lhs"))
rules <- apriori(stackOver, parameter = list(supp=0.096, conf=0.91), appearance = list(rhs=c("JavaScript=Y"), default="lhs"))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)


 # remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

#Visualizing Association Rules
#Package arulesViz supports visualization of association rules with scatter plot, balloon plot, graph, parallel coordinates plot, etc.
install.packages( arules , scatterplot3d, vcd, seriation, igraph,"grid","cluster","TSP","gclus", "colorspace")
install.packages("arulesViz")

library(arulesViz)
plot(rules.pruned)

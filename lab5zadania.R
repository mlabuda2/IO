###################################################################### Zad 2 ##################################################################### 

iris.log <- log(iris[,1:4])
iris.stand <- scale(iris.log, center=TRUE)
iris.pca <- prcomp(iris.stand)
iris.final <- predict(iris.pca)[,1:2]
iris.numeric <- data.matrix(iris[,-5])
colnames(iris.numeric) <- NULL
iris.log <- log(iris.numeric)
iris.preproc <- scale(iris.log)
iris.pca <- prcomp(iris.preproc)
iris.pca.data <- predict(iris.pca)[,1:2]
#
iris.kmeans <- kmeans(iris.pca.data, 3)
plot(iris.pca.data, col = iris.kmeans$cluster)
points(iris.kmeans$centers, col = 1:3, pch = 4, cex = 3)
plot(iris.final, type = "p")
lines(iris.final[1:50,], col = "red", type = "p" )
lines(iris.final[51:100,], col = "green", type = "p" )
lines(iris.final[101:150,], col = "blue", type = "p" )

##################################################################### Zad 3 ##################################################################### 

# A2:  {chleb=TRUE} => {piwo=TRUE,czipsy=TRUE}
# Support: 0.4
# Confidence: 0.4/0.9 = 0.4444444

# A3: {piwo=TRUE} => {czipsy=TRUE}
# Support: 0.5
# Confidence: 0.5/0.7 = 0.7142857

# A4: {czipsy=TRUE} => {piwo=TRUE}
# Support: 0.5
# Confidence: 0.5/0.5 = 1

# A5: {masło=TRUE, ser=TRUE} => {chleb=TRUE}
# Support: 0.2
# Confidence: 0.2/0.2 = 1

##################################################################### Zad 4 ##################################################################### 

library(arules)

# find association rules with default settings
rules <- apriori(titanic.raw)
inspect(rules)
# rules with rhs containing "Survived" only
rules <- apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8), 
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"), 
                                   default="lhs"), 
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))


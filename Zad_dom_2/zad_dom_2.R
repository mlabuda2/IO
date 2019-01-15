setwd("/home/mati/IO/Zad_dom_2")

normalize <- function(x)
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}

### Czyszczenie ###
library(editrules)
# Dane
customers.dirty <- read.csv("~/Pobrane/WA_Fn-UseC_-Telco-Customer-Churn.csv")
set.seed(1234)
# Filtrowanie
nrow(customers.dirty)
clean.customers <- subset(customers.dirty,
                          is.finite(customers.dirty$SeniorCitizen) &
                          is.finite(customers.dirty$tenure) & 
                          is.finite(customers.dirty$MonthlyCharges) &
                          is.finite(customers.dirty$TotalCharges) &
                          customers.dirty$gender %in% c("Male", "Female") &
                          customers.dirty$Partner %in% c("No", "Yes") &
                          customers.dirty$Dependents %in% c("No", "Yes") &
                          customers.dirty$PhoneService %in% c("No", "Yes") &
                          customers.dirty$Churn %in% c("No", "Yes") &
                          customers.dirty$MultipleLines %in% c("No", "Yes", "No phone service") &
                          customers.dirty$InternetService %in% c("No", "Fiber optic", "DSL") &
                          customers.dirty$OnlineSecurity %in% c("No", "Yes", "No internet service") &
                          customers.dirty$OnlineBackup %in% c("No", "Yes", "No internet service") &
                          customers.dirty$DeviceProtection %in% c("No", "Yes", "No internet service") &
                          customers.dirty$TechSupport %in% c("No", "Yes", "No internet service") &
                          customers.dirty$StreamingTV %in% c("No", "Yes", "No internet service") &
                          customers.dirty$StreamingMovies %in% c("No", "Yes", "No internet service") &
                          customers.dirty$Contract %in% c("Month-to-month", "One year", "Two year") &
                          customers.dirty$PaymentMethod %in% c("Bank transfer (automatic)", "Mailed check", "Credit card (automatic)", "Electronic check")
                          )
nrow(clean.customers)

# Reguły
F <- editset(expression(
                PhoneService %in% c('Yes','No'),
                InternetService %in% c("No", "Fiber optic", "DSL"),
                if ( PhoneService == 'No') MultipleLines=="No phone service",
                if ( InternetService == 'No') OnlineSecurity=="No internet service",
                if ( InternetService == 'No') OnlineBackup=="No internet service",
                if ( InternetService == 'No') DeviceProtection=="No internet service",
                if ( InternetService == 'No') TechSupport=="No internet service",
                if ( InternetService == 'No') StreamingTV=="No internet service",
                if ( InternetService == 'No') StreamingMovies=="No internet service"
                )
            ) 

as.data.frame(F)
ve <- violatedEdits(F, clean.customers)
summary(ve)
plot(ve)

library(VIM)
library(deducorrect)
# Wypełnianie luk
R <- correctionRules("edit.txt")
correct.customers <- correctWithRules(R, clean.customers)
corrected.customers <- correct.customers$corrected

# Sprawdzanie NA

check_na <- function() {
  for (i in 1:21){
    print(sum(is.na(corrected.customers[i])))
  }
}
check_na()


#cc.norm <- corrected.customers
#cc.norm[6] <- normalize(corrected.customers[6])
#cc.norm[19] <- normalize(corrected.customers[19])
#cc.norm[20] <- normalize(corrected.customers[20])
#cc.training <- corrected.customers[ind==1, 2:20]
#cc.test <- corrected.customers[ind==2, 2:20]

#cc.norm.training <- cc.norm[ind==1, 2:20]
#cc.norm.test <- cc.norm[ind==2, 2:20]
#cc.trainLabels <- corrected.customers[ind==1, 21]
#cc.testLabels <- corrected.customers[ind==2, 21]

# Podział na zbiór test i tren
ind <- sample(2, nrow(corrected.customers), replace=TRUE, prob=c(0.67, 0.33))

# Klasyfikatory działają tylko na kolumnach numerycznych
cc.numerals <- corrected.customers
cc.numerals[1:2]<-NULL
cc.numerals[2:3]<-NULL
cc.numerals[3:14]<-NULL
View(cc.numerals)

#Normalizacja do Sieci neuronowych
cc.numerals.norm <- cc.numerals
cc.numerals.norm <- as.data.frame(lapply(cc.numerals[1:4], normalize))
cc.numerals.norm$yes <- c(cc.numerals$Churn == "Yes")
cc.numerals.norm$no <- c(cc.numerals$Churn == "No")
cc.numerals.norm.training <- cc.numerals.norm[ind==1, 1:6]
cc.numerals.norm.test <- cc.numerals.norm[ind==2, 1:6]

ccn.training <- cc.numerals[ind==1, 1:4]
ccn.test <- cc.numerals[ind==2, 1:4]
ccn.trainLabels <- cc.numerals[ind==1, 5]
ccn.testLabels <- cc.numerals[ind==2, 5]

require(neuralnet)
library(caret)
library(class)
library(e1071)
library(party)
#knn
cc_pred1 <- knn(train = ccn.training, test = ccn.test, cl = ccn.trainLabels, k=1)
cc_pred3 <- knn(train = ccn.training, test = ccn.test, cl = ccn.trainLabels, k=3)
cc_pred5 <- knn(train = ccn.training, test = ccn.test, cl = ccn.trainLabels, k=5)
cc_pred11 <- knn(train = ccn.training, test = ccn.test, cl = ccn.trainLabels, k=11)
cc_pred25 <- knn(train = ccn.training, test = ccn.test, cl = ccn.trainLabels, k=25)

cm_knn1 <- confusionMatrix(cc_pred1, ccn.testLabels)
cm_knn3 <- confusionMatrix(cc_pred3, ccn.testLabels)
cm_knn5 <- confusionMatrix(cc_pred5, ccn.testLabels)
cm_knn11 <- confusionMatrix(cc_pred11, ccn.testLabels)
cm_knn25 <- confusionMatrix(cc_pred25, ccn.testLabels)

knn1_accuracy <- cm_knn1$overall['Accuracy']
knn3_accuracy <- cm_knn3$overall['Accuracy']
knn5_accuracy <- cm_knn5$overall['Accuracy']
knn11_accuracy <- cm_knn11$overall['Accuracy']
knn25_accuracy <- cm_knn25$overall['Accuracy']

knn1_accuracy
knn3_accuracy
knn5_accuracy
knn11_accuracy
knn25_accuracy

knn1_tp <- cm_knn1$table["No","No"]
knn3_tp <- cm_knn3$table["No","No"]
knn5_tp <- cm_knn5$table["No","No"]
knn11_tp <- cm_knn11$table["No","No"]
knn25_tp <- cm_knn25$table["No","No"]
knn1_tn <- cm_knn1$table["Yes","Yes"]
knn3_tn <- cm_knn3$table["Yes","Yes"]
knn5_tn <- cm_knn5$table["Yes","Yes"]
knn11_tn <- cm_knn11$table["Yes","Yes"]
knn25_tn <- cm_knn25$table["Yes","Yes"]
knn1_fn <- cm_knn1$table["No","Yes"]
knn3_fn <- cm_knn3$table["No","Yes"]
knn5_fn <- cm_knn5$table["No","Yes"]
knn11_fn <- cm_knn11$table["No","Yes"]
knn25_fn <- cm_knn25$table["No","Yes"]
knn1_fp <- cm_knn1$table["Yes","No"]
knn3_fp <- cm_knn3$table["Yes","No"]
knn5_fp <- cm_knn5$table["Yes","No"]
knn11_fp <- cm_knn11$table["Yes","No"]
knn25_fp <- cm_knn25$table["Yes","No"]
knn1_fpr <- knn1_fp/(knn1_fp+knn1_tn)
knn3_fpr <- knn3_fp/(knn3_fp+knn3_tn)
knn5_fpr <- knn5_fp/(knn5_fp+knn5_tn)
knn11_fpr <- knn11_fp/(knn11_fp+knn11_tn)
knn25_fpr <- knn25_fp/(knn25_fp+knn25_tn)
knn1_tpr <- knn1_tp/(knn1_tp+knn1_fn)
knn3_tpr <- knn3_tp/(knn3_tp+knn3_fn)
knn5_tpr <- knn5_tp/(knn5_tp+knn5_fn)
knn11_tpr <- knn11_tp/(knn11_tp+knn11_fn)
knn25_tpr <- knn25_tp/(knn25_tp+knn25_fn)

# NaiveBayes
nb <- naiveBayes(Churn ~ ., data=cc.numerals)
nb_pred <- predict(nb, cc.numerals[,-5])
cm_nb <- confusionMatrix(nb_pred, cc.numerals[,5])
nb_accuracy <- cm_nb$overall['Accuracy']
nb_tp <- cm_nb$table["No","No"]
nb_tn <- cm_nb$table["Yes","Yes"]
nb_fn <- cm_nb$table["No","Yes"]
nb_fp <- cm_nb$table["Yes","No"]
nb_fpr <- nb_fp/(nb_fp+nb_tn)
nb_tpr <- nb_tp/(nb_tp+nb_fn)

# Drzewa
ccn_ctree <- ctree(Churn ~., data=cc.numerals)
ctree_pred <- predict(ccn_ctree, cc.numerals[,-5])
cm_ctree <- confusionMatrix(ctree_pred, cc.numerals[,5])
ctree_accuracy <- cm_ctree$overall['Accuracy']
ct_tp <- cm_ctree$table["No","No"]
ct_tn <- cm_ctree$table["Yes","Yes"]
ct_fn <- cm_ctree$table["No","Yes"]
ct_fp <- cm_ctree$table["Yes","No"]
ct_fpr <- ct_fp/(ct_fp+ct_tn)
ct_tpr <- ct_tp/(ct_tp+ct_fn)

# Sieci Neuronowe 
cc.nn <- neuralnet(yes + no ~ 
                     SeniorCitizen + tenure + MonthlyCharges + TotalCharges, data=cc.numerals.norm.training, hidden=3, stepmax=1e6)
ccnn.predict <- compute(cc.nn,cc.numerals.norm.test[,1:4])$net.result
idx <- apply(ccnn.predict, c(1), maxidx)
ccnn.predict <- c("Yes", "No")[idx]
ccnn.cm <- confusionMatrix(table(ccnn.predict, ccn.testLabels))
nn_tp <- ccnn.cm$table["No","No"]
nn_tn <- ccnn.cm$table["Yes","Yes"]
nn_fn <- ccnn.cm$table["No","Yes"]
nn_fp <- ccnn.cm$table["Yes","No"]
nn_fpr <- nn_fp/(nn_fp+nn_tn)
nn_tpr <- nn_tp/(nn_tp+nn_fn)
nn_accuracy <- ccnn.cm$overall['Accuracy']


# Wykres słupkowy
classificators <- matrix(c(knn1_accuracy, knn3_accuracy, knn5_accuracy, knn11_accuracy, knn25_accuracy,
                           nb_accuracy, ctree_accuracy, nn_accuracy), ncol=8)
classificators.names <- matrix(c("1NN", "3NN", "5NN", "11NN", "25NN", "NaiveBayes", "Trees", "NeuralNets"), ncol=8)
barplot(classificators, main="Skuteczność Klasyfikatorów", names.arg=classificators.names,
        ylab="Skuteczność [%]", xlab="Klasyfikatory")
# Wykres punktowy
# FPR = FP/N = FP/(FP+TN) = 1 - SPC
# TPR = TP/P = TP/(TP+FN)
colors <- c("blue", "coral", "aquamarine", "cornflowerblue","yellow", "darkgreen", "burlywood3", "firebrick3")
fpr <- c(knn1_fpr, knn3_fpr, knn5_fpr, knn11_fpr, knn25_fpr, nb_fpr, ct_fpr, nn_fpr)
tpr <- c(knn1_tpr, knn3_tpr, knn5_tpr, knn11_tpr, knn25_tpr, nb_tpr, ct_tpr, nn_tpr)
plot(fpr, tpr, col=colors, bg=colors, pch=21, xlab="False Positive Rate", ylab="True Positive Rate",
     main="False Positive and True Positive Ratings")
legend("topright", classificators.names, fill=colors)


# GRUPOWANIE KMEANS
ccn.log <- cc.numerals[1:4]
ccn.log[2:4] <- log(cc.numerals[2:4])
ccn.stand <- scale(ccn.log, center=TRUE)
ccn.pca <- prcomp(ccn.stand)
ccn.final <- predict(ccn.pca)[,1:2]
# k=3
ccn.kmeans <- kmeans(ccn.final, 2)
plot(ccn.final, col = ccn.kmeans$cluster)
points(ccn.kmeans$centers, col = 1:3, pch = 4, cex = 3)
# k=5
ccn.kmeans <- kmeans(ccn.final, 5)
plot(ccn.final, col = ccn.kmeans$cluster)
points(ccn.kmeans$centers, col = 1:3, pch = 4, cex = 3)

# ASOCJACJE
library(arules)
library(arulesViz)

cc2 <- corrected.customers
cc2$SeniorCitizen <- NULL

# find association rules with default settings
rules <- apriori(cc2)
inspect(rules)
# rules with rhs containing "Survived" only
rules <- apriori(cc2[2:21], parameter = list(minlen=15, supp=0.01, conf=0.8), 
                 appearance = list(rhs=c("Churn=No", "Churn=Yes"), 
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

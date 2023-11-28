################################
##
##  Author: Ruojia Sun, adapted from code by Prof. Ami Gates
##
################################

## LIBRARIES
library(rpart)   ## FOR Decision Trees
library(rattle)  ## FOR Decision Tree Vis
library(rpart.plot)
library(ggplot2)
library(caret)

###########################################################
##
##          Create testing and
##          training sets
################################################################

## Replace with your path
setwd("C:/Users/sunru/Downloads/ml-project") 
head(HarveyDF<-read.csv("harvey-anniversary-survey-cleaned.csv"))

## Set all features as factors
HarveyDF <- as.data.frame(unclass(HarveyDF),stringsAsFactors=TRUE) 
str(HarveyDF)

(Size <- (as.integer(nrow(HarveyDF)/4)))  ## Test will be 1/4 of the data
(SAMPLE <- sample(nrow(HarveyDF), Size))

## Create testing and training sets
(DF_Test<-HarveyDF[SAMPLE, ])
DF_Test$index <- as.numeric(row.names(DF_Test))
(DF_Test[order(DF_Test$index), ])
(DF_Train<-HarveyDF[-SAMPLE, ])

## Testing set
## Remove the labels and KEEP THEM
(DF_Test_Labels <- DF_Test$recovery) ## Label is "recovery"
DF_Test_NL<-DF_Test[ , -which(names(DF_Test) %in% c("recovery"))]
# (DF_Test_NL[1:5, 1:5])

## Training set
## Remove the labels and KEEP THEM
(DF_Train_Labels <- DF_Train$recovery)
DF_Train_NL<-DF_Train[ , -which(names(DF_Train) %in% c("recovery"))]
# (DF_Train_NL[1:5, 1:5])


###################################################
##
##     Decision Trees
##
##      First - train the model with your training data
##
##      Second - test the model - get predictions - compare
##               to the known labels you have.
###########################################################


## This code uses rpart to create decision tree
## Here, the ~ .  means to train using all data variables
## The DF_Train#label tells it what the label is called
## In this dataset, the label is called "label".

# DT 1: default cp (0.01) and splitting method (GINI)
DT <- rpart(DF_Train$recovery ~ ., data = DF_Train, method="class")
# summary(DT)

## DT 2: cp = 0.005, GINI
## The small cp the larger the tree, if cp is too small you have overfitting
DT2<-rpart(DF_Train$recovery ~ ., data = DF_Train,cp=0.005, method="class")
# summary(DT2)

## DT 3: cp = 0.005, information gain
DT3<-rpart(DF_Train$recovery ~ ., 
           data = DF_Train,cp=0.005, method="class",
           parms = list(split="information"),minsplit=2)
# summary(DT3)

## DT 4: cp = 0, information gain
DT4<-rpart(DF_Train$recovery ~ ., 
           data = DF_Train,cp=0, method="class",
           parms = list(split="information"),minsplit=2)
# summary(DT4)


DT3$variable.importance  ## before re-eval to add to 100

############################################################
##
## Predict the Testset using all 4 trees - 
## We will build a tree and a confusion matrix for all 4
##
###############################################################
## 
## DT 1
DT_Prediction= predict(DT, DF_Test_NL, type="class")
## Confusion Matrix
conf_mat <-table(DT_Prediction,DF_Test_Labels)
accuracy <- (conf_mat[1,1]+ conf_mat[2,2])/sum(conf_mat)
print(sprintf("accuracy: %.3f percent", 100*accuracy))
# Plot tree
fancyRpartPlot(DT)

# Plot confusion matrix
plt <- as.data.frame(conf_mat)
ggplot(plt, aes(DT_Prediction, DF_Test_Labels, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=8) +
  scale_fill_gradient(low="white", high="#009194",limits = c(0,max(plt$Freq)))+
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("No","Yes")) +
  scale_y_discrete(labels=c("No","Yes")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))


## DT 2
DT_Prediction2 <- predict(DT2, DF_Test_NL, type = "class")
conf_mat <- table(DT_Prediction2,DF_Test_Labels) ## one way to make a confu mat
accuracy <- (conf_mat[1,1]+ conf_mat[2,2])/sum(conf_mat)
print(sprintf("accuracy: %.3f percent", 100*accuracy))
# Plot tree
fancyRpartPlot(DT2)

# Plot confusion matrix
plt <- as.data.frame(conf_mat)
ggplot(plt, aes(DT_Prediction2, DF_Test_Labels, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=8) +
  scale_fill_gradient(low="white", high="#009194",limits = c(0,max(plt$Freq)))+
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("No","Yes")) +
  scale_y_discrete(labels=c("No","Yes")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))


## DT 3

DT_Prediction3 <- predict(DT3, DF_Test_NL, type = "class")
conf_mat <- table(DT_Prediction3,DF_Test_Labels) ## one way to make a confu mat
accuracy <- (conf_mat[1,1]+ conf_mat[2,2])/sum(conf_mat)
print(sprintf("accuracy: %.3f percent", 100*accuracy))
# Plot tree
fancyRpartPlot(DT3)

# Plot confusion matrix
plt <- as.data.frame(conf_mat)
ggplot(plt, aes(DT_Prediction3, DF_Test_Labels, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=8) +
  scale_fill_gradient(low="white", high="#009194",limits = c(0,max(plt$Freq)))+
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("No","Yes")) +
  scale_y_discrete(labels=c("No","Yes")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))

## DT 4
DT_Prediction4 <- predict(DT4, DF_Test_NL, type = "class")
conf_mat <- table(DT_Prediction4,DF_Test_Labels) ## one way to make a confu mat
accuracy <- (conf_mat[1,1]+ conf_mat[2,2])/sum(conf_mat)
print(sprintf("accuracy: %.3f percent", 100*accuracy))
# Plot tree
fancyRpartPlot(DT4)

# Plot confusion matrix
plt <- as.data.frame(conf_mat)
ggplot(plt, aes(DT_Prediction4, DF_Test_Labels, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=8) +
  scale_fill_gradient(low="white", high="#009194",limits = c(0,max(plt$Freq)))+
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("No","Yes")) +
  scale_y_discrete(labels=c("No","Yes")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))


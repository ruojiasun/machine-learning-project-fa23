####################################################
# Author: Ruojia Sun, adapted from code by Prof. Ami Gates
####################################################

####################################################
# 3(1) Determine if a user should get a credit card
# Model: Decision tree
####################################################

## LIBRARIES
library(rpart)   ## FOR Decision Trees
library(rattle)  ## FOR Decision Tree Vis
library(rpart.plot)
library(ggplot2)
library(caret)

setwd("C:/Users/sunru/Downloads/")
data<-read.csv("amazon-user.csv")

## Set all features as factors
amazon_DF <- as.data.frame(unclass(data[,1:2]),stringsAsFactors=TRUE) 
str(amazon_DF)

(Size <- (as.integer(nrow(amazon_DF)/4)))  ## Test will be 1/4 of the data
(SAMPLE <- sample(nrow(amazon_DF), Size))

## Create testing and training sets
(DF_Test<-amazon_DF[SAMPLE, ])
DF_Test$index <- as.numeric(row.names(DF_Test))
(DF_Test[order(DF_Test$index), ])
(DF_Train<-amazon_DF[-SAMPLE, ])

## Testing set
## Remove the labels and KEEP THEM
(DF_Test_Labels <- DF_Test$Credit.card) ## Label is "Credit.card"
DF_Test_NL<-DF_Test[ , -which(names(DF_Test) %in% c("Credit.card"))]
# (DF_Test_NL[1:5, 1:5])

## Training set
## Remove the labels and KEEP THEM
(DF_Train_Labels <- DF_Train$Credit.card)
DF_Train_NL<-DF_Train[ , -which(names(DF_Train) %in% c("Credit.card"))]
# (DF_Train_NL[1:5, 1:5])


###################################################
##
##   Train the model with your training data
##
###########################################################


## This code uses rpart to create decision tree
## Here, the ~ .  means to train using all data variables
## The DF_Train#label tells it what the label is called
## In this dataset, the label is called "label".

# DT 1: default cp (0.01) and splitting method (GINI)
DT <- rpart(DF_Train$Credit.card ~ ., data = DF_Train, method="class")
# summary(DT)

############################################################
##
## Build trees and predict the Testset
##
############################################################

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
plt$DT_Prediction <- factor(plt$DT_Prediction, levels=rev(levels(plt$DT_Prediction)))
ggplot(plt, aes(DT_Prediction, DF_Test_Labels, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=8) +
  scale_fill_gradient(low="white", high="#009194",limits = c(0,max(plt$Freq)))+
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Yes","No")) +
  scale_y_discrete(labels=c("No","Yes")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))


####################################################
# 3(2) Determine which products to advertise
# Model: Association rule mining
####################################################

# # DO these installs once
# install.packages("arules")
# install.packages("arulesViz", dependencies = TRUE)

library(arules)
library(arulesViz)

data_purchases<- data[,3:4]
write.csv(data_purchases, file = tmp <- file(), row.names = FALSE)

amazon_transactions <- read.transactions(tmp,
                                        rm.duplicates = FALSE,
                                        format = "basket",  ##if you use "single" also use cols=c(1,2)
                                        sep=",",  ## csv file
                                        cols=NULL) ## The dataset has no row numbers
(inspect(amazon_transactions))

##### Use apriori to get the RULES
Frules = arules::apriori(amazon_transactions, parameter = list(support=0.05,
                                                              confidence=.5, minlen=2))
(inspect(Frules))


## Sort rules by a measure such as conf, sup, or lift
SortedRules <- sort(Frules, by="lift", decreasing=TRUE)
inspect(SortedRules[1:5])
(summary(SortedRules))


## Visualize

subrules <- head(sort(SortedRules, by="lift"),5)
# plot(subrules)
#plot(subrules, method="graph", engine="interactive")
plot(subrules, method="graph", engine="htmlwidget")
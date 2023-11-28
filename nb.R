################################
##
##  Author: Ruojia Sun, adapted from code by Prof. Ami Gates
##
################################

# install.packages('e1071')
library(e1071)
# install.packages('naivebayes')
library(naivebayes)
# install.packages('tidyverse')
library(tidyverse)
#install.packages('ggplot2')
library(ggplot2)
# install.packages('caret')
library(caret)
# install.packages('klaR')
library(klaR)
# install.packages('dplyr')
library(dplyr)

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

# ##Check balance of test set
# table(DF_Test$recovery)

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


#################################################################
##
##     RUN Naive Bayes 
##
#################################################################

# Naive Bayes with e1071
(NB_e1071<-naiveBayes(DF_Train_NL, DF_Train_Labels, laplace = 1))
NB_e1071_Pred <- predict(NB_e1071, DF_Test_NL)
(conf_mat <- table(NB_e1071_Pred,DF_Test_Labels))
# calculate accuracy
accuracy <- (conf_mat[1,1]+ conf_mat[2,2])/sum(conf_mat)
print(sprintf("accuracy: %.3f percent", 100*accuracy))

# Plot confusion matrix
plt <- as.data.frame(conf_mat)

ggplot(plt, aes(NB_e1071_Pred, DF_Test_Labels, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=8) +
  scale_fill_gradient(low="white", high="#009194",limits = c(0,max(plt$Freq)))+
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("No","Yes")) +
  scale_y_discrete(labels=c("No","Yes")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))


# Naive Bayes with k-fold cross validation with caret/klaR
train_control <- trainControl(method="cv", number=10)
y<-HarveyDF$recovery
x<-HarveyDF[ , -which(names(HarveyDF) %in% c("recovery"))]
NB_CV <- train(x, y, trControl=train_control, method="nb")
print(NB_CV)

# Variable importance
Var_Imp <- varImp(NB_CV)
plot(Var_Imp)

#####################################
#
# Plot conditional probabilities
#
#####################################

# Home damage
p_home_damage <- as.data.frame(NB_e1071$tables$home.damage)
p_home_damage <- p_home_damage %>% 
  mutate_if(is.numeric, round, digits=7)

ggplot(p_home_damage, aes(home.damage, DF_Train_Labels, fill=Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=10) +
  scale_fill_gradient(low="white", high="#003194",limits = c(0,1))+
  labs(x = "Home Damage",y = "Recovery") +
  scale_x_discrete(labels=c("Damaged","Not Damaged")) +
  scale_y_discrete(labels=c("No","Yes"))+  
  theme(axis.text=element_text(size=14),
    axis.title=element_text(size=14,face="bold"),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14))

# Reduced hours
p_reduced_hours <- as.data.frame(NB_e1071$tables$reduced.hours)
p_reduced_hours <- p_reduced_hours %>% 
  mutate_if(is.numeric, round, digits=7)

ggplot(p_reduced_hours, aes(reduced.hours, DF_Train_Labels, fill=Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=10) +
  scale_fill_gradient(low="white", high="#003194",limits = c(0,1))+
  labs(x = "Reduced Hours",y = "Recovery") +
  scale_x_discrete(labels=c("No","Yes")) +
  scale_y_discrete(labels=c("No","Yes"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))

# Race
p_race <- as.data.frame(NB_e1071$tables$race)
p_race <- p_race %>% 
  mutate_if(is.numeric, round, digits=7)

ggplot(p_race, aes(race, DF_Train_Labels, fill=Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=10) +
  scale_fill_gradient(low="white", high="#003194",limits = c(0,1))+
  labs(x = "Race",y = "Recovery") +
  scale_x_discrete(labels=c("Black or African-American","White")) +
  scale_y_discrete(labels=c("No","Yes")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))

# Income
p_income <- as.data.frame(NB_e1071$tables$income)
p_income <- p_income %>% 
  mutate_if(is.numeric, round, digits=7)

ggplot(p_income, aes(income, DF_Train_Labels, fill=Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=8) +
  scale_fill_gradient(low="white", high="#003194",limits = c(0,1))+
  labs(x = "Income",y = "Recovery") +
  scale_x_discrete(labels=c("100 TO UNDER 200%","200%+","UNDER 100%")) +
  scale_y_discrete(labels=c("No","Yes"))+  
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))

# Gender
p_gender <- as.data.frame(NB_e1071$tables$gender)
p_gender <- p_gender %>% 
  mutate_if(is.numeric, round, digits=7)

ggplot(p_gender, aes(gender, DF_Train_Labels, fill=Freq)) +
  geom_tile() + geom_text(aes(label=Freq), size=10) +
  scale_fill_gradient(low="white", high="#003194",limits = c(0,1))+
  labs(x = "Gender",y = "Recovery") +
  scale_x_discrete(labels=c("Female","Male")) +
  scale_y_discrete(labels=c("No","Yes")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14))


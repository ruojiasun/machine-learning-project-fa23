########################################
# Author: Ruojia Sun, adapted from code by Prof. Ami Gates
########################################

# # DO these installs once
# install.packages("arules")
# install.packages("TSP")
# install.packages("data.table")
# # NOTE: If you are asked if you want to INSTALL FROM SOURCE - click YES!
# install.packages("arulesViz", dependencies = TRUE)
# # IMPORTANT ## arules ONLY grabs rules with ONE item on the right
# install.packages("sp") 
# install.packages("dplyr", dependencies = TRUE)
# install.packages("purrr", dependencies = TRUE)
# install.packages("devtools", dependencies = TRUE)
# install.packages("tidyr")
# install_github("mhahsler/arulesViz")

library(arules)
library(arulesViz)

setwd("C:/Users/sunru/Downloads/ml-project")
Strong_Survey_Data <- read.transactions("strong_transaction.csv",
                           rm.duplicates = FALSE,
                           format = "basket",  ##if you use "single" also use cols=c(1,2)
                           sep=",",  ## csv file
                           cols=NULL) ## The dataset has no row numbers
inspect(Strong_Survey_Data)

##### Use apriori to get the RULES
Frules = arules::apriori(Strong_Survey_Data, parameter = list(support=0.05,
                                                 confidence=.5, minlen=2))
inspect(Frules)

## Plot of which items are most frequent
itemFrequencyPlot(Strong_Survey_Data, topN=20, type="absolute")

## Sort rules by a measure such as conf, sup, or lift
SortedRules <- sort(Frules, by="lift", decreasing=TRUE)
inspect(SortedRules[1:15])
(summary(SortedRules))

## Selecting or targeting specific rules  RHS
LostJobRules <- apriori(data=Strong_Survey_Data,parameter = list(supp=.01, conf=.01, minlen=2),
                     appearance = list(default="lhs", rhs="lost job"),
                     control=list(verbose=FALSE))
LostJobRules <- sort(LostJobRules, decreasing=TRUE, by="lift")
inspect(LostJobRules[1:15])


## Visualize
## tcltk

subrules <- head(sort(SortedRules, by="lift"),15)
# plot(subrules)
#plot(subrules, method="graph", engine="interactive")
plot(subrules, method="graph", engine="htmlwidget")

LostJobsubrules <- head(sort(LostJobRules, by="lift"),15)
# plot(subrules)
#plot(subrules, method="graph", engine="interactive")
plot(LostJobsubrules, method="graph", engine="htmlwidget")

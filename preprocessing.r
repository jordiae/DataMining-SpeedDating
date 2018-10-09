
#Install packages
#Little test
install.packages("BaylorEdPsych")
install.packages("plyr")
install.packages("dplyr")

library(BaylorEdPsych)
install.packages("mvnmle")
library(mvnmle)
installed.packages("cluster")
library(cluster)
installed.packages("dplyr")
library("plyr")
library("dplyr")

# 1 - Building the original data matrix and introducing the data into the pre-processing tool
# NAs can be both 'NA' or empty, in this dataset
original_data <-read.csv("Speed.csv", header=TRUE,na.strings=c("","NA"))

# Checking the dataset. Visualization, basic descriptive statistics.

dim(original_data) # size
summary(original_data)
sum(is.na(original_data))/(sum(!is.na(original_data)) + sum(is.na(original_data)))*100 # % NAs
# "raw" type of each variable. Warning: only raw type, integers may codify categorical
# variables, we have had to manually inspect them
sapply(original_data, class) 
# Number of NAs per columns
na_count <-sapply(original_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count


# Also: read the Kaggle description, read the PDF telling the meaning of each variable, visualizing values for different rows in Kaggle


# 2 - Determining the working data matrix

# Rows:we are going to select the non-variations and preference scale 1-100 rounds in order to have coherent and clean data

selected_rows_data <- original_data [!(original_data$wave==5 | original_data$wave ==6 | original_data$wave ==7 | original_data$wave ==8 | original_data$wave ==9 | original_data$wave ==12 | original_data$wave ==13 | original_data$wave ==14 | original_data$wave ==18 | original_data$wave ==19 | original_data$wave ==20 | original_data$wave ==21),]


# Columns:
# We have many, many variables. Some of them are introducing noise,
# some of them are redundant or way to concrete, some of them are out of the scope
# of our intended analysis etc (explained in doc)
# "Expert" Variable selection (justification: redundancy, scope)
# We are kipping only some identification columns, but won't analyze them, obviously
# undergra, iid and pid only for missing imputation purposes
selected_vars<-c("iid","pid","undergra","gender","round","order","match","int_corr","samerace","age_o","race_o","pf_o_att","pf_o_sin", "pf_o_int", "pf_o_fun", "pf_o_amb","pf_o_sha" ,"dec_o","attr_o","sinc_o","intel_o","fun_o","amb_o","shar_o","age","field_cd","mn_sat","tuition","race","imprace","income","goal","date","go_out","dec","like","imprelig")
selected_columns_data  <- selected_rows_data[ selected_vars]

#declare qualitative variables

# doubts: "importance", like etc out of 10 (1-10), are really categorical?
# order is numeric? it doesn't make sense to analyze it by itself


selected_columns_data$gender <- as.factor(selected_columns_data$gender)
selected_columns_data$match <- as.factor(selected_columns_data$match)
selected_columns_data$samerace <- as.factor(selected_columns_data$samerace)
selected_columns_data$field_cd <- as.factor(selected_columns_data$field_cd)
selected_columns_data$race <- as.factor(selected_columns_data$race)
selected_columns_data$race_o <- as.factor(selected_columns_data$race_o)
selected_columns_data$goal <- as.factor(selected_columns_data$goal)
selected_columns_data$date <- as.factor(selected_columns_data$date)
selected_columns_data$go_out <- as.factor(selected_columns_data$go_out)
selected_columns_data$dec <- as.factor(selected_columns_data$dec)

#change labels of categorical values
selected_columns_data$gender <- revalue(selected_columns_data$gender, c("0"="F","1"="M"))
selected_columns_data$match <- revalue(selected_columns_data$match, c("0"="N","1"="Y"))
selected_columns_data$samerace <- revalue(selected_columns_data$samerace, c("0"="N","1"="Y"))
selected_columns_data$field_cd <- revalue(selected_columns_data$field_cd, c("1"="Law","2"="Math", 
                                                                            "3"="SocialSci", "4"="MedPharma", 
                                                                            "5"="Eng","6"="Jour", "7"="HRP","8"="Econ",
                                                                            "9"="Edu","10"="Science", "11"="SocialW",
                                                                            "12" = "Undergrad", "13"="PolSci", "14"="Film",
                                                                            "15"="Art", "16"="Lan","17"="Arch", "18"="Other"))
selected_columns_data$race <- revalue(selected_columns_data$race, c("1"="Black", "2" = "Cauc", 
                                                                    "3"="Latin", "4"="Asian", "5"="Nat",
                                                                    "6" = "Other"))
selected_columns_data$race_o <- revalue(selected_columns_data$race_o, c("1"="Black", "2" = "Cauc", 
                                                                    "3"="Latin", "4"="Asian", "5"="Nat",
                                                                    "6" = "Other"))
selected_columns_data$goal <- revalue(selected_columns_data$goal, c("1"="Fun", "2"="Meet", "3"="Date", "4"="Serious", "5"="Say","6"="Other"))

selected_columns_data$date <- revalue(selected_columns_data$date, c("1"="SevWeek", "2"="2Week", "3"="1Week", "4"="2Month","5"="1Month",
                                                                    "6"="SevYear","7"="Never"))
selected_columns_data$go_out <- revalue(selected_columns_data$go_out, c("1"="SevWeek", "2"="2Week", "3"="1Week", "4"="2Month","5"="1Month",
                                                                    "6"="SevYear","7"="Never"))
selected_columns_data$dec <- revalue(selected_columns_data$dec, c("0"="N","1"="Y"))
selected_columns_data$dec_o <- revalue(selected_columns_data$dec, c("0"="N","1"="Y"))




# We realize that tuition, mn_sat and income are detected as categorical by R, and
# simple declaring them as numeric doesn't work, so we have to manually remove commas
selected_columns_data$mn_sat <-as.numeric(gsub(",","",selected_columns_data$mn_sat))
selected_columns_data$tuition <- as.numeric(gsub(",","",selected_columns_data$tuition))
selected_columns_data$income <- as.numeric(gsub(",","",selected_columns_data$income))









# So, now he have the working matrix (selected_columns_data)


# 3 - outlier detection, visualization
# Plots of all variables, manual inspection
n_plot <- c("pid","iid","met")
for(i in 1:length(selected_columns_data[1,])) {
  name <- colnames(selected_columns_data)[i]
  if(!(name %in% n_plot)){
    if (is.factor(selected_columns_data[,i])) {
      plot(selected_columns_data[,i], main=name)
    }else {
      boxplot(selected_columns_data[,i], main=name)
    }
  }
}

# And: % of NA's for each selected column

na_count_selected <-sapply(selected_columns_data, function(y) 100*sum(length(which(is.na(y))))/nrow(selected_columns_data))
na_count_selected <- data.frame(na_count_selected)
na_count_selected

# So, inspecting plots, NA's and taking into account the meaning of each variable,
# we try to detect possible outliers (id's ommited)

# Please notice that by using boxplots we are not implying that all variables follow
# a Gaussian distribution, it's for for detecting "potential" outliers but we won't necessarily
# label them as actual outliers

# gender: no outliers, no NAs, all values 0 or 1, OK.
# round: ok.
# order: ok.
# match: same as gender.
# int_corr: 1.91% missing , no outliers in boxplot

# samerace: same as gender.
# age_o: 0.53% missing, 3 values don't fit in the boxplot but are "real" ages
# race_o: 0.53% missing, it seems it has no outliers
# pf_o_att 0.96% missing. 
# dec_o: same as gender

# attr_o 1.28% missing
# sinc_o 2.34% missing
# intel_o 2.55% missing
# fun_o 3.21 % missing
# amb_o 8.9% missing
# shar_o 13.48% missing
# last too maybe too big, we are going to take a look at it later
# No outliers, but, again, fractional values. We are going to take the floor but still
# keep them as categorical.

# age: 0.53% missing, no outliers (same as age_o)
# field_cd: 1.04% missing, no outliers apparently

# mn_sat: 62.5% missing, apparently 2 potential outliers but they are not; they are "real" SAT # scores

#tuition: 58.0 %missing, no apparent outliers

#race 0.53% missing no outliers apparently

#imprace 0.96% missing,  There are 8 rows with value 0 it’s supposed to be a value between #1-10, maybe 0 is equivalent to NA? (outliners in 0)

#income 48.72% missing, no apparent outliers


#goal 0.96% missing no outliners

#date 1.44% missing no outliners
#go_out 0.96% missing no outliners

#dec 0% missing binary with no outliners

#like 1.41% missing, outliners in some intermediate values(ex: 4.5, 5.5…)

# met 3.16% missing wrong data 1 is YES and the rest NO binary? Shall we delete this
# variable?

# imprelig, 0.96 %no outliners





# ERROR DETECTION AND TREATMENT





# imprace -> 0 is not a valid scale value (1-10), we will replace all occurrences (8)  by NA
selected_columns_data$imprace[selected_columns_data$imprace == 0] <- NA


# met
# We are going to delete this variable because the values are not coherent with the documentation





# MISSING IMPUTATION
data_pending_missing_imputation <- selected_columns_data

# tuition and mn_sat. structural -> 0
data_pending_missing_imputation$tuition[is.na(data_pending_missing_imputation$undergra)] <- 0
data_pending_missing_imputation$mn_sat[is.na(data_pending_missing_imputation$undergra)] <- 0
data_pending_missing_imputation$university <- data_pending_missing_imputation

data_pending_missing_imputation$university <- as.numeric(is.na(data_pending_missing_imputation$undergra))

data_pending_missing_imputation$university <- as.factor(data_pending_missing_imputation$university)

#change labels of categorical values
data_pending_missing_imputation$university <- revalue(data_pending_missing_imputation$university, c("0"="N","1"="Y"))

data_pending_missing_imputation <- data_pending_missing_imputation[data_pending_missing_imputation$iid != "28" & data_pending_missing_imputation$pid != "28" & data_pending_missing_imputation$iid != "58" & data_pending_missing_imputation$pid != "58" & data_pending_missing_imputation$iid != "59" & data_pending_missing_imputation$pid != "59", ]


#
# int_corr: 1.91% missing , no outliers in boxplot.in
rowswithmissingint_corr<-filter(.data = data_pending_missing_imputation,is.na(int_corr))
summary(rowswithmissingint_corr)
# After looking at all the rows with missing Data it can be seen, that there are three persons, that are responsible for the missing Values:
# Person 28 is involved in the first 32 rows with missing Values(the probablity is high, that this person didn't fill the Questionare right)
# Solving suggestion: Remove all rows for this person(filling the data would need a lot of assumptions about the Person and bias our results)

# The similar problem accurs for Person 58 and 59.
# cutting all the rows where they accur might be the best solution (Discussion)
# These persons also didn't answer many other columns, so in my opinion its the best way if we cut them

# age_o: 0.53% missing, 3 values don't fit in the boxplot but are "real" ages
# All persons that didn't talk about their age are 58 and 59 so if we cut them there won't be any missing values left

# race_o: 0.53% missing, it seems it has no outliers
# Same as age_o Person 58 and 59 are responsible for the missing values
# pf_o_att 0.96% missing.
# For all Values that miss in pf_o_att also by cutting Person 28,58,59 all missing Values will be cut out of the data set
# dec_o: same as gender

# attr_o 1.28% missing
# Again some missing Values are related to 58 and 59 since we want to focus on other values now, the rows with pid or iid will be cut out before continuing with the preprocessing
data_pending_missing_imputation <- as.data.frame(filter(data_pending_missing_imputation, data_pending_missing_imputation$pid != 28 & data_pending_missing_imputation$iid != 28 & data_pending_missing_imputation$pid != 58& data_pending_missing_imputation$iid != 58 &data_pending_missing_imputation$pid != 59& data_pending_missing_imputation$iid != 59))

# After that, there are still two types of missing values for the following attributes. Rows with single missing Values and
# rows with all of them missing. I would suggest cutting out the rows with all the values missing, because imputation could lead to
# a big bias in our test. If there are no values the questionare was probably not answered.

data_pending_missing_imputation <-  as.data.frame(filter(data_pending_missing_imputation, !is.na(data_pending_missing_imputation$attr_o)| !is.na(data_pending_missing_imputation$sinc_o)| !is.na(data_pending_missing_imputation$intel_o)| !is.na(data_pending_missing_imputation$fun_o)| !is.na(data_pending_missing_imputation$amb_o)| !is.na(data_pending_missing_imputation$shar_o)))
# After that there are only 4 rows left with missing attr_o. For this rows this is the only missing value. The possibilities are now:
# 1.) Remove these rows (would not suggest because if we will lose the data)
# 2.) Impute values by expert opinion (could be a good shot, but we aren't experts and cannot access expert opinions right now)
# 3.) Impute values with the mean of the other 5 values
# 4.) Impute values with the mean of all attr_o values
# 5.) Impute values with the median of all attr_o values related to that person
# I think the most precise Imputation would be the median of the attr_o values related to that person
# Filter the data so only the rows with the specific iid are left:
iid10 <- filter(data_pending_missing_imputation, data_pending_missing_imputation$iid == 10)
# Median = 8
# set value for this row 8
data_pending_missing_imputation[96,"attr_o"] <- 8
iid22 <- filter(data_pending_missing_imputation,  data_pending_missing_imputation$iid == 22)
# Median = 7
# set value for this row 7
data_pending_missing_imputation[224,"attr_o"] <- 7
iid37 <- filter(data_pending_missing_imputation,  data_pending_missing_imputation$iid == 37)
# Median = 7
# set value for this row 7
data_pending_missing_imputation[447,"attr_o"] <- 7
iid104 <- filter(data_pending_missing_imputation,  data_pending_missing_imputation$iid == 104)
# Median = 10
# set value for this row 10
data_pending_missing_imputation[1440,"attr_o"] <- 10
# #sinc_o 2.34% missing
# # Again, there are some columns without any of the remaining 5 Values in there, so firep is to cut these out of the dataset,
# data_pending_missing_imputation <- filter(data_pending_missing_imputation, !is.na(sinc_o)| !is.na(intel_o)| !is.na(fun_o)|!is.na(amb_o) |!is.na(shar_o))
# # after doing that there are still 32 NAs left over
# # As an efficient solution taking the median over the values is one option. Again we are facing a difficult decicion
# # For now the median of all the missing values
# # It might be also good to remove all rows that miss more than one attribute of these. After that a Imputation for the
# # single values can start.
# # One option to tackle the single missing values could be taking the median of the 5 other attributes in order to
# subsetMIMI = data_pending_missing_imputation[,c('int_corr', 'attr_o', 'age', 'age_o')]
# ####################Still to implement
# #
#
# # intel_o 2.55% missing
#
# # fun_o 3.21 % missing
# # amb_o 8.9% missing
# # shar_o 13.48% missing
#
#
# # age: 13.48% missing, no outliers (same as age_o)
# # field_cd: 1.04% missing, no outliers apparently
# # Solution: Coding the NA as not given answer and add this category
#
# # mn_sat: 62.5% missing, apparently 2 potential outliers but they are not; they are "real" SAT # scores
#
# #tuition: 58.0 %missing, no apparent outliers
# # Set all of the NA to "0" because they probably didn't attend college
#
# #race 0.53% missing no outliers apparently
#
# #imprace 0.96% missing,
# # One person didnt fill out how important the race attribute is to itself
# # There are at least two options:
# # 1.) Cutting out all the lines
# # 2.) Assuming that this person didn't want to answer this question
# # income 48.72% missing, no apparent outliers
# # Different approaches: taking the Mean of all other incomes(might bias the analysis)
# # Adding 0 as salary just to analyse if there is an influence weather somebody wanted to say it or not
# # Since more than half of the people didn't want to share their income it might be something interesting to analyse
# ############ Implementation missing
#
# #goal 0.96% missing no outliners
# # Already fixed by previous cut rows
#
# #date 1.44% missing no outliners
# # Again the missing value is only refering to one person 413 maybe we could estimate a value either by analysing this specific person
# # or by taking the mean or the median of all other persons or of all other female person
#
# #go_out 0.96% missing no outliners
# # fixed by the previous cutting of rows
# # like 25 still missing
# # The missing Values seem to be random, therefore we could maybe use a prediction model like linear combination or something like this
#
#
#
#



selected_vars<-c("gender","round","order","match","int_corr","samerace","age_o","race_o","pf_o_att","pf_o_sin", "pf_o_int", "pf_o_fun", "pf_o_amb","pf_o_sha" ,"dec_o","attr_o","sinc_o","intel_o","fun_o","amb_o","shar_o","age","field_cd","mn_sat","tuition","race","imprace","income","goal","date","go_out","dec","like","imprelig")
data_pending_missing_imputation  <- data_pending_missing_imputation[selected_vars]

littleTest <- LittleMCAR(data_pending_missing_imputation)
littleTest$amount.missing
#The rows with the most number of NA have 17 NA
#Can we erase this rows with more than 15 NA?
littleTest$data$DataSet80




# Ultimate missing imputation


addUnknown <- function(x){
  if(is.factor(x) && sum(is.na(x) > 0 )) {
    y = factor(x, levels=c(levels(x), "Unknown"))
    y[is.na(y)] <- "Unknown"
    return(y)
  }
  return(x)
}

data_pending_missing_imputation <- as.data.frame(lapply(data_pending_missing_imputation, addUnknown))



# # MIMMI algorithm
# subsetMIMI = data_pending_missing_imputation[,c('int_corr', 'attr_o', 'age', 'age_o')]
# 
# # Creates dissimilarity matrix
# dissimMatrix <- daisy(subsetMIMI, metric = "gower", stand=TRUE)
# 
# # Creates distance matrix and hierarchical tree
# distMatrix<-dissimMatrix^2
# 
# hierarchicalTree <- hclust(distMatrix,method="ward.D2")  
# 
# plot(hierarchicalTree)
# 
# ######################################
# # Looking at the plot we choose to split the dataset in 7 clusters.
# k<-7
# cutTree <- cutree(hierarchicalTree, k=k)
# 
# # We add the cluster number into the dataset 
# data_pending_missing_imputation$CLUSTER <- as.factor(cutTree)
# 
# # Computation and assignation of the variable means for each cluster
# 
# for (i in 1:k) {
#   for (variableName in colnames(data)) {
#     if (!is.factor(data[,variableName])) {
#       data[data$CLUSTER == i & is.na(data[,variableName]), variableName] <- mean(data[data$CLUSTER == i & !is.na(data[,variableName]), variableName]) 
#     }
#   }
# }
# 
# # Deletes cluster column
# data_pending_missing_imputation <- subset(data_pending_missing_imputation, select = -c(CLUSTER))
# 
# # Detects the number of NA in each column
# percentatgesNA = data.frame(matrix(0, length(data), 3))
# names(percentatgesNA) = c("Variable", "Factor", "Percentage")
# 
# for (i in 1:ncol(data_pending_missing_imputation)){
#   percentatgesNA[i,]$Variable <- names(data_pending_missing_imputation)[i]
#   
#   percentatgesNA[i,]$Factor <- is.factor(data_pending_missing_imputation[,i])
#   
#   numNA <- sum(is.na(data_pending_missing_imputation[i]))
#   percentatgesNA[i,]$Percentage <- numNA/nrow(data_pending_missing_imputation) * 100
# }


vars_according_NAs <- as.data.frame(lapply(data_pending_missing_imputation, function(x) if (is.numeric(x)) return(sum(is.na(x))) else return(-1)))

colnames(vars_according_NAs)

fullVariables <- c()
uncompleteVars <- c()

for (var in colnames(vars_according_NAs)){
  if (vars_according_NAs[var] > 0) {
    uncompleteVars <- c(uncompleteVars,vars_according_NAs[var])
  }
  else if (vars_according_NAs[var] == 0) {
    fullVariables <- c(fullVariables,vars_according_NAs[var])
  }
    
}

fullVariables <- colnames(as.data.frame(fullVariables))

uncompleteVars <- sort(as.data.frame(uncompleteVars))

aux<-data_pending_missing_imputation[,fullVariables]
library(class)








for (k in colnames(as.data.frame(uncompleteVars))){
  aux1 <- aux[!is.na(data_pending_missing_imputation[,k]),]
  dim(aux1) 
  aux2 <- aux[is.na(data_pending_missing_imputation[,k]),]
  dim(aux2)
  
  RefValues<- data_pending_missing_imputation[!is.na(data_pending_missing_imputation[,k]),k]
  #Find nns for aux2
  knn.values = knn(aux1,aux2,RefValues)   
  
  #CARE: neither aux1 nor aux2 can contain NAs
  
  
  #CARE: knn.ing is generated as a factor. 
  #Be sure to retrieve the correct values
  
  data_pending_missing_imputation[is.na(data_pending_missing_imputation[,k]),k] = as.numeric(as.character(knn.values))
  fullVariables<-c(fullVariables, k)
  aux<-data_pending_missing_imputation[,fullVariables]
}

data_after_imputation <- data_pending_missing_imputation

# NEW VARIABLES
# We are going to create the var "difference of age"
data_after_imputation$diff_age <- abs(data_after_imputation$age - data_after_imputation$age_o)


# Correct out of scale knn values
data_after_imputation$like[data_after_imputation$like < 1] <- 1


# 100 scale

data_after_imputation$pf_sum <- rowSums(data_after_imputation[,c("pf_o_att", "pf_o_sin", "pf_o_fun", "pf_o_int", "pf_o_amb", "pf_o_sha")])
data_after_imputation$at_o_sum <- rowSums(data_after_imputation[,c("attr_o", "sinc_o", "intel_o", "fun_o", "amb_o", "shar_o")])

# No rows with total = 0for pf_sum. Only one for at_o_sum. We are going to delete it.
data_after_imputation<-data_after_imputation[data_after_imputation$pf_sum != 0 & data_after_imputation$at_o_sum != 0,]

# Scale: they must add up to 100

data_after_imputation$pf_o_att <- round(data_after_imputation$pf_o_att/data_after_imputation$pf_sum*100)
data_after_imputation$pf_o_sin <- round(data_after_imputation$pf_o_sin/data_after_imputation$pf_sum*100)
data_after_imputation$pf_o_fun <- round(data_after_imputation$pf_o_fun/data_after_imputation$pf_sum*100)
data_after_imputation$pf_o_int <- round(data_after_imputation$pf_o_int/data_after_imputation$pf_sum*100)
data_after_imputation$pf_o_amb <- round(data_after_imputation$pf_o_amb/data_after_imputation$pf_sum*100)
data_after_imputation$pf_o_sha <- round(data_after_imputation$pf_o_sha/data_after_imputation$pf_sum*100)

data_after_imputation$attr_o <- round(data_after_imputation$attr_o/data_after_imputation$at_o_sum*100)
data_after_imputation$sinc_o <- round(data_after_imputation$sinc_o/data_after_imputation$at_o_sum*100)
data_after_imputation$intel_o <- round(data_after_imputation$intel_o/data_after_imputation$at_o_sum*100)
data_after_imputation$fun_o <- round(data_after_imputation$fun_o/data_after_imputation$at_o_sum*100)
data_after_imputation$amb_o <- round(data_after_imputation$amb_o/data_after_imputation$at_o_sum*100)
data_after_imputation$shar_o <- round(data_after_imputation$shar_o/data_after_imputation$at_o_sum*100)

data_after_imputation$pf_sum <- NULL
data_after_imputation$at_o_sum <- NULL








write.csv(data_after_imputation, file = "SpeedClean.csv")






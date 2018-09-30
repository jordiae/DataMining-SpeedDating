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

selected_vars<-c("iid","gender","round","order","pid","match","int_corr","samerace","age_o","race_o","pf_o_att","pf_o_sin", "pf_o_int", "pf_o_fun", "pf_o_amb","pf_o_sha" ,"dec_o","attr_o","sinc_o","intel_o","fun_o","amb_o","shar_o","age","field_cd","mn_sat","tuition","race","imprace","income","goal","date","go_out","dec","like","met","imprelig")
selected_columns_data  <- selected_rows_data[ selected_vars]

#declare qualitative variables

# doubts: "importance", like etc out of 10 (1-10), are really categorical?
# order is numeric? it doesn't make sense to analyze it by itself


selected_columns_data$gender <- as.factor(selected_columns_data$gender)
selected_columns_data$round <- as.factor(selected_columns_data$round)
selected_columns_data$match <- as.factor(selected_columns_data$match)
selected_columns_data$samerace <- as.factor(selected_columns_data$samerace)
selected_columns_data$pf_o_att <- as.factor(selected_columns_data$pf_o_att)
selected_columns_data$pf_o_sin <- as.factor(selected_columns_data$pf_o_sin)
selected_columns_data$pf_o_int <- as.factor(selected_columns_data$pf_o_int)
selected_columns_data$pf_o_fun <- as.factor(selected_columns_data$pf_o_fun)
selected_columns_data$pf_o_amb <- as.factor(selected_columns_data$pf_o_amb)
selected_columns_data$pf_o_sha <- as.factor(selected_columns_data$pf_o_sha)
selected_columns_data$dec_o <- as.factor(selected_columns_data$dec_o)
selected_columns_data$attr_o <- as.factor(selected_columns_data$attr_o)
selected_columns_data$sinc_o <- as.factor(selected_columns_data$sinc_o)
selected_columns_data$intel_o <- as.factor(selected_columns_data$intel_o)
selected_columns_data$fun_o <- as.factor(selected_columns_data$fun_o)
selected_columns_data$amb_o <- as.factor(selected_columns_data$amb_o)
selected_columns_data$shar_o <- as.factor(selected_columns_data$shar_o)
selected_columns_data$field_cd <- as.factor(selected_columns_data$field_cd)
selected_columns_data$race <- as.factor(selected_columns_data$race)
selected_columns_data$race_o <- as.factor(selected_columns_data$race_o)
selected_columns_data$imprace <- as.factor(selected_columns_data$imprace)
selected_columns_data$goal <- as.factor(selected_columns_data$goal)
selected_columns_data$date <- as.factor(selected_columns_data$date)
selected_columns_data$go_out <- as.factor(selected_columns_data$go_out)
selected_columns_data$dec <- as.factor(selected_columns_data$dec)
selected_columns_data$like <- as.factor(selected_columns_data$like)
selected_columns_data$met <- as.factor(selected_columns_data$met)
selected_columns_data$imprelig <- as.factor(selected_columns_data$imprelig)


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

# iid: (id)
# gender: no outliers, no NAs, all values 0 or 1, OK.
# round: ok.
# order: ok.
# pid: (id)
# match: same as gender.
# int_corr: 1.91% missing , no outliers in boxplot.
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

# age: 13.48% missing, no outliers (same as age_o)
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


# pf_o_att We had considered it as categorical variable, but by manual
# inspection we see that there are fractional values. Actually, individuals have 100 points
# to distribute among different attributes. Some individuals did divide their 100 points in
# non-integer values. Like, 100/3, 100/3 and 100/3 for 3 attributes, and 0 for the others.
# So, we think that maybe we should relabel these attribute related variables as numeric
# (DOUBT), although speaking with the teacher she said that they should be categoric (?).
# Another possibility would be to take the floor. For the moment, we will take the floor:
selected_columns_data_floor <- selected_columns_data
selected_columns_data_floor$pf_o_att<-as.numeric(selected_columns_data_floor$pf_o_att)
selected_columns_data_floor$pf_o_att <- floor(selected_columns_data_floor$pf_o_att)
selected_columns_data_floor$pf_o_att <- as.factor(selected_columns_data_floor$pf_o_att)

# The same applies to the other attribute related variables. All other pf_o_... have the same
# % missings and some fractions, for the moment we do the same (floor + keep it as
# categoric)

selected_columns_data_floor$pf_o_sin<-as.numeric(selected_columns_data_floor$pf_o_sin)
selected_columns_data_floor$pf_o_sin <- floor(selected_columns_data_floor$pf_o_sin)
selected_columns_data_floor$pf_o_sin <- as.factor(selected_columns_data_floor$pf_o_sin)

selected_columns_data_floor$pf_o_int<-as.numeric(selected_columns_data_floor$pf_o_int)
selected_columns_data_floor$pf_o_int <- floor(selected_columns_data_floor$pf_o_int)
selected_columns_data_floor$pf_o_int <- as.factor(selected_columns_data_floor$pf_o_int)

selected_columns_data_floor$pf_o_fun<-as.numeric(selected_columns_data_floor$pf_o_fun)
selected_columns_data_floor$pf_o_fun <- floor(selected_columns_data_floor$pf_o_fun)
selected_columns_data_floor$pf_o_fun <- as.factor(selected_columns_data_floor$pf_o_fun)

selected_columns_data_floor$pf_o_amb<-as.numeric(selected_columns_data_floor$pf_o_amb)
selected_columns_data_floor$pf_o_amb<- floor(selected_columns_data_floor$pf_o_amb)
selected_columns_data_floor$pf_o_amb<-as.factor(selected_columns_data_floor$pf_o_amb)

selected_columns_data_floor$pf_o_sha<-as.numeric(selected_columns_data_floor$pf_o_sha)
selected_columns_data_floor$pf_o_sha <- floor(selected_columns_data_floor$pf_o_sha)
selected_columns_data_floor$pf_o_sha<-as.factor(selected_columns_data_floor$pf_o_sha)




#idem with all attributes variables
selected_columns_data_floor$attr_o<-as.numeric(selected_columns_data_floor$attr_o)
selected_columns_data_floor$attr_o<- floor(selected_columns_data_floor$attr_o)
selected_columns_data_floor$attr_o<-as.factor(selected_columns_data_floor$attr_o)

selected_columns_data_floor$sinc_o<-as.numeric(selected_columns_data_floor$sinc_o)
selected_columns_data_floor$sinc_o<- floor(selected_columns_data_floor$sinc_o)
selected_columns_data_floor$sinc_o<-as.factor(selected_columns_data_floor$sinc_o)

selected_columns_data_floor$intel_o<-as.numeric(selected_columns_data_floor$intel_o)
selected_columns_data_floor$intel_o<- floor(selected_columns_data_floor$intel_o)
selected_columns_data_floor$intel_o<-as.factor(selected_columns_data_floor$intel_o)

selected_columns_data_floor$fun_o<-as.numeric(selected_columns_data_floor$fun_o)
selected_columns_data_floor$fun_o<- floor(selected_columns_data_floor$fun_o)
selected_columns_data_floor$fun_o<-as.factor(selected_columns_data_floor$fun_o)

selected_columns_data_floor$amb_o<-as.numeric(selected_columns_data_floor$amb_o)
selected_columns_data_floor$amb_o<- floor(selected_columns_data_floor$amb_o)
selected_columns_data_floor$amb_o<-as.factor(selected_columns_data_floor$amb_o)

selected_columns_data_floor$shar_o<-as.numeric(selected_columns_data_floor$shar_o)
selected_columns_data_floor$shar_o<- floor(selected_columns_data_floor$shar_o)
selected_columns_data_floor$shar_o<-as.factor(selected_columns_data_floor$shar_o)


# imprace -> 0 is not a valid scale value (1-10), we will replace all occurrences (8)  by NA
selected_columns_data_floor$imprace[selected_columns_data_floor$imprace == 0] <- NA
# update levels
selected_columns_data_floor$imprace <- droplevels(selected_columns_data_floor$imprace)

# like
# We will round (1-10 scale, some people scored 4.5 for instance)
selected_columns_data_floor$like<-as.numeric(selected_columns_data_floor$like)
selected_columns_data_floor$like<- round(selected_columns_data_floor$like)
selected_columns_data_floor$like<-as.factor(selected_columns_data_floor$like)

# met
# We are going to delete this variable because the values are not coherent with the # documentation
selected_vars_no_met<-c("iid","gender","round","order","pid","match","int_corr","samerace","age_o","race_o","pf_o_att","pf_o_sin", "pf_o_int", "pf_o_fun", "pf_o_amb","pf_o_sha" ,"dec_o","attr_o","sinc_o","intel_o","fun_o","amb_o","shar_o","age","field_cd","mn_sat","tuition","race","imprace","income","goal","date","go_out","dec","like","imprelig")
data_pending_missing_imputation  <- selected_columns_data_floor[ selected_vars_no_met]





# MISSING IMPUTATION

# age: structural

# tuition and mn_sat. structural -> 0
data_pending_missing_imputation$tuition[is.na(data_pending_missing_imputation$tuition)] <- 0
data_pending_missing_imputation$mn_sat[is.na(data_pending_missing_imputation$mn_sat)] <- 0

# NEW VARIABLES
# We are going to create the var "difference of age"
# Something like data$diff_age <- abs(data$age - data$age_o)













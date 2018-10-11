# Import the Data of our cleaned dataset without any missing values

speed_data <-read.csv("SpeedClean.csv",header = TRUE)

#Univartiate Analysis

#gender:
#Counting the values of both available gender:
summary(speed_data$gender)
#   F    M 
#1827 1829 


#Match: Counting the "Matches" and "no Matches"
summary(speed_data$match)
# N    Y 
# 3027  629 
# Percentage of match = 17,2 % Y and 82,8 N


# Int_corr:
summary(speed_data$int_corr)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.7000 -0.0200  0.2100  0.1948  0.4300  0.9000 
# As expected for a correlation there are values between -1 and 1, the extreme points arent reached, which means, that there wasn't a complete match 
# and not a complete missmatch between the participents. We can see that the matching seems to be higher than the missmatching (0,9 and -0,7). 

# samerace:
summary(speed_data$samerace)
# N    Y 
# 2085 1571 
#age:
summary(speed_data$age)
# age_o:
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.00   24.00   26.00   26.09   28.00   39.00 
summary(speed_data$age_o)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 21.0    24.0    26.0    26.1    28.0    39.0

# race_o:
summary(speed_data$race_o)
# Asian Black  Cauc Latin Other 
# 756   201  2199   289   211 
# There are two big groups of races within the Speeddating sessions: Asian and Cauc
barplot(summary(speed_data$race_o))

# pf_o_amb:
summary(speed_data$pf_o_amb)
# pf_o_att:
summary(speed_data$pf_o_att)
# pf_o_sin:
summary(speed_data$pf_o_sin)

# pf_o_int:
summary(speed_data$pf_o_int)

# pf_o_fun:
summary(speed_data$pf_o_fun)

# pf_o_sha:
summary(speed_data$pf_o_sha)

# dec_o:
summary(speed_data$dec_o)
#attr_o:
summary(speed_data$attr_o)
#amb_o:
summary(speed_data$amb_o)
# sinc_o:
summary(speed_data$sinc_o)
#fun_o:
summary(speed_data$fun_o)
#intel_o:
summary(speed_data$intel_o)
#shar_o:
summary(speed_data$shar_o)

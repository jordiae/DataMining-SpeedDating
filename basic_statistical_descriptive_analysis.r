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
sd(speed_data$int_corr)
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
# Standart Deviation:
sd(speed_data$age)
summary(speed_data$age_o)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 21.0    24.0    26.0    26.1    28.0    39.0
# As we can see the values for age and age_o are the same. They should be, by checking this we made sure that there wasn't a mistake in the data.
# race_o:
summary(speed_data$race_o)
# Asian Black  Cauc Latin Other 
# 756   201  2199   289   211 
# There are two big groups of races within the Speeddating sessions: Asian and Cauc
barplot(summary(speed_data$race_o))
# For the following 12 variables we will calculate the basic values(Mininma, 1st Quantile, Median, Mean, 3rd Quantile, Maximum and Standart Derivation )
# pf_o_amb:
summary(speed_data$pf_o_amb)
with(speed_data, Hist(pf_o_amb, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$pf_o_amb)
# pf_o_att:
summary(speed_data$pf_o_att)
with(speed_data, Hist(pf_o_att, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$pf_o_att)

# pf_o_sin:
summary(speed_data$pf_o_sin)
with(speed_data, Hist(pf_o_sin, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$pf_o_sin)
# pf_o_int:
summary(speed_data$pf_o_int)
with(speed_data, Hist(pf_o_int, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$pf_o_int)
# pf_o_fun:
summary(speed_data$pf_o_fun)
with(speed_data, Hist(pf_o_fun, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$pf_o_fun)

# pf_o_sha:
summary(speed_data$pf_o_sha)
with(speed_data, Hist(pf_o_sha, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$pf_o_sha)
# dec_o:
summary(speed_data$dec_o)
with(speed_data, Hist(dec_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))

# Standart Deviation:
sd(speed_data$dec_o)
#attr_o:
summary(speed_data$attr_o)
with(speed_data, Hist(attr_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$attr_o)
#amb_o:
summary(speed_data$amb_o)
with(speed_data, Hist(amb_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$amb_o)
# sinc_o:
summary(speed_data$sinc_o)
with(speed_data, Hist(sinc_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$sinc_o)
#fun_o:
summary(speed_data$fun_o)
with(speed_data, Hist(fun_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$fun_o)
#intel_o:
summary(speed_data$intel_o)
with(speed_data, Hist(intel_o, scale="frequency", breaks="Sturges", 
                                                 col="darkgray"))
# Standart Deviation:
sd(speed_data$intel_o)

#shar_o:
summary(speed_data$shar_o)
with(speed_data, Hist(shar_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# Standart Deviation:
sd(speed_data$shar_o)
 # field_cd:
summary(speed_data$field_cd)
barplot(summary(speed_data$field_cd))
#mn_sat
summary(speed_data$mn_sat)
# Standart Deviation:
sd(speed_data$mn_sat)

summary(speed_data$like)
sd(speed_data$like)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   5.000   6.000   6.135   7.000  10.000 
#This variable represent a mark from 1 to 10 of how much do you liked your partner,
#as we can see the mean and the median are similar and close to 6 so overall there
#are a lightly positive mark.

summary(speed_data$dec)
#  N    Y 
#2097 1559
#The 46.5% of people decided to "match" the partner and the remaining 53.5%
#rejected his partner.

summary(speed_data$go_out)
#1Month   1Week  2Month   2Week   Never SevWeek SevYear 
#98     745     212    1357      36    1157      51 

barplot(summary(speed_data$go_out))
#As we can see on the plot most of the people go out every 2 or several weeks

summary(speed_data$date)

barplot(summary(speed_data$date))
#As we can see on the plot most of the people date every several years or every 2 months.

summary(speed_data$goal)
barplot(summary(speed_data$goal))
#As we can see on the plot the main goals are having fun and meeting new people.

summary(speed_data$income)
sd(speed_data$income)
#There are a big difference between the higher and the lower income, the standard deviation
#is pretty high (17k). We can relate this to people who have studies and people
#who don't.

summary(speed_data$imprelig)
sd(speed_data$imprelig)
#In general people give little importance to religion.

summary(speed_data$imprace)
sd(speed_data$imprace)
#In general people give little importance to race.

summary(speed_data$race)
barplot(summary(speed_data$race))
#Most people have Caucastic race.

summary(speed_data$tuition)
sd(speed_data$tuition)
#There are a high standard deviation (12091) because there are a lot of people
#who haven't gone to college so they didn't paid.

# Bivariate Analysis
with(speed_data, tapply(amb_o, list(field_cd), mean, na.rm=TRUE))
barplot(with(speed_data, tapply(amb_o, list(field_cd), sd, na.rm=TRUE)))




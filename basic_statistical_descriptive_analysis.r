# Import the Data of our cleaned dataset without any missing values

speed_data <-read.csv("SpeedClean.csv",header = TRUE)
array_attributes <- 

#Univariate Analysis

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
# standard Deviation:
sd(speed_data$age)
summary(speed_data$age_o)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 21.0    24.0    26.0    26.1    28.0    39.0
# As we can see the values for age and age_o are the same. They should be, by checking this we made sure that there wasn't a mistake in the data.
# race_o:
summary(speed_data$race_o)
# Asian Black  Cauc Latin Other 
# 756   201  2199   289   211 
# There are two big groups of races within the Speespeed_dataating sessions: Asian and Cauc
barplot(summary(speed_data$race_o))
# For the following 12 variables we will calculate the basic values(Mininma, 1st Quantile, Median, Mean, 3rd Quantile, Maximum and standard Derivation )
# pf_o_amb:
summary(speed_data$pf_o_amb)
with(speed_data, hist(pf_o_amb, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$pf_o_amb)
# pf_o_att:
summary(speed_data$pf_o_att)
with(speed_data, hist(pf_o_att, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$pf_o_att)

# pf_o_sin:
summary(speed_data$pf_o_sin)
with(speed_data, hist(pf_o_sin, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$pf_o_sin)
# pf_o_int:
summary(speed_data$pf_o_int)
with(speed_data, hist(pf_o_int, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$pf_o_int)
# pf_o_fun:
summary(speed_data$pf_o_fun)
with(speed_data, hist(pf_o_fun, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$pf_o_fun)

# pf_o_sha:
summary(speed_data$pf_o_sha)
with(speed_data, hist(pf_o_sha, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$pf_o_sha)
# dec_o:
summary(speed_data$dec_o)

# standard Deviation:
sd(speed_data$dec_o)
#attr_o:
summary(speed_data$attr_o)
with(speed_data, hist(attr_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$attr_o)
#amb_o:
summary(speed_data$amb_o)
with(speed_data, hist(amb_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$amb_o)
# sinc_o:
summary(speed_data$sinc_o)
with(speed_data, hist(sinc_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$sinc_o)
#fun_o:
summary(speed_data$fun_o)
with(speed_data, hist(fun_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$fun_o)
#intel_o:
summary(speed_data$intel_o)
with(speed_data, hist(intel_o, scale="frequency", breaks="Sturges", 
                                                 col="darkgray"))
# standard Deviation:
sd(speed_data$intel_o)

#shar_o:
summary(speed_data$shar_o)
with(speed_data, hist(shar_o, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$shar_o)
 # field_cd:
summary(speed_data$field_cd)
barplot(summary(speed_data$field_cd))
#mn_sat
summary(speed_data$mn_sat)
with(speed_data, hist(mn_sat, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
# standard Deviation:
sd(speed_data$mn_sat)

summary(speed_data$like)
with(speed_data, hist(like, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
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
with(speed_data, hist(income, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
sd(speed_data$income)
#There are a big difference between the higher and the lower income, the standard deviation
#is pretty high (17k). We can relate this to people who have studies and people
#who don't.

summary(speed_data$imprelig)
with(speed_data, hist(imprelig, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
sd(speed_data$imprelig)
#In general people give little importance to religion.

summary(speed_data$imprace)
with(speed_data, hist(imprace, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
sd(speed_data$imprace)
#In general people give little importance to race.

summary(speed_data$race)
barplot(summary(speed_data$race))
#Most people have Caucastic race.

summary(speed_data$tuition)
with(speed_data, hist(tuition, scale="frequency", breaks="Sturges", 
                      col="darkgray"))
sd(speed_data$tuition)
#There are a high standard deviation (12091) because there are a lot of people
#who haven't gone to college so they didn't paid.

# Bivariate Analysis
# In the following plot will be analized how the different Groups regarding the field of study influence the points given in the different categories
with(speed_data, tapply(amb_o, list(field_cd), mean, na.rm=TRUE))

barplot(with(speed_data, tapply(amb_o, list(field_cd), mean, na.rm=TRUE)))
barplot(with(speed_data, tapply(attr_o, list(field_cd), mean, na.rm=TRUE)))
barplot(with(speed_data, tapply(sinc_o, list(field_cd), mean, na.rm=TRUE)))
barplot(with(speed_data, tapply(intel_o, list(field_cd), sd, na.rm=TRUE)))
barplot(with(speed_data, tapply(fun_o, list(field_cd), sd, na.rm=TRUE)))
barplot(with(speed_data, tapply(shar_o, list(field_cd), sd, na.rm=TRUE)))
# Now for the expections that people have:
barplot(with(speed_data, tapply(pf_o_sin, list(field_cd), sd, na.rm=TRUE)))
barplot(with(speed_data, tapply(pf_o_fun, list(field_cd), sd, na.rm=TRUE)))
barplot(with(speed_data, tapply(pf_o_int, list(field_cd), sd, na.rm=TRUE)))
barplot(with(speed_data, tapply(pf_o_amb, list(field_cd), sd, na.rm=TRUE)))
barplot(with(speed_data, tapply(pf_o_sha, list(field_cd), sd, na.rm=TRUE)))
barplot(with(speed_data, tapply(pf_o_att, list(field_cd), sd, na.rm=TRUE)))
# Here can be seen, how much in a particular field they value a particular Attributte
barplot(with(speed_data, tapply(amb_o, list(field_cd), sd, na.rm=TRUE)))

# Decriptive analysis after preprocessing
class(speed_data)
dim(speed_data)
n<-dim(speed_data)[1]
n
K<-dim(speed_data)[2]
K

names(speed_data)

listOfColors<-c("blueviolet","darkviolet","mediumvioletred", "palevioletred","violet", "violetred", "violetred4")
listOfColors<-c("orange","blue","green", "white","yellow", "red", "violet")
listOfColors<-palette()
listOfColors<-rainbow(14)

par(ask=TRUE)

for(k in 1:K){
  if (is.factor(speed_data[,k])){ 
    frecs<-table(speed_data[,k], useNA="ifany")
    proportions<-frecs/n
    #ojo, decidir si calcular porcentages con o sin missing values
    pie(frecs, cex=0.6, main=paste("Pie of", names(speed_data)[k]))
    barplot(frecs, las=3, cex.names=0.7, main=paste("Barplot of", names(speed_data)[k]), col=listOfColors)
    print(frecs)
    print(proportions)
  }else{
    hist(speed_data[,k], main=paste("Histogram of", names(speed_data)[k]))
    boxplot(speed_data[,k], horizontal=TRUE, main=paste("Boxplot of", names(speed_data)[k]))
    print(summary(speed_data[,k]))
    print(paste("sd: ", sd(speed_data[,k])))
    print(paste("vc: ", sd(speed_data[,k])/mean(speed_data[,k])))
  }
}








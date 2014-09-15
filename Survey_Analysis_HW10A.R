#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 10_A
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


### PROBLEM 2 - SOCIAL SCIENCE:  

# Write a three-page mini-paper addressing some interesting social science question using the NES or GSS.  
# The topic and the analysis do not need to be deep, but they must be original, and you need to go beyond simple toplines and crosstabs.

### EMPTY ENVIRONMENT:
ls <- ls()
remove(ls)

### LOAD PACKAGES:
library ("diff")
library ("foreign")
library ("arm")
library ("calibrate")
library ("gdata")
library ("maps")
library ("mapdata")
library ("survey")
library ("mi")
library ("Hmisc")
library ("Amelia")

### DATA:
# Data is from the General Social Survey 2010 Cross-Section and Panel Combined
# SOURCE: http://www.thearda.com/Archive/Files/Downloads/GSS10PAN_DL2.asp
# read the data into R
gss.2010 <- read.dta("General Social Survey 2010 Cross-Section and Panel Combined.DTA")
# how many rows (cases) are in the dataset
nrow(gss.2010)
# names of the columns (variable names)
names(gss.2010)


### RECODE VARIABLE "id":
# table the original variable
table(gss.2010$id)
# rename "id" variable
names(gss.2010)[2]<-"RespondentID"
# table new created variable
table(gss.2010$RespondentID)
mode(gss.2010$RespondentID)
class(gss.2010$RespondentID)



### RECODE VARIABLE "sex":
# table the original variable
table(gss.2010$sex)
mode(gss.2010$sex)
class(gss.2010$sex)
# recode "sex" variable
gss.2010$Female <- NA
gss.2010$Female[gss.2010$sex == 'Male'] <- 0
gss.2010$Female[gss.2010$sex == 'Female'] <- 1
# table new created variable
table(gss.2010$Female)
mode(gss.2010$Female)
class(gss.2010$Female)



### RECODE VARIABLE "age":
# table the original variable
table(gss.2010$age)
mode(gss.2010$age)
class(gss.2010$age)
# we need to exclude the 99 age category in the "age" variable, because it represent missing cases
# category "99" in the codebook is labeled as "No answer"
gss.2010$Age <- ifelse(gss.2010$age == "No answer", NA, gss.2010$age)
# table variable after changes
table(gss.2010$Age)
# recode the age variable into 6 categoies
gss.2010$Age <- ifelse (gss.2010$Age < 8, "18-24 years",
                        ifelse (gss.2010$Age >= 8 & gss.2010$Age < 18, "25-34 years",
                                ifelse (gss.2010$Age >= 18 & gss.2010$Age < 28, "35-44 years",
                                        ifelse (gss.2010$Age >= 28 & gss.2010$Age < 38, "45-54 years",
                                                ifelse (gss.2010$Age >= 38 & gss.2010$Age < 48, "55-64 years",
                                                        ifelse (gss.2010$Age >= 48, "Over 65 years", NA))))))
# create age as a 6 category factor
gss.2010$Age <- as.factor(gss.2010$Age)
# relevel the variable so "18-24 years" is the base category for age
gss.2010$Age <- relevel(gss.2010$Age, ref="18-24 years")
# table new created variable - Age
table(gss.2010$Age)	
mode(gss.2010$Age)
class(gss.2010$Age)



### RECODE VARIABLE "age":
# table the original variable
table(gss.2010$age)
mode(gss.2010$age)
class(gss.2010$age)
# we need to exclude the 99 age category in the "age" variable, because it represent missing cases
# category "99" in the codebook is labeled as "No answer"
gss.2010$AgeContinuous <- ifelse(gss.2010$age == "No answer", NA, gss.2010$age)
# table variable after changes
table(gss.2010$AgeContinuous)
# recode the age variable 
gss.2010$AgeContinuous <- gss.2010$AgeContinuous + 17
# table new created variable - AgeContinuous
table(gss.2010$AgeContinuous)  
mode(gss.2010$AgeContinuous)
class(gss.2010$AgeContinuous)



### CREATE AgeContinuousSquared:
gss.2010$AgeContinuousSquared <- gss.2010$AgeContinuous * gss.2010$AgeContinuous
# table new created variable - AgeContinuousSquared
table(gss.2010$AgeContinuousSquared)  
mode(gss.2010$AgeContinuousSquared)
class(gss.2010$AgeContinuousSquared)



### RECODE VARIABLE "educ":
# table the original variable
table(gss.2010$educ)
# what type of variable is educ
mode(gss.2010$educ)
# recode the string parts of the variable first
gss.2010$educ <- ifelse(gss.2010$educ == "No formal school", "0", gss.2010$educ)
table(gss.2010$educ)
gss.2010$educ <- ifelse(gss.2010$educ == "Don't know", NA, gss.2010$educ)
table(gss.2010$educ)
gss.2010$educ <- ifelse(gss.2010$educ == "No answer", NA, gss.2010$educ)
table(gss.2010$educ)
# recode the educ variable into 5 categoies
gss.2010$Education <- ifelse (gss.2010$educ == 0 | gss.2010$educ == 2 | gss.2010$educ == 3 | gss.2010$educ == 4 | gss.2010$educ == 5, "4 years",
                              ifelse (gss.2010$educ == 6 | gss.2010$educ == 7 | gss.2010$educ == 8 | gss.2010$educ == 9, "8 years",
                                      ifelse (gss.2010$educ == 10 | gss.2010$educ == 11 | gss.2010$educ == 12 | gss.2010$educ == 13, "12 years",
                                              ifelse (gss.2010$educ == 14 | gss.2010$educ == 15 | gss.2010$educ == 16 | gss.2010$educ == 17, "16 years",
                                                      ifelse (gss.2010$educ == 18 | gss.2010$educ == 19 | gss.2010$educ == 20 | gss.2010$educ == 21, "Over 17 years", NA)))))
# create education as a 5 category factor
gss.2010$Education <- as.factor(gss.2010$Education)
# relevel the variable so "4 years" is the base category for education
gss.2010$Education <- relevel(gss.2010$Education, ref="4 years")
# table new created variable - Education
table(gss.2010$Education)	
mode(gss.2010$Education)
class(gss.2010$Education)



### RECODE VARIABLE "region":
# table the original variable
table(gss.2010$region)
# recode the "region" variable 
gss.2010$Region <- NA
gss.2010$Region[gss.2010$region == 'New England'] <- "New England"
gss.2010$Region[gss.2010$region == 'Middle Atlantic'] <- "Middle Atlantic"
gss.2010$Region[gss.2010$region == 'East North Central'] <- "East North Central"
gss.2010$Region[gss.2010$region == 'West North Central'] <- "West North Central"
gss.2010$Region[gss.2010$region == 'South Atlantic'] <- "South Atlantic"
gss.2010$Region[gss.2010$region == 'East South Central'] <- "East South Central"
gss.2010$Region[gss.2010$region == 'West South Central'] <- "West South Central"
gss.2010$Region[gss.2010$region == 'Mountain'] <- "Mountain"
gss.2010$Region[gss.2010$region == 'Pacific'] <- "Pacific"
# create region as a 9 category factor
gss.2010$Region <- as.factor(gss.2010$Region)
# relevel the variable so "East North Central" is the base category for race
gss.2010$Region <- relevel(gss.2010$Region, ref="East North Central")
# table new created variable
table(gss.2010$Region)
mode(gss.2010$Region)
class(gss.2010$Region)



### RECODE VARIABLE "partyid":
# table the original variable
table(gss.2010$partyid)
# recode and create new variable					  
gss.2010$PartyID <- NA
gss.2010$PartyID[gss.2010$partyid == 'Strong Democrat'] <- -3
gss.2010$PartyID[gss.2010$partyid == 'Not Strong Democrat'] <- -2
gss.2010$PartyID[gss.2010$partyid == 'Independent, near Democrat'] <- -1
gss.2010$PartyID[gss.2010$partyid == 'Independent'] <- 0
gss.2010$PartyID[gss.2010$partyid == 'Other party'] <- 0
gss.2010$PartyID[gss.2010$partyid == 'Independent, near Republican'] <- 1
gss.2010$PartyID[gss.2010$partyid == 'Not Strong Republican'] <- 2
gss.2010$PartyID[gss.2010$partyid == 'Strong Republican'] <- 3
# table new created variable - PartyID
table(gss.2010$PartyID)
mode(gss.2010$PartyID)
class(gss.2010$PartyID)



### RECODE VARIABLE "reliten":
# table the original variable
table(gss.2010$reliten)
# recode and create new variable					  
gss.2010$Religious <- NA
gss.2010$Religious[gss.2010$reliten == 'No religion'] <- 0
gss.2010$Religious[gss.2010$reliten == 'Somewhat strong'] <- 1
gss.2010$Religious[gss.2010$reliten == 'Not very strong'] <- 2
gss.2010$Religious[gss.2010$reliten == 'Strong'] <- 3
# table new created variable - Religious
table(gss.2010$Religious)
mode(gss.2010$Religious)
class(gss.2010$Religious)



### RECODE VARIABLE "happy":
# table the original variable
table(gss.2010$happy)
# recode and create new variable					  
gss.2010$Happy <- NA
gss.2010$Happy[gss.2010$happy == 'Not too happy'] <- 0
gss.2010$Happy[gss.2010$happy == 'Pretty happy'] <- 1
gss.2010$Happy[gss.2010$happy == 'Very happy'] <- 2
# table new created variable - Happy
table(gss.2010$Happy)
mode(gss.2010$Happy)
class(gss.2010$Happy)



### RECODE VARIABLE "wtcombnr":
# table the original variable
table(gss.2010$wtcombnr)
# rename "wtcombnr" variable
names(gss.2010)[1124]<-"Weights"
# table new created variable
table(gss.2010$Weights)
mode(gss.2010$Weights)
class(gss.2010$Weights)



### RECODE VARIABLE "race":
# table hispanico1 variable
table(gss.2010$hispanic)
# makes an indicator for hispanic
gss.2010$hispanico1 <- ifelse(gss.2010$hispanic != "Not Hispanic", 1, 0)
# create a index for hispanic
hindex <- c("Mexico", "Puerto Rico", "Spain", "Philippines", "Other", "Spanish", "Other Spanish")
# table ethnic variable
table(gss.2010$ethnic)
# if the ethnicity in the list "hindex" then 1 else 0
gss.2010$hispanico2 <- ifelse(gss.2010$ethnic %in% hindex, 1, 0)
# two ethnicity indicators, based on GSS questions
gss.2010$hispanico1[is.na(gss.2010$hispanico1)] <- 0
gss.2010$hispanico2[is.na(gss.2010$hispanico2)] <- 0
# table race variable
table(gss.2010$race)
# make race character variable
gss.2010$Race <- as.character(gss.2010$race)
table(gss.2010$Race)
# create fourth hispanic category
gss.2010$Race <- ifelse ((gss.2010$hispanico1==1 | gss.2010$hispanico2==1), "Hispanic", as.character(gss.2010$race))
# create race 4 category factor
gss.2010$Race <- as.factor(gss.2010$Race)
# gss.2010$Race <- factor(gss.2010$Race, 1:4, levels=c("White","Black","Hispanic","Other"))
# relevel the variable so "White" is the base category for race
gss.2010$Race <- relevel(gss.2010$Race, ref="White")
# table new Race variable
table(gss.2010$Race)
mode(gss.2010$Race)
class(gss.2010$Race)



### RECODE VARIABLE "urban":
# table the original variable
table(gss.2010$srcbelt)
# recode the "srcbelt" variable 
gss.2010$Urban <- NA
gss.2010$Urban[gss.2010$srcbelt == 'Central city of 12 largest SMSAs'] <- "Urban"
gss.2010$Urban[gss.2010$srcbelt == 'Central city of remainder of the 100 largest SMSAs'] <- "Urban"
gss.2010$Urban[gss.2010$srcbelt == 'Suburbs of 12 largest SMSAs'] <- "Suburban"
gss.2010$Urban[gss.2010$srcbelt == 'Suburbs of the remaining 100 largest SMSAs'] <- "Suburban"
gss.2010$Urban[gss.2010$srcbelt == 'Other urban (counties having towns of 10,000 or more)'] <- "Urban"
gss.2010$Urban[gss.2010$srcbelt == 'Other rural (counties having no towns of 10,000 or more)'] <- "Rural"
# create race 4 category factor
gss.2010$Urban <- as.factor(gss.2010$Urban)
# gss.2010$Urban <- factor(gss.2010$Urban, 1:3, levels=c("Urban","Suburban","Rural"))
# relevel the variable so "Rural" is the base category for urban
gss.2010$Urban <- relevel(gss.2010$Urban, ref="Rural")
# table new created variable
table(gss.2010$Urban)
mode(gss.2010$Urban)
class(gss.2010$Urban)



### RECODE VARIABLE "polviews":
# table the original variable
table(gss.2010$polviews)
# recode and create new variable					  
gss.2010$PoliticalIdeology <- NA
gss.2010$PoliticalIdeology[gss.2010$polviews == 'Extremely liberal'] <- -3
gss.2010$PoliticalIdeology[gss.2010$polviews == 'Liberal'] <- -2
gss.2010$PoliticalIdeology[gss.2010$polviews == 'Slightly liberal'] <- -1
gss.2010$PoliticalIdeology[gss.2010$polviews == 'Moderate'] <- 0
gss.2010$PoliticalIdeology[gss.2010$polviews == 'Slightly conservative'] <- 1
gss.2010$PoliticalIdeology[gss.2010$polviews == 'Conservative'] <- 2
gss.2010$PoliticalIdeology[gss.2010$polviews == 'Extremely conservative'] <- 3
# table new created variable - PoliticalIdeology
table(gss.2010$PoliticalIdeology)
mode(gss.2010$PoliticalIdeology)
class(gss.2010$PoliticalIdeology)



### RECODE VARIABLE "income":
# table the original variable
table(gss.2010$INCOME06)
mode(gss.2010$INCOME06)
class(gss.2010$INCOME06)
# recode and create new variable
gss.2010$Income <- NA
gss.2010$Income[gss.2010$INCOME06 == 'Under $1,000'] <- "Under $5,000"
gss.2010$Income[gss.2010$INCOME06 == '$1,000 to $2,999'] <- "Under $5,000"
gss.2010$Income[gss.2010$INCOME06 == '$3,000 to $3,999'] <- "Under $5,000"
gss.2010$Income[gss.2010$INCOME06 == '$4,000 to $4,999'] <- "Under $5,000"
gss.2010$Income[gss.2010$INCOME06 == '$5,000 to $5,999'] <- "$5,000 to $9,999"
gss.2010$Income[gss.2010$INCOME06 == '$6,000 to $6,999'] <- "$5,000 to $9,999"
gss.2010$Income[gss.2010$INCOME06 == '$7,000 to $7,999'] <- "$5,000 to $9,999"
gss.2010$Income[gss.2010$INCOME06 == '$8,000 to $9,999'] <- "$5,000 to $9,999"
gss.2010$Income[gss.2010$INCOME06 == '$10,000 to $12,499'] <- "$10,000 to $14,999"
gss.2010$Income[gss.2010$INCOME06 == '$12,500 to $14,999'] <- "$10,000 to $14,999"
gss.2010$Income[gss.2010$INCOME06 == '$15,000 to $17,499'] <- "$15,000 to $19,999"
gss.2010$Income[gss.2010$INCOME06 == '$17,500 to $19,999'] <- "$15,000 to $19,999"
gss.2010$Income[gss.2010$INCOME06 == '$20,000 to $22,499'] <- "$20,000 to $29,999"
gss.2010$Income[gss.2010$INCOME06 == '$22,500 to $24,999'] <- "$20,000 to $29,999"
gss.2010$Income[gss.2010$INCOME06 == '$25,000 to $24,999'] <- "$20,000 to $29,999"
gss.2010$Income[gss.2010$INCOME06 == '$30,000 to $34,999'] <- "$30,000 to $39,999"
gss.2010$Income[gss.2010$INCOME06 == '$35,000 to $39,999'] <- "$30,000 to $39,999"
gss.2010$Income[gss.2010$INCOME06 == '$40,000 to $49,999'] <- "$40,000 to $49,999"
gss.2010$Income[gss.2010$INCOME06 == '$50,000 to $59,999'] <- "$50,000 to $59,999"
gss.2010$Income[gss.2010$INCOME06 == '$60,000 to $74,999'] <- "$60,000 to $74,999"
gss.2010$Income[gss.2010$INCOME06 == '$75,000 to $89,999'] <- "$75,000 to $89,999"
gss.2010$Income[gss.2010$INCOME06 == '$90,000 to $109,999'] <- "$90,000 to $109,999"
gss.2010$Income[gss.2010$INCOME06 == '$110,000 to $129,999'] <- "$110,000 to $129,999"
gss.2010$Income[gss.2010$INCOME06 == '$130,000 to $149,999'] <- "$130,000 to $149,999"
gss.2010$Income[gss.2010$INCOME06 == '$150,000 or over'] <- "$150,000 or over"
# create education as a 14 category factor
gss.2010$Income <- as.factor(gss.2010$Income)
# relevel the variable so "Under $5,000" is the base category for income
gss.2010$Income <- relevel(gss.2010$Income, ref="Under $5,000")
# table new created variable - Income
table(gss.2010$Income)  
mode(gss.2010$Income)
class(gss.2010$Income)



### RECODE VARIABLE "satfin":
# table the original variable
table(gss.2010$satfin)
mode(gss.2010$satfin)
class(gss.2010$satfin)
# recode and create new variable  				  
gss.2010$FinancialSatisfaction <- NA
gss.2010$FinancialSatisfaction[gss.2010$satfin == 'Satisfied'] <- 2
gss.2010$FinancialSatisfaction[gss.2010$satfin == 'More or less satisfied'] <- 1
gss.2010$FinancialSatisfaction[gss.2010$satfin == 'Not at all satisfied'] <- 0
# table new created variable - FinancialSatisfaction
table(gss.2010$FinancialSatisfaction)
mode(gss.2010$FinancialSatisfaction)
class(gss.2010$FinancialSatisfaction)



### RECODE VARIABLE "satjob":
# table the original variable
table(gss.2010$satjob)
mode(gss.2010$satjob)
class(gss.2010$satjob)
# recode and create new variable    			  
gss.2010$JobSatisfaction <- NA
gss.2010$JobSatisfaction[gss.2010$satjob == 'Very satisfied'] <- 3
gss.2010$JobSatisfaction[gss.2010$satjob == 'Moderately satisfied'] <- 2
gss.2010$JobSatisfaction[gss.2010$satjob == 'A little dissatisfied'] <- 1
gss.2010$JobSatisfaction[gss.2010$satjob == 'Very dissatisfied'] <- 0
# table new created variable - JobSatisfaction
table(gss.2010$JobSatisfaction)
mode(gss.2010$JobSatisfaction)
class(gss.2010$JobSatisfaction)



### RECODE VARIABLE "health":
# table the original variable
table(gss.2010$health)
mode(gss.2010$health)
class(gss.2010$health)
# recode and create new variable      		  
gss.2010$GoodHealth <- NA
gss.2010$GoodHealth[gss.2010$health == 'Excellent'] <- 3
gss.2010$GoodHealth[gss.2010$health == 'Good'] <- 2
gss.2010$GoodHealth[gss.2010$health == 'Fair'] <- 1
gss.2010$GoodHealth[gss.2010$health == 'Poor'] <- 0
# table new created variable - GoodHealth
table(gss.2010$GoodHealth)
mode(gss.2010$GoodHealth)
class(gss.2010$GoodHealth)



### RECODE VARIABLE "life":
# table the original variable
table(gss.2010$life)
mode(gss.2010$life)
class(gss.2010$life)
# recode and create new variable        	  
gss.2010$ExcitingLife <- NA
gss.2010$ExcitingLife[gss.2010$life == 'Exciting'] <- 2
gss.2010$ExcitingLife[gss.2010$life == 'Routine'] <- 1
gss.2010$ExcitingLife[gss.2010$life == 'Dull'] <- 0
# table new created variable - ExcitingLife
table(gss.2010$ExcitingLife)
mode(gss.2010$ExcitingLife)
class(gss.2010$ExcitingLife)



### RECODE VARIABLE "marital":
# table the original variable
table(gss.2010$marital)
mode(gss.2010$marital)
class(gss.2010$marital)
# recode and create new variable            
gss.2010$MaritalStatus <- NA
gss.2010$MaritalStatus[gss.2010$marital == 'Married'] <- "Married"
gss.2010$MaritalStatus[gss.2010$marital == 'Widowed'] <- "Widowed"
gss.2010$MaritalStatus[gss.2010$marital == 'Divorced'] <- "Divorced"
gss.2010$MaritalStatus[gss.2010$marital == 'Separated'] <- "Separated"
gss.2010$MaritalStatus[gss.2010$marital == 'Never married'] <- "Never married"
# create education as a 5 category factor
gss.2010$MaritalStatus <- as.factor(gss.2010$MaritalStatus)
# relevel the variable so "Married" is the base category for income
gss.2010$MaritalStatus <- relevel(gss.2010$MaritalStatus, ref="Married")
# table new created variable - MaritalStatus
table(gss.2010$MaritalStatus)
mode(gss.2010$MaritalStatus)
class(gss.2010$MaritalStatus)



### SUBSETTING THE DATA
# subset() function
gss.2010a <- subset(gss.2010, select = c(RespondentID, Female, Age, AgeContinuous, AgeContinuousSquared, Education, Region, PartyID, Religious, Happy, Weights, Race, Urban, PoliticalIdeology, Income, FinancialSatisfaction, JobSatisfaction, GoodHealth, ExcitingLife, MaritalStatus))
# how many rows (cases) are in the subset of data
nrow(gss.2010a)
# names of the columns in the subset of data (variable names)
names(gss.2010a)	



### SUMMARY STATISTICS:
summary(gss.2010a$Happy)
summary(gss.2010a$FinancialSatisfaction)
summary(gss.2010a$JobSatisfaction)
summary(gss.2010a$GoodHealth)
summary(gss.2010a$ExcitingLife)


# Controls:
summary(gss.2010a$Female)
summary(gss.2010a$Age)
summary(gss.2010a$Education)
summary(gss.2010a$Income)
summary(gss.2010a$MaritalStatus)
summary(gss.2010a$Race)
summary(gss.2010a$Religious)
summary(gss.2010a$Urban)
summary(gss.2010a$Region)
summary(gss.2010a$PoliticalIdeology)
summary(gss.2010a$PartyID)



### CORRELATION PLOT
# subsetting data for correlation plot
gss.2010b <- subset(gss.2010, select = c(ExcitingLife, GoodHealth, JobSatisfaction, FinancialSatisfaction, Happy))
# how many rows (cases) are in the subset of data
nrow(gss.2010b)
# names of the columns in the subset of data (variable names)
names(gss.2010b)
# format plot
png("hwk10_corrplot_linear.png", height = 600, width = 600)
par(mfrow = c(1,1))
# corrplot() function
names <- c("Exciting Life", "Good Health", "Job Satisfaction", "Financial Satisfaction", "Happiness")
corrplot (data=gss.2010b, varnames=names, cutpts=NULL, abs=TRUE, n.col.legend=8, details=TRUE, cex.col=0.7, cex.var=1, digits=2, color=TRUE)
title (main = "Correlation Plot")
# THE END
dev.off()



### CREATING WEIGHTED DESIGN
# Using "survey" package:
# WEIGHTED DESIGN:
weighted_design <- svydesign (id=~1, weights=~Weights, data=data.frame(gss.2010a))
summary(weighted_design)



### GENERIC X-Y PLOTTING
# format plot
png("hwk10_plots_linear.png", height = 1000, width = 1000)
par(mfrow = c(2,2))


### FINANCIAL SATISFACTION PLOT:
# plot() function
plot (x = gss.2010a$FinancialSatisfaction, 
      y = gss.2010a$Happy, 
      main = "Relationship between Happiness and Financial Satisfaction", 
      xlab = "Financial Satisfaction", 
      ylab = "Happiness",
      xaxs = "i", yaxs = "i",
      xlim = c(0,2), ylim = c(0,2),
      lty = "blank",
      col = "white")
# points (x = gss.2010a$FinancialSatisfaction, y = gss.2010a$Happy, pch = 20, col = "black")
# bivariate linear regression
lm.FinancialSatisfaction <- svyglm (formula = Happy ~ FinancialSatisfaction, design=weighted_design)
summary(lm.FinancialSatisfaction)
# show the coeffients of the regression
lm.FinancialSatisfaction$coef
# intercept linear coefficient
intercept.FinancialSatisfaction <- lm.FinancialSatisfaction$coef[1]
print(intercept.FinancialSatisfaction)
# b1 linear coefficient
b1.FinancialSatisfaction <- lm.FinancialSatisfaction$coef[2]
print(b1.FinancialSatisfaction)
# curve() function
curve (intercept.FinancialSatisfaction + b1.FinancialSatisfaction * x, add = TRUE, col = "black", lty = "solid", lwd = 2)
# text for legend
legend.text <- vector('expression', 1)
legend.text[1] <- substitute(expression(bolditalic("Happiness") ~~ bold("=") ~~ italic(MYVALUE) + italic(MYVALUE) * bolditalic("Financial Satisfaction")), 
                            list(MYVALUE = format(intercept.FinancialSatisfaction, b1.FinancialSatisfaction, digits=3)))[2]
# legend
legend("bottomleft", legend = legend.text, lty=c("solid"), horiz=FALSE, bg="lightgray", inset=0.06)


### JOB SATISFACTION PLOT:
# plot() function
plot (x = gss.2010a$JobSatisfaction, 
      y = gss.2010a$Happy, 
      main = "Relationship between Happiness and Job Satisfaction", 
      xlab = "Job Satisfaction", 
      ylab = "Happiness",
      xaxs = "i", yaxs = "i",
      xlim = c(0,3), ylim = c(0,2),
      lty = "blank",
      col = "white")
# points (x = gss.2010a$JobSatisfaction, y = gss.2010a$Happy, pch = 20, col = "black")
# bivariate linear regression
lm.JobSatisfaction <- svyglm (formula = Happy ~ JobSatisfaction, design=weighted_design)
summary(lm.JobSatisfaction)
# show the coeffients of the regression
lm.JobSatisfaction$coef
# intercept linear coefficient
intercept.JobSatisfaction <- lm.JobSatisfaction$coef[1]
print(intercept.JobSatisfaction)
# b1 linear coefficient
b1.JobSatisfaction <- lm.JobSatisfaction$coef[2]
print(b1.JobSatisfaction)
# curve() function
curve (intercept.JobSatisfaction + b1.JobSatisfaction * x, add = TRUE, col = "black", lty = "dashed", lwd = 2)
# text for legend
legend.text <- vector('expression', 1)
legend.text[1] <- substitute(expression(bolditalic("Happiness") ~~ bold("=") ~~ italic(MYVALUE) + italic(MYVALUE) * bolditalic("Job Satisfaction")), 
                            list(MYVALUE = format(intercept.JobSatisfaction, b1.JobSatisfaction, digits=3)))[2]
# legend
legend("bottomleft", legend = legend.text, lty=c("dashed"), horiz=FALSE, bg="lightgray", inset=0.06)


### GOOD HEALTH PLOT:
# plot() function
plot (x = gss.2010a$GoodHealth, 
      y = gss.2010a$Happy, 
      main = "Relationship between Happiness and Health", 
      xlab = "Health", 
      ylab = "Happiness",
      xaxs = "i", yaxs = "i",
      xlim = c(0,3), ylim = c(0,2),
      lty = "blank",
      col = "white")
# points (x = gss.2010a$GoodHealth, y = gss.2010a$Happy, pch = 20, col = "black")
# bivariate linear regression
lm.GoodHealth <- svyglm (formula = Happy ~ GoodHealth, design=weighted_design)
summary(lm.GoodHealth)
# show the coeffients of the regression
lm.GoodHealth$coef
# intercept linear coefficient
intercept.GoodHealth <- lm.GoodHealth$coef[1]
print(intercept.GoodHealth)
# b1 linear coefficient
b1.GoodHealth <- lm.GoodHealth$coef[2]
print(b1.GoodHealth)
# curve() function
curve (intercept.GoodHealth + b1.GoodHealth * x, add = TRUE, col = "black", lty = "dotted", lwd = 2)
# text for legend
legend.text <- vector('expression', 1)
legend.text[1] <- substitute(expression(bolditalic("Happiness") ~~ bold("=") ~~ italic(MYVALUE) + italic(MYVALUE) * bolditalic("Health")), 
                             list(MYVALUE = format(intercept.GoodHealth, b1.GoodHealth, digits=3)))[2]
# legend
legend("bottomleft", legend = legend.text, lty=c("dotted"), horiz=FALSE, bg="lightgray", inset=0.06)


### EXCITING LIFE PLOT:
# plot() function
plot (x = gss.2010a$ExcitingLife, 
      y = gss.2010a$Happy, 
      main = "Relationship between Happiness and Exciting Life", 
      xlab = "ExcitingLife", 
      ylab = "Happiness",
      xaxs = "i", yaxs = "i",
      xlim = c(0,2), ylim = c(0,2),
      lty = "blank",
      col = "white")
# points (x = gss.2010a$ExcitingLife, y = gss.2010a$Happy, pch = 20, col = "black")
# bivariate linear regression
lm.ExcitingLife <- svyglm (formula = Happy ~ ExcitingLife, design=weighted_design)
summary(lm.ExcitingLife)
# show the coeffients of the regression
lm.ExcitingLife$coef
# intercept linear coefficient
intercept.ExcitingLife <- lm.ExcitingLife$coef[1]
print(intercept.ExcitingLife)
# b1 linear coefficient
b1.ExcitingLife <- lm.ExcitingLife$coef[2]
print(b1.ExcitingLife)
# curve() function
curve (intercept.ExcitingLife + b1.ExcitingLife * x, add = TRUE, col = "black", lty = "dotdash", lwd = 2)
# text for legend
legend.text <- vector('expression', 1)
legend.text[1] <- substitute(expression(bolditalic("Happiness") ~~ bold("=") ~~ italic(MYVALUE) + italic(MYVALUE) * bolditalic("Exciting Life")), 
                             list(MYVALUE = format(intercept.ExcitingLife, b1.ExcitingLife, digits=3)))[2]
# legend
legend("bottomleft", legend = legend.text, lty=c("dotdash"), horiz=FALSE, bg="lightgray", inset=0.06)


# THE END
dev.off()



### LINEAR REGRESSION MODEL WITHOUT IMPUTATION
lm.no.imputation <- svyglm (formula = Happy ~ FinancialSatisfaction + JobSatisfaction + GoodHealth + ExcitingLife, design=weighted_design)
summary(lm.no.imputation)
display(lm.no.imputation)

lm.no.imputation.with.controls <- svyglm (formula = Happy ~ FinancialSatisfaction + JobSatisfaction + GoodHealth + ExcitingLife + Female + Age + Education + Income + MaritalStatus + Race + Religious + Urban + Region + PoliticalIdeology + PartyID, design=weighted_design)
summary(lm.no.imputation.with.controls)
display(lm.no.imputation.with.controls)


### MISSING DATA IMPUTATION
# aregImpute() function
ImputationHMISC <- with(gss.2010a, aregImpute(formula = ~ Happy + FinancialSatisfaction + JobSatisfaction + GoodHealth + ExcitingLife + Female + Age + Education + Income + MaritalStatus + Race + Religious + Urban + Region + PoliticalIdeology + PartyID, data=gss.2010a, nk=0, n.impute=50))
summary(ImputationHMISC)
print(ImputationHMISC)


### CHECK VARIABLES
ImputationHMISC$impute$Happy
ImputationHMISC$impute$FinancialSatisfaction
ImputationHMISC$impute$JobSatisfaction
ImputationHMISC$impute$GoodHealth
ImputationHMISC$impute$ExcitingLife
ImputationHMISC$impute$Female
ImputationHMISC$impute$Age
ImputationHMISC$impute$Education
ImputationHMISC$impute$Income
ImputationHMISC$impute$MaritalStatus
ImputationHMISC$impute$Race
ImputationHMISC$impute$Religious
ImputationHMISC$impute$Urban
ImputationHMISC$impute$Region
ImputationHMISC$impute$PoliticalIdeology
ImputationHMISC$impute$PartyID


### GENERATE IMPUTED DATASET
Hmisc.data <- gss.2010a
hmisc <- impute.transcan(ImputationHMISC, imputation=1, data=gss.2010a, list.out=TRUE)
Hmisc.data[names(hmisc)] <- hmisc


### CREATING WEIGHTED DESIGN 
# design with Hmisc imputations
design_Hmisc_imputation <- svydesign(id=~1, weights=~Weights, data=data.frame(Hmisc.data))


### LINEAR REGRESSION MODEL WITH HMISC IMPUTATION

lm.Hmisc.imputation <- svyglm (formula = Happy ~ FinancialSatisfaction + JobSatisfaction + GoodHealth + ExcitingLife, design=design_Hmisc_imputation)
summary(lm.Hmisc.imputation)
display(lm.Hmisc.imputation)

lm.Hmisc.imputation.with.controls <- svyglm (formula = Happy ~ FinancialSatisfaction + JobSatisfaction + GoodHealth + ExcitingLife + Female + Age + Education + Income + MaritalStatus + Race + Religious + Urban + Region + PoliticalIdeology + PartyID, design=design_Hmisc_imputation)
summary(lm.Hmisc.imputation.with.controls)
display(lm.Hmisc.imputation.with.controls)


### LINEAR REGRESSION MODEL WITH HMISC IMPUTATION AND WITH INTERACTION TERMS

lm.Hmisc.imputation.interactions.all <- svyglm (formula = Happy ~ FinancialSatisfaction + JobSatisfaction + GoodHealth + ExcitingLife + FinancialSatisfaction:JobSatisfaction + FinancialSatisfaction:GoodHealth + FinancialSatisfaction:ExcitingLife + JobSatisfaction:GoodHealth + JobSatisfaction:ExcitingLife + GoodHealth:ExcitingLife, design=design_Hmisc_imputation)
summary(lm.Hmisc.imputation.interactions.all)
display(lm.Hmisc.imputation.interactions.all)

lm.Hmisc.imputation.interactions.sig <- svyglm (formula = Happy ~ FinancialSatisfaction + JobSatisfaction + GoodHealth + ExcitingLife + JobSatisfaction:ExcitingLife, design=design_Hmisc_imputation)
summary(lm.Hmisc.imputation.interactions.sig)
display(lm.Hmisc.imputation.interactions.sig)

lm.Hmisc.imputation.with.controls.interactions <- svyglm (formula = Happy ~ FinancialSatisfaction + JobSatisfaction + GoodHealth + ExcitingLife + FinancialSatisfaction:JobSatisfaction + FinancialSatisfaction:GoodHealth + FinancialSatisfaction:ExcitingLife + JobSatisfaction:GoodHealth + JobSatisfaction:ExcitingLife + GoodHealth:ExcitingLife + Female + Age + Education + Income + MaritalStatus + Race + Religious + Urban + Region + PoliticalIdeology + PartyID, design=design_Hmisc_imputation)
summary(lm.Hmisc.imputation.with.controls.interactions)
display(lm.Hmisc.imputation.with.controls.interactions)

lm.Hmisc.imputation.with.controls.interactions.sig <- svyglm (formula = Happy ~ FinancialSatisfaction + JobSatisfaction + GoodHealth + ExcitingLife + JobSatisfaction:ExcitingLife + Female + Age + Education + Income + MaritalStatus + Race + Religious + Urban + Region + PoliticalIdeology + PartyID, design=design_Hmisc_imputation)
summary(lm.Hmisc.imputation.with.controls.interactions.sig)
display(lm.Hmisc.imputation.with.controls.interactions.sig)

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 1 
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 2 - LINEAR REGRESSION


### EMPTY ENVIRONMENT:
		ls <- ls()
		remove(ls)

		
### LOAD PACKAGES:
		library ("diff")
		library ("foreign")
		library ("arm")
		library ("calibrate")
		library ("gdata")



### DATA:
		# Data is from Pew Research Center polls taken during the 2008 election campaign
		# SOURCE: http://www.stat.columbia.edu/~gelman/surveys.course/pew_research_center_june_elect_wknd_data.dta
		# read the data into R
		pew.data <- read.dta("pew_research_center_june_elect_wknd_data.dta")
		# how many rows (cases) are in the dataset
		nrow(pew.data)
		# names of the columns (variable names)
		names(pew.data)
	


### SUBSETTING THE DATA
		# subset() function
		pew.data <- subset(pew.data, select = c(ideo, sex, age, age2, marital))
		# how many rows (cases) are in the subset of data
		nrow(pew.data)
		# names of the columns in the subset of data (variable names)
		names(pew.data)		



### Political Ideology 
		# table the original variable
		table(pew.data$ideo)
		
		
		# FIRST OPTION to recode and create new variable
		pew.data$Political.Ideology <- ifelse (pew.data$ideo == 'dk/refused', 0,
							  		   ifelse (pew.data$ideo == 'very liberal', -2,
							  		   ifelse (pew.data$ideo == 'liberal', -1,
							  	 	   ifelse (pew.data$ideo == 'moderate', 0,
							  		   ifelse (pew.data$ideo == 'conservative', 1,
							  		   ifelse (pew.data$ideo == 'very conservative', 2, NA))))))
				
				# table new created variable - Political.Ideology
				table(pew.data$Political.Ideology)
		
		
		# SECOND OPTION to recode and create new variable					  
		pew.data$Political.Ideology2 <- NA
		pew.data$Political.Ideology2[pew.data$ideo == 'very liberal'] <- -2
		pew.data$Political.Ideology2[pew.data$ideo == 'liberal'] <- -1
		pew.data$Political.Ideology2[pew.data$ideo == 'moderate'] <- 0
		pew.data$Political.Ideology2[pew.data$ideo == 'dk/refused'] <- 0
		pew.data$Political.Ideology2[pew.data$ideo == 'conservative'] <- 1
		pew.data$Political.Ideology2[pew.data$ideo == 'very conservative'] <- 2
		
				# table new created variable - Political.Ideology2
				table(pew.data$Political.Ideology2)
		
		# NOTE: both Political.Ideology and Political.Ideology2 are identical variables
		# NOTE: for the regression will use only the first one: Political.Ideology
		
		
### Gender
		# table the original variable
		table(pew.data$sex)
		
		# FIRST OPTION to recode and create new variable
		pew.data$Female <- ifelse (pew.data$sex == 'male', 0, 1)
				
				# table new created variable - Female
				table(pew.data$Female)
		
		
		# SECOND OPTION to recode and create new variable					  
		pew.data$Female2 <- NA
		pew.data$Female2[pew.data$sex == 'male'] <- 0
		pew.data$Female2[pew.data$sex == 'female'] <- 1
		
		
				# table new created variable - Female2
				table(pew.data$Female2)
		
		# NOTE: both Female and Female2 are identical variables
		# NOTE: for the regression will use only the first one: Female	


### Age
		# table the original variable (in the dataset we have two age variables: "age" and "age2" - we need to explore both)
		table(pew.data$age)
		table(pew.data$age2)	
		# "age" is the variable of interest, because its continuous, unlike "age2", which is has only four age intervals
		# we can observe that the "dk/refused" from "age2" is 517, the same number of people age 99 in the first variable
		# we need to exclude the 99 age category in the first variable, because it represent missing cases
		pew.data$age[pew.data$age>97] <- NA
		# table variable after changes
		table(pew.data$age)
		# recode the age variable into 6 categoies
				pew.data$Age6 <- ifelse (pew.data$age < 25, "18-24 years",
							  	 ifelse (pew.data$age >= 25 & pew.data$age < 35, "25-34 years",
							  	 ifelse (pew.data$age >= 35 & pew.data$age < 45, "35-44 years",
							  	 ifelse (pew.data$age >= 45 & pew.data$age < 55, "45-54 years",
							  	 ifelse (pew.data$age >= 55 & pew.data$age < 65, "55-64 years",
							  	 ifelse (pew.data$age >= 65, "Over 65 years", NA))))))
		# table new created variable - Age6
				table(pew.data$Age6)
		# recode the "Age6" variable 
 				pew.data$Age <- NA
				pew.data$Age[pew.data$Age6 == '18-24 years'] <- 0
				pew.data$Age[pew.data$Age6 == '25-34 years'] <- 1
				pew.data$Age[pew.data$Age6 == '35-44 years'] <- 2
				pew.data$Age[pew.data$Age6 == '45-54 years'] <- 3
				pew.data$Age[pew.data$Age6 == '55-64 years'] <- 4
				pew.data$Age[pew.data$Age6 == 'Over 65 years'] <- 5
		# table new created variable - Age
				table(pew.data$Age)	
				
				
		
### Marital Status
		# table the original variable 
		table(pew.data$marital)
		# we need to exclude the "dk/refused" category 
		pew.data$marital[pew.data$marital == 'dk/refused'] <- NA
		# table variable after changes
		table(pew.data$marital)
		

### SUMMARY OF VARIABLES:
summary(pew.data$Political.Ideology)
summary(pew.data$Female)
summary(pew.data$Age)
summary(pew.data$age)
summary(pew.data$marital)


		
### LINEAR REGRESSION WITHOUT INTERACTION TERMS


		### "age" - CONTINUOUS VARIABLE (18 to 97 years):
		# lm() function
		lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$age + pew.data$marital)
		
		# print() function
		reg.without.interaction1 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$age + pew.data$marital)
		print(reg.without.interaction1)
		
		# summary() function
		reg.without.interaction1 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$age + pew.data$marital)
		summary(reg.without.interaction1)
		
		# display() function
		reg.without.interaction1 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$age + pew.data$marital)
		display(reg.without.interaction1)
		
		
		### "Age" HAS 6 CATEGORIES:
		# lm() function
		lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital)
		
		# print() function
		reg.without.interaction2 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital)
		print(reg.without.interaction2)
		
		# summary() function
		reg.without.interaction2 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital)
		summary(reg.without.interaction2)
		
		# display() function
		reg.without.interaction2 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital)
		display(reg.without.interaction2)
		
		
		
### LINEAR REGRESSION WITH INTERACTION TERMS


		### "age" - CONTINUOUS VARIABLE (18 to 97 years):
		# lm() function
		lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$age + pew.data$marital + pew.data$Female:pew.data$age + pew.data$Female:pew.data$marital + pew.data$age:pew.data$marital)
		
		# print() function
		reg.with.interaction1 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$age + pew.data$marital + pew.data$Female:pew.data$age + pew.data$Female:pew.data$marital + pew.data$age:pew.data$marital)
		print(reg.with.interaction1)
		
		# summary() function
		reg.with.interaction1 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$age + pew.data$marital + pew.data$Female:pew.data$age + pew.data$Female:pew.data$marital + pew.data$age:pew.data$marital)
		summary(reg.with.interaction1)
		
		# display() function
		reg.with.interaction1 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$age + pew.data$marital + pew.data$Female:pew.data$age + pew.data$Female:pew.data$marital + pew.data$age:pew.data$marital)
		display(reg.with.interaction1)
		

		
		### "Age" HAS 6 CATEGORIES:
		# lm() function
		lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital)
		
		# print() function
		reg.with.interaction2 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital)
		print(reg.with.interaction2)
		
		# summary() function
		reg.with.interaction2 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital)
		summary(reg.with.interaction2)
		
		# display() function
		reg.with.interaction2 <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital)
		display(reg.with.interaction2)
		



### EVALUATING THE FINAL MODEL:

		# LINEAR REGRESSION WITH INTERACTION TERMS 
		# "Age" HAS 6 CATEGORIES
		# display() function
		
		reg.final.model <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital)
		display(reg.final.model)
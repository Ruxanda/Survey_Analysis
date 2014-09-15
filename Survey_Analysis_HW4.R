#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 4 
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 3 - MISSING-DATA IMPUTATION:  

# Create a miniature version of the 2010 General Social Survey (http://www.thearda.com/Archive/Files/Codebooks/GSS10PAN_CB.asp), including the following variables:  sex, age, ethnicity (use four categories), urban/suburban/rural, education (use five categories), political ideology (on a 7-point scale from “extremely liberal” to “extremely conservative”), and general happiness.

# (a) Fit a logistic regression on whether respondents feel “not too happy,” given the other variables in the dataset.  Display (using display()) the results for the logistic regression fit to the complete cases (this is the result if you just feed the data including NA’s into R).

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
		# what type of variable is age
		mode(gss.2010$age)
		# we need to exclude the 99 age category in the "age" variable, because it represent missing cases
		# category "99" in the codebook is labeled as "No answer"
		gss.2010$age <- ifelse(gss.2010$age == "No answer", NA, gss.2010$age)
		# table variable after changes
		table(gss.2010$age)
		# recode the age variable into 6 categoies
		gss.2010$Age <- ifelse (gss.2010$age < 8, "18-24 years",
						ifelse (gss.2010$age >= 8 & gss.2010$age < 18, "25-34 years",
						ifelse (gss.2010$age >= 18 & gss.2010$age < 28, "35-44 years",
						ifelse (gss.2010$age >= 28 & gss.2010$age < 38, "45-54 years",
						ifelse (gss.2010$age >= 38 & gss.2010$age < 48, "55-64 years",
						ifelse (gss.2010$age >= 48, "Over 65 years", NA))))))
		# create age as a 6 category factor
		gss.2010$Age <- as.factor(gss.2010$Age)
		# relevel the variable so "18-24 years" is the base category for age
		gss.2010$Age <- relevel(gss.2010$Age, ref="18-24 years")
		# table new created variable - Age
		table(gss.2010$Age)	
		mode(gss.2010$Age)
		class(gss.2010$Age)

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
		gss.2010$Unhappy <- NA
		gss.2010$Unhappy[gss.2010$happy == 'Not too happy'] <- 1
		gss.2010$Unhappy[gss.2010$happy == 'Pretty happy'] <- 0
		gss.2010$Unhappy[gss.2010$happy == 'Very happy'] <- 0
		# table new created variable - Happy
		table(gss.2010$Unhappy)
		mode(gss.2010$Unhappy)

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
		# relevel the variable so "Urban" is the base category for urban
		gss.2010$Urban <- relevel(gss.2010$Urban, ref="Urban")
		# table new created variable
		table(gss.2010$Urban)
		mode(gss.2010$Urban)
		class(gss.2010$Urban)

### RECODE VARIABLE "ideo":
# table the original variable
		table(gss.2010$polviews)
		# recode and create new variable					  
		gss.2010$Political.Ideology <- NA
		gss.2010$Political.Ideology[gss.2010$polviews == 'Extremely liberal'] <- -3
		gss.2010$Political.Ideology[gss.2010$polviews == 'Liberal'] <- -2
		gss.2010$Political.Ideology[gss.2010$polviews == 'Slightly liberal'] <- -1
		gss.2010$Political.Ideology[gss.2010$polviews == 'Moderate'] <- 0
		gss.2010$Political.Ideology[gss.2010$polviews == 'Slightly conservative'] <- 1
		gss.2010$Political.Ideology[gss.2010$polviews == 'Conservative'] <- 2
		gss.2010$Political.Ideology[gss.2010$polviews == 'Extremely conservative'] <- 3
		# table new created variable - Political.Ideology
		table(gss.2010$Political.Ideology)
		mode(gss.2010$Political.Ideology)
		class(gss.2010$Political.Ideology)

### SUBSETTING THE DATA
		# subset() function
		gss.2010a <- subset(gss.2010, select = c(RespondentID, Female, Age, Education, Region, PartyID, Religious, Unhappy, Weights, Race, Urban, Political.Ideology))
		# how many rows (cases) are in the subset of data
		nrow(gss.2010a)
		# names of the columns in the subset of data (variable names)
		names(gss.2010a)	
		
### CREATING WEIGHTED DESIGN
	   # Using "survey" package:
	   # WEIGHTED DESIGN:
		weighted_design <- svydesign (id=~1, weights=~Weights, data=data.frame(gss.2010a))
		summary(weighted_design)
		
### LOGISTIC REGRESSION MODEL WITHOUT IMPUTATION
logit.no.imputation <- svyglm (formula = Unhappy ~ Female + Age + Political.Ideology + Education + Race + Urban, design=weighted_design, family=quasibinomial(link="logit"))
summary(logit.no.imputation)



#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

# (b) Impute the missing values using mi() in the “mi” package in R.  Then take one of the completed datasets and fit and display a logistic regression as above.

### SUBSETTING THE DATA
		# subset() function
		gss.2010b <- subset(gss.2010, select = c(RespondentID, Female, Age, Education, Region, PartyID, Religious, Unhappy, Weights, Race, Urban, Political.Ideology))
		# how many rows (cases) are in the subset of data
		nrow(gss.2010b)
		# names of the columns in the subset of data (variable names)
		names(gss.2010b)

### Summary of all the variables in dataset gss.2010b	
summary(gss.2010b$RespondentID)
summary(gss.2010b$Female)
summary(gss.2010b$Age)
summary(gss.2010b$Education)
summary(gss.2010b$Region)
summary(gss.2010b$PartyID)
summary(gss.2010b$Religious)
summary(gss.2010b$Unhappy)
summary(gss.2010b$Weights)
summary(gss.2010b$Race)
summary(gss.2010b$Urban)
summary(gss.2010b$Political.Ideology)		
		
### OBTAIN INFORMATION ABOUT PARAMETERS IN THE DATASET
info.gss.2010b <- mi.info(gss.2010b)
info.gss.2010b$params

### MISSING DATA IMPUTATION
	# mi() function
IMP <- mi(gss.2010b, max.minutes=20, add.noise=FALSE, n.iter=40)
IMP1 <- mi.data.frame(IMP, m=1)
IMP2 <- mi.data.frame(IMP, m=2)
IMP3 <- mi.data.frame(IMP, m=3)

converged(IMP, check="data")
converged(IMP, check="coef")

### CREATING WEIGHTED DESIGN 
	# design with mi imputations
design_mi_imputation <- svydesign(id=~1, weights=~Weights, data=data.frame(IMP1))

### LOGISTIC REGRESSION MODEL WITH MI IMPUTATION
logit.mi.imputation <- svyglm (formula = Unhappy ~ Female + Age + Political.Ideology + Education + Race + Urban, design=design_mi_imputation, family=quasibinomial(link="logit"))
summary(logit.mi.imputation)


#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

# (c) Repeat, this time imputing using aregImpute() in the “Hmisc” package.

### SUBSETTING THE DATA
		# subset() function
		gss.2010c <- subset(gss.2010, select = c(RespondentID, Female, Age, Education, Region, PartyID, Religious, Unhappy, Weights, Race, Urban, Political.Ideology))
		# how many rows (cases) are in the subset of data
		nrow(gss.2010c)
		# names of the columns in the subset of data (variable names)
		names(gss.2010c)
		
		
### Summary of all the variables in dataset gss.2010b	
summary(gss.2010c$RespondentID)
summary(gss.2010c$Female)
summary(gss.2010c$Age)
summary(gss.2010c$Education)
summary(gss.2010c$Region)
summary(gss.2010c$PartyID)
summary(gss.2010c$Religious)
summary(gss.2010c$Unhappy)
summary(gss.2010c$Weights)
summary(gss.2010c$Race)
summary(gss.2010c$Urban)
summary(gss.2010c$Political.Ideology)


	
### MISSING DATA IMPUTATION
	# aregImpute() function
ImputationHMISC <- with(gss.2010c, aregImpute(formula = ~Unhappy + Female + Age + Political.Ideology + Education + Race + Urban, data=gss.2010c, n.impute=50))
summary(ImputationHMISC)
print(ImputationHMISC)


### CHECK VARIABLES
ImputationHMISC$impute$Unhappy
ImputationHMISC$impute$Female
ImputationHMISC$impute$Age
ImputationHMISC$impute$Political.Ideology
ImputationHMISC$impute$Education
ImputationHMISC$impute$Race
ImputationHMISC$impute$Urban


### TAKING A LOOK AT THE IMPUTATIONS

dfUnhappy <- as.data.frame(ImputationHMISC$impute$Unhappy)
dfAge <- as.data.frame(ImputationHMISC$impute$Age)
dfPolitical.Ideology <- as.data.frame(ImputationHMISC$impute$Political.Ideology)
dfEducation <- as.data.frame(ImputationHMISC$impute$Education)

dfUnhappy$id <- as.numeric (as.vector (rownames(dfUnhappy)))
dfAge$id <- as.numeric (as.vector (rownames(dfAge)))
dfPolitical.Ideology$id <- as.numeric (as.vector (rownames(dfPolitical.Ideology)))
dfEducation$id <- as.numeric (as.vector (rownames(dfEducation)))

table(dfUnhappy$id)
table(dfAge$id)
table(dfPolitical.Ideology$id)
table(dfEducation$id)

names(dfUnhappy)
names(dfAge)
names(dfPolitical.Ideology)
names(dfEducation)

### GENERATE IMPUTED DATASET
Hmisc.data <- gss.2010c
hmisc <- impute.transcan(ImputationHMISC, imputation=1, data=gss.2010c, list.out=TRUE)
Hmisc.data[names(hmisc)] <- hmisc


### CREATING WEIGHTED DESIGN 
	# design with Hmisc imputations
design_Hmisc_imputation <- svydesign(id=~1, weights=~Weights, data=data.frame(Hmisc.data))


### LOGISTIC REGRESSION MODEL WITH HMISC IMPUTATION
logit.Hmisc.imputation <- svyglm (formula = Unhappy ~ Female + Age + Political.Ideology + Education + Race + Urban, design=design_Hmisc_imputation, family=quasibinomial(link="logit"))
summary(logit.Hmisc.imputation)

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


### PROBLEM 4 - RATIO AND REGRESSION ESTIMATION:

# Exercise 5.3 from Lumley:  Using the data from Wave 1 of the 1996 SIPP panel (see Lumley Figure 3.8):

# (a) Estimate the ratio of population totals for monthly rent (“tmthrnt”) and total household income (“thtrninc”) over the whole population and over the subpopulation who pay rent.

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
		library ("RSQLite")

### DATA:
		# Data is from the Survey of income and program participation, Wave 1 of 1996 panel		
		# SOURCE: http://faculty.washington.edu/tlumley/svybook/
		# read the data into R
		m <- dbDriver("SQLite")
		# create connection
		con <- dbConnect("SQLite", dbname = "sipp.db") 
		# list the tables available in the database
		dbListTables(con)
		
		
### CREATE SURVEY DESIGN FOR ENTRE SAMPLE
		sipp.hh <- svydesign (id=~ghlfsam, strata=~gvarstr, nest=TRUE, weight=~whfnwgt, data="household", dbtype="SQLite", dbname="sipp.db")
		
### GET DATA
		household <- dbGetQuery(con, "SELECT * FROM household")
		# names of the columns (variable names)
		names(household)
		
		
### CREATE SURVEY DESIGN FOR SUBSET WHERE RENT IS GREATER THAN 0
		sipp.hh.rent <- subset(sipp.hh, tmthrnt>0)
				
		
### RATIO FOR ENTIRE POPULATION
		tot.ratio1 <- svyratio(~tmthrnt, ~thtrninc, sipp.hh)
		tot.ratio1		
		
	
### Ratio for rent > 0 subset
		tot.ratio1.subset <- svyratio (~tmthrnt, ~thtrninc, sipp.hh.rent)
		tot.ratio1.subset

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


# (b) Compute the individual-level ratio, i.e., the proportion of household income paid in rent, and estimate the population mean over the whole population and the subpopulation who pay rent.


### CREATE SURVEY DESIGN FOR SUBSET WHERE RENT IS GREATER THAN 0
		sipp.hh <- update(sipp.hh, has_income = (thtrninc>0), ~has_income, sipp.hh)
		sipp.hh.indv.rent <- subset(sipp.hh, tmthrnt>0)


### RATIO FOR ENTIRE POPULATION
		indv.ratio2 <- svyratio(~I(tmthrnt*has_income), ~has_income, sipp.hh)
		indv.ratio2	
	

### Ratio for rent > 0 subset
		indv.ratio2.subset <- svyratio (~I(tmthrnt*has_income), ~has_income, sipp.hh.indv.rent)
		indv.ratio2.subset


### Individual level ratio - the proportion of household income paid in rent
		household$rent.percentage <- household$tmthrnt / household$thtrninc
		# eliminate the individuals with no income
		household$rent.percentage[household$thtrninc == 0] <- NA
		# summarize newly created ratio
		summary(household$rent.percentage)
		
		
### Subset the dataset
		rent.ratio.household <- subset(household, is.na(rent.percentage) == FALSE)
		
		
### Create new survey design 
		sipp.hh.indv <- svydesign(id=~ghlfsam, strata=~gvarstr, nest=TRUE, weight=~whfnwgt, data=rent.ratio.household, survey.adj.domain.lonely=TRUE)
		sipp.hh.indv.rent <- subset(sipp.hh.indv, tmthrnt>0)
		
### Population mean of the individual level ratio (the proportion of household income paid in rent) over the whole population 		
		rent.mean.population <- svymean(~rent.percentage, sipp.hh.indv)
		rent.mean.population
		
### Population mean of the individual level ratio (the proportion of household income paid in rent) over the subpopulation who pays rent		
		rent.mean.subpopulation <- svymean(~rent.percentage, sipp.hh.indv.rent)
		rent.mean.subpopulation


#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
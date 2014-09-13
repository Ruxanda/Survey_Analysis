#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 3 
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 1 - WEIGHTED ANALYSIS.  Using the Pew surveys from the previous homework:

# (a) Compute the weighted average proportion liberal in each state and plot vs. the raw average; this should be a square plot (in R, par (pty=”s”)) with identical scales on x and y axes, and each state indicated by its two-letter abbreviation.

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
		pew.data <- subset(pew.data, select = c(ideo, state, weight))
		# how many rows (cases) are in the subset of data
		nrow(pew.data)
		# names of the columns in the subset of data (variable names)
		names(pew.data)		



### Political Ideology (Liberal)
		# table the original variable
		table(pew.data$ideo)
		
		
		# FIRST OPTION to recode and create new variable
		pew.data$Liberal <- ifelse (pew.data$ideo == 'dk/refused', 0,
							  		   ifelse (pew.data$ideo == 'very liberal', 1,
							  		   ifelse (pew.data$ideo == 'liberal', 1,
							  	 	   ifelse (pew.data$ideo == 'moderate', 0,
							  		   ifelse (pew.data$ideo == 'conservative', 0,
							  		   ifelse (pew.data$ideo == 'very conservative', 0, NA))))))
				
				# table new created variable - Political.Ideology
				table(pew.data$Liberal)
		
		
		# SECOND OPTION to recode and create new variable					  
		pew.data$Liberal2 <- NA
		pew.data$Liberal2[pew.data$ideo == 'very liberal'] <- 1
		pew.data$Liberal2[pew.data$ideo == 'liberal'] <- 1
		pew.data$Liberal2[pew.data$ideo == 'moderate'] <- 0
		pew.data$Liberal2[pew.data$ideo == 'dk/refused'] <- 0
		pew.data$Liberal2[pew.data$ideo == 'conservative'] <- 0
		pew.data$Liberal2[pew.data$ideo == 'very conservative'] <- 0
		
				# table new created variable - Political.Ideology2
				table(pew.data$Liberal2)
		
		# NOTE: both Liberal and Liberal2 are identical variables
		# NOTE: for the logistic regression will use only the first one: Liberal
		
		
### State
		# table the original variable
		table(pew.data$state)
		# STEP 1: exclude Washington DC
		pew.data <- subset(pew.data, pew.data$state!="washington dc")
		# table(pew.data$state)
		# STEP 2: exclude Alaska
		pew.data <- subset(pew.data, pew.data$state!="alaska")
		# table(pew.data$state)
		# STEP 3: exclude Hawaii
		pew.data <- subset(pew.data, pew.data$state!="hawaii")
		# table(pew.data$state)

		
### SUBSETTING THE DATA - removing observations with NA values
		# subset() function
		no.na <- subset(pew.data, pew.data$Liberal==1 | pew.data$Liberal==0)
		# how many rows (cases) are in the subset of data
		nrow(no.na)
		# names of the columns in the subset of data (variable names)
		names(no.na)	
		
		
### COMPUTE THE MEAN OF LIBERAL BY STATE WITHOUT WEIGHTS
		# aggregate() function
		agg.meanvotebystate <- aggregate (x=no.na$Liberal, list(no.na$state), FUN=mean)
		# FUN= lenght to get the number of rows by state
		# names of the variables in the dataset
		names(agg.meanvotebystate)
		# create new vector "state.agg"
		agg.meanvotebystate$state.agg <- as.vector(agg.meanvotebystate$Group.1)
		# table new vector "state.agg"
		table(agg.meanvotebystate$state.agg)
		# create new vector "mean.ideo"
		agg.meanvotebystate$mean.ideo <- as.vector(agg.meanvotebystate$x)
		# table new vector "mean.ideo"
		table(agg.meanvotebystate$mean.ideo)
		# names of the variables in the dataset
		names(agg.meanvotebystate)
		# table "state.agg" by "mean.ideo"
		table(agg.meanvotebystate$state.agg, agg.meanvotebystate$mean.ideo)


		
		
### STATE NAMES AND STATE ABBREVIATIONS IN R
		# "state.name" - list of state names in R
		table(state.name)
		# STEP 1: convert upper-case characters in a character vector to lower-case
		state.n <- as.vector(tolower(state.name))
		table(state.n)
		# STEP 2: exclude Alaska
		state.n <- subset(state.n, state.n!="alaska")
		# table(state.n)
		# STEP 3: exclude Hawaii
		state.n <- subset(state.n, state.n!="hawaii")
		# table(state.n)
		
		# "state.abb" -  list of state abbreviations in R
		table(state.abb)
		# STEP 1: make new vector
		state.ab <- as.vector(state.abb)
		table(state.ab)
		#STEP 2: exclude Alaska
		state.ab <- subset(state.ab, state.ab!="AK")
		# table(state.ab)
		# STEP 3: exclude Hawaii
		state.ab <- subset(state.ab, state.ab!="HI")
		# table(state.ab)
		
		# cbind() function - combine into one dataset the state.n and state.ab variables
		abbrev <- as.data.frame(cbind(state.ab, state.n))
		# how many rows (cases) are in the subset of data
		nrow(abbrev)
		# names of the columns in the subset of data (variable names)
		names(abbrev)
	
		
		
				
### MERGE LIBERAL IDEOLOGY BY STATE WITH STATE ABBREVIATION
	# STEP 1:	
		# merge() function - merge two data frames by common row names (state names)
		pew.data <- merge (pew.data, abbrev, by.x="state", by.y="state.n")
		# how many rows (cases) are in the dataset
		nrow(pew.data)
		# names of the columns (variable names)
		names(pew.data)
		# table state by state abbreviation
		table(pew.data$state, pew.data$state.ab)
		
	# STEP 2:	
		# merge() function - merge two data frames by common row names (state names)
		pew.data <- merge (pew.data, agg.meanvotebystate, by.x="state", by.y="state.agg")
		# how many rows (cases) are in the dataset
		nrow(pew.data)
		# names of the columns (variable names)
		names(pew.data)

	



### SUBSETTING THE DATA - removing observations with NA values
		# subset() function
		pew.data <- subset(pew.data, is.na(pew.data$Liberal)== FALSE)
		attach (pew.data)
		# how many rows (cases) are in the subset of data
		nrow(pew.data)
		# names of the columns in the subset of data (variable names)
		names(pew.data)	
		
		
		
### Using "survey" package:
		
		# WEIGHTED
		weighted_design <- svydesign (id=~1, weights=~weight, data=pew.data)
		summary(weighted_design)
				
		
		
### COMPUTE THE MEAN OF LIBERAL BY STATE WITH WEIGHTS
		# svymean() function
		svy.means.state <- svyby (formula=~Liberal, by=state, design=weighted_design, FUN=svymean)
		# how many rows (cases) are in the subset of data
		nrow(svy.means.state)
		# names of the columns in the subset of data (variable names)
		names(svy.means.state)	
		# create new vector "state.svy"
		svy.means.state$state.svy <- as.vector(svy.means.state$by)
		# table new vector "state.svy"
		table(svy.means.state$state.svy)
		# create new vector "mean.ideo.weighted"
		svy.means.state$mean.ideo.weighted <- as.vector(svy.means.state$Liberal)
		# table new vector "mean.ideo"
		table(svy.means.state$mean.ideo.weighted)
		# create new vector "mean.ideo.se"
		svy.means.state$mean.ideo.se <- as.vector(svy.means.state$se)
		# table new vector "mean.ideo.se"
		table(svy.means.state$mean.ideo.se)
		# names of the variables in the dataset
		names(svy.means.state)

		

### MERGE ELECTION RESULTS WITH STATE ABBREVIATION	
		# merge() function - merge two data frames by common row names (state names)
		pew.data <- merge (pew.data, svy.means.state, by.x="state", by.y="state.svy")
		# how many rows (cases) are in the dataset
		nrow(pew.data)
		# names of the columns (variable names)
		names(pew.data)

		
		
			
### REMOVE STATES THAT DO NOT HAVE DATA
	pew.data <- subset(pew.data, pew.data$mean.ideo!=0)
	# how many rows (cases) are in the dataset
	nrow(pew.data)
	# names of the columns (variable names)
	names(pew.data)




### PLOTS FORMAT
	png ("hwk3_1.png", height=900, width=900)
	par (mfrow=c(1,1), pty="s")



### PLOT THE WEIGHTED AVERAGE PROPORTION LIBERAL IN EACH STATE VERSUS THE RAW AVERAGE
plot (x=pew.data$mean.ideo, 
		y=pew.data$mean.ideo.weighted, 
		main="Relationship between weighted and unweighted average proportion of liberals in each state", 
		xlab="Unweighted average proportion of liberals", 
		ylab="Weighted average proportion of liberals", 
		pch=".")
text(x=pew.data$mean.ideo, y=pew.data$mean.ideo.weighted, labels=pew.data$state.ab)



### THE END
dev.off()




#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 1 - WEIGHTED ANALYSIS.  Using the Pew surveys from the previous homework:

# (b) Using the “survey” package in R, fit a linear regression (using the lm() function in R) to predict political ideology, given sex, age, and marital status.  Compare to the unweighted results.

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
		pew.data <- subset(pew.data, select = c(ideo, sex, age, age2, marital, weight))
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

### REMOVE STATES THAT DO NOT HAVE DATA
	# pew.data <- subset(pew.data, pew.data$Liberal!=0)
	# how many rows (cases) are in the dataset
	nrow(pew.data)
	# names of the columns (variable names)
	names(pew.data)
	
	

		
### LINEAR REGRESSION WITHOUT INTERACTION TERMS AND WITHOUT WEIGHTS

		### "Age" HAS 6 CATEGORIES:
		
		# summary() function
		reg.without.interaction.without.weights <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital)
		summary(reg.without.interaction.without.weights)
		
		# display() function
		reg.without.interaction.without.weights <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital)
		display(reg.without.interaction.without.weights)
		
		
		
### LINEAR REGRESSION WITH INTERACTION TERMS AND WITHOUT WEIGHTS
		
		### "Age" HAS 6 CATEGORIES:

		# summary() function
		reg.with.interaction.without.weights <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital)
		summary(reg.with.interaction.without.weights)
		
		# display() function
		reg.with.interaction.without.weights <- lm(pew.data$Political.Ideology ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital)
		display(reg.with.interaction.without.weights)
		

### Using "survey" package:
		
		# WEIGHTED
		weighted_design <- svydesign (id=~1, weights=~weight, data=pew.data)
		summary(weighted_design)


		
### LINEAR REGRESSION WITHOUT INTERACTION TERMS AND WITH WEIGHTS

		### "Age" HAS 6 CATEGORIES:
		
		# summary() function
		reg.without.interaction.with.weights <- svyglm(formula = Political.Ideology ~ Female + Age + marital, design=weighted_design)
		summary(reg.without.interaction.with.weights)
		
		# print() function
		reg.without.interaction.with.weights <- svyglm(formula = Political.Ideology ~ Female + Age + marital, design = weighted_design)
		print(reg.without.interaction.with.weights)
		
		
		
### LINEAR REGRESSION WITH INTERACTION TERMS AND WITH WEIGHTS
		
		### "Age" HAS 6 CATEGORIES:

		# summary() function
		reg.with.interaction.with.weights <- svyglm(formula = Political.Ideology ~ Female + Age + marital + Female:Age + Female:marital + Age:marital, design = weighted_design)
		summary(reg.with.interaction.with.weights)
		
		# print() function
		reg.with.interaction.with.weights <- svyglm(formula = Political.Ideology ~ Female + Age + marital + Female:Age + Female:marital + Age:marital, design = weighted_design)
		print(reg.with.interaction.with.weights)
		


#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
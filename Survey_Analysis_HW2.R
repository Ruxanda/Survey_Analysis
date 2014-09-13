#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 2 
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 3 - LOGISTIC REGRESSION

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
summary(pew.data$Liberal)
summary(pew.data$Female)
summary(pew.data$Age)
summary(pew.data$age)
summary(pew.data$marital)


		
### LOGISTIC REGRESSION WITHOUT INTERACTION TERMS


		### "age" - CONTINUOUS VARIABLE (18 to 97 years):
		# glm() function
		glm(pew.data$Liberal ~ pew.data$Female + pew.data$age + pew.data$marital, family=binomial(link="logit"))
		
		# print() function
		logit.without.interaction1 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$age + pew.data$marital, family=binomial(link="logit"))
		print(logit.without.interaction1)
		
		# summary() function
		logit.without.interaction1 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$age + pew.data$marital, family=binomial(link="logit"))
		summary(logit.without.interaction1)
		
		# display() function
		logit.without.interaction1 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$age + pew.data$marital, family=binomial(link="logit"))
		display(logit.without.interaction1)
		
		
		### "Age" HAS 6 CATEGORIES:
		# glm() function
		glm(pew.data$Liberal ~ pew.data$Female + pew.data$Age + pew.data$marital, family=binomial(link="logit"))
		
		# print() function
		logit.without.interaction2 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$Age + pew.data$marital, family=binomial(link="logit"))
		print(logit.without.interaction2)
		
		# summary() function
		logit.without.interaction2 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$Age + pew.data$marital, family=binomial(link="logit"))
		summary(logit.without.interaction2)
		
		# display() function
		logit.without.interaction2 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$Age + pew.data$marital, family=binomial(link="logit"))
		display(logit.without.interaction2)
		
		
		
### LOGISTIC REGRESSION WITH INTERACTION TERMS


		### "age" - CONTINUOUS VARIABLE (18 to 97 years):
		# glm() function
		glm(pew.data$Liberal ~ pew.data$Female + pew.data$age + pew.data$marital + pew.data$Female:pew.data$age + pew.data$Female:pew.data$marital + pew.data$age:pew.data$marital, family=binomial(link="logit"))
		
		# print() function
		logit.with.interaction1 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$age + pew.data$marital + pew.data$Female:pew.data$age + pew.data$Female:pew.data$marital + pew.data$age:pew.data$marital, family=binomial(link="logit"))
		print(logit.with.interaction1)
		
		# summary() function
		logit.with.interaction1 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$age + pew.data$marital + pew.data$Female:pew.data$age + pew.data$Female:pew.data$marital + pew.data$age:pew.data$marital, family=binomial(link="logit"))
		summary(logit.with.interaction1)
		
		# display() function
		logit.with.interaction1 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$age + pew.data$marital + pew.data$Female:pew.data$age + pew.data$Female:pew.data$marital + pew.data$age:pew.data$marital, family=binomial(link="logit"))
		display(logit.with.interaction1)
		

		
		### "Age" HAS 6 CATEGORIES:
		# glm() function
		glm(pew.data$Liberal ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital, family=binomial(link="logit"))
		
		# print() function
		logit.with.interaction2 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital, family=binomial(link="logit"))
		print(logit.with.interaction2)
		
		# summary() function
		logit.with.interaction2 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital, family=binomial(link="logit"))
		summary(logit.with.interaction2)
		
		# display() function
		logit.with.interaction2 <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital, family=binomial(link="logit"))
		display(logit.with.interaction2)
		



### EVALUATING THE FINAL LOGISTIC MODEL:

		# LOGISTIC REGRESSION WITH INTERACTION TERMS 
		# "Age" HAS 6 CATEGORIES
		# display() function
		
		logit.final.model <- glm(pew.data$Liberal ~ pew.data$Female + pew.data$Age + pew.data$marital + pew.data$Female:pew.data$Age + pew.data$Female:pew.data$marital + pew.data$Age:pew.data$marital, family=binomial(link="logit"))
		display(logit.final.model)
		


#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


### PROBLEM 4 - WORKING WITH SURVEY DATA IN R

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
		library ("ggplot2")

### DATA:
		# Data is from Pew Research Center polls taken during the 2008 election campaign
		# SOURCE: http://www.stat.columbia.edu/~gelman/surveys.course/pew_research_center_june_elect_wknd_data.dta
		# read the data into R
		pew.data <- read.dta("pew_research_center_june_elect_wknd_data.dta")
		# how many rows (cases) are in the dataset
		nrow(pew.data)
		# names of the columns (variable names)
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
		# NOTE: for the graphs will use only the first one: Liberal
		
		
### State
		# table the original variable
		table(pew.data$state)
		# STEP 1: exclude Washington DC
		pew.data <- subset(pew.data, pew.data$state!="washington dc")
		table(pew.data$state)
		# STEP 2: exclude Alaska
		pew.data <- subset(pew.data, pew.data$state!="alaska")
		table(pew.data$state)
		# STEP 3: exclude Hawaii
		pew.data <- subset(pew.data, pew.data$state!="hawaii")
		table(pew.data$state)

		
### SUBSETTING THE DATA - removing observations with NA values
		# subset() function
		no.na <- subset(pew.data, pew.data$Liberal==1 | pew.data$Liberal==0)
		# how many rows (cases) are in the subset of data
		nrow(no.na)
		# names of the columns in the subset of data (variable names)
		names(no.na)	
		
		
### COMPUTE THE MEAN OF LIBERAL BY STATE
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
		table(state.n)
		# STEP 3: exclude Hawaii
		state.n <- subset(state.n, state.n!="hawaii")
		table(state.n)
		
		# "state.abb" -  list of state abbreviations in R
		table(state.abb)
		# STEP 1: make new vector
		state.ab <- as.vector(state.abb)
		table(state.ab)
		#STEP 2: exclude Alaska
		state.ab <- subset(state.ab, state.ab!="AK")
		table(state.ab)
		# STEP 3: exclude Hawaii
		state.ab <- subset(state.ab, state.ab!="HI")
		table(state.ab)
		
		# cbind() function - combine into one dataset the state.n and state.ab variables
		abbrev <- as.data.frame(cbind(state.ab, state.n))
		# how many rows (cases) are in the subset of data
		nrow(abbrev)
		# names of the columns in the subset of data (variable names)
		names(abbrev)
	

### DATA:
		# Data is from the 2008 election results
		# read the data into R
		election.result <- read.csv("2008ElectionResult.csv")
		# how many rows (cases) are in the dataset
		nrow(election.result)
		# names of the columns (variable names)
		names(election.result)
		# table "state"
		table(election.result$state)
		# convert upper-case characters in a character vector to lower-case
		election.result$state <- tolower(election.result$state)
		table(election.result$state)
		# exclude District of Columbia
		election.result <- subset(election.result, election.result!="district of columbia")
		table(election.result$state)
		# exclude Alaska
		election.result <- subset(election.result, election.result!="alaska")
		table(election.result$state)
		# exclude Hawaii
		election.result <- subset(election.result, election.result!="hawaii")
		table(election.result$state)
		# subsetting the data
		election.result <- subset(election.result, select= c("state", "vote_Obama_pct"))
		names(election.result)
		
		
				
### MERGE ELECTION RESULTS WITH STATE ABBREVIATION
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

	# STEP 3:	
		# merge() function - merge two data frames by common row names (state names)
		pew.data <- merge (pew.data, election.result, by.x="state", by.y="state")
		# how many rows (cases) are in the dataset
		nrow(pew.data)
		# names of the columns (variable names)
		names(pew.data)

		
### REMOVE STATES THAT DO NOT HAVE DATA
	pew.data <- subset(pew.data, pew.data$mean.ideo!=0)

	

### PLOTS FORMAT
	png ("hwk1_1.png", height=900, width=900)
	par (mfrow=c(2,2))



### PLOT ESTIMATED PROPORTION LIBERAL IN EACH STATE VERSUS OBAMA'S VOTE SHARE IN 2008
plot (x=pew.data$mean.ideo, 
		y=pew.data$vote_Obama_pct, 
		main="Relationship between Obama vote share and Liberal ID", 
		xlab="Percentage of Liberal ID", 
		ylab="Obama Vote Share", 
		pch=".")
text(x=pew.data$mean.ideo, y=pew.data$vote_Obama_pct, labels=pew.data$state.ab)




### PLOT ESTIMATED PROPORTION LIBERAL IN EACH STATE VERSUS SAMPLE SIZE IN EACH STATE
plot (x=pew.data$mean.ideo, 
		y=pew.data$state, 
		main="Relationship between sample size in each state and Liberal ID", 
		xlab="Percentage of Liberal ID", 
		ylab="Sample size in each state", 
		pch=".")
text(x=pew.data$mean.ideo, y=pew.data$state, labels=pew.data$state.ab)




		
### MAP ESTIMATED PROPORTION LIBERAL USING COLORS IN A US MAP
		
			
		# CHOOSE COLORS
		# colors = c("darkred", "firebrick3", "mediumvioletred", "darkorchid3", "dodgerblue2", "dodgerblue4")
		# colors = cm.colors(6)
		colors = c( "lightcyan", "deepskyblue", "dodgerblue1", "royalblue", "mediumblue", "navyblue")
		# GET QUARTILES
		summary(pew.data$mean.ideo)
		pew.data$colorBuckets <- as.numeric(cut(pew.data$mean.ideo, c(0.1, 0.15, 0.17, 0.19, 0.21, 0.25, 1.00)))
		leg.txt <- c("<10%", "10-15%", "15-17%", "17-19%", "19-25%", ">25%")
		# match the colors to the values in the map using the state abbreviation
		data(state.fips)
		names(state.fips)
		colorsmatched <- pew.data$colorBuckets[match(state.fips$abb, pew.data$state.ab)]
		# plot the map
		map(database="state", fill=TRUE, col=colors[colorsmatched], res=0)
		title("Percent that identify as Liberal by state")
		legend("bottomright", leg.txt, horiz=TRUE, fill=colors)
		
		
### THE END
dev.off()
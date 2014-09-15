#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 7 
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 1 - SURVEY INTERVIEWING:  

# Design a survey form and try it out on five friends.

### EMPTY ENVIRONMENT:
ls <- ls()
remove(ls)

### LOAD PACKAGES:
library ("diff")
library ("foreign")
library ("arm")
library ("calibrate")
library ("survey")
library ("xlsx")

### DATA:
# Survey W4365
# read the data into R
SurveyW4365 <- read.xlsx("MySurvey.xlsx", sheetName = "Sheet1", as.data.frame = TRUE, header = TRUE)
# how many rows (cases) are in the dataset
nrow(SurveyW4365)
# names of the columns (variable names)
names(SurveyW4365)

### VARIABLE "RespondentID":
# table the original variable
table(SurveyW4365$RespondentID)
mode(SurveyW4365$RespondentID)
class(SurveyW4365$RespondentID)

### VARIABLE "Married":
# table the original variable
table(SurveyW4365$Married)
mode(SurveyW4365$Married)
class(SurveyW4365$Married)

### VARIABLE "HappyMarried":
# table the original variable
table(SurveyW4365$HappyMarried)
mode(SurveyW4365$HappyMarried)
class(SurveyW4365$HappyMarried)

### VARIABLE "Health":
# table the original variable
table(SurveyW4365$Health)
mode(SurveyW4365$Health)
class(SurveyW4365$Health)

### VARIABLE "Happy":
# table the original variable
table(SurveyW4365$Happy)
mode(SurveyW4365$Happy)
class(SurveyW4365$Happy)

### VARIABLE "SatisfiedWork":
# table the original variable
table(SurveyW4365$SatisfiedWork)
mode(SurveyW4365$SatisfiedWork)
class(SurveyW4365$SatisfiedWork)

### VARIABLE "SatisfiedFinance":
# table the original variable
table(SurveyW4365$SatisfiedFinance)
mode(SurveyW4365$SatisfiedFinance)
class(SurveyW4365$SatisfiedFinance)

### VARIABLE "Religious":
# table the original variable
table(SurveyW4365$Religious)
mode(SurveyW4365$Religious)
class(SurveyW4365$Religious)

### VARIABLE "YearBorn":
# table the original variable
table(SurveyW4365$YearBorn)
mode(SurveyW4365$YearBorn)
class(SurveyW4365$YearBorn)
# make Age variable
SurveyW4365$Age <- 2013 - SurveyW4365$YearBorn
table(SurveyW4365$Age)
mode(SurveyW4365$Age)
class(SurveyW4365$Age)

### VARIABLE "Female":
# table the original variable
table(SurveyW4365$Female)
mode(SurveyW4365$Female)
class(SurveyW4365$Female)

### VARIABLE "Race":
# table the original variable
table(SurveyW4365$Race)
mode(SurveyW4365$Race)
class(SurveyW4365$Race)
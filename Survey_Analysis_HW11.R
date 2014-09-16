#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 11 
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


### PROBLEM 2 - IDEAL-POINT MODELING:  

# You will create a measure of economic ideology using the following questions from the 2000 Annenberg survey:  
            # Are tax rates a problem
            # Favor cutting taxes or strengthening social security
            # Federal government should reduce the top tax rate
            # Federal government should adopt flat tax
            # Federal government should spend more on social security
            # Favor investing social security in stock market
            # Is poverty a problem
            # Federal government should reduce income differences
            # Federal government should spend more on aid to mothers with young children
            # Federal government should expend effort to eliminate many business regulations

# Fit a hierarchical logistic regression to estimate ideal points for individuals and survey questions.


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
library ("lme4")
library ("car")
library ("gplots")
library ("sm")
library ("lattice")

### DATA:
# Data is from the Annenberg 2000 
# read the data into R
annenberg.2000 <- read.dta(file = "combined_annenberg2000_nat.dta")
# how many rows (cases) are in the dataset
nrow(annenberg.2000)
# names of the columns (variable names)
names(annenberg.2000)


### Create a measure of economic ideology using the following questions from the 2000 Annenberg survey
### Recode so that 0 is liberal and 1 is conservative position.


###################################################################################################
### Are tax rates a problem (CBB01)
###################################################################################################
# Question Wording: "The amount of money Americans pay in taxes—is this an extremely serious problem, serious, not too serious or not a problem at all?"
# table the original variable
table(annenberg.2000$CBB01)
mode(annenberg.2000$CBB01)
class(annenberg.2000$CBB01)
# recode "CBB01" variable
annenberg.2000$TaxRatesProblem <- NA
annenberg.2000$TaxRatesProblem[annenberg.2000$CBB01 == 'Extremely serious'] <- 1
annenberg.2000$TaxRatesProblem[annenberg.2000$CBB01 == 'Serious'] <- 1
annenberg.2000$TaxRatesProblem[annenberg.2000$CBB01 == 'Not too serious'] <- 0
annenberg.2000$TaxRatesProblem[annenberg.2000$CBB01 == 'Not a problem'] <- 0
# table new created variable
table(annenberg.2000$TaxRatesProblem)
mode(annenberg.2000$TaxRatesProblem)
class(annenberg.2000$TaxRatesProblem)


###################################################################################################
### Favor cutting taxes or strengthening social security (CBB05)
###################################################################################################
# Question Wording: "Which do you personally think is more important, cutting taxes or strengthening the Social Security system?"
# table the original variable
table(annenberg.2000$CBB05)
mode(annenberg.2000$CBB05)
class(annenberg.2000$CBB05)
# recode "CBB05" variable
annenberg.2000$CuttingTaxes <- NA
annenberg.2000$CuttingTaxes[annenberg.2000$CBB05 == 'Cutting taxes'] <- 1
annenberg.2000$CuttingTaxes[annenberg.2000$CBB05 == 'Strengthening Social Security'] <- 0
# table new created variable
table(annenberg.2000$CuttingTaxes)
mode(annenberg.2000$CuttingTaxes)
class(annenberg.2000$CuttingTaxes)


###################################################################################################
### Federal government should reduce the top tax rate (CBB10)
###################################################################################################
# Question Wording: "Reduce the taxes paid by higher-income Americans—should the federal government do this or not?"
# table the original variable
table(annenberg.2000$CBB10)
mode(annenberg.2000$CBB10)
class(annenberg.2000$CBB10)
# recode "CBB10" variable
annenberg.2000$ReduceTaxHighIncome <- NA
annenberg.2000$ReduceTaxHighIncome[annenberg.2000$CBB10 == 'Yes'] <- 1
annenberg.2000$ReduceTaxHighIncome[annenberg.2000$CBB10 == 'No'] <- 0
# table new created variable
table(annenberg.2000$ReduceTaxHighIncome)
mode(annenberg.2000$ReduceTaxHighIncome)
class(annenberg.2000$ReduceTaxHighIncome)


###################################################################################################
### Federal government should adopt flat tax (CBB13)
###################################################################################################
# Question Wording: "Federal government should adopt flat tax"
# table the original variable
table(annenberg.2000$CBB13)
mode(annenberg.2000$CBB13)
class(annenberg.2000$CBB13)
# recode "CBB13" variable
annenberg.2000$AdoptFlatTax <- NA
annenberg.2000$AdoptFlatTax[annenberg.2000$CBB13 == 'Yes'] <- 1
annenberg.2000$AdoptFlatTax[annenberg.2000$CBB13 == 'No'] <- 0
# table new created variable
table(annenberg.2000$AdoptFlatTax)
mode(annenberg.2000$AdoptFlatTax)
class(annenberg.2000$AdoptFlatTax)


###################################################################################################
### Federal government should spend more on social security (CBC01)
###################################################################################################
# Question Wording: "Social Security benefits—should the federal government spend more money on this, the same as now, less or no money at all?"
# table the original variable
table(annenberg.2000$CBC01)
mode(annenberg.2000$CBC01)
class(annenberg.2000$CBC01)
# recode "CBC01" variable
annenberg.2000$SpendLessSocialSecurity <- NA
annenberg.2000$SpendLessSocialSecurity[annenberg.2000$CBC01 == 'More'] <- 0
annenberg.2000$SpendLessSocialSecurity[annenberg.2000$CBC01 == 'Less'] <- 1
annenberg.2000$SpendLessSocialSecurity[annenberg.2000$CBC01 == 'Same'] <- 0
annenberg.2000$SpendLessSocialSecurity[annenberg.2000$CBC01 == 'None'] <- 1
# table new created variable
table(annenberg.2000$SpendLessSocialSecurity)
mode(annenberg.2000$SpendLessSocialSecurity)
class(annenberg.2000$SpendLessSocialSecurity)


###################################################################################################
### Favor investing social security in stock market (CBC05)
###################################################################################################
# Question Wording: "Do you personally favor or oppose allowing workers to invest some of their Social Security contributions in the stock market?"
# table the original variable
table(annenberg.2000$CBC05)
mode(annenberg.2000$CBC05)
class(annenberg.2000$CBC05)
# recode "CBC05" variable
annenberg.2000$FavorInvestingSocialSecurityInStockMarket <- NA
annenberg.2000$FavorInvestingSocialSecurityInStockMarket[annenberg.2000$CBC05 == 'Favor'] <- 1
annenberg.2000$FavorInvestingSocialSecurityInStockMarket[annenberg.2000$CBC05 == 'Oppose'] <- 0
# table new created variable
table(annenberg.2000$FavorInvestingSocialSecurityInStockMarket)
mode(annenberg.2000$FavorInvestingSocialSecurityInStockMarket)
class(annenberg.2000$FavorInvestingSocialSecurityInStockMarket)


###################################################################################################
### Is poverty a problem (CBP01)
###################################################################################################
# Question Wording: "The amount of poverty in the United States—is this an extremely serious problem, serious, not too serious or not a problem at all?"
# table the original variable
table(annenberg.2000$CBP01)
mode(annenberg.2000$CBP01)
class(annenberg.2000$CBP01)
# recode "CBP01" variable
annenberg.2000$PovertyNoProblem <- NA
annenberg.2000$PovertyNoProblem[annenberg.2000$CBP01 == 'Extremely serious'] <- 0
annenberg.2000$PovertyNoProblem[annenberg.2000$CBP01 == 'Serious'] <- 0
annenberg.2000$PovertyNoProblem[annenberg.2000$CBP01 == 'Not too serious'] <- 1
annenberg.2000$PovertyNoProblem[annenberg.2000$CBP01 == 'Not a problem'] <- 1
# table new created variable
table(annenberg.2000$PovertyNoProblem)
mode(annenberg.2000$PovertyNoProblem)
class(annenberg.2000$PovertyNoProblem)


###################################################################################################
### Federal government should reduce income differences (CBP02)
###################################################################################################
# Question Wording: "Try to reduce the income differences between rich and poor Americans—should the federal government do this or not?"
# table the original variable
table(annenberg.2000$CBP02)
mode(annenberg.2000$CBP02)
class(annenberg.2000$CBP02)
# recode "CBP02" variable
annenberg.2000$NoReduceIncomeDifferences <- NA
annenberg.2000$NoReduceIncomeDifferences[annenberg.2000$CBP02 == 'No'] <- 1
annenberg.2000$NoReduceIncomeDifferences[annenberg.2000$CBP02 == 'Yes'] <- 0
# table new created variable
table(annenberg.2000$NoReduceIncomeDifferences)
mode(annenberg.2000$NoReduceIncomeDifferences)
class(annenberg.2000$NoReduceIncomeDifferences)


###################################################################################################
### Federal government should spend more on aid to mothers with young children (CBP03)
###################################################################################################
# Question Wording: "Providing assistance to poor mothers with young children—should the federal government spend more money on this, the same as now, less or no money at all?"
# table the original variable
table(annenberg.2000$CBP03)
mode(annenberg.2000$CBP03)
class(annenberg.2000$CBP03)
# recode "CBP03" variable
annenberg.2000$MothersNoAid <- NA
annenberg.2000$MothersNoAid[annenberg.2000$CBP03 == 'More'] <- 0
annenberg.2000$MothersNoAid[annenberg.2000$CBP03 == 'Same'] <- 0
annenberg.2000$MothersNoAid[annenberg.2000$CBP03 == 'Less'] <- 1
annenberg.2000$MothersNoAid[annenberg.2000$CBP03 == 'None'] <- 1
# table new created variable
table(annenberg.2000$MothersNoAid)
mode(annenberg.2000$MothersNoAid)
class(annenberg.2000$MothersNoAid)


###################################################################################################
### Federal government should expend effort to eliminate many business regulations (CBT01)
###################################################################################################
# Question Wording: "Federal government should expend effort to eliminate many business regulations"
# table the original variable
table(annenberg.2000$CBT01)
mode(annenberg.2000$CBT01)
class(annenberg.2000$CBT01)
# recode "CBT01" variable
annenberg.2000$EliminateBusinessRegulation <- NA
annenberg.2000$EliminateBusinessRegulation[annenberg.2000$CBT01 == 'More'] <- 1
annenberg.2000$EliminateBusinessRegulation[annenberg.2000$CBT01 == 'Same'] <- 1
annenberg.2000$EliminateBusinessRegulation[annenberg.2000$CBT01 == 'Less'] <- 0
annenberg.2000$EliminateBusinessRegulation[annenberg.2000$CBT01 == 'None'] <- 0
# table new created variable
table(annenberg.2000$EliminateBusinessRegulation)
mode(annenberg.2000$EliminateBusinessRegulation)
class(annenberg.2000$EliminateBusinessRegulation)


###################################################################################################
### RECODE VARIABLE "ckey":
###################################################################################################
# table the original variable
table(annenberg.2000$ckey)
mode(annenberg.2000$ckey)
class(annenberg.2000$ckey)
# rename "ckey" variable
names(annenberg.2000)[2]<-"RespondentID"
# table new created variable
table(annenberg.2000$RespondentID)
mode(annenberg.2000$RespondentID)
class(annenberg.2000$RespondentID)


###################################################################################################
### RECODE VARIABLE "CV01":
###################################################################################################
# table the original variable
table(annenberg.2000$CV01)
mode(annenberg.2000$CV01)
class(annenberg.2000$CV01)
# recode "CV01" variable
annenberg.2000$PartyID <- NA
annenberg.2000$PartyID[annenberg.2000$CV01 == 'Republican'] <- "Republican"
annenberg.2000$PartyID[annenberg.2000$CV01 == 'Democrat'] <- "Democrat"
annenberg.2000$PartyID[annenberg.2000$CV01 == 'Independent'] <- "Independent"
# table new created variable
table(annenberg.2000$PartyID)
mode(annenberg.2000$PartyID)
class(annenberg.2000$PartyID)


###################################################################################################
### Bind ID variable and variable name
###################################################################################################

TaxRatesProblem <- data.frame(annenberg.2000$RespondentID, 'CBB01', annenberg.2000$TaxRatesProblem)
names(TaxRatesProblem)
names(TaxRatesProblem)[1] <- "RespondentID"
names(TaxRatesProblem)[2] <- "QuestionID"
names(TaxRatesProblem)[3] <- "Answer"
names(TaxRatesProblem)

CuttingTaxes <- data.frame(annenberg.2000$RespondentID, 'CBB05', annenberg.2000$CuttingTaxes)
names(CuttingTaxes)
names(CuttingTaxes)[1] <- "RespondentID"
names(CuttingTaxes)[2] <- "QuestionID"
names(CuttingTaxes)[3] <- "Answer"
names(CuttingTaxes)

ReduceTaxHighIncome <- data.frame(annenberg.2000$RespondentID, 'CBB10', annenberg.2000$ReduceTaxHighIncome)
names(ReduceTaxHighIncome)
names(ReduceTaxHighIncome)[1] <- "RespondentID"
names(ReduceTaxHighIncome)[2] <- "QuestionID"
names(ReduceTaxHighIncome)[3] <- "Answer"
names(ReduceTaxHighIncome)

AdoptFlatTax <- data.frame(annenberg.2000$RespondentID, 'CBB13', annenberg.2000$AdoptFlatTax)
names(AdoptFlatTax)
names(AdoptFlatTax)[1] <- "RespondentID"
names(AdoptFlatTax)[2] <- "QuestionID"
names(AdoptFlatTax)[3] <- "Answer"
names(AdoptFlatTax)

SpendLessSocialSecurity <- data.frame(annenberg.2000$RespondentID, 'CBC01', annenberg.2000$SpendLessSocialSecurity)
names(SpendLessSocialSecurity)
names(SpendLessSocialSecurity)[1] <- "RespondentID"
names(SpendLessSocialSecurity)[2] <- "QuestionID"
names(SpendLessSocialSecurity)[3] <- "Answer"
names(SpendLessSocialSecurity)

FavorInvestingSocialSecurityInStockMarket <- data.frame(annenberg.2000$RespondentID, 'CBC05', annenberg.2000$FavorInvestingSocialSecurityInStockMarket)
names(FavorInvestingSocialSecurityInStockMarket)
names(FavorInvestingSocialSecurityInStockMarket)[1] <- "RespondentID"
names(FavorInvestingSocialSecurityInStockMarket)[2] <- "QuestionID"
names(FavorInvestingSocialSecurityInStockMarket)[3] <- "Answer"
names(FavorInvestingSocialSecurityInStockMarket)

PovertyNoProblem <- data.frame(annenberg.2000$RespondentID, 'CBP01', annenberg.2000$PovertyNoProblem)
names(PovertyNoProblem)
names(PovertyNoProblem)[1] <- "RespondentID"
names(PovertyNoProblem)[2] <- "QuestionID"
names(PovertyNoProblem)[3] <- "Answer"
names(PovertyNoProblem)

NoReduceIncomeDifferences <- data.frame(annenberg.2000$RespondentID, 'CBP02', annenberg.2000$NoReduceIncomeDifferences)
names(NoReduceIncomeDifferences)
names(NoReduceIncomeDifferences)[1] <- "RespondentID"
names(NoReduceIncomeDifferences)[2] <- "QuestionID"
names(NoReduceIncomeDifferences)[3] <- "Answer"
names(NoReduceIncomeDifferences)

MothersNoAid <- data.frame(annenberg.2000$RespondentID, 'CBP03', annenberg.2000$MothersNoAid)
names(MothersNoAid)
names(MothersNoAid)[1] <- "RespondentID"
names(MothersNoAid)[2] <- "QuestionID"
names(MothersNoAid)[3] <- "Answer"
names(MothersNoAid)

EliminateBusinessRegulation <- data.frame(annenberg.2000$RespondentID, 'CBT01', annenberg.2000$EliminateBusinessRegulation)
names(EliminateBusinessRegulation)
names(EliminateBusinessRegulation)[1] <- "RespondentID"
names(EliminateBusinessRegulation)[2] <- "QuestionID"
names(EliminateBusinessRegulation)[3] <- "Answer"
names(EliminateBusinessRegulation)


###################################################################################################
### Create new dataset by row binding the above data frames
###################################################################################################

# Create new dataset
my.annenberg.2000 <- rbind(TaxRatesProblem, 
                           CuttingTaxes, 
                           ReduceTaxHighIncome, 
                           AdoptFlatTax, 
                           SpendLessSocialSecurity, 
                           FavorInvestingSocialSecurityInStockMarket, 
                           PovertyNoProblem, 
                           NoReduceIncomeDifferences, 
                           MothersNoAid, 
                           EliminateBusinessRegulation)
# how many rows (cases) are in the dataset
nrow(my.annenberg.2000) # 583,730 observations
# names of the columns (variable names)
names(my.annenberg.2000) # "RespondentID" "QuestionID"   "Answer"


### Respondent ID
      # table variable
      table(my.annenberg.2000$RespondentID)
      mode(my.annenberg.2000$RespondentID)
      class(my.annenberg.2000$RespondentID)
      # transform into factor
      my.annenberg.2000[,1] <- factor(my.annenberg.2000[,1])
      # table transformed variable
      table(my.annenberg.2000$RespondentID)
      mode(my.annenberg.2000$RespondentID)
      class(my.annenberg.2000$RespondentID)


### QuestionID
      # table variable
      table(my.annenberg.2000$QuestionID)
      mode(my.annenberg.2000$QuestionID)
      class(my.annenberg.2000$QuestionID)
      # transform into factor
      my.annenberg.2000[,2] <- factor(my.annenberg.2000[,2])
      # table transformed variable
      table(my.annenberg.2000$QuestionID)
      mode(my.annenberg.2000$QuestionID)
      class(my.annenberg.2000$QuestionID)


### Answer
      # table variable
      table(my.annenberg.2000$Answer)
      mode(my.annenberg.2000$Answer)
      class(my.annenberg.2000$Answer)
      # transform into factor
      my.annenberg.2000[,3] <- factor(my.annenberg.2000[,3])
      # table transformed variable
      table(my.annenberg.2000$Answer)
      mode(my.annenberg.2000$Answer)
      class(my.annenberg.2000$Answer)


### Subset Dataset
my.annenberg.2000 <- subset(my.annenberg.2000, !is.na(my.annenberg.2000$Answer))
# how many rows (cases) are in the dataset
nrow(my.annenberg.2000) # 358,227 observations
# names of the columns (variable names)
names(my.annenberg.2000) # "RespondentID" "QuestionID"   "Answer"


###################################################################################################
### Hierarchical logistic regression
###################################################################################################

HierarchicalLogit <- glmer(formula = Answer ~ (1|QuestionID) + (1|RespondentID), data = my.annenberg.2000, verbose = TRUE, family = binomial(logit))
summary(HierarchicalLogit)
display(HierarchicalLogit)

RandomEffects <- ranef(HierarchicalLogit)
names(RandomEffects)
table(RandomEffects$RespondentID)
table(RandomEffects$QuestionID)

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

# (a) Display the estimated ideal points and standard errors of the survey questions 
# (listing the questions in order of their estimated ideal points).
      # Are tax rates a problem (CBB01)
      # Favor cutting taxes or strengthening social security (CBB05)
      # Federal government should reduce the top tax rate (CBB10)
      # Federal government should adopt flat tax (CBB13)
      # Federal government should spend more on social security (CBC01)
      # Favor investing social security in stock market (CBC05)
      # Is poverty a problem (CBP01)
      # Federal government should reduce income differences (CBP02)
      # Federal government should spend more on aid to mothers with young children (CBP03)
      # Federal government should expend effort to eliminate many business regulations (CBT01)

coef(HierarchicalLogit)
      ##########################
      # $QuestionID
      # (Intercept)
      # CBB01  0.679822523
      # CBB05 -0.872737207
      # CBB10 -1.153356344
      # CBB13  0.268816334
      # CBC01 -3.051488014
      # CBC05  0.529178861
      # CBP01 -1.467945986
      # CBP02  0.004186266
      # CBP03 -2.086502584
      # CBT01  0.948973293
      ##########################

se.coef(HierarchicalLogit)
      ##########################
      # $QuestionID
      # (Intercept)
      # CBB01 0.009672476
      # CBB05 0.010774930
      # CBB10 0.015808613
      # CBB13 0.014025977
      # CBC01 0.020319356
      # CBC05 0.013142394
      # CBP01 0.011380544
      # CBP02 0.013974926
      # CBP03 0.020659077
      # CBT01 0.015433953
      ##########################

fixef(HierarchicalLogit)
      ##########################
      # (Intercept) 
      # -0.6570906  
      ##########################

se.fixef(HierarchicalLogit)
      ##########################
      # (Intercept) 
      # 0.4188996 
      ##########################

### Create empty variables for the random effects and standard errors
RandomEffects <- rep (NA, length = 10)
RandomEffects.SE <- rep (NA, length = 10)
RandomEffects.Upper <- rep (NA, length = 10)
RandomEffects.Lower <- rep (NA, length = 10)

ranef(HierarchicalLogit)
      ##########################
      # $QuestionID
      # (Intercept)
      # CBB01   1.3369131
      # CBB05  -0.2156466
      # CBB10  -0.4962658
      # CBB13   0.9259069
      # CBC01  -2.3943974
      # CBC05   1.1862694
      # CBP01  -0.8108554
      # CBP02   0.6612768
      # CBP03  -1.4294120
      # CBT01   1.6060639
      ##########################

RandomEffects[9] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[1]
matrix(RandomEffects)
RandomEffects[5] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[2]
matrix(RandomEffects)
RandomEffects[4] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[3]
matrix(RandomEffects)
RandomEffects[7] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[4]
matrix(RandomEffects)
RandomEffects[1] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[5]
matrix(RandomEffects)
RandomEffects[8] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[6]
matrix(RandomEffects)
RandomEffects[3] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[7]
matrix(RandomEffects)
RandomEffects[6] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[8]
matrix(RandomEffects)
RandomEffects[2] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[9]
matrix(RandomEffects)
RandomEffects[10] <- as.matrix(ranef(HierarchicalLogit)$QuestionID)[10]
matrix(RandomEffects)

se.ranef(HierarchicalLogit)
      ##########################
      # $QuestionID
      #          [,1]
      # [1,] 0.009672476
      # [2,] 0.010774930
      # [3,] 0.015808613
      # [4,] 0.014025977
      # [5,] 0.020319356
      # [6,] 0.013142394
      # [7,] 0.011380544
      # [8,] 0.013974926
      # [9,] 0.020659077
      # [10,] 0.015433953
      ##########################

RandomEffects.SE[9] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[1]
matrix(RandomEffects.SE)
RandomEffects.SE[5] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[2]
matrix(RandomEffects.SE)
RandomEffects.SE[4] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[3]
matrix(RandomEffects.SE)
RandomEffects.SE[7] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[4]
matrix(RandomEffects.SE)
RandomEffects.SE[1] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[5]
matrix(RandomEffects.SE)
RandomEffects.SE[8] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[6]
matrix(RandomEffects.SE)
RandomEffects.SE[3] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[7]
matrix(RandomEffects.SE)
RandomEffects.SE[6] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[8]
matrix(RandomEffects.SE)
RandomEffects.SE[2] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[9]
matrix(RandomEffects.SE)
RandomEffects.SE[10] <- as.matrix(se.ranef(HierarchicalLogit)$QuestionID)[10]
matrix(RandomEffects.SE)


### Create Upper and Lower bounds for each random effect for QuestionID
RandomEffects.Lower = RandomEffects - ( 2 * RandomEffects.SE )
matrix(RandomEffects.Lower)
RandomEffects.Upper = RandomEffects + ( 2 * RandomEffects.SE )
matrix(RandomEffects.Upper)


### Create QuestionID variable listing the questions in order of their estimated ideal points
Questions <- c(1:10)
matrix(Questions)


### Create new dataset
Graph.Data <- data.frame(Questions, RandomEffects, RandomEffects.Lower, RandomEffects.Upper)
nrow(Graph.Data)
names(Graph.Data)


### Format plot
png("hwk11_QuestionID.png", height = 600, width = 1000)
par(mfrow = c(1,1), mai=c(bottom = 1, left = 1, top = 1, right = 7))

### Graph Ideal Points for QuestionID
plot (x = Graph.Data$RandomEffects, 
      y = Graph.Data$Questions, 
      main = "Estimated Ideal Points of the Survey Questions", 
      xlab = "Ideal Points", 
      ylab = "Survey Questions",
      ylim = c(1,10),
      lty = "blank",
      yaxt = "n")
# Make x axis tick marks without labels
axis(side = 2, at = c(1:10), labels = FALSE, tick = TRUE, outer = FALSE, padj = 1) # left axis
axis(side = 4, at = c(1:10), labels = FALSE, tick = TRUE, outer = FALSE, padj = 1) # right axis
# Add text to y-axis
mini.labels <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
mtext (text = mini.labels,
       side = 2, #left
       at = c(1:10), # location of each string in user coordinates
       line = 1, # margin line, starting at 0 counting outwards
       padj = 0, # right or top alignment
       col = c("blue", "blue", "blue", "blue", "blue", "red", "red", "red", "red", "red"),
       las = 1)
labels <- c("Federal government should spend more on social security", 
                 "Federal government should spend more on aid to mothers with young children", 
                 "Is poverty a problem", 
                 "Federal government should reduce the top tax rate", 
                 "Favor cutting taxes or strengthening social security", 
                 "Federal government should reduce income differences", 
                 "Federal government should adopt flat tax", 
                 "Favor investing social security in stock market", 
                 "Are tax rates a problem", 
                 "Federal government should expend effort to eliminate many business regulations")
mtext (text = labels,
       side = 4, #right
       at = c(1:10), # location of each string in user coordinates
       line = 1, # margin line, starting at 0 counting outwards
       padj = 0, # right or top alignment
       col = c("blue", "blue", "blue", "blue", "blue", "red", "red", "red", "red", "red"),
       las = 1)
# Add black line on x-axis at zero
abline (v=0)
# Add grey dotted lines for each horizontal point on y-axis
abline (h = c(1:10), col = "lightgray", lty = "dotted")
# Add color points to graph
color <- ifelse (Graph.Data$RandomEffects<0, "blue", "red")
points (x = Graph.Data$RandomEffects, 
        y = Graph.Data$Questions,
        pch = 1,
        col = color)
# Add 95% confidence intervals to each point on graph
plotCI(x = Graph.Data$RandomEffects, 
       y = Graph.Data$Questions,
       uiw = NULL,
       liw = NULL,
       ui = Graph.Data$RandomEffects.Upper,
       li = Graph.Data$RandomEffects.Lower,
       err = "x",
       col = color,
       barcol = color,
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)

### THE END
dev.off()

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

# (b) Display the distribution of estimated ideal points of the survey respondents.  
# On this same graph, display the distributions for Democrats, Independents, and Republicans.


### Create separate datasets for Democrats, Independents and Republicans
nrow(my.annenberg.2000) # 358,227
my.annenberg.2000.democrats <- subset(my.annenberg.2000, annenberg.2000$PartyID == 'Democrat')
nrow(my.annenberg.2000.democrats) # 111,720
my.annenberg.2000.independents <- subset(my.annenberg.2000, annenberg.2000$PartyID == 'Independent')
nrow(my.annenberg.2000.independents) # 101,995
my.annenberg.2000.republicans <- subset(my.annenberg.2000, annenberg.2000$PartyID == 'Republican')
nrow(my.annenberg.2000.republicans) # 99,884


### Hierarchical Logistic regressions for Democrats, Independents and Republicans
HierarchicalLogitDemocrats <- glmer(formula = Answer ~ (1|QuestionID) + (1|RespondentID), data = my.annenberg.2000.democrats, verbose = TRUE, family = binomial(logit))
summary(HierarchicalLogitDemocrats)
display(HierarchicalLogitDemocrats)

HierarchicalLogitIndependents <- glmer(formula = Answer ~ (1|QuestionID) + (1|RespondentID), data = my.annenberg.2000.independents, verbose = TRUE, family = binomial(logit))
summary(HierarchicalLogitIndependents)
display(HierarchicalLogitIndependents)

HierarchicalLogitRepublicans <- glmer(formula = Answer ~ (1|QuestionID) + (1|RespondentID), data = my.annenberg.2000.republicans, verbose = TRUE, family = binomial(logit))
summary(HierarchicalLogitRepublicans)
display(HierarchicalLogitRepublicans)


### Random effects
RandomEffectsDemocrats <- as.matrix(ranef(HierarchicalLogitDemocrats)$RespondentID)
matrix(RandomEffectsDemocrats)
RandomEffectsIndependents <- as.matrix(ranef(HierarchicalLogitIndependents)$RespondentID)
matrix(RandomEffectsIndependents)
RandomEffectsRepublicans <- as.matrix(ranef(HierarchicalLogitRepublicans)$RespondentID)
matrix(RandomEffectsRepublicans)


###############################################################################################################

### Format plot
png("hwk11_RespondentID_hist.png", height = 600, width = 800)
par(mfrow = c(1,1))

### Histogram
demx <- RandomEffectsDemocrats
demh <- hist(demx, breaks=100, col="blue", xlab="Random Effects for Respondent ID", 
        main="Histogram for Democrats, Independents and Republicans",
        sub = "hist() function") 
demxfit <- seq(min(demx), max(demx), length=40) 
demyfit <- dnorm(demxfit, mean=mean(demx), sd=sd(demx)) 
demyfit <- demyfit * diff(demh$mids[1:2]) * length(demx) 
lines (demxfit, demyfit, col="blue", lwd=5)

indx <- RandomEffectsIndependents
indh <- hist(indx, breaks=100, col="green", xlab="Random Effects for Respondent ID", 
             main="Histogram for Democrats, Independents and Republicans", add = TRUE) 
indxfit <- seq(min(indx), max(indx), length=40) 
indyfit <- dnorm(indxfit, mean=mean(indx), sd=sd(indx)) 
indyfit <- indyfit * diff(indh$mids[1:2]) * length(indx) 
lines (indxfit, indyfit, col="green", lwd=5)

repx <- RandomEffectsRepublicans
reph <- hist(repx, breaks=100, col="red", xlab="Random Effects for Respondent ID", 
             main="Histogram for Democrats, Independents and Republicans", add = TRUE) 
repxfit <- seq(min(repx), max(repx), length=40) 
repyfit <- dnorm(repxfit, mean=mean(repx), sd=sd(repx)) 
repyfit <- repyfit * diff(reph$mids[1:2]) * length(repx) 
lines (repxfit, repyfit, col="red", lwd=5)

legend.text = c("Democrats", "Independets", "Republicans")
legend ("topright", legend = legend.text, fill=c("blue", "green", "red"), horiz=FALSE, inset=0.06)

### THE END
dev.off()


###############################################################################################################

### Format plot
png("hwk11_RespondentID_kernel.png", height = 600, width = 800)
par(mfrow = c(1,1))

### Kernel Density Plot
sm.density(x = RandomEffectsDemocrats, xlab="Random Effects for Respondent ID", col = "blue", lty = 5)
sm.density(x = RandomEffectsIndependents, xlab="Random Effects for Respondent ID", col = "green", add = TRUE, lty = 5)
sm.density(x = RandomEffectsRepublicans, xlab="Random Effects for Respondent ID", col = "red", add = TRUE, lty = 5)
title (main="Kernel Density Plot for Democrats, Independents and Republicans",
       sub = "sm.density() function")
legend.text = c("Democrats", "Independets", "Republicans")
legend ("topright", legend = legend.text, fill=c("blue", "green", "red"), horiz=FALSE, inset=0.06)

### THE END
dev.off()


###############################################################################################################

### Format plot
png("hwk11_RespondentID_densityplot.png", height = 600, width = 1000)
par(mfrow = c(1,1))

### Histogram
densityplot( ~ RandomEffectsDemocrats + RandomEffectsIndependents + RandomEffectsRepublicans, 
             col = c("blue", "green", "red"), xlab="Random Effects for Respondent ID", 
             main = "Density Plot for Democrats, Independents and Republicans",
             sub = "densityplot() function", auto.key = list(space = "right")) 

### THE END
dev.off()

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

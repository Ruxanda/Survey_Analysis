#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 8
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 2: SURVEY MEASUREMENT

# Find a measurement effect in an existing survey.

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
library("car")
library("Deducer")

### DATA:
# Data is from the American National Election Studies 2008 Time Series
# SOURCE: http://http://www.electionstudies.org/studypages/download/datacenter_all.htm
# read the data into R
anes.2008 <- read.dta("anes_timeseries_2008.dta")
# how many rows (cases) are in the dataset
nrow(anes.2008)
# names of the columns (variable names)
names(anes.2008)


### Weights:
# V080101 - WT.1. PRE CROSS-SECTION SAMPLE WEIGHT - POST-STRAT, centered
# CROSS-SECTION SAMPLE WEIGHT - PRE-ELECTION WAVE, POST-STRATIFIED: centered at mean 1.0
# table the original variable
table(anes.2008$V080101)
# rename "wtcombnr" variable
names(anes.2008)[3]<-"Weights"
# table new created variable
table(anes.2008$Weights)
mode(anes.2008$Weights)
class(anes.2008$Weights)


### MEASUREMENT EFFECT DUE TO QUESTION WORDING - INDEPENDENT VARIABLE:
      # =============================================================================
      #   V082418 PreRandom.18. Order of parties in text of party performance
      # =============================================================================
      #   PRE-ELECTION RANDOMIZATION
      # -----------------------------------------------------------------
      #   Order of Democratic party / Republican party in question text of party performance questions (G1 and G2a/G2b)
      # -----------------------------------------------------------------
      #   VALID CODES:
            # 1. Democratic party is 1st (in question text) - G1 and G2a/G2b
            # 2. Republican party is 1st (in question text) - G1 and G2a/G2b
      # REFERENCE:
            # G1. (party performance on economy)
            # G2a. (VERSION P party performance on war)
            # G2b. (VERSION Q party performance on war)
            # PreRandom.19. (VERSION P/Q party performance on war)
      # NOTES:
            # This variable provides the order in which the names of the two major parties were included in the text of the party performance questions, G1 (party performance on economy) and G2a/G2b (party performance on war).
            # For party performance on war, Respondents were randomly assigned to standard question G2a (VERSION P) or else to revised version G2b (VERSION Q).
      # TYPE:
            # Numeric
      # table the original variable
      table(anes.2008$V082418)
      mode(anes.2008$V082418)
      class(anes.2008$V082418)
      # recode form variable
      anes.2008$RepublicanFirst <- NA
      anes.2008$RepublicanFirst[anes.2008$V082418 == "1. Democratic party is 1st (in question text) - G1 and G2a/G2b"] <- 0
      anes.2008$RepublicanFirst[anes.2008$V082418 == "2. Republican party is 1st (in question text) - G1 and G2a/G2b"] <- 1
      # table new variable
      table(anes.2008$RepublicanFirst)
      mode(anes.2008$RepublicanFirst)
      class(anes.2008$RepublicanFirst)


### DEPENDEDNT VARIABLE:
      # =============================================================================
      #   V083094a G2a. [VERSION P] Which party better: keeping out of war
      # =============================================================================
      #   PRE-ELECTION SURVEY IF R SELECTED FOR PARTY-PERFORMANCE ON WAR VERSION P:
      # QUESTION WORDING:
            # Looking ahead, do you think the problem of KEEPING OUT OF WAR would be handled better in the next four years by [the DEMOCRATS, the REPUBLICANS / the REPUBLICANS, the DEMOCRATS], or ABOUT THE SAME by both?
      # INTERVIEWER INSTRUCTION:
            # {IF 'DK' OR 'NEITHER PARTY' IS VOLUNTEERED, DO NOT PROBE}
      # VALID CODES:
            # 1. Democrats
            # 3. Republicans
            # 5. About the same by both
            # 7. Neither party {VOL}
      # MISSING CODES:
            # -1. INAP, R selected for VERSION Q
            # -8. Don't know
            # -9. Refused
      # REFERENCE:
            # PreRandom.18. (order of parties in text of party performance)
            # PreRandom.19. (VERSION P/Q party war performance)
            # Appendix B1. (Pre-election questions with split administration)
      # NOTES:
            # The order of the major parties in the question text of party performance items G1 and G2a/G2b was randomized, as documented in PreRandom.18.
            # Respondents were randomly assigned to the standard war question G2a (VERSION P) or to new war question G2b (VERSION Q), as documented in PreRandom.19.
            # The P/Q version assignment is specific to Pre-election party war-performance questions only (G2a or G2b) and is independent of the major "OLD/NEW" split documented in PreRandom.2.
      # TYPE:
            # Numeric
      # table the original variable
      table(anes.2008$V083094a)
      mode(anes.2008$V083094a)
      class(anes.2008$V083094a)
      # recode V083094a
      anes.2008$RepublicansHandleWar <- NA
      anes.2008$RepublicansHandleWar[anes.2008$V083094a == "1. Democrats"] <- 0
      anes.2008$RepublicansHandleWar[anes.2008$V083094a == "3. Republicans"] <- 1
      # table new variable
      table(anes.2008$RepublicansHandleWar)
      mode(anes.2008$RepublicansHandleWar)
      class(anes.2008$RepublicansHandleWar)

### Specify survey design
design <- svydesign(ids=~1, data = anes.2008, weights=anes.2008$Weights)

## Test whether design was associated with response
logit.model.form <- svyglm(RepublicansHandleWar ~ RepublicanFirst, design=design, family=quasibinomial(link=logit))
summary(logit.model.form)
display(logit.model.form)

## Compare means using svyttest
svyttest(RepublicansHandleWar ~ RepublicanFirst, design)

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
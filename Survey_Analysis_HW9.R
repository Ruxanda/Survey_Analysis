#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 9
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 1: SURVEY RESPONSES

# From 1984 through 2008 (and maybe in other years), the National Election Study asked attitudes on several issues, and also perceptions of the stances on these 
# issues held by the major presidential candidates.  (For example, in 2004 these issues included the role of women, gun-control policy, government aid to African 
# Americans, the level of spending that the government should undertake in the economy, the role of the government in providing an economic environment where 
# there is job security, and the level at which the government should spend on defense.  Each respondent was asked how he or she stood on these issues and where 
# they would place George W. Bush and John Kerry.)  It turns out that attitudes about the candidates’ views are strongly correlated with respondents’ own 
# ideologies; see Figure 5 of Gelman and Cai (2008).
      # (a) Replicate Figure 5 of Gelman and Cai (2008) using the 2000 NES.
      # (b) Discuss any difficulties you have, and compare your results to the 2004 results.

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
      library ("foreign")
      library ("car")
      library ("Deducer")
      library ("lattice")
      library ("sfsmisc")
      library ("gplots")

### DATA:
      # Data is from the American National Election Studies 2000 Time Series
      # SOURCE: http://http://www.electionstudies.org/studypages/download/datacenter_all.htm
      # read the data into R
      anes.2000 <- read.dta("anes2000TS.dta")
      # how many rows (cases) are in the dataset
      nrow(anes.2000)
      # names of the columns (variable names)
      names(anes.2000)

### Weights:
      # V000002 Process.5. Sample weight
      # This is a 6-digit variable with a coded decimal point and 4 actual decimal places.
      # A poststratification adjustment using the 2000 CPS March Supplement estimates as the standard was done for the combined RDD and area samples. 
      # The cells were formed by crossing 6 age groups by 4 levels of education.
      # The age groups were: 18-29, 30-39, 40-49, 50-59, 60-69, 70+. 
      # The education levels were:  < high school graduation, high school graduate, some college, and 4 years of college or more.
      # table the original variable
      table(anes.2000$V000002)
      # rename "wtcombnr" variable
      names(anes.2000)[5]<-"Weights"
      # table new created variable
      table(anes.2000$Weights)
      mode(anes.2000$Weights)
      class(anes.2000$Weights)

### Party ID
      # Generally speaking, do you think of yourself as a Republican, a Democrat, an Independent, or what?
      # table the original variable
      table(anes.2000$V000519)
      mode(anes.2000$V000519)
      class(anes.2000$V000519)
      # recode V000519
      anes.2000$PartyID <- NA
      anes.2000$PartyID[anes.2000$V000519 == "1. DEMOCRAT"] <- "Democrat"
      anes.2000$PartyID[anes.2000$V000519 == "2. REPUBLICAN"] <- "Republican"
      anes.2000$PartyID[anes.2000$V000519 == "3. INDEPENDENT"] <- "Independent"
      # table new variable
      table(anes.2000$PartyID)
      mode(anes.2000$PartyID)
      class(anes.2000$PartyID)

#################################################
### First Economic Issue - JOBS
#################################################

# Respondent:
      # Where would you place yourself on this scale, or haven't you thought much about this?
      # SUMMARY: 5PT GUARANTEED JOBS SELF-PLACEMENT FROM 7PT SCALE/BRANCH
      # Built from L4a and L4x1
      # 1. SCALE: 1 / BRANCHING: Strongly - govt see to jobs & std living
      # 2. SCALE: 2 / BRANCHING: Not strongly - govt see to jobs & std living
      # 3. SCALE: 3,4,5 / BRANCHING: Other/depends/neither
      # 4. SCALE: 6 / BRANCHING: Not strongly - govt leave people on own
      # 5. SCALE: 7 / BRANCHING: Strongly - govt leave people on own
      # table the original variable
      table(anes.2000$V000620)
      mode(anes.2000$V000620)
      class(anes.2000$V000620)
      # recode V000620
      anes.2000$JobsRespondent <- NA
      anes.2000$JobsRespondent[anes.2000$V000620 == "1. SCALE: 1 / BRANCHING: Strongly - govt"] <- -2
      anes.2000$JobsRespondent[anes.2000$V000620 == "2. SCALE: 2 / BRANCHING: Not strongly -"] <- -1
      anes.2000$JobsRespondent[anes.2000$V000620 == "3. SCALE: 3,4,5 / BRANCHING: Other/depen"] <- 0
      anes.2000$JobsRespondent[anes.2000$V000620 == "4. SCALE: 6 / BRANCHING: Not strongly -"] <- 1
      anes.2000$JobsRespondent[anes.2000$V000620 == "5. SCALE: 7 / BRANCHING: Strongly - govt"] <- 2
      # table new variable
      table(anes.2000$JobsRespondent)
      mode(anes.2000$JobsRespondent)
      class(anes.2000$JobsRespondent)


# Gore:
      # Where would you place yourself on this scale, or haven't you thought much about this?
      # SUMMARY: 5PT GUARANTEED JOBS GORE PLACEMENT FROM 7PT SCALE/BRANCH
      # Built from L4a and L4x1
      # 1. SCALE: 1 / BRANCHING: Strongly - govt see to jobs & std living
      # 2. SCALE: 2 / BRANCHING: Not strongly - govt see to jobs & std living
      # 3. SCALE: 3,4,5 / BRANCHING: Other/depends/neither
      # 4. SCALE: 6 / BRANCHING: Not strongly - govt leave people on own
      # 5. SCALE: 7 / BRANCHING: Strongly - govt leave people on own
      # table the original variable
      table(anes.2000$V000625)
      mode(anes.2000$V000625)
      class(anes.2000$V000625)
      # recode V000625
      anes.2000$JobsGore <- NA
      anes.2000$JobsGore[anes.2000$V000625 == "1. SCALE: 1 / BRANCHING: Strongly - govt"] <- -2
      anes.2000$JobsGore[anes.2000$V000625 == "2. SCALE: 2 / BRANCHING: Not strongly -"] <- -1
      anes.2000$JobsGore[anes.2000$V000625 == "3. SCALE: 3,4,5 / BRANCHING: Other/depen"] <- 0
      anes.2000$JobsGore[anes.2000$V000625 == "4. SCALE: 6 / BRANCHING: Not strongly -"] <- 1
      anes.2000$JobsGore[anes.2000$V000625 == "5. SCALE: 7 / BRANCHING: Strongly - govt"] <- 2
      # table new variable
      table(anes.2000$JobsGore)
      mode(anes.2000$JobsGore)
      class(anes.2000$JobsGore)


# Bush:
      # Where would you place George W. Bush (on this issue)?
      # SUMMARY: 5PT GUARANTEED JOBS BUSH PLACEMENT FROM 7PT SCALE/BRANCH
      # Built from L4a and L4cx1
      # 1. SCALE: 1 / BRANCHING: Strongly - govt see to jobs & std living
      # 2. SCALE: 2 / BRANCHING: Not strongly - govt see to jobs & std living
      # 3. SCALE: 3,4,5 / BRANCHING: Other/depends/neither
      # 4. SCALE: 6 / BRANCHING: Not strongly - govt leave people on own
      # 5. SCALE: 7 / BRANCHING: Strongly - govt leave people on own
      # table the original variable
      table(anes.2000$V000630)
      mode(anes.2000$V000630)
      class(anes.2000$V000630)
      # recode V000630
      anes.2000$JobsBush <- NA
      anes.2000$JobsBush[anes.2000$V000630 == "1. SCALE: 1 / BRANCHING: Strongly - govt"] <- -2
      anes.2000$JobsBush[anes.2000$V000630 == "2. SCALE: 2 / BRANCHING: Not strongly -"] <- -1
      anes.2000$JobsBush[anes.2000$V000630 == "3. SCALE: 3,4,5 / BRANCHING: Other/depen"] <- 0
      anes.2000$JobsBush[anes.2000$V000630 == "4. SCALE: 6 / BRANCHING: Not strongly -"] <- 1
      anes.2000$JobsBush[anes.2000$V000630 == "5. SCALE: 7 / BRANCHING: Strongly - govt"] <- 2
      # table new variable
      table(anes.2000$JobsBush)
      mode(anes.2000$JobsBush)
      class(anes.2000$JobsBush)



#################################################
### Second Economic Issue - SERVICES AND SPENDING
#################################################


# Respondent:
      # Where would you place yourself on this scale, or haven't you thought much about this?
      # COMBINED 5PT SERV/SPEND SELF-PLACEMENT FROM 7PT SCALE /BRANCH
      # Built from L1a and L1ax1
      # 1. SCALE: 1; BRANCHING: Reduce spending and services a great deal
      # 2. SCALE 2; BRANCHING: Reduce spending and services only some
      # 3. SCALE: 3,4,5; BRANCHING: Stay same as now (3 in L1a.T)
      # 4. SCALE: 6; BRANCHING: Increase spending and services only some
      # 5. SCALE: 7; BRANCHING: Increase spending and services a great deal
      # table the original variable
      table(anes.2000$V000550)
      mode(anes.2000$V000550)
      class(anes.2000$V000550)
      # recode V000550
      anes.2000$SpendingRespondent <- NA
      anes.2000$SpendingRespondent[anes.2000$V000550 == "1. SCALE: 1. BRANCHING: Reduce spending"] <- 2
      anes.2000$SpendingRespondent[anes.2000$V000550 == "2. SCALE 2. BRANCHING: Reduce spending a"] <- 1
      anes.2000$SpendingRespondent[anes.2000$V000550 == "3. SCALE: 3,4,5. BRANCHING: Stay same as"] <- 0
      anes.2000$SpendingRespondent[anes.2000$V000550 == "4. SCALE: 6. BRANCHING: Increase spendin"] <- -1
      anes.2000$SpendingRespondent[anes.2000$V000550 == "5. SCALE: 7. BRANCHING: Increase spendin"] <- -2
      # table new variable
      table(anes.2000$SpendingRespondent)
      mode(anes.2000$SpendingRespondent)
      class(anes.2000$SpendingRespondent)


# Gore:
      # Where would you place Al Gore (on this issue)?
      # SUMMARY: 5PT SERV/SPEND GORE PLACEMENT FROM 7PT SCALE/BRANCH
      # Built from L1c and L1cx1
      # 1. SCALE: 1; BRANCHING: Reduce spending and services a great deal
      # 2. SCALE 2; BRANCHING: Reduce spending and services only some
      # 3. SCALE: 3,4,5; BRANCHING: Stay same as now (3 in L1a.T)
      # 4. SCALE: 6; BRANCHING: Increase spending and services only some
      # 5. SCALE: 7; BRANCHING: Increase spending and services a great deal
      # table the original variable
      table(anes.2000$V000562)
      mode(anes.2000$V000562)
      class(anes.2000$V000562)
      # recode V000562
      anes.2000$SpendingGore <- NA
      anes.2000$SpendingGore[anes.2000$V000562 == "1. SCALE: 1. BRANCHING: Reduce spending"] <- 2
      anes.2000$SpendingGore[anes.2000$V000562 == "2. SCALE 2. BRANCHING: Reduce spending a"] <- 1
      anes.2000$SpendingGore[anes.2000$V000562 == "3. SCALE: 3,4,5. BRANCHING: Stay same as"] <- 0
      anes.2000$SpendingGore[anes.2000$V000562 == "4. SCALE: 6. BRANCHING: Increase spendin"] <- -1
      anes.2000$SpendingGore[anes.2000$V000562 == "5. SCALE: 7. BRANCHING: Increase spendin"] <- -2
      # table new variable
      table(anes.2000$SpendingGore)
      mode(anes.2000$SpendingGore)
      class(anes.2000$SpendingGore)


# Bush:
      # Where would you place George W. Bush (on this issue)?
      # SUMMARY: 5PT SERV/SPEND BUSH PLACEMENT FROM 7PT SCALE /BRANCH
      # Built from L1d and L1dx1
      # 1. SCALE: 1; BRANCHING: Reduce spending and services a great deal
      # 2. SCALE 2; BRANCHING: Reduce spending and services only some
      # 3. SCALE: 3,4,5; BRANCHING: Stay same as now (3 in L1a.T)
      # 4. SCALE: 6; BRANCHING: Increase spending and services only some
      # 5. SCALE: 7; BRANCHING: Increase spending and services a great deal
      # table the original variable
      table(anes.2000$V000568)
      mode(anes.2000$V000568)
      class(anes.2000$V000568)
      # recode V000568
      anes.2000$SpendingBush <- NA
      anes.2000$SpendingBush[anes.2000$V000568 == "1. SCALE: 1. BRANCHING: Reduce spending"] <- 2
      anes.2000$SpendingBush[anes.2000$V000568 == "2. SCALE 2. BRANCHING: Reduce spending a"] <- 1
      anes.2000$SpendingBush[anes.2000$V000568 == "3. SCALE: 3,4,5. BRANCHING: Stay same as"] <- 0
      anes.2000$SpendingBush[anes.2000$V000568 == "4. SCALE: 6. BRANCHING: Increase spendin"] <- -1
      anes.2000$SpendingBush[anes.2000$V000568 == "5. SCALE: 7. BRANCHING: Increase spendin"] <- -2
      # table new variable
      table(anes.2000$SpendingBush)
      mode(anes.2000$SpendingBush)
      class(anes.2000$SpendingBush)



#################################################
### Third Economic Issue - DEFENSE
#################################################


# Respondent:
      # Where would you place yourself on this scale, or haven't you thought much about this?
      # SUMMARY: 5PT DEFENSE SPENDING SELF-PLACEMENT FROM 7PT SCALE/BRANCH
      # Built from L2a and from L2a.T/L2a1.T/L2a1a.T/L2a1b.T.
      # 1. SCALE: 1; BRANCHING: Decrease defense spending a lot
      # 2. SCALE: 2; BRANCHING: Decrease defense spending a little
      # 3. SCALE: 3,4,5; BRANCHING: About the right amount (5 in L2a1.T)
      # 4. SCALE: 6; BRANCHING: Increase defense spending a little
      # 5. SCALE: 7; BRANCHING: Increase defense spending a lot
      # table the original variable
      table(anes.2000$V000587)
      mode(anes.2000$V000587)
      class(anes.2000$V000587)
      # recode V000587
      anes.2000$DefenseRespondent <- NA
      anes.2000$DefenseRespondent[anes.2000$V000587 == "1. SCALE: 1. BRANCHING: Decrease defense"] <- -2
      anes.2000$DefenseRespondent[anes.2000$V000587 == "2. SCALE: 2. BRANCHING: Decrease defense"] <- -1
      anes.2000$DefenseRespondent[anes.2000$V000587 == "3. SCALE: 3,4,5. BRANCHING: About the ri"] <- 0
      anes.2000$DefenseRespondent[anes.2000$V000587 == "4. SCALE: 6. BRANCHING: Increase defense"] <- 1
      anes.2000$DefenseRespondent[anes.2000$V000587 == "5. SCALE: 7. BRANCHING: Increase defense"] <- 2
      # table new variable
      table(anes.2000$DefenseRespondent)
      mode(anes.2000$DefenseRespondent)
      class(anes.2000$DefenseRespondent)


# Gore:
      # Where would you place Al Gore on this issue?
      # SUMMARY: 5PT DEF SPENDING GORE PLACEMENT FROM 7PT SCALE/BRANCH
      # Built from L2b and L2bx1.
      # 1. SCALE: 1; BRANCHING: Decrease defense spending a lot
      # 2. SCALE: 2; BRANCHING: Decrease defense spending a little
      # 3. SCALE: 3,4,5; BRANCHING: About the right amount (5 in L2a1.T)
      # 4. SCALE: 6; BRANCHING: Increase defense spending a little
      # 5. SCALE: 7; BRANCHING: Increase defense spending a lot
      # table the original variable
      table(anes.2000$V000592)
      mode(anes.2000$V000592)
      class(anes.2000$V000592)
      # recode V000592
      anes.2000$DefenseGore <- NA
      anes.2000$DefenseGore[anes.2000$V000592 == "1. SCALE: 1. BRANCHING: Decrease defense"] <- -2
      anes.2000$DefenseGore[anes.2000$V000592 == "2. SCALE: 2. BRANCHING: Decrease defense"] <- -1
      anes.2000$DefenseGore[anes.2000$V000592 == "3. SCALE: 3,4,5. BRANCHING: About the ri"] <- 0
      anes.2000$DefenseGore[anes.2000$V000592 == "4. SCALE: 6. BRANCHING: Increase defense"] <- 1
      anes.2000$DefenseGore[anes.2000$V000592 == "5. SCALE: 7. BRANCHING: Increase defense"] <- 2
      # table new variable
      table(anes.2000$DefenseGore)
      mode(anes.2000$DefenseGore)
      class(anes.2000$DefenseGore)


# Bush:
      # Where would you place George W. Bush (on this issue)?
      # SUMMARY: 5PT DEF SPENDING GW BUSH PLACEMENT FROM 7PT SCALE/BRANCH
      # Built from L2c and L2cx1.
      # 1. SCALE: 1; BRANCHING: Decrease defense spending a lot
      # 2. SCALE: 2; BRANCHING: Decrease defense spending a little
      # 3. SCALE: 3,4,5; BRANCHING: About the right amount (5 in L2a1.T)
      # 4. SCALE: 6; BRANCHING: Increase defense spending a little
      # 5. SCALE: 7; BRANCHING: Increase defense spending a lot
      # table the original variable
      table(anes.2000$V000597)
      mode(anes.2000$V000597)
      class(anes.2000$V000597)
      # recode V000597
      anes.2000$DefenseBush <- NA
      anes.2000$DefenseBush[anes.2000$V000597 == "1. SCALE: 1. BRANCHING: Decrease defense"] <- -2
      anes.2000$DefenseBush[anes.2000$V000597 == "2. SCALE: 2. BRANCHING: Decrease defense"] <- -1
      anes.2000$DefenseBush[anes.2000$V000597 == "3. SCALE: 3,4,5. BRANCHING: About the ri"] <- 0
      anes.2000$DefenseBush[anes.2000$V000597 == "4. SCALE: 6. BRANCHING: Increase defense"] <- 1
      anes.2000$DefenseBush[anes.2000$V000597 == "5. SCALE: 7. BRANCHING: Increase defense"] <- 2
      # table new variable
      table(anes.2000$DefenseBush)
      mode(anes.2000$DefenseBush)
      class(anes.2000$DefenseBush)


#################################################
### First Social Issue - AID TO BLACKS
#################################################


# Respondent:
      # Where would you place yourself on this scale, or haven't you thought much about this?
      # SUMMARY: 5PT AID BLACKS SELF-PLACEMENT FROM 7PT SCALE/BRANCHING
      # Built from L5a and L5ax1.
      # 1. SCALE: 1 / BRANCHING: Govt help blacks to great extent
      # 2. SCALE: 2 / BRANCHING: Govt help blacks to some extent
      # 3. SCALE: 3,4,5 / BRANCHING: Other/neither/depends
      # 4. SCALE: 6 / BRANCHING: Should help themselves to some extent
      # 5. SCALE: 7 / BRANCHING: Should help themselves to a great extent
      # table the original variable
      table(anes.2000$V000645)
      mode(anes.2000$V000645)
      class(anes.2000$V000645)
      # recode V000645
      anes.2000$AidBlacksRespondent <- NA
      anes.2000$AidBlacksRespondent[anes.2000$V000645 == "1"] <- -2
      anes.2000$AidBlacksRespondent[anes.2000$V000645 == "2"] <- -1
      anes.2000$AidBlacksRespondent[anes.2000$V000645 == "3"] <- 0
      anes.2000$AidBlacksRespondent[anes.2000$V000645 == "4"] <- 1
      anes.2000$AidBlacksRespondent[anes.2000$V000645 == "5"] <- 2
      # table new variable
      table(anes.2000$AidBlacksRespondent)
      mode(anes.2000$AidBlacksRespondent)
      class(anes.2000$AidBlacksRespondent)



# Gore:
      # Where would you place Al Gore (on this issue)?
      # SUMMARY: 5PT AID BLACKS GORE PLACEMENT FROM 7PT SCALE/BRANCHING
      # Built from L5c and L5cx1.
      # 1. SCALE: 1 / BRANCHING: Govt help blacks to great extent
      # 2. SCALE: 2 / BRANCHING: Govt help blacks to some extent
      # 3. SCALE: 3,4,5 / BRANCHING: Other/neither/depends
      # 4. SCALE: 6 / BRANCHING: Should help themselves to some extent
      # 5. SCALE: 7 / BRANCHING: Should help themselves to a great extent
      # table the original variable
      table(anes.2000$V000655)
      mode(anes.2000$V000655)
      class(anes.2000$V000655)
      # recode V000655
      anes.2000$AidBlacksGore <- NA
      anes.2000$AidBlacksGore[anes.2000$V000655 == "1. SCALE: 1 / BRANCHING: Govt help black"] <- -2
      anes.2000$AidBlacksGore[anes.2000$V000655 == "2. SCALE: 2 / BRANCHING: Govt help black"] <- -1
      anes.2000$AidBlacksGore[anes.2000$V000655 == "3. SCALE: 3,4,5 / BRANCHING: Other/neith"] <- 0
      anes.2000$AidBlacksGore[anes.2000$V000655 == "4. SCALE: 6 / BRANCHING: Should help the"] <- 1
      anes.2000$AidBlacksGore[anes.2000$V000655 == "5. SCALE: 7 / BRANCHING: Should help the"] <- 2
      # table new variable
      table(anes.2000$AidBlacksGore)
      mode(anes.2000$AidBlacksGore)
      class(anes.2000$AidBlacksGore)



# Bush:
        # Where would you place George W. Bush (on this issue)?
        # SUMMARY: 5PT AID BLACKS BUSH PLACEMENT FROM 7PT SCALE/BRANCHING
        # Built from L5d and L5dx1.
        # 1. SCALE: 1 / BRANCHING: Govt help blacks to great extent
        # 2. SCALE: 2 / BRANCHING: Govt help blacks to some extent
        # 3. SCALE: 3,4,5 / BRANCHING: Other/neither/depends
        # 4. SCALE: 6 / BRANCHING: Should help themselves to some extent
        # 5. SCALE: 7 / BRANCHING: Should help themselves to a great extent
        # table the original variable
        table(anes.2000$V000660)
        mode(anes.2000$V000660)
        class(anes.2000$V000660)
        # recode V000660
        anes.2000$AidBlacksBush <- NA
        anes.2000$AidBlacksBush[anes.2000$V000660 == "1. SCALE: 1 / BRANCHING: Govt help black"] <- -2
        anes.2000$AidBlacksBush[anes.2000$V000660 == "2. SCALE: 2 / BRANCHING: Govt help black"] <- -1
        anes.2000$AidBlacksBush[anes.2000$V000660 == "3. SCALE: 3,4,5 / BRANCHING: Other/neith"] <- 0
        anes.2000$AidBlacksBush[anes.2000$V000660 == "4. SCALE: 6 / BRANCHING: Should help the"] <- 1
        anes.2000$AidBlacksBush[anes.2000$V000660 == "5. SCALE: 7 / BRANCHING: Should help the"] <- 2
        # table new variable
        table(anes.2000$AidBlacksBush)
        mode(anes.2000$AidBlacksBush)
        class(anes.2000$AidBlacksBush)


#################################################
### Second Social Issue - WOMAN'S ROLE
#################################################

# Respondent:
        # Where would you place yourself on this scale, or haven't you thought much about this?
        # SUMMARY: 5PT WOMENS ROLE SELF-PLACEMENT FROM 7PT SCALE/BRANCHING
        # Built from P1ax and P1a1x1
        # 1. SCALE: 1 / BRANCHING: Strongly - women equal role
        # 2. SCALE: 2 / BRANCHING: Not strongly - equal role
        # 3. SCALE: 3 / BRANCHING: Other/depends/neither
        # 4. SCALE: 4 / BRANCHING: Not strongly - place is in the home
        # 5. SCALE: 5 / BRANCHING: Strongly - place is in the home
        # table the original variable
        table(anes.2000$V000760)
        mode(anes.2000$V000760)
        class(anes.2000$V000760)
        # recode V000760
        anes.2000$WomansRoleRespondent <- NA
        anes.2000$WomansRoleRespondent[anes.2000$V000760 == "1. SCALE: 1 / BRANCHING: Strongly - wome"] <- -2
        anes.2000$WomansRoleRespondent[anes.2000$V000760 == "2. SCALE: 2 / BRANCHING: Not strongly -"] <- -1
        anes.2000$WomansRoleRespondent[anes.2000$V000760 == "3. SCALE: 3 / BRANCHING: Other/depends/n"] <- 0
        anes.2000$WomansRoleRespondent[anes.2000$V000760 == "4. SCALE: 4 / BRANCHING: Not strongly -"] <- 1
        anes.2000$WomansRoleRespondent[anes.2000$V000760 == "5. SCALE: 5 / BRANCHING: Strongly - plac"] <- 2
        # table new variable
        table(anes.2000$WomansRoleRespondent)
        mode(anes.2000$WomansRoleRespondent)
        class(anes.2000$WomansRoleRespondent)



# Gore:
      # Where would you place Al Gore (on this issue)?
      # SUMMARY: 5PT WOMENS ROLE GORE PLACEMENT FROM 7PT SCALE/BRANCHING
      # Built from P1b and P1bx1
      # 1. SCALE: 1 / BRANCHING: Strongly - women equal role
      # 2. SCALE: 2 / BRANCHING: Not strongly - equal role
      # 3. SCALE: 3 / BRANCHING: Other/depends/neither
      # 4. SCALE: 4 / BRANCHING: Not strongly - place is in the home
      # 5. SCALE: 5 / BRANCHING: Strongly - place is in the home
      # table the original variable
      table(anes.2000$V000765)
      mode(anes.2000$V000765)
      class(anes.2000$V000765)
      # recode V000765
      anes.2000$WomansRoleGore <- NA
      anes.2000$WomansRoleGore[anes.2000$V000765 == "1. SCALE: 1 / BRANCHING: Strongly - wome"] <- -2
      anes.2000$WomansRoleGore[anes.2000$V000765 == "2. SCALE: 2 / BRANCHING: Not strongly -"] <- -1
      anes.2000$WomansRoleGore[anes.2000$V000765 == "3. SCALE: 3 / BRANCHING: Other/depends/n"] <- 0
      anes.2000$WomansRoleGore[anes.2000$V000765 == "4. SCALE: 4 / BRANCHING: Not strongly -"] <- 1
      anes.2000$WomansRoleGore[anes.2000$V000765 == "5. SCALE: 5 / BRANCHING: Strongly - plac"] <- 2
      # table new variable
      table(anes.2000$WomansRoleGore)
      mode(anes.2000$WomansRoleGore)
      class(anes.2000$WomansRoleGore)



# Bush:
      # Where would you place George W. Bush (on this issue)?
      # SUMMARY: 5PT WOMENS ROLE BUSH PLACEMENT FROM 7PT SCALE/BRANCHING
      # Built from P1c and P1cx1.
      # 1. SCALE: 1 / BRANCHING: Strongly - women equal role
      # 2. SCALE: 2 / BRANCHING: Not strongly - equal role
      # 3. SCALE: 3 / BRANCHING: Other/depends/neither
      # 4. SCALE: 4 / BRANCHING: Not strongly - place is in the home
      # 5. SCALE: 5 / BRANCHING: Strongly - place is in the home
      # table the original variable
      table(anes.2000$V000770)
      mode(anes.2000$V000770)
      class(anes.2000$V000770)
      # recode V000770
      anes.2000$WomansRoleBush <- NA
      anes.2000$WomansRoleBush[anes.2000$V000770 == "1. SCALE: 1 / BRANCHING: Strongly - wome"] <- -2
      anes.2000$WomansRoleBush[anes.2000$V000770 == "2. SCALE: 2 / BRANCHING: Not strongly -"] <- -1
      anes.2000$WomansRoleBush[anes.2000$V000770 == "3. SCALE: 3 / BRANCHING: Other/depends/n"] <- 0
      anes.2000$WomansRoleBush[anes.2000$V000770 == "4. SCALE: 4 / BRANCHING: Not strongly -"] <- 1
      anes.2000$WomansRoleBush[anes.2000$V000770 == "5. SCALE: 5 / BRANCHING: Strongly - plac"] <- 2
      # table new variable
      table(anes.2000$WomansRoleBush)
      mode(anes.2000$WomansRoleBush)
      class(anes.2000$WomansRoleBush)



#################################################
### Third Social Issue - ENVIRONMENTAL REGULATION
#################################################


# Respondent:
      # Where would you place yourself on this scale, or haven't you thought much about this?
      # SUMMARY: 5PT ENV/JOBS SELF-PLACEMENT FROM 7PT SCALE/BRANCHING
      # Built from M4a and M4a1x1.
      # 1. SCALE: 1 / BRANCHING: Environment much more important
      # 2. SCALE: 2 / BRANCHING: Environment somewhat more important
      # 3. SCALE: 3 / BRANCHING: Other/depends/neither
      # 4. SCALE: 4 / BRANCHING: Jobs somewhat more important
      # 5. SCALE: 5 / BRANCHING: Jobs much more important
      # table the original variable
      table(anes.2000$V000713)
      mode(anes.2000$V000713)
      class(anes.2000$V000713)
      # recode V000713
      anes.2000$EnvironmentalRegulationRespondent <- NA
      anes.2000$EnvironmentalRegulationRespondent[anes.2000$V000713 == "1. SCALE: 1 / BRANCHING: Environment muc"] <- -2
      anes.2000$EnvironmentalRegulationRespondent[anes.2000$V000713 == "2. SCALE: 2 / BRANCHING: Environment som"] <- -1
      anes.2000$EnvironmentalRegulationRespondent[anes.2000$V000713 == "3. SCALE: 3 / BRANCHING: Other/depends/n"] <- 0
      anes.2000$EnvironmentalRegulationRespondent[anes.2000$V000713 == "4. SCALE: 4 / BRANCHING: Jobs somewhat m"] <- 1
      anes.2000$EnvironmentalRegulationRespondent[anes.2000$V000713 == "5. SCALE: 5 / BRANCHING: Jobs much more"] <- 2
      # table new variable
      table(anes.2000$EnvironmentalRegulationRespondent)
      mode(anes.2000$EnvironmentalRegulationRespondent)
      class(anes.2000$EnvironmentalRegulationRespondent)



# Gore:
      # Where would you place Al Gore on this issue?
      # SUMMARY: 5PT ENV/JOBS GORE PLACEMENT FROM 7PT SCALE/BRANCHING
      # 1. SCALE: 1 / BRANCHING: Environment much more important
      # 2. SCALE: 2 / BRANCHING: Environment somewhat more important
      # 3. SCALE: 3 / BRANCHING: Other/depends/neither
      # 4. SCALE: 4 / BRANCHING: Jobs somewhat more important
      # 5. SCALE: 5 / BRANCHING: Jobs much more important
      # table the original variable
      table(anes.2000$V000718)
      mode(anes.2000$V000718)
      class(anes.2000$V000718)
      # recode V000718
      anes.2000$EnvironmentalRegulationGore <- NA
      anes.2000$EnvironmentalRegulationGore[anes.2000$V000718 == "1. SCALE: 1 / BRANCHING: Environment muc"] <- -2
      anes.2000$EnvironmentalRegulationGore[anes.2000$V000718 == "2. SCALE: 2 / BRANCHING: Environment som"] <- -1
      anes.2000$EnvironmentalRegulationGore[anes.2000$V000718 == "3. SCALE: 3 / BRANCHING: Other/depends/n"] <- 0
      anes.2000$EnvironmentalRegulationGore[anes.2000$V000718 == "4. SCALE: 4 / BRANCHING: Jobs somewhat m"] <- 1
      anes.2000$EnvironmentalRegulationGore[anes.2000$V000718 == "5. SCALE: 5 / BRANCHING: Jobs much more"] <- 2
      # table new variable
      table(anes.2000$EnvironmentalRegulationGore)
      mode(anes.2000$EnvironmentalRegulationGore)
      class(anes.2000$EnvironmentalRegulationGore)



# Bush:
      # Where would you place George W. Bush (on this issue)?
      # SUMMARY: 5PT ENV/JOBS BUSH PLACEMENT FROM 7PT SCALE/BRANCHING
      # Built from M4c and M4cx1.
      # 1. SCALE: 1 / BRANCHING: Environment much more important
      # 2. SCALE: 2 / BRANCHING: Environment somewhat more important
      # 3. SCALE: 3 / BRANCHING: Other/depends/neither
      # 4. SCALE: 4 / BRANCHING: Jobs somewhat more important
      # 5. SCALE: 5 / BRANCHING: Jobs much more important
      # table the original variable
      table(anes.2000$V000723)
      mode(anes.2000$V000723)
      class(anes.2000$V000723)
      # recode V000723
      anes.2000$EnvironmentalRegulationBush <- NA
      anes.2000$EnvironmentalRegulationBush[anes.2000$V000723 == "1. SCALE: 1 / BRANCHING: Environment muc"] <- -2
      anes.2000$EnvironmentalRegulationBush[anes.2000$V000723 == "2. SCALE: 2 / BRANCHING: Environment som"] <- -1
      anes.2000$EnvironmentalRegulationBush[anes.2000$V000723 == "3. SCALE: 3 / BRANCHING: Other/depends/n"] <- 0
      anes.2000$EnvironmentalRegulationBush[anes.2000$V000723 == "4. SCALE: 4 / BRANCHING: Jobs somewhat m"] <- 1
      anes.2000$EnvironmentalRegulationBush[anes.2000$V000723 == "5. SCALE: 5 / BRANCHING: Jobs much more"] <- 2
      # table new variable
      table(anes.2000$EnvironmentalRegulationBush)
      mode(anes.2000$EnvironmentalRegulationBush)
      class(anes.2000$EnvironmentalRegulationBush)



#################################################
### Create ECONOMIC INDEX
#################################################

# Respondent
anes.2000$EconomicIndexRespondent <- anes.2000$JobsRespondent + anes.2000$SpendingRespondent + anes.2000$DefenseRespondent
table(anes.2000$EconomicIndexRespondent)
mode(anes.2000$EconomicIndexRespondent)
class(anes.2000$EconomicIndexRespondent)

# Gore
anes.2000$EconomicIndexGore <- anes.2000$JobsGore + anes.2000$SpendingGore + anes.2000$DefenseGore
table(anes.2000$EconomicIndexGore)
mode(anes.2000$EconomicIndexGore)
class(anes.2000$EconomicIndexGore)

# Bush
anes.2000$EconomicIndexBush <- anes.2000$JobsBush + anes.2000$SpendingBush + anes.2000$DefenseBush
table(anes.2000$EconomicIndexBush)
mode(anes.2000$EconomicIndexBush)
class(anes.2000$EconomicIndexBush)


#################################################
### Create SOCIAL INDEX
#################################################

# Respondent
anes.2000$SocialIndexRespondent <- anes.2000$AidBlacksRespondent + anes.2000$WomansRoleRespondent + anes.2000$EnvironmentalRegulationRespondent
table(anes.2000$SocialIndexRespondent)
mode(anes.2000$SocialIndexRespondent)
class(anes.2000$SocialIndexRespondent)

# Gore
anes.2000$SocialIndexGore <- anes.2000$AidBlacksGore + anes.2000$WomansRoleGore + anes.2000$EnvironmentalRegulationGore
table(anes.2000$SocialIndexGore)
mode(anes.2000$SocialIndexGore)
class(anes.2000$SocialIndexGore)

# Bush
anes.2000$SocialIndexBush <- anes.2000$AidBlacksBush + anes.2000$WomansRoleBush + anes.2000$EnvironmentalRegulationBush
table(anes.2000$SocialIndexBush)
mode(anes.2000$SocialIndexBush)
class(anes.2000$SocialIndexBush)


#####################################################
### Obtain Coeffients and Standard Errors for Graphs
#####################################################

### Create list of the indexes used as dependent varibles in the loop
IndexList <- c('EconomicIndexGore', 
               'EconomicIndexBush', 
               'SocialIndexGore', 
               'SocialIndexBush')
table(IndexList)


### Create empty variables for the coefficients and standard errors
DemocratConstantTermCoefficients <- rep (NA, length(IndexList))
IndependentConstantTermCoefficients <- rep (NA, length(IndexList))
RepublicanConstantTermCoefficients <- rep (NA, length(IndexList))
DemocratConstantTermStandardErrors <- rep (NA, length(IndexList))
IndependentConstantTermStandardErrors <- rep (NA, length(IndexList))
RepublicanConstantTermStandardErrors <- rep (NA, length(IndexList))


DemocratEconomicIndexRespondentCoefficients <- rep (NA, length(IndexList))
IndependentEconomicIndexRespondentCoefficients <- rep (NA, length(IndexList))
RepublicanEconomicIndexRespondentCoefficients <- rep (NA, length(IndexList))
DemocratEconomicIndexRespondentStandardErrors <- rep (NA, length(IndexList))
IndependentEconomicIndexRespondentStandardErrors <- rep (NA, length(IndexList))
RepublicanEconomicIndexRespondentStandardErrors <- rep (NA, length(IndexList))


DemocratSocialIndexRespondentCoefficients <- rep (NA, length(IndexList))
IndependentSocialIndexRespondentCoefficients <- rep (NA, length(IndexList))
RepublicanSocialIndexRespondentCoefficients <- rep (NA, length(IndexList))
DemocratSocialIndexRespondentStandardErrors <- rep (NA, length(IndexList))
IndependentSocialIndexRespondentStandardErrors <- rep (NA, length(IndexList))
RepublicanSocialIndexRespondentStandardErrors <- rep (NA, length(IndexList))

### Specify survey design
design <- svydesign(ids=~1, data = anes.2000, weights = anes.2000$Weights)

### Create a loop
for(i in 1:4) {
  print(IndexList[i])
  DemocratModel <- svyglm(paste0(IndexList[i], '~EconomicIndexRespondent + SocialIndexRespondent'), design=subset(design, PartyID == 'Democrat'))
  summary(DemocratModel)
  DemocratConstantTermCoefficients[i] <- DemocratModel$coef[1]
  DemocratConstantTermStandardErrors[i] <- summary(DemocratModel)$coef[, "Std. Error"][1]
  DemocratEconomicIndexRespondentCoefficients[i] <- DemocratModel$coef[2]
  DemocratEconomicIndexRespondentStandardErrors[i] <- summary(DemocratModel)$coef[, "Std. Error"][2]
  DemocratSocialIndexRespondentCoefficients[i] <- DemocratModel$coef[3]
  DemocratSocialIndexRespondentStandardErrors[i] <- summary(DemocratModel)$coef[, "Std. Error"][3]

  RepublicanModel <- svyglm(paste0(IndexList[i], '~EconomicIndexRespondent + SocialIndexRespondent'), design=subset(design, PartyID == 'Republican'))
  summary(RepublicanModel)
  RepublicanConstantTermCoefficients[i] <- RepublicanModel$coef[1]
  RepublicanConstantTermStandardErrors[i] <- summary(RepublicanModel)$coef[, "Std. Error"][1]
  RepublicanEconomicIndexRespondentCoefficients[i] <- RepublicanModel$coef[2]
  RepublicanEconomicIndexRespondentStandardErrors[i] <- summary(RepublicanModel)$coef[, "Std. Error"][2]
  RepublicanSocialIndexRespondentCoefficients[i] <- RepublicanModel$coef[3]
  RepublicanSocialIndexRespondentStandardErrors[i] <- summary(RepublicanModel)$coef[, "Std. Error"][3]

  IndependentModel <- svyglm(paste0(IndexList[i], '~EconomicIndexRespondent + SocialIndexRespondent'), design=subset(design, PartyID == 'Independent'))
  summary(IndependentModel)
  IndependentConstantTermCoefficients[i] <- IndependentModel$coef[1]
  IndependentConstantTermStandardErrors[i] <- summary(IndependentModel)$coef[, "Std. Error"][1]
  IndependentEconomicIndexRespondentCoefficients[i] <- IndependentModel$coef[2]
  IndependentEconomicIndexRespondentStandardErrors[i] <- summary(IndependentModel)$coef[, "Std. Error"][2]
  IndependentSocialIndexRespondentCoefficients[i] <- IndependentModel$coef[3]
  IndependentSocialIndexRespondentStandardErrors[i] <- summary(IndependentModel)$coef[, "Std. Error"][3]
}


### Table coefficients obtained from the loop
table(DemocratConstantTermCoefficients)
table(DemocratEconomicIndexRespondentCoefficients)
table(DemocratSocialIndexRespondentCoefficients)
table(DemocratConstantTermStandardErrors)
table(DemocratEconomicIndexRespondentStandardErrors)
table(DemocratSocialIndexRespondentStandardErrors)

table(RepublicanConstantTermCoefficients)
table(RepublicanEconomicIndexRespondentCoefficients)
table(RepublicanSocialIndexRespondentCoefficients)
table(RepublicanConstantTermStandardErrors)
table(RepublicanEconomicIndexRespondentStandardErrors)
table(RepublicanSocialIndexRespondentStandardErrors)

table(IndependentConstantTermCoefficients)
table(IndependentEconomicIndexRespondentCoefficients)
table(IndependentSocialIndexRespondentCoefficients)
table(IndependentConstantTermStandardErrors)
table(IndependentEconomicIndexRespondentStandardErrors)
table(IndependentSocialIndexRespondentStandardErrors)


### Create new data frame for graphs
GraphDataConstantCoefficients <- rbind (DemocratConstantTermCoefficients, 
                                        IndependentConstantTermCoefficients, 
                                        RepublicanConstantTermCoefficients)
matrix(GraphDataConstantCoefficients)


GraphDataConstantStandardError <- rbind (DemocratConstantTermStandardErrors, 
                                         IndependentConstantTermStandardErrors, 
                                         RepublicanConstantTermStandardErrors)
matrix(GraphDataConstantStandardError)


GraphDataEconomicIndexCoefficients <- rbind (DemocratEconomicIndexRespondentCoefficients, 
                                             IndependentEconomicIndexRespondentCoefficients, 
                                             RepublicanEconomicIndexRespondentCoefficients)
matrix(GraphDataEconomicIndexCoefficients)


GraphDataEconomicIndexStandardError <- rbind (DemocratEconomicIndexRespondentStandardErrors, 
                                              IndependentEconomicIndexRespondentStandardErrors, 
                                              RepublicanEconomicIndexRespondentStandardErrors)
matrix(GraphDataEconomicIndexStandardError)


GraphDataSocialIndexCoefficients <- rbind (DemocratSocialIndexRespondentCoefficients, 
                                           IndependentSocialIndexRespondentCoefficients, 
                                           RepublicanSocialIndexRespondentCoefficients)
matrix(GraphDataSocialIndexCoefficients)


GraphDataSocialIndexStandardError <- rbind (DemocratSocialIndexRespondentStandardErrors, 
                                            IndependentSocialIndexRespondentStandardErrors,
                                            RepublicanSocialIndexRespondentStandardErrors)
matrix(GraphDataSocialIndexStandardError)

PartyID <- c("D", "I", "R")

GraphData <- data.frame (GraphDataConstantCoefficients, GraphDataConstantStandardError, 
                         GraphDataEconomicIndexCoefficients, GraphDataEconomicIndexStandardError, 
                         GraphDataSocialIndexCoefficients, GraphDataSocialIndexStandardError,
                         PartyID)
names(GraphData)

matrix(GraphData$X1)
matrix(GraphData$X2)
matrix(GraphData$X3)
matrix(GraphData$X4)

matrix(GraphData$X1.1)
matrix(GraphData$X2.1)
matrix(GraphData$X3.1)
matrix(GraphData$X4.1)

matrix(GraphData$X1.2)
matrix(GraphData$X2.2)
matrix(GraphData$X3.2)
matrix(GraphData$X4.2)

matrix(GraphData$X1.3)
matrix(GraphData$X2.3)
matrix(GraphData$X3.3)
matrix(GraphData$X4.3)

matrix(GraphData$X1.4)
matrix(GraphData$X2.4)
matrix(GraphData$X3.4)
matrix(GraphData$X4.4)

matrix(GraphData$X1.5)
matrix(GraphData$X2.5)
matrix(GraphData$X3.5)
matrix(GraphData$X4.5)

matrix(GraphData$PartyID)

names(GraphData) <- c("EconomicIndexGoreConstantTermCoefficient", 
                      "EconomicIndexBushConstantTermCoefficient", 
                      "SocialIndexGoreConstantTermCoefficient", 
                      "SocialIndexBushConstantTermCoefficient", 
                      "EconomicIndexGoreConstantTermStandardError", 
                      "EconomicIndexBushConstantTermStandardError", 
                      "SocialIndexGoreConstantTermStandardError", 
                      "SocialIndexBushConstantTermStandardError", 
                      "EconomicIndexGoreBetaEconomicsCoefficient", 
                      "EconomicIndexBushBetaEconomicsCoefficient", 
                      "SocialIndexGoreBetaEconomicsCoefficient", 
                      "SocialIndexBushBetaEconomicsCoefficient", 
                      "EconomicIndexGoreBetaEconomicsStandardError", 
                      "EconomicIndexBushBetaEconomicsStandardError", 
                      "SocialIndexGoreBetaEconomicsStandardError", 
                      "SocialIndexBushBetaEconomicsStandardError", 
                      "EconomicIndexGoreBetaSocialCoefficient", 
                      "EconomicIndexBushBetaSocialCoefficient", 
                      "SocialIndexGoreBetaSocialCoefficient", 
                      "SocialIndexBushBetaSocialCoefficient", 
                      "EconomicIndexGoreBetaSocialStandardError", 
                      "EconomicIndexBushBetaSocialStandardError", 
                      "SocialIndexGoreBetaSocialStandardError", 
                      "SocialIndexBushBetaSocialStandardError",
                      "PartyID")
names(GraphData)


### Create Upper and Lower bounds for each CONSTANT TERM COEFFICIENT (one standard error)
GraphData$EconomicIndexGoreConstantTermCoefficient.Lower = GraphData$EconomicIndexGoreConstantTermCoefficient - GraphData$EconomicIndexGoreConstantTermStandardError
matrix(GraphData$EconomicIndexGoreConstantTermCoefficient.Lower)
GraphData$EconomicIndexGoreConstantTermCoefficient.Upper = GraphData$EconomicIndexGoreConstantTermCoefficient + GraphData$EconomicIndexGoreConstantTermStandardError
matrix(GraphData$EconomicIndexGoreConstantTermCoefficient.Upper)

GraphData$EconomicIndexBushConstantTermCoefficient.Lower = GraphData$EconomicIndexBushConstantTermCoefficient - GraphData$EconomicIndexBushConstantTermStandardError
matrix(GraphData$EconomicIndexBushConstantTermCoefficient.Lower)
GraphData$EconomicIndexBushConstantTermCoefficient.Upper = GraphData$EconomicIndexBushConstantTermCoefficient + GraphData$EconomicIndexBushConstantTermStandardError
matrix(GraphData$EconomicIndexBushConstantTermCoefficient.Upper)

GraphData$SocialIndexGoreConstantTermCoefficient.Lower = GraphData$SocialIndexGoreConstantTermCoefficient - GraphData$SocialIndexGoreConstantTermStandardError
matrix(GraphData$SocialIndexGoreConstantTermCoefficient.Lower)
GraphData$SocialIndexGoreConstantTermCoefficient.Upper = GraphData$SocialIndexGoreConstantTermCoefficient + GraphData$SocialIndexGoreConstantTermStandardError
matrix(GraphData$SocialIndexGoreConstantTermCoefficient.Upper)

GraphData$SocialIndexBushConstantTermCoefficient.Lower = GraphData$SocialIndexBushConstantTermCoefficient - GraphData$SocialIndexBushConstantTermStandardError
matrix(GraphData$SocialIndexBushConstantTermCoefficient.Lower)
GraphData$SocialIndexBushConstantTermCoefficient.Upper = GraphData$SocialIndexBushConstantTermCoefficient + GraphData$SocialIndexBushConstantTermStandardError
matrix(GraphData$SocialIndexBushConstantTermCoefficient.Upper)


### Create Upper and Lower bounds for each BETA ECONOMICS COEFFICIENT (one standard error)
GraphData$EconomicIndexGoreBetaEconomicsCoefficient.Lower = GraphData$EconomicIndexGoreBetaEconomicsCoefficient - GraphData$EconomicIndexGoreBetaEconomicsStandardError
matrix(GraphData$EconomicIndexGoreBetaEconomicsCoefficient.Lower)
GraphData$EconomicIndexGoreBetaEconomicsCoefficient.Upper = GraphData$EconomicIndexGoreBetaEconomicsCoefficient + GraphData$EconomicIndexGoreBetaEconomicsStandardError
matrix(GraphData$EconomicIndexGoreBetaEconomicsCoefficient.Upper)

GraphData$EconomicIndexBushBetaEconomicsCoefficient.Lower = GraphData$EconomicIndexBushBetaEconomicsCoefficient - GraphData$EconomicIndexBushBetaEconomicsStandardError
matrix(GraphData$EconomicIndexBushBetaEconomicsCoefficient.Lower)
GraphData$EconomicIndexBushBetaEconomicsCoefficient.Upper = GraphData$EconomicIndexBushBetaEconomicsCoefficient + GraphData$EconomicIndexBushBetaEconomicsStandardError
matrix(GraphData$EconomicIndexBushBetaEconomicsCoefficient.Upper)

GraphData$SocialIndexGoreBetaEconomicsCoefficient.Lower = GraphData$SocialIndexGoreBetaEconomicsCoefficient - GraphData$SocialIndexGoreBetaEconomicsStandardError
matrix(GraphData$SocialIndexGoreBetaEconomicsCoefficient.Lower)
GraphData$SocialIndexGoreBetaEconomicsCoefficient.Upper = GraphData$SocialIndexGoreBetaEconomicsCoefficient + GraphData$SocialIndexGoreBetaEconomicsStandardError
matrix(GraphData$SocialIndexGoreBetaEconomicsCoefficient.Upper)

GraphData$SocialIndexBushBetaEconomicsCoefficient.Lower = GraphData$SocialIndexBushBetaEconomicsCoefficient - GraphData$SocialIndexBushBetaEconomicsStandardError
matrix(GraphData$SocialIndexBushBetaEconomicsCoefficient.Lower)
GraphData$SocialIndexBushBetaEconomicsCoefficient.Upper = GraphData$SocialIndexBushBetaEconomicsCoefficient + GraphData$SocialIndexBushBetaEconomicsStandardError
matrix(GraphData$SocialIndexBushBetaEconomicsCoefficient.Upper)


### Create Upper and Lower bounds for each BETA SOCIAL COEFFICIENT (one standard error)
GraphData$EconomicIndexGoreBetaSocialCoefficient.Lower = GraphData$EconomicIndexGoreBetaSocialCoefficient - GraphData$EconomicIndexGoreBetaSocialStandardError
matrix(GraphData$EconomicIndexGoreBetaSocialCoefficient.Lower)
GraphData$EconomicIndexGoreBetaSocialCoefficient.Upper = GraphData$EconomicIndexGoreBetaSocialCoefficient + GraphData$EconomicIndexGoreBetaSocialStandardError
matrix(GraphData$EconomicIndexGoreBetaSocialCoefficient.Upper)

GraphData$EconomicIndexBushBetaSocialCoefficient.Lower = GraphData$EconomicIndexBushBetaSocialCoefficient - GraphData$EconomicIndexBushBetaSocialStandardError
matrix(GraphData$EconomicIndexBushBetaSocialCoefficient.Lower)
GraphData$EconomicIndexBushBetaSocialCoefficient.Upper = GraphData$EconomicIndexBushBetaSocialCoefficient + GraphData$EconomicIndexBushBetaSocialStandardError
matrix(GraphData$EconomicIndexBushBetaSocialCoefficient.Upper)

GraphData$SocialIndexGoreBetaSocialCoefficient.Lower = GraphData$SocialIndexGoreBetaSocialCoefficient - GraphData$SocialIndexGoreBetaSocialStandardError
matrix(GraphData$SocialIndexGoreBetaSocialCoefficient.Lower)
GraphData$SocialIndexGoreBetaSocialCoefficient.Upper = GraphData$SocialIndexGoreBetaSocialCoefficient + GraphData$SocialIndexGoreBetaSocialStandardError
matrix(GraphData$SocialIndexGoreBetaSocialCoefficient.Upper)

GraphData$SocialIndexBushBetaSocialCoefficient.Lower = GraphData$SocialIndexBushBetaSocialCoefficient - GraphData$SocialIndexBushBetaSocialStandardError
matrix(GraphData$SocialIndexBushBetaSocialCoefficient.Lower)
GraphData$SocialIndexBushBetaSocialCoefficient.Upper = GraphData$SocialIndexBushBetaSocialCoefficient + GraphData$SocialIndexBushBetaSocialStandardError
matrix(GraphData$SocialIndexBushBetaSocialCoefficient.Upper)

names(GraphData)


### Format plots
png("hwk9.png", height = 800, width = 1000)
par(mfrow = c(3,4))


### Graph "Bush Economic Index" - Constant Term
plot (x = GraphData$PartyID, 
      y = GraphData$EconomicIndexBushConstantTermCoefficient, 
      main = "Bush Economic Index", 
      xlab = "Party ID", 
      ylab = "Constant Term",
      ylim = c(-0.5,2),
      lty = "blank")
abline (h=0)
points (x = GraphData$EconomicIndexBushConstantTermCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$EconomicIndexBushConstantTermCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$EconomicIndexBushConstantTermCoefficient.Upper,
       li = GraphData$EconomicIndexBushConstantTermCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Gore Economic Index" - Constant Term
plot (x = GraphData$PartyID, 
      y = GraphData$EconomicIndexGoreConstantTermCoefficient, 
      main = "Gore Economic Index", 
      xlab = "Party ID", 
      ylab = "Constant Term",
      ylim = c(-1.5,0.5),
      lty = "blank")
abline (h=0)
points (x = GraphData$EconomicIndexGoreConstantTermCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$EconomicIndexGoreConstantTermCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$EconomicIndexGoreConstantTermCoefficient.Upper,
       li = GraphData$EconomicIndexGoreConstantTermCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Bush Social Index" - Constant Term
plot (x = GraphData$PartyID, 
      y = GraphData$SocialIndexBushConstantTermCoefficient, 
      main = "Bush Social Index", 
      xlab = "Party ID", 
      ylab = "Constant Term",
      ylim = c(-0.5,1.5),
      lty = "blank")
abline (h=0)
points (x = GraphData$SocialIndexBushConstantTermCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$SocialIndexBushConstantTermCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$SocialIndexBushConstantTermCoefficient.Upper,
       li = GraphData$SocialIndexBushConstantTermCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Gore Social Index" - Constant Term
plot (x = GraphData$PartyID, 
      y = GraphData$SocialIndexGoreConstantTermCoefficient, 
      main = "Gore Social Index", 
      xlab = "Party ID", 
      ylab = "Constant Term",
      ylim = c(-2.5,0.5),
      lty = "blank")
abline (h=0)
points (x = GraphData$SocialIndexGoreConstantTermCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$SocialIndexGoreConstantTermCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$SocialIndexGoreConstantTermCoefficient.Upper,
       li = GraphData$SocialIndexGoreConstantTermCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Bush Economic Economic Index" - Coef for self econ
plot (x = GraphData$PartyID, 
      y = GraphData$EconomicIndexBushBetaEconomicsCoefficient, 
      main = "Bush Economic Index", 
      xlab = "Party ID", 
      ylab = "Coef for self econ",
      ylim = c(-0.5,1),
      lty = "blank")
abline (h=0)
points (x = GraphData$EconomicIndexBushBetaEconomicsCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$EconomicIndexBushBetaEconomicsCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$EconomicIndexBushBetaEconomicsCoefficient.Upper,
       li = GraphData$EconomicIndexBushBetaEconomicsCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Gore Economic Index" - Coef for self econ
plot (x = GraphData$PartyID, 
      y = GraphData$EconomicIndexGoreBetaEconomicsCoefficient, 
      main = "Gore Economic Index", 
      xlab = "Party ID", 
      ylab = "Coef for self econ",
      ylim = c(-1,0.5),
      lty = "blank")
abline (h=0)
points (x = GraphData$EconomicIndexGoreBetaEconomicsCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$EconomicIndexGoreBetaEconomicsCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$EconomicIndexGoreBetaEconomicsCoefficient.Upper,
       li = GraphData$EconomicIndexGoreBetaEconomicsCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Bush Social Index" - Coef for self econ
plot (x = GraphData$PartyID, 
      y = GraphData$SocialIndexBushBetaEconomicsCoefficient, 
      main = "Bush Social Index", 
      xlab = "Party ID", 
      ylab = "Coef for self econ",
      ylim = c(-0.5,0.5),
      lty = "blank")
abline (h=0)
points (x = GraphData$SocialIndexBushBetaEconomicsCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$SocialIndexBushBetaEconomicsCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$SocialIndexBushBetaEconomicsCoefficient.Upper,
       li = GraphData$SocialIndexBushBetaEconomicsCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Gore Social Index" - Coef for self econ
plot (x = GraphData$PartyID, 
      y = GraphData$SocialIndexGoreBetaEconomicsCoefficient, 
      main = "Gore Social Index", 
      xlab = "Party ID", 
      ylab = "Coef for self econ",
      ylim = c(-0.5,0.5),
      lty = "blank")
abline (h=0)
points (x = GraphData$SocialIndexGoreBetaEconomicsCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$SocialIndexGoreBetaEconomicsCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$SocialIndexGoreBetaEconomicsCoefficient.Upper,
       li = GraphData$SocialIndexGoreBetaEconomicsCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Bush Economic Index" - Coef for self social
plot (x = GraphData$PartyID, 
      y = GraphData$EconomicIndexBushBetaSocialCoefficient, 
      main = "Bush Economic Index", 
      xlab = "Party ID", 
      ylab = "Coef for self social",
      ylim = c(-0.5,0.5),
      lty = "blank")
abline (h=0)
points (x = GraphData$EconomicIndexBushBetaSocialCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$EconomicIndexBushBetaSocialCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$EconomicIndexBushBetaSocialCoefficient.Upper,
       li = GraphData$EconomicIndexBushBetaSocialCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Gore Economic Index" - Coef for self social
plot (x = GraphData$PartyID, 
      y = GraphData$EconomicIndexGoreBetaSocialCoefficient, 
      main = "Gore Economic Index", 
      xlab = "Party ID", 
      ylab = "Coef for self social",
      ylim = c(-0.5,0.5),
      lty = "blank")
abline (h=0)
points (x = GraphData$EconomicIndexGoreBetaSocialCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$EconomicIndexGoreBetaSocialCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$EconomicIndexGoreBetaSocialCoefficient.Upper,
       li = GraphData$EconomicIndexGoreBetaSocialCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Bush Social Index" - Coef for self social
plot (x = GraphData$PartyID, 
      y = GraphData$SocialIndexBushBetaSocialCoefficient, 
      main = "Bush Social Index", 
      xlab = "Party ID", 
      ylab = "Coef for self social",
      ylim = c(-0.5,1),
      lty = "blank")
abline (h=0)
points (x = GraphData$SocialIndexBushBetaSocialCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$SocialIndexBushBetaSocialCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$SocialIndexBushBetaSocialCoefficient.Upper,
       li = GraphData$SocialIndexBushBetaSocialCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)


### Graph "Gore Social Index" - Coef for self social
plot (x = GraphData$PartyID, 
      y = GraphData$SocialIndexGoreBetaSocialCoefficient, 
      main = "Gore Social Index", 
      xlab = "Party ID", 
      ylab = "Coef for self social",
      ylim = c(-0.5,1),
      lty = "blank")
abline (h=0)
points (x = GraphData$SocialIndexGoreBetaSocialCoefficient, 
        pch = 19,
        col = "black")
plotCI(x = GraphData$SocialIndexGoreBetaSocialCoefficient, 
       uiw = NULL,
       liw = NULL,
       ui = GraphData$SocialIndexGoreBetaSocialCoefficient.Upper,
       li = GraphData$SocialIndexGoreBetaSocialCoefficient.Lower,
       err = "y",
       col = "black",
       barcol = "black",
       lwd = 1,
       gap = 0,
       sfrac = 0,
       add = TRUE)

### THE END
dev.off()

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
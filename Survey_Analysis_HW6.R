#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 6 
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 3 - SIMULATION AND ANALYSIS OF STRATIFIED SAMPLE

# Write an R function to take a random subsample of the 2010 General Social Survey using regions of the country as strata.

# (a) Perform a sample of size 100 with each stratum sampled in proportion to its population size (in this case, the “population” is just the full 2010 GSS).  Use this subsample to estimate the proportion of people who favor a law, which would require a person to obtain a police permit before he or she could buy a gun.  Also compute the standard error for this estimate, first directly using the formula for the standard error of a stratified sample, then using the “survey” package in R.  (These two standard errors should be identical.)

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
		library ("sampling")

### DATA:
		# Data is from the General Social Survey 2010 Cross-Section and Panel Combined
		# SOURCE: http://www.thearda.com/Archive/Files/Downloads/GSS10PAN_DL2.asp
		# read the data into R
		gss.2010 <- read.dta("General Social Survey 2010 Cross-Section and Panel Combined.DTA")
		# how many rows (cases) are in the dataset
		nrow(gss.2010)
		# names of the columns (variable names)
		names(gss.2010)
	

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


### Calculate the size of each strata
		aggregate.size.by.region <- aggregate (x = gss.2010.GunLaw$Region, list(gss.2010.GunLaw$Region), FUN=length)
		# table dataset
		table (aggregate.size.by.region)
		# names of the variables in the dataset
		names (aggregate.size.by.region)
		# create new vector "Region.agg"
		aggregate.size.by.region$Region.agg <- as.vector(aggregate.size.by.region$Group.1)
		# table new vector "Region.agg"
		table(aggregate.size.by.region$Region.agg)
		# create new vector "N.Region"
		aggregate.size.by.region$N.Region <- as.vector(aggregate.size.by.region$x)
		# table new vector "N.Region"
		table(aggregate.size.by.region$N.Region)
		# names of the variables in the dataset
		names(aggregate.size.by.region)
		# table "Region.agg" by "N.Region"
		table(aggregate.size.by.region$Region.agg, aggregate.size.by.region$N.Region)


### Merge gss.2010 data with aggregate data for length of each strata
		# merge() function - merge two data frames by common row names (region names)
		gss.2010 <- merge (gss.2010, aggregate.size.by.region, by.x="Region", by.y="Region.agg")
		# how many rows (cases) are in the dataset
		nrow(gss.2010)
		# names of the columns (variable names)
		names(gss.2010)
		
		
		
### DETERMINE THE PROPORTION OF SAMPLED CASES WITHIN EACH CLUSTER
		# make a strata variable (numerical values)
		Strata <- table (gss.2010$Region)
		table(Strata)
		# obtain the names of each strata (character)
		StrataNames <- rownames(Strata)
		table(StrataNames)
		# proportion of cases in each strata over the total number of cases in the dataset times 100
		Strata.Proportion <- Strata / sum (Strata) * 100
		table(Strata.Proportion)
		# round the percentage proportions
		Strata.Proportion.Rounded <- round(Strata.Proportion, digits=0)
		table(Strata.Proportion.Rounded)
		# combine strata names with the rounded strata proportions
		Region.Percentage.Rounded <- cbind(StrataNames, Strata.Proportion.Rounded)
		Region.Percentage.Rounded



#####################################################################
#
#                    StrataNames          Strata.Proportion.Rounded
# East North Central "East North Central" "17"                     
# East South Central "East South Central" "6"                      
# Middle Atlantic    "Middle Atlantic"    "13"                     
# Mountain           "Mountain"           "7"                      
# New England        "New England"        "4"                      
# Pacific            "Pacific"            "14"                     
# South Atlantic     "South Atlantic"     "21"                     
# West North Central "West North Central" "6"                      
# West South Central "West South Central" "11"                     
# 
#######################################################################
		
		
		
### RECODE VARIABLE "gunlaw":
		# table the original variable
		table(gss.2010$gunlaw)
		# recode the "gunlaw" variable
		gss.2010$FavorGunLaw <- NA
		gss.2010$FavorGunLaw[gss.2010$gunlaw =='Favor'] <- 1
		gss.2010$FavorGunLaw[gss.2010$gunlaw == 'Oppose'] <- 0
		# table the newly created variable
		table(gss.2010$FavorGunLaw)
		mode(gss.2010$FavorGunLaw)
		class(gss.2010$FavorGunLaw)		


### SUBSETTING THE DATA
		# subset() function
		gss.2010.GunLaw <- subset(gss.2010, gss.2010$FavorGunLaw == 0 | gss.2010$FavorGunLaw == 1)
		# how many rows (cases) are in the subset of data
		nrow(gss.2010.GunLaw)
		# names of the columns in the subset of data (variable names)
		names(gss.2010.GunLaw) 

		
### TAKE RANDOM SAMPLES IN EACH STRATA PROPORTIONAL TO THE STRATA SIZES


		# sample 18 cases in "East North Central"
		East.North.Central <- subset(gss.2010.GunLaw, gss.2010.GunLaw$region == "East North Central")
		East.North.Central.Sample <- sample(East.North.Central$FavorGunLaw, size=17, replace=TRUE)
		East.North.Central.Sample

		# sample 6 cases in "East South Central"
		East.South.Central <- subset(gss.2010.GunLaw, gss.2010.GunLaw$region == "East South Central")
		East.South.Central.Sample <- sample(East.South.Central$FavorGunLaw, size=6, replace=TRUE)
		East.South.Central.Sample

		# sample 13 cases in "Middle Atlantic"
		Middle.Atlantic <- subset(gss.2010.GunLaw, gss.2010.GunLaw$region == "Middle Atlantic")
		Middle.Atlantic.Sample <- sample(Middle.Atlantic$FavorGunLaw, size=13, replace=TRUE)
		Middle.Atlantic.Sample

		# sample 7 cases in "Mountain"
		Mountain <- subset(gss.2010.GunLaw, gss.2010.GunLaw$region == "Mountain")
		Mountain.Sample <- sample(Mountain$FavorGunLaw, size=7, replace=TRUE)
		Mountain.Sample

		# sample 4 cases in "New England"
		New.England <- subset(gss.2010.GunLaw, gss.2010.GunLaw$region == "New England")
		New.England.Sample <- sample(New.England$FavorGunLaw, size=4, replace=TRUE)
		New.England.Sample		

		# sample 14 cases in "Pacific"
		Pacific <- subset(gss.2010.GunLaw, gss.2010.GunLaw$region == "Pacific")
		Pacific.Sample <- sample(Pacific$FavorGunLaw, size=14, replace=TRUE)
		Pacific.Sample

		# sample 22 cases in "South Atlantic"
		South.Atlantic <- subset(gss.2010.GunLaw, gss.2010.GunLaw$region == "South Atlantic")
		South.Atlantic.Sample <- sample(South.Atlantic$FavorGunLaw, size=21, replace=TRUE)
		South.Atlantic.Sample

		# sample 6 cases in "West North Central"
		West.North.Central <- subset(gss.2010.GunLaw, gss.2010.GunLaw$region == "West North Central")
		West.North.Central.Sample <- sample(West.North.Central$FavorGunLaw, size=6, replace=TRUE)
		West.North.Central.Sample

		# sample 11 cases in "West South Central"
		West.South.Central <- subset(gss.2010.GunLaw, gss.2010.GunLaw$region == "West South Central")
		West.South.Central.Sample <- sample(West.South.Central$FavorGunLaw, size=11, replace=TRUE)
		West.South.Central.Sample

		# COMBINE RANDOM SAMPLES FROM EACH STRATA
		Stratified.Sample <- c(East.North.Central.Sample, East.South.Central.Sample, Middle.Atlantic.Sample, Mountain.Sample, New.England.Sample, Pacific.Sample, South.Atlantic.Sample, West.North.Central.Sample, West.South.Central.Sample)
		str(Stratified.Sample)
		
			
### CALCULATE THE PROPORTION OF INDIVIDUALS IN THE SAMPLE THAT FAVOR THE GUN LAW FOR THE ENTIRE POPULATION (GSS 2010)
		Stratified.Sample.Mean <- mean(Stratified.Sample)  
		print(Stratified.Sample.Mean)   ### 0.7575758
		

### CALCULATE THE STANDARD ERROR FOR THE PROPORTION OF INDIVIDUALS IN THE SAMPLE THAT FAVOR THE GUN LAW
		# N = number of observations in the population 
		N <- length(gss.2010$N.Region)
		print(N) ## 4900
		# Nh = number of observations in stratum h of the population
		Nh <- Strata
		print(Nh)
		# nh = number of observations in stratum h of the sample 
		nh <- Strata.Proportion.Rounded
		print(nh)
		# Ph = proportion for each stratum h 
		Ph <- c( mean(East.North.Central.Sample), mean(East.South.Central.Sample), mean(Middle.Atlantic.Sample), mean(Mountain.Sample), mean(New.England.Sample), mean(Pacific.Sample), mean(South.Atlantic.Sample), mean(West.North.Central.Sample), mean(West.South.Central.Sample))
		print(Ph)
		
		# FORMULA:
		SE <- 1 / N * sqrt ( sum ( Nh^2 * ( 1 - ( nh / Nh ) ) * ( ( Ph * ( 1 - Ph ) ) / ( nh - 1 ) ) ) )
		print(SE)  ## 0.04299198


### Sample 100 cases, using regions as strata and each stratum is sampled in proportion to its population size (in this case, the “population” is just the full 2010 GSS)
		# create new data frame with all the regions
		Data.Stratified.Sample <- as.data.frame (Stratified.Sample)
		Regions <- c ( rep("East North Central", 17), rep("East South Central", 6), rep("Middle Atlantic", 13), rep("Mountain", 7), rep("New England", 4), rep("Pacific", 14), rep("South Atlantic", 21), rep("West North Central", 6), rep("West South Central", 11) )
		New.Data.Stratified.Sample <- cbind (Regions, Data.Stratified.Sample)
		# Survey Package
		stratified.sample.data <- svydesign ( id = ~1, strata = ~Regions, data = New.Data.Stratified.Sample)
		# extract the observed data
		svymean(~Stratified.Sample, stratified.sample.data)

###                      mean    SE
### Stratified.Sample 0.75758 0.0434


#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


# (b) Put step (a) above in a loop and do it 100 times.  Check that your estimate is unbiased and that its standard deviation is approximately equal to the average standard error computed in the 100 simulations.

### DETERMINE THE PROPORTION OF SAMPLED CASES WITHIN EACH CLUSTER
		# make a strata variable (numerical values)
		Strata <- table (gss.2010$Region)
		table(Strata)
		# obtain the names of each strata (character)
		StrataNames <- rownames(Strata)
		table(StrataNames)
		# proportion of cases in each strata over the total number of cases in the dataset times 100
		Strata.Proportion <- Strata / sum (Strata) * 100
		table(Strata.Proportion)
		# rounding up the proportion to the lower digit
		Strata.Proportion.Floor <- floor(Strata.Proportion) 
		table(Strata.Proportion.Floor)
		# difference between the proportion and the lower rounding
		Probility.of.higher <- Strata.Proportion - Strata.Proportion.Floor
		table(Probility.of.higher)	
		# the other probability (1 - Probility.of.higher)	
		One.Minus.Probility.of.higher <- 1 - Probility.of.higher
		table(One.Minus.Probility.of.higher)
		# combine strata names with the probabilities of higher
		Region.Probility.of.higher <- cbind(Strata.Proportion, Probility.of.higher, One.Minus.Probility.of.higher)
		Region.Probility.of.higher


##############################################################################################################
#
#                    Strata.Proportion Probility.of.higher One.Minus.Probility.of.higher
# East North Central         17.469388          0.46938776                    0.53061224
# East South Central          5.877551          0.87755102                    0.12244898
# Middle Atlantic            12.979592          0.97959184                    0.02040816
# Mountain                    7.346939          0.34693878                    0.65306122
# New England                 3.714286          0.71428571                    0.28571429
# Pacific                    14.265306          0.26530612                    0.73469388
# South Atlantic             21.346939          0.34693878                    0.65306122
# West North Central          6.061224          0.06122449                    0.93877551
# West South Central         10.938776          0.93877551                    0.06122449
#
##############################################################################################################


### TAKE RANDOM SAMPLES IN EACH STRATA PROPORTIONAL TO THE STRATA SIZES WITH PROBABILITIES

SAMPLE.100 <- function () {
	
		# sample 18 cases in "East North Central" and replicate it 100 times
		East.North.Central.Sample.100 <- sample(East.North.Central$FavorGunLaw, sample(c(17,18), 1, prob = c(Probility.of.higher[1], One.Minus.Probility.of.higher[1])), replace=TRUE)
		table(East.North.Central.Sample.100)
		
		# sample 6 cases in "East South Central" and replicate it 100 times
		East.South.Central.Sample.100 <- sample(East.South.Central$FavorGunLaw, sample(c(5,6), 1, prob = c(Probility.of.higher[2], One.Minus.Probility.of.higher[2])), replace=TRUE)
		East.South.Central.Sample.100

		# sample 13 cases in "Middle Atlantic" and replicate it 100 times
		Middle.Atlantic.Sample.100 <- sample(Middle.Atlantic$FavorGunLaw, sample(c(12,13), 1, prob = c(Probility.of.higher[3], One.Minus.Probility.of.higher[3])), replace=TRUE)
		Middle.Atlantic.Sample.100

		# sample 7 cases in "Mountain" and replicate it 100 times
		Mountain.Sample.100 <- sample(Mountain$FavorGunLaw, sample(c(7,8), 1, prob = c(Probility.of.higher[4], One.Minus.Probility.of.higher[4])), replace=TRUE)
		Mountain.Sample.100

		# sample 4 cases in "New England" and replicate it 100 times
		New.England.Sample.100 <- sample(New.England$FavorGunLaw, sample(c(3,4), 1, prob = c(Probility.of.higher[5], One.Minus.Probility.of.higher[5])), replace=TRUE)
		New.England.Sample.100		

		# sample 14 cases in "Pacific" and replicate it 100 times
		Pacific.Sample.100 <- sample(Pacific$FavorGunLaw, sample(c(14,15), 1, prob = c(Probility.of.higher[6], One.Minus.Probility.of.higher[6])), replace=TRUE)
		Pacific.Sample.100

		# sample 22 cases in "South Atlantic" and replicate it 100 times
		South.Atlantic.Sample.100 <- sample(South.Atlantic$FavorGunLaw, sample(c(21,22), 1, prob = c(Probility.of.higher[7], One.Minus.Probility.of.higher[7])), replace=TRUE)
		South.Atlantic.Sample.100

		# sample 6 cases in "West North Central" and replicate it 100 times
		West.North.Central.Sample.100 <- sample(West.North.Central$FavorGunLaw, sample(c(5,6), 1, prob = c(Probility.of.higher[8], One.Minus.Probility.of.higher[8])), replace=TRUE)
		West.North.Central.Sample.100

		# sample 11 cases in "West South Central" and replicate it 100 times
		West.South.Central.Sample.100 <- sample(West.South.Central$FavorGunLaw, sample(c(10,11), 1, prob = c(Probility.of.higher[9], One.Minus.Probility.of.higher[9])), replace=TRUE)
		West.South.Central.Sample.100


### COMBINE RANDOM SAMPLES FROM EACH STRATA and replicate it 100 times
		Stratified.Sample.100 <- c(East.North.Central.Sample.100, East.South.Central.Sample.100, Middle.Atlantic.Sample.100, Mountain.Sample.100, New.England.Sample.100, Pacific.Sample.100, South.Atlantic.Sample.100, West.North.Central.Sample.100, West.South.Central.Sample.100)
		str(Stratified.Sample.100)

		return(mean(Stratified.Sample.100))
}

	
	
### REPLICATE THE FUNCTION SAMPLE.100 100 times
		Simulate.SAMPLE.100 <- replicate(100, SAMPLE.100())
		
		
### CALCULATE MEAN AND STANDARD DEVIATION
		mean(Simulate.SAMPLE.100)  ## 0.7485273
		sd(Simulate.SAMPLE.100)  ## 0.03977789
		

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

### PROBLEM 4: SIMULATION AND ANALYSIS OF CLUSTER SAMPLE

# Write an R function to take a random subsample of the 2010 General Social survey using occupations as clusters.

# (a) Take a cluster sample in the following way:  first sample 20 occupations at random, then sample 50% of the respondents from each sampled occupation.  From this sample, estimate the proportion of people in the population who favor a law which would require a person to obtain a police permit before he or she could buy a gun.  Compute the standard error of this estimate.

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
		library ("plyr")

### DATA:
		# Data is from the General Social Survey 2010 Cross-Section and Panel Combined
		# SOURCE: http://www.thearda.com/Archive/Files/Downloads/GSS10PAN_DL2.asp
		# read the data into R
		gss.2010 <- read.dta("General Social Survey 2010 Cross-Section and Panel Combined.DTA")
		# how many rows (cases) are in the dataset
		nrow(gss.2010)
		# names of the columns (variable names)
		names(gss.2010)
	
		
### RECODE VARIABLE "gunlaw":
		# table the original variable
		table(gss.2010$gunlaw)
		# recode the "gunlaw" variable
		gss.2010$FavorGunLaw <- NA
		gss.2010$FavorGunLaw[gss.2010$gunlaw =='Favor'] <- 1
		gss.2010$FavorGunLaw[gss.2010$gunlaw == 'Oppose'] <- 0
		# table the newly created variable
		table(gss.2010$FavorGunLaw)
		mode(gss.2010$FavorGunLaw)
		class(gss.2010$FavorGunLaw)		


### OCCUPATIONS VARIABLE:
		# table the original variable
		table(gss.2010$OCC80)		
		mode(gss.2010$OCC80)
		class(gss.2010$OCC80)


### SUBSETTING THE DATA
		# subset() function
		gss.2010.FavorGunLaw <- subset(gss.2010, (gss.2010$FavorGunLaw == 0 | gss.2010$FavorGunLaw == 1))
		table(gss.2010.FavorGunLaw$OCC80)
		# how many rows (cases) are in the subset of data
		nrow(gss.2010.FavorGunLaw)
		# names of the columns in the subset of data (variable names)
		names(gss.2010.FavorGunLaw) 
		

		
### SAMPLE 20 OCCUPATIONS AT RANDOM
		# table original variable
		table(gss.2010.FavorGunLaw$OCC80)
		# obtain the levels for all occupations
		Occupations <- levels (gss.2010.FavorGunLaw$OCC80)
		table(Occupations)
		# eliminate "No Answer" "Inapplicable"
		Occupations <- Occupations[2:381]
		table(Occupations)
		# Create a table with the occupations that have cases		
		Occupations.Table <- ddply ( gss.2010.Occupations, .(OCC80), function(x) sum(!is.na(x$FavorGunLaw)) == 0)
		Occupations.Table
		# Create vector with the occupations that have cases for the FavorGunLaw variable, based on the Occupation.Table variable
		Occupations.with.cases <- c(5, 7, 8, 9, 13, 14, 15, 16, 19, 23, 25, 26, 27, 28, 29, 33, 35, 36, 37, 43, 44, 45, 46, 48, 53, 55, 56, 57, 58, 59, 63, 64, 65, 66, 73, 75, 76, 77, 78, 79, 84, 85, 86, 88, 89, 95, 96, 98, 99, 103, 104, 105, 106, 114, 116, 118, 119, 123, 124, 127, 128, 135, 137, 143, 154, 155, 156, 157, 158, 159, 163, 164, 165, 166, 167, 174, 175, 176, 177, 178, 183, 184, 185, 186, 187, 188, 189, 194, 195, 197, 198, 199, 203, 204, 206, 207, 208, 213, 216, 217, 218, 223, 224, 225, 226, 227, 228, 229, 234, 235, 243, 253, 254, 255, 256, 257, 259, 263, 264, 265, 266, 267, 268, 269, 274, 275, 276, 277, 303, 304, 305, 306, 307, 308, 313, 314, 315, 316, 317, 318, 319, 327, 328, 329, 335, 336, 337, 338, 339, 343, 348, 354, 355, 356, 357, 359, 363, 364, 365, 368, 373, 375, 376, 378, 379, 383, 385, 386, 387, 389, 403, 405, 406, 407, 413, 414, 417, 418, 423, 424, 426, 427, 430, 431, 433, 434, 435, 436, 438, 439, 443, 444, 445, 446, 447, 448, 449, 453, 455, 456, 458, 459, 464, 465, 466, 467, 468, 469, 473, 475, 477, 479, 484, 486, 487, 496, 498, 503, 505, 507, 508, 509, 514, 515, 516, 517, 518, 523, 525, 526, 527, 529, 534, 544, 547, 549, 554, 558, 563, 565, 566, 567, 573, 575, 576, 577, 579, 584, 585, 588, 593, 595, 596, 597, 599, 614, 616, 617, 633, 634, 637, 647, 653, 654, 657, 666, 667, 668, 669, 673, 674, 676, 677, 679, 684, 686, 687, 688, 689, 694, 695, 696, 704, 705, 706, 709, 715, 719, 724, 727, 734, 736, 738, 739, 743, 744, 747, 748, 749, 753, 754, 755, 756, 759, 769, 774, 777, 779, 783, 785, 796, 804, 805, 806, 808, 809, 826, 828, 844, 845, 848, 849, 853, 855, 856, 859, 866, 869, 876, 877, 878, 883, 885, 887, 888, 889)
		Occupations.with.cases
		# take a random sample of 20 clusters (occupations)
		Occupation.Sample <- sample (Occupations.with.cases, size=20, replace=FALSE)
		table(Occupation.Sample)

		
##############################################################################################
#		
# Occupation.Sample
# 19  36  58  77 118 135 137 143 177 274 445 507 508 516 633 667 684 724 749 828
#  
##############################################################################################

### TAKE RANDOM SAMPLES OF 50% RESPONDENTS IN EACH SAMPLED CLUSTER
		
				# sample 50% of respondents in cluster "19" (N=207)
		Cluster1 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "19")
		Cluster1.Sample <- sample(Cluster1$FavorGunLaw, sample(c(103,104), 1, prob = c(.5, .5)), replace=TRUE)
		Cluster1.Sample

				# sample 50% of respondents in cluster "36" (N=9)
		Cluster2 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "36")
		Cluster2.Sample <- sample(Cluster2$FavorGunLaw, sample(c(4,5), 1, prob = c(.5, .5)), replace=TRUE)
		Cluster2.Sample
		
				# sample 50% of respondents in cluster "58" (N=1)
		Cluster3 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "58")
		Cluster3.Sample <- sample(Cluster3$FavorGunLaw, size=1, replace=TRUE)
		Cluster3.Sample

				# sample 50% of respondents in cluster "77" (N=4)
		Cluster4 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "77")
		Cluster4.Sample <- sample(Cluster4$FavorGunLaw, size=2, replace=TRUE)
		Cluster4.Sample

				# sample 50% of respondents in cluster "118" (N=2)
		Cluster5 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "118")
		Cluster5.Sample <- sample(Cluster5$FavorGunLaw, size=1, replace=TRUE)
		Cluster5.Sample		
		
				# sample 50% of respondents in cluster "135" (N=1)
		Cluster6 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "135")
		Cluster6.Sample <- sample(Cluster6$FavorGunLaw, size=1, replace=TRUE)
		Cluster6.Sample
		
				# sample 50% of respondents in cluster "137" (N=5)
		Cluster7 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "137")
		Cluster7.Sample <- sample(Cluster7$FavorGunLaw, sample(c(2,3), 1, prob = c(.5, .5)), replace=TRUE)
		Cluster7.Sample
		
				# sample 50% of respondents in cluster "143" (N=1)
		Cluster8 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "143")
		Cluster8.Sample <- sample(Cluster8$FavorGunLaw, size=1, replace=TRUE)
		Cluster8.Sample
		
				# sample 50% of respondents in cluster "177" (N=6)
		Cluster9 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "177")
		Cluster9.Sample <- sample(Cluster9$FavorGunLaw, size=3, replace=TRUE)
		Cluster9.Sample
		
				# sample 50% of respondents in cluster "274" (N=89)
		Cluster10 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "274")
		Cluster10.Sample <- sample(Cluster10$FavorGunLaw, sample(c(44,45), 1, prob = c(.5, .5)), replace=TRUE)
		Cluster10.Sample

				# sample 50% of respondents in cluster "445" (N=8)
		Cluster11 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "445")
		Cluster11.Sample <- sample(Cluster11$FavorGunLaw, size=4, replace=TRUE)
		Cluster11.Sample
		
				# sample 50% of respondents in cluster "507" (N=2)
		Cluster12 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "507")
		Cluster12.Sample <- sample(Cluster12$FavorGunLaw, size=1, replace=TRUE)
		Cluster12.Sample
				
				# sample 50% of respondents in cluster "508" (N=2)
		Cluster13 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "508")
		Cluster13.Sample <- sample(Cluster13$FavorGunLaw, size=1, replace=TRUE)
		Cluster13.Sample
		
				# sample 50% of respondents in cluster "516" (N=4)
		Cluster14 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "516")
		Cluster14.Sample <- sample(Cluster14$FavorGunLaw, size=2, replace=TRUE)
		Cluster14.Sample
		
				# sample 50% of respondents in cluster "633" (N=26)
		Cluster15 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "633")
		Cluster15.Sample <- sample(Cluster15$FavorGunLaw, size=13, replace=TRUE)
		Cluster15.Sample
		
				# sample 50% of respondents in cluster "667" (N=1)
		Cluster16 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "667")
		Cluster16.Sample <- sample(Cluster16$FavorGunLaw, size=1, replace=TRUE)
		Cluster16.Sample
		
				# sample 50% of respondents in cluster "684" (N=1)
		Cluster17 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "684")
		Cluster17.Sample <- sample(Cluster17$FavorGunLaw, size=1, replace=TRUE)
		Cluster17.Sample
		
				# sample 50% of respondents in cluster "724" (N=1)
		Cluster18 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "724")
		Cluster18.Sample <- sample(Cluster18$FavorGunLaw, size=1, replace=TRUE)
		Cluster18.Sample
		
				# sample 50% of respondents in cluster "749" (N=1)
		Cluster19 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "749")
		Cluster19.Sample <- sample(Cluster19$FavorGunLaw, size=1, replace=TRUE)
		Cluster19.Sample
		
				# sample 50% of respondents in cluster "828" (N=1)
		Cluster20 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "828")
		Cluster20.Sample <- sample(Cluster20$FavorGunLaw, size=1, replace=TRUE)
		Cluster20.Sample
		
### COMBINE RANDOM SAMPLES FROM EACH CLUSTER
		Clustered.Sample <- c(Cluster1.Sample, Cluster2.Sample, Cluster3.Sample, Cluster4.Sample, Cluster5.Sample, Cluster6.Sample, Cluster7.Sample, Cluster8.Sample, Cluster9.Sample, Cluster10.Sample, Cluster11.Sample, Cluster12.Sample, Cluster13.Sample, Cluster14.Sample, Cluster15.Sample, Cluster16.Sample, Cluster17.Sample, Cluster18.Sample, Cluster19.Sample, Cluster20.Sample)
		str(Clustered.Sample) ## 189 cases
		
  
### CALCULATE THE POPULATION PROPORTION BY HAND
		# N = number of clusters in the population
		N <- 381
		print(N)
		# n = number of clusters in the sample
		n <- 20
		print(n)
		# M = number of observations in the population
		Mi <- c( 207, 9, 1, 4, 2, 1, 5, 1, 6, 89, 8, 2, 2, 4, 26, 1, 1, 1, 1, 1)
		print(Mi)
		M <- sum(Mi)
		print(M)
		# P = proportion
		P <- c( mean(Cluster1.Sample), mean(Cluster2.Sample), mean(Cluster3.Sample), mean(Cluster4.Sample), mean(Cluster5.Sample), mean(Cluster6.Sample), mean(Cluster7.Sample), mean(Cluster8.Sample), mean(Cluster9.Sample), mean(Cluster10.Sample), mean(Cluster11.Sample), mean(Cluster12.Sample), mean(Cluster13.Sample), mean(Cluster14.Sample), mean(Cluster15.Sample), mean(Cluster16.Sample), mean(Cluster17.Sample), mean(Cluster18.Sample), mean(Cluster19.Sample), mean(Cluster20.Sample))
		print(P)
		# FORMULA:
		Population.Proportion <- ( N / ( N * M ) ) * ( sum( Mi * P ) )
		print(Population.Proportion) ### 0.7386622

		
### CALCULATE THE STANDARD ERROR OF THE POPULATION PROPORTION BY HAND
		# N = number of clusters in the population
		N <- 381
		print(N)
		# n = number of clusters in the sample
		n <- 20
		print(n)
		# M = number of observations in the population
		Mi <- c( 207, 9, 1, 4, 2, 1, 5, 1, 6, 89, 8, 2, 2, 4, 26, 1, 1, 1, 1, 1)
		print(Mi)
		M <- sum(Mi)
		print(M)
		# m = number of observations in the sample
		mi <- c( 104, 4, 1, 2, 1, 1, 2, 1, 3, 44, 4, 1, 1, 2, 13, 1, 1, 1, 1, 1 )
		print(mi)
		m <- sum(mi)
		print(m)
		# P = proportion
		Pi <- c( mean(Cluster1.Sample), mean(Cluster2.Sample), mean(Cluster3.Sample), mean(Cluster4.Sample), mean(Cluster5.Sample), mean(Cluster6.Sample), mean(Cluster7.Sample), mean(Cluster8.Sample), mean(Cluster9.Sample), mean(Cluster10.Sample), mean(Cluster11.Sample), mean(Cluster12.Sample), mean(Cluster13.Sample), mean(Cluster14.Sample), mean(Cluster15.Sample), mean(Cluster16.Sample), mean(Cluster17.Sample), mean(Cluster18.Sample), mean(Cluster19.Sample), mean(Cluster20.Sample))
		print(Pi)
		# tprop = the sample estimate of the number of successes in the population
		tprop <- Population.Proportion
		print(tprop)
		# FORMULA:
		SE <- ( 1 / M ) * sqrt ( ( N^2 ) * ( (1 - (n/N)) / n ) * ( sum( ( (Mi * (Pi - (tprop / N)))^2 ) / (n - 1) ) + ( N / n ) ) * sum( (1 - (mi / Mi)) * (Mi^2) * Pi * ( (1 - Pi) / (mi - 1) ) ) )  
		print(SE) ### 	
		
			
### CALCULATE THE POPULATION PROPORTION AND THE STANDARD ERROR FOR THE PROPORTION ESTIMATE 
		# Survey Package
		# create new dataset
		Data.Clustered.Sample <- as.data.frame (Clustered.Sample)
		Occupations <- c ( rep("Cluster1", 104), rep("Cluster2", 4), rep("Cluster3", 1), rep("Cluster4", 2), rep("Cluster5", 1), rep("Cluster6", 1), rep("Cluster7", 2), rep("Cluster 8", 1), rep("Cluster9", 3), rep("Cluster10", 44), rep("Cluster11", 4), rep("Cluster12", 1), rep("Cluster13", 1), rep("Cluster14", 2), rep("Cluster15", 13), rep("Cluster16", 1), rep("Cluster17", 1), rep("Cluster18", 1), rep("Cluster19", 1), rep("Cluster20", 1) )
		New.Data.Clustered.Sample <- cbind (Occupations, Data.Clustered.Sample)
		# Survey Package
		clustered.sample.data <- svydesign ( id = ~1, data = New.Data.Clustered.Sample)
		# extract the observed data
		svymean (~Clustered.Sample, clustered.sample.data)	
		
		
## 		                    mean     SE
##        Clustered.Sample 0.73545 0.0322

			
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


# (b) Repeat (a), but this time taking the sample as follows:  first sample 20 occupations at random, then sample 5 people from each sampled occupation (or, if there are fewer then 5 people with that occupation category, sample all of them).  Again get an estimate and standard error for the gun control question.

# take a random sample of 20 clusters (occupations)
		Occupation.Sample <- sample (Occupations.with.cases, size=20, replace=FALSE)
		table(Occupation.Sample)
		
##############################################################################################
#		
# Occupation.Sample
# 5  26  29  35  89 137 178 187 226 235 253 255 303 386 435 473 509 877 878 885
#  
##############################################################################################

### TAKE RANDOM SAMPLES OF 5 RESPONDENTS IN EACH SAMPLED CLUSTER
		
				# sample 5 respondents in cluster "5" (N=16)
		Cluster1 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "5")
		Cluster1.Sample <- sample(Cluster1$FavorGunLaw, size=5, replace=TRUE)
		Cluster1.Sample

				# sample 5 respondents in cluster "26" (N=15)
		Cluster2 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "26")
		Cluster2.Sample <- sample(Cluster2$FavorGunLaw, size=5, replace=TRUE)
		Cluster2.Sample
		
				# sample 5 respondents in cluster "29" (N=1)
		Cluster3 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "29")
		Cluster3.Sample <- sample(Cluster3$FavorGunLaw, size=1, replace=TRUE)
		Cluster3.Sample

				# sample 5 respondents in cluster "35" (N=1)
		Cluster4 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "35")
		Cluster4.Sample <- sample(Cluster4$FavorGunLaw, size=1, replace=TRUE)
		Cluster4.Sample

				# sample 5 respondents in cluster "89" (N=2)
		Cluster5 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "89")
		Cluster5.Sample <- sample(Cluster5$FavorGunLaw, size=2, replace=TRUE)
		Cluster5.Sample		
		
				# sample 5 respondents in cluster "137" (N=5)
		Cluster6 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "137")
		Cluster6.Sample <- sample(Cluster6$FavorGunLaw, size=5, replace=TRUE)
		Cluster6.Sample
		
				# sample 5 respondents in cluster "178" (N=14)
		Cluster7 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "178")
		Cluster7.Sample <- sample(Cluster7$FavorGunLaw, size=5, replace=TRUE)
		Cluster7.Sample
		
				# sample 5 respondents in cluster "187" (N=7)
		Cluster8 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "187")
		Cluster8.Sample <- sample(Cluster8$FavorGunLaw, size=5, replace=TRUE)
		Cluster8.Sample
		
				# sample 5 respondents in cluster "226" (N=3)
		Cluster9 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "226")
		Cluster9.Sample <- sample(Cluster9$FavorGunLaw, size=3, replace=TRUE)
		Cluster9.Sample
		
				# sample 5 respondents in cluster "235" (N=31)
		Cluster10 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "235")
		Cluster10.Sample <- sample(Cluster10$FavorGunLaw, size=5, replace=TRUE)
		Cluster10.Sample

				# sample 5 respondents in cluster "253" (N=13)
		Cluster11 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "253")
		Cluster11.Sample <- sample(Cluster11$FavorGunLaw, size=5, replace=TRUE)
		Cluster11.Sample
		
				# sample 5 respondents in cluster "255" (N=8)
		Cluster12 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "255")
		Cluster12.Sample <- sample(Cluster12$FavorGunLaw, size=5, replace=TRUE)
		Cluster12.Sample
				
				# sample 5 respondents in cluster "303" (N=18)
		Cluster13 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "303")
		Cluster13.Sample <- sample(Cluster13$FavorGunLaw, size=5, replace=TRUE)
		Cluster13.Sample
		
				# sample 5 respondents in cluster "386" (N=2)
		Cluster14 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "386")
		Cluster14.Sample <- sample(Cluster14$FavorGunLaw, size=2, replace=TRUE)
		Cluster14.Sample
		
				# sample 5 respondents in cluster "435" (N=53)
		Cluster15 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "435")
		Cluster15.Sample <- sample(Cluster15$FavorGunLaw, size=5, replace=TRUE)
		Cluster15.Sample
		
				# sample 5 respondents in cluster "473" (N=19)
		Cluster16 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "473")
		Cluster16.Sample <- sample(Cluster16$FavorGunLaw, size=5, replace=TRUE)
		Cluster16.Sample
		
				# sample 5 respondents in cluster "509" (N=2)
		Cluster17 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "509")
		Cluster17.Sample <- sample(Cluster17$FavorGunLaw, size=2, replace=TRUE)
		Cluster17.Sample
		
				# sample 5 respondents in cluster "877" (N=15)
		Cluster18 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "877")
		Cluster18.Sample <- sample(Cluster18$FavorGunLaw, size=5, replace=TRUE)
		Cluster18.Sample
		
				# sample 5 respondents in cluster "878" (N=1)
		Cluster19 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "878")
		Cluster19.Sample <- sample(Cluster19$FavorGunLaw, size=1, replace=TRUE)
		Cluster19.Sample
		
				# sample 5 respondents in cluster "885" (N=2)
		Cluster20 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "885")
		Cluster20.Sample <- sample(Cluster20$FavorGunLaw, size=2, replace=TRUE)
		Cluster20.Sample
		
### COMBINE RANDOM SAMPLES FROM EACH CLUSTER
		Clustered.Sample <- c(Cluster1.Sample, Cluster2.Sample, Cluster3.Sample, Cluster4.Sample, Cluster5.Sample, Cluster6.Sample, Cluster7.Sample, Cluster8.Sample, Cluster9.Sample, Cluster10.Sample, Cluster11.Sample, Cluster12.Sample, Cluster13.Sample, Cluster14.Sample, Cluster15.Sample, Cluster16.Sample, Cluster17.Sample, Cluster18.Sample, Cluster19.Sample, Cluster20.Sample)
		str(Clustered.Sample) ## 74 cases
		
### CALCULATE THE POPULATION PROPORTION BY HAND
		# N = number of clusters in the population
		N <- 381
		print(N)
		# n = number of clusters in the sample
		n <- 20
		print(n)
		# M = number of observations in the population
		Mi <- c( 16, 15, 1, 1, 2, 5, 14, 7, 3, 31, 13, 8, 18, 2, 53, 19, 1, 15, 1, 2)
		print(Mi)
		M <- sum(Mi)
		print(M)
		# P = proportion
		P <- c( mean(Cluster1.Sample), mean(Cluster2.Sample), mean(Cluster3.Sample), mean(Cluster4.Sample), mean(Cluster5.Sample), mean(Cluster6.Sample), mean(Cluster7.Sample), mean(Cluster8.Sample), mean(Cluster9.Sample), mean(Cluster10.Sample), mean(Cluster11.Sample), mean(Cluster12.Sample), mean(Cluster13.Sample), mean(Cluster14.Sample), mean(Cluster15.Sample), mean(Cluster16.Sample), mean(Cluster17.Sample), mean(Cluster18.Sample), mean(Cluster19.Sample), mean(Cluster20.Sample))
		print(P)
		# FORMULA:
		Population.Proportion <- ( N / ( N * M ) ) * ( sum( Mi * P ) )
		print(Population.Proportion) ### 0.746696
		
		
### CALCULATE THE POPULATION PROPORTION AND THE STANDARD ERROR FOR THE PROPORTION ESTIMATE 
		# Survey Package
		# create new dataset
		Data.Clustered.Sample <- as.data.frame (Clustered.Sample)
		Occupations <- c ( rep("Cluster1", 5), rep("Cluster2", 5), rep("Cluster3", 1), rep("Cluster4", 1), rep("Cluster5", 2), rep("Cluster6", 5), rep("Cluster7", 5), rep("Cluster 8", 5), rep("Cluster9", 3), rep("Cluster10", 5), rep("Cluster11", 5), rep("Cluster12", 5), rep("Cluster13", 5), rep("Cluster14", 2), rep("Cluster15", 5), rep("Cluster16", 5), rep("Cluster17", 2), rep("Cluster18", 5), rep("Cluster19", 1), rep("Cluster20", 2) )
		New.Data.Clustered.Sample <- cbind (Occupations, Data.Clustered.Sample)
		# Survey Package
		clustered.sample.data <- svydesign ( id = ~1, data = New.Data.Clustered.Sample)
		# extract the observed data
		svymean (~Clustered.Sample, clustered.sample.data)	
		
		
## 		                    mean     SE
##        Clustered.Sample 0.74324 0.0511

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

# (c) Repeat (a), but this time first sample 20 occupations with probability proportional to size, then sample 5 from each sampled occupation (or, if there are fewer then 5 people with that occupation category, sample all of them).  Again get an estimate and standard error for the gun control question

### DETERMINE THE PROPORTION OF CASES WITHIN EACH CLUSTER
		# make a cluster variable (numerical values)
		Cluster <- table (gss.2010.FavorGunLaw$OCC80)
		table(Cluster)
		# obtain the names of each strata (character)
		ClusterNames <- rownames(Cluster)
		table(ClusterNames)
		# proportion of cases in each strata over the total number of cases in the dataset times 100
		Cluster.Proportion <- Cluster / sum (Cluster) * 100
		table(Cluster.Proportion)
		# round the percentage proportions
		Cluster.Proportion.Rounded <- round(Cluster.Proportion, digits=0)
		table(Cluster.Proportion.Rounded)
		# combine strata names with the rounded strata proportions
		Occupation.Percentage.Rounded <- cbind(ClusterNames, Cluster.Proportion.Rounded)
		Occupation.Percentage.Rounded
		Occupations.with.cases <- c(5, 7, 8, 9, 13, 14, 15, 16, 19, 23, 25, 26, 27, 28, 29, 33, 35, 36, 37, 43, 44, 45, 46, 48, 53, 55, 56, 57, 58, 59, 63, 64, 65, 66, 73, 75, 76, 77, 78, 79, 84, 85, 86, 88, 89, 95, 96, 98, 99, 103, 104, 105, 106, 114, 116, 118, 119, 123, 124, 127, 128, 135, 137, 143, 154, 155, 156, 157, 158, 159, 163, 164, 165, 166, 167, 174, 175, 176, 177, 178, 183, 184, 185, 186, 187, 188, 189, 194, 195, 197, 198, 199, 203, 204, 206, 207, 208, 213, 216, 217, 218, 223, 224, 225, 226, 227, 228, 229, 234, 235, 243, 253, 254, 255, 256, 257, 259, 263, 264, 265, 266, 267, 268, 269, 274, 275, 276, 277, 303, 304, 305, 306, 307, 308, 313, 314, 315, 316, 317, 318, 319, 327, 328, 329, 335, 336, 337, 338, 339, 343, 348, 354, 355, 356, 357, 359, 363, 364, 365, 368, 373, 375, 376, 378, 379, 383, 385, 386, 387, 389, 403, 405, 406, 407, 413, 414, 417, 418, 423, 424, 426, 427, 430, 431, 433, 434, 435, 436, 438, 439, 443, 444, 445, 446, 447, 448, 449, 453, 455, 456, 458, 459, 464, 465, 466, 467, 468, 469, 473, 475, 477, 479, 484, 486, 487, 496, 498, 503, 505, 507, 508, 509, 514, 515, 516, 517, 518, 523, 525, 526, 527, 529, 534, 544, 547, 549, 554, 558, 563, 565, 566, 567, 573, 575, 576, 577, 579, 584, 585, 588, 593, 595, 596, 597, 599, 614, 616, 617, 633, 634, 637, 647, 653, 654, 657, 666, 667, 668, 669, 673, 674, 676, 677, 679, 684, 686, 687, 688, 689, 694, 695, 696, 704, 705, 706, 709, 715, 719, 724, 727, 734, 736, 738, 739, 743, 744, 747, 748, 749, 753, 754, 755, 756, 759, 769, 774, 777, 779, 783, 785, 796, 804, 805, 806, 808, 809, 826, 828, 844, 845, 848, 849, 853, 855, 856, 859, 866, 869, 876, 877, 878, 883, 885, 887, 888, 889)
		Occupations.with.cases
		Occupation.weights <- c(1, 0, 0, 0, 1, 1, 0, 0, 6, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 3, 0, 2, 0, 1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 1, 2, 0, 1, 2, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1)
		Occupation.weights
		# take a sample of 20 occupations with probability proportional to size
		Occupation.Sample <- sample (Occupations.with.cases, size=20, prob=Occupation.weights, replace=FALSE)
		table(Occupation.Sample)


		
##############################################################################################
#		
# Occupation.Sample
# 5  14  19  95 155 156 174 203 243 254 274 418 435 446 447 453 458 486 777 804
#  
##############################################################################################


### TAKE RANDOM SAMPLES OF 5 RESPONDENTS IN EACH SAMPLED CLUSTER PROPORTIONAL TO THE CLUSTER SIZES
		
				# sample 5 respondents in cluster "5" (N=16)
		Cluster1 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "5")
		Cluster1.Sample <- sample(Cluster1$FavorGunLaw, size=5, replace=TRUE)
		Cluster1.Sample

				# sample 5 respondents in cluster "14" (N=16)
		Cluster2 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "14")
		Cluster2.Sample <- sample(Cluster2$FavorGunLaw, size=5, replace=TRUE)
		Cluster2.Sample
		
				# sample 5 respondents in cluster "19" (N=207)
		Cluster3 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "19")
		Cluster3.Sample <- sample(Cluster3$FavorGunLaw, size=5, replace=TRUE)
		Cluster3.Sample

				# sample 5 respondents in cluster "95" (N=67)
		Cluster4 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "95")
		Cluster4.Sample <- sample(Cluster4$FavorGunLaw, size=5, replace=TRUE)
		Cluster4.Sample

				# sample 5 respondents in cluster "155" (N=23)
		Cluster5 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "155")
		Cluster5.Sample <- sample(Cluster5$FavorGunLaw, size=5, replace=TRUE)
		Cluster5.Sample		
		
				# sample 5 respondents in cluster "156" (N=77)
		Cluster6 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "156")
		Cluster6.Sample <- sample(Cluster6$FavorGunLaw, size=5, replace=TRUE)
		Cluster6.Sample
		
				# sample 5 respondents in cluster "174" (N=27)
		Cluster7 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "174")
		Cluster7.Sample <- sample(Cluster7$FavorGunLaw, size=5, replace=TRUE)
		Cluster7.Sample
		
				# sample 5 respondents in cluster "203" (N=16)
		Cluster8 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "203")
		Cluster8.Sample <- sample(Cluster8$FavorGunLaw, size=5, replace=TRUE)
		Cluster8.Sample
		
				# sample 5 respondents in cluster "243" (N=52)
		Cluster9 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "243")
		Cluster9.Sample <- sample(Cluster9$FavorGunLaw, size=5, replace=TRUE)
		Cluster9.Sample
		
				# sample 5 respondents in cluster "254" (N=20)
		Cluster10 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "254")
		Cluster10.Sample <- sample(Cluster10$FavorGunLaw, size=5, replace=TRUE)
		Cluster10.Sample

				# sample 5 respondents in cluster "274" (N=89)
		Cluster11 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "274")
		Cluster11.Sample <- sample(Cluster11$FavorGunLaw, size=5, replace=TRUE)
		Cluster11.Sample
		
				# sample 5 respondents in cluster "418" (N=21)
		Cluster12 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "418")
		Cluster12.Sample <- sample(Cluster12$FavorGunLaw, size=5, replace=TRUE)
		Cluster12.Sample
				
				# sample 5 respondents in cluster "435" (N=53)
		Cluster13 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "435")
		Cluster13.Sample <- sample(Cluster13$FavorGunLaw, size=5, replace=TRUE)
		Cluster13.Sample
		
				# sample 5 respondents in cluster "446" (N=23)
		Cluster14 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "446")
		Cluster14.Sample <- sample(Cluster14$FavorGunLaw, size=5, replace=TRUE)
		Cluster14.Sample
		
				# sample 5 respondents in cluster "447" (N=48)
		Cluster15 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "447")
		Cluster15.Sample <- sample(Cluster15$FavorGunLaw, size=5, replace=TRUE)
		Cluster15.Sample
		
				# sample 5 respondents in cluster "453" (N=65)
		Cluster16 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "453")
		Cluster16.Sample <- sample(Cluster16$FavorGunLaw, size=5, replace=TRUE)
		Cluster16.Sample
		
				# sample 5 respondents in cluster "458" (N=21)
		Cluster17 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "458")
		Cluster17.Sample <- sample(Cluster17$FavorGunLaw, size=5, replace=TRUE)
		Cluster17.Sample
		
				# sample 5 respondents in cluster "486" (N=16)
		Cluster18 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "486")
		Cluster18.Sample <- sample(Cluster18$FavorGunLaw, size=5, replace=TRUE)
		Cluster18.Sample
		
				# sample 5 respondents in cluster "777" (N=17)
		Cluster19 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "777")
		Cluster19.Sample <- sample(Cluster19$FavorGunLaw, size=5, replace=TRUE)
		Cluster19.Sample
		
				# sample 5 respondents in cluster "804" (N=44)
		Cluster20 <- subset(gss.2010.Occupations, gss.2010.FavorGunLaw$OCC80 == "804")
		Cluster20.Sample <- sample(Cluster20$FavorGunLaw, size=5, replace=TRUE)
		Cluster20.Sample
		

### COMBINE RANDOM SAMPLES FROM EACH CLUSTER
		Clustered.Sample <- c(Cluster1.Sample, Cluster2.Sample, Cluster3.Sample, Cluster4.Sample, Cluster5.Sample, Cluster6.Sample, Cluster7.Sample, Cluster8.Sample, Cluster9.Sample, Cluster10.Sample, Cluster11.Sample, Cluster12.Sample, Cluster13.Sample, Cluster14.Sample, Cluster15.Sample, Cluster16.Sample, Cluster17.Sample, Cluster18.Sample, Cluster19.Sample, Cluster20.Sample)
		str(Clustered.Sample) ## 100 cases
		
  
### CALCULATE THE POPULATION PROPORTION BY HAND
		# N = number of clusters in the population
		N <- 381
		print(N)
		# n = number of clusters in the sample
		n <- 20
		print(n)
		# M = number of observations in the population
		Mi <- c( 16, 16, 207, 67, 23, 77, 27, 16, 52, 20, 89, 21, 53, 23, 48, 65, 21, 16, 17, 44)
		print(Mi)
		M <- sum(Mi)
		print(M)
		# P = proportion
		P <- c( mean(Cluster1.Sample), mean(Cluster2.Sample), mean(Cluster3.Sample), mean(Cluster4.Sample), mean(Cluster5.Sample), mean(Cluster6.Sample), mean(Cluster7.Sample), mean(Cluster8.Sample), mean(Cluster9.Sample), mean(Cluster10.Sample), mean(Cluster11.Sample), mean(Cluster12.Sample), mean(Cluster13.Sample), mean(Cluster14.Sample), mean(Cluster15.Sample), mean(Cluster16.Sample), mean(Cluster17.Sample), mean(Cluster18.Sample), mean(Cluster19.Sample), mean(Cluster20.Sample))
		print(P)
		# FORMULA:
		Population.Proportion <- ( N / ( N * M ) ) * ( sum( Mi * P ) )
		print(Population.Proportion) ### 0.6562092
		
		
### CALCULATE THE POPULATION PROPORTION AND THE STANDARD ERROR FOR THE PROPORTION ESTIMATE 
		# Survey Package
		# create new dataset
		Data.Clustered.Sample <- as.data.frame (Clustered.Sample)
		Occupations <- c ( rep("Cluster1", 5), rep("Cluster2", 5), rep("Cluster3", 5), rep("Cluster4", 5), rep("Cluster5", 5), rep("Cluster6", 5), rep("Cluster7", 5), rep("Cluster 8", 5), rep("Cluster9", 5), rep("Cluster10", 5), rep("Cluster11", 5), rep("Cluster12", 5), rep("Cluster13", 5), rep("Cluster14", 5), rep("Cluster15", 5), rep("Cluster16", 5), rep("Cluster17", 5), rep("Cluster18", 5), rep("Cluster19", 5), rep("Cluster20", 5) )
		New.Data.Clustered.Sample <- cbind (Occupations, Data.Clustered.Sample)
		# Survey Package
		clustered.sample.data <- svydesign ( id = ~1, data = New.Data.Clustered.Sample)
		# extract the observed data
		svymean (~Clustered.Sample, clustered.sample.data)	
		
		
## 		                    mean     SE
##        Clustered.Sample   0.7   0.0461

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 12 
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


### PROBLEM 3 - MULTILEVEL MODELING

# From the Pollster data, estimate a time series of presidential approval, adjusting for house effects and then smoothing the curve using some function such as lowess.  Compare to the smoothed average of the unadjusted approval numbers from this series and comment on any differences.

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
library ("lme4")
library ("car")
library ("gplots")
library ("sm")
library ("lattice")
library ("base")
library ("ggplot2")
library ("plyr")


### DATA:
# Data is from Polls 
# read the data into R
pollster <- read.csv(file = "Pollster_Data.csv", header = TRUE)
# how many rows (cases) are in the dataset
nrow(pollster) # 589
# names of the columns (variable names)
names(pollster) # "Pollster", "Start.Date", "End.Date", "Entry.Date.Time..ET.", "Number.of.Observations", "Population", "Mode", "Obama", "Romney", "Undecided", "Pollster.URL", "Source.URL"


### End.Date
# table the original variable
table(pollster$End.Date)
mode(pollster$End.Date)
class(pollster$End.Date)

# recode End.Date.Character as month/day/year
pollster$End.Date.New <- as.Date(pollster$End.Date, "%m/%d/%y")
# table new variable
table(pollster$End.Date.New)
mode(pollster$End.Date.New)
class(pollster$End.Date.New)

# recode End.Date as numeric variable
pollster$End.Date.Numeric <- as.numeric(pollster$End.Date.New)
# table new variable
table(pollster$End.Date.Numeric)
mode(pollster$End.Date.Numeric)
class(pollster$End.Date.Numeric)

# recode End.Date as factor variable
pollster$End.Date.Factor <- factor(pollster$End.Date.New)
# table new variable
table(pollster$End.Date.Factor)
mode(pollster$End.Date.Factor)
class(pollster$End.Date.Factor)


### Pollster
# table the original variable
table(pollster$Pollster)
mode(pollster$Pollster)
class(pollster$Pollster)
# recode Pollster as factor variable
pollster$House <- factor(pollster$Pollster)
# table new variable
table(pollster$House)
mode(pollster$House)
class(pollster$House)


### Obama
# table the original variable
table(pollster$Obama)
mode(pollster$Obama)
class(pollster$Obama)
# recode Obama as numeric variable
pollster$Obama <- as.numeric(pollster$Obama)
# table new variable
table(pollster$Obama)
mode(pollster$Obama)
class(pollster$Obama)


#################################################################################################################################################################

### MODEL WITH CONTROLS
MyModelWithControls <- with (pollster,  lmer(Obama ~ 1 + End.Date.Numeric + (1 | House) + (1 | End.Date.Factor), data = pollster))
summary(MyModelWithControls)
display(MyModelWithControls)


### Random effects

# obtain random effects for "House" 
House.Effects <- data.frame ( ranef(MyModelWithControls)$House )
nrow(House.Effects)
names(House.Effects)
# create dataset with 2 columns
ncol(House.Effects)
House.Effects <- data.frame ( rownames(House.Effects), (House.Effects) )
ncol(House.Effects)
nrow(House.Effects)
# give rownames to House.Effects in the dataset
names(House.Effects)[1] <- "House"
names(House.Effects)[2] <- "House.Effects"
names(House.Effects)
# join the House.Effects dataset to pollster dataset
pollster <- join (pollster, House.Effects, by = "House")
nrow(pollster)
names(pollster)


# obtain random effects for "End.Date.Factor" 
Date.Effects <- data.frame ( ranef(MyModelWithControls)$End.Date.Factor )
nrow(Date.Effects)
names(Date.Effects)
# create dataset with 2 columns
ncol(Date.Effects)
Date.Effects <- data.frame ( rownames(Date.Effects), (Date.Effects) )
ncol(Date.Effects)
nrow(Date.Effects)
# give rownames to Data.Effects in the dataset
names(Date.Effects)[1] <- "End.Date.Factor"
names(Date.Effects)[2] <- "Date.Effects"
names(Date.Effects)
# join the Data.Effects dataset to pollster dataset
pollster <- join (pollster, Date.Effects, by = "End.Date.Factor")
nrow(pollster)
names(pollster)


# obtain the intercept of fixed effects
fixef(MyModelWithControls)
pollster$intercept <- fixef(MyModelWithControls)[1]
table(pollster$intercept)
nrow(pollster)
names(pollster)


# obtain the tine fixed effect
fixef(MyModelWithControls)
pollster$time.fixed.effect <- fixef(MyModelWithControls)[2]
table(pollster$time.fixed.effect)
nrow(pollster)
names(pollster)


### Polster with control
pollster$cell.prediction.with.control <- with (data = pollster, intercept + (time.fixed.effect * End.Date.Numeric) + Date.Effects)
table(pollster$cell.prediction.with.control)


#################################################################################################################################################################


### MODEL WITHOUT CONTROLS
MyModelWithoutControls <- with (pollster,  lmer(Obama ~ 1 + End.Date.Numeric + (1 | End.Date.Factor), data = pollster))
summary(MyModelWithoutControls)
display(MyModelWithoutControls)


### Random effects

# obtain random effects for "End.Date.Factor" 
Date.Effects.Unadjusted <- data.frame ( ranef(MyModelWithoutControls)$End.Date.Factor )
nrow(Date.Effects.Unadjusted)
names(Date.Effects.Unadjusted)
# create dataset with 2 columns
ncol(Date.Effects.Unadjusted)
Date.Effects.Unadjusted <- data.frame ( rownames(Date.Effects.Unadjusted), (Date.Effects.Unadjusted) )
ncol(Date.Effects.Unadjusted)
nrow(Date.Effects.Unadjusted)
# give rownames to Data.Effects in the dataset
names(Date.Effects.Unadjusted)[1] <- "End.Date.Factor"
names(Date.Effects.Unadjusted)[2] <- "Date.Effects.Unadjusted"
names(Date.Effects.Unadjusted)
# join the Data.Effects dataset to pollster dataset
pollster <- join (pollster, Date.Effects.Unadjusted, by = "End.Date.Factor")
nrow(pollster)
names(pollster)


# obtain the intercept of fixed effects
fixef(MyModelWithoutControls)
pollster$intercept.unadjusted <- fixef(MyModelWithoutControls)[1]
table(pollster$intercept.unadjusted)
nrow(pollster)
names(pollster)


# obtain the tine fixed effect
fixef(MyModelWithoutControls)
pollster$time.fixed.effect.unadjusted <- fixef(MyModelWithoutControls)[2]
table(pollster$time.fixed.effect.unadjusted)
nrow(pollster)
names(pollster)


### Polster without control
pollster$cell.prediction.without.control <- with (data = pollster, intercept.unadjusted + (time.fixed.effect.unadjusted * End.Date.Numeric) + Date.Effects.Unadjusted)
table(pollster$cell.prediction.without.control)


#################################################################################################################################################################


### Plot ggplo2 - templete
# ggplot (data = , aes() + geom_point() + stat_smooth() )
# stat_smooth() function for LINE
# geom_point() functions for POINTS


### Format plot
png("hwk12_problem3.png", height = 1000, width = 1400)


### Plot with control and points
g1 <- ggplot (data = pollster, 
        aes (x = End.Date.New, color = "Type of curve")) +
        stat_smooth (aes (y = pollster$cell.prediction.with.control, color = "Adjusted") ) +
        geom_point(aes (y = pollster$cell.prediction.with.control, color = "Adjusted", pch = 50), shape = 1 ) +
        xlab("Date") + ylab("% Obama") + ggtitle("Lowess curve adjusted for house effects and data points")


### Plot with no control and points
g2 <- ggplot (data = pollster, 
        aes (x = End.Date.New, color = "Type of curve")) +
        stat_smooth (aes (y = pollster$cell.prediction.without.control, color = "Unadjusted") ) +
        geom_point(aes (y = pollster$cell.prediction.without.control, color = "Unadjusted", pch = 50), shape = 1 )  +
        xlab("Date") + ylab("% Obama") + ggtitle("Lowess curve unadjusted for house effects and data points")


### Plot with control, no control and points
g3 <- ggplot (data = pollster, 
        aes (x = End.Date.New, color = "Type of curve")) +
        stat_smooth (aes (y = pollster$cell.prediction.with.control, color = "Adjusted") ) +
        geom_point(aes (y = pollster$cell.prediction.with.control, color = "Adjusted", pch = 50), shape = 1 ) +
        stat_smooth (aes (y = pollster$cell.prediction.without.control, color = "Unadjusted") ) +
        geom_point(aes (y = pollster$cell.prediction.without.control, color = "Unadjusted", pch = 50), shape = 1 )  +
        xlab("Date") + ylab("% Obama") + ggtitle("Adjusted and Unadjusted lowess curves for house effects and data points")


### Plot with control, no control and points
g4 <- ggplot (data = pollster, 
        aes (x = End.Date.New, color = "Type of curve")) +
        stat_smooth (aes (y = pollster$cell.prediction.with.control, colour = "Adjusted") ) +
        stat_smooth (aes (y = pollster$cell.prediction.without.control, colour = "Unadjusted") )  +
        xlab("Date") + ylab("% Obama") + ggtitle("Adjusted and Unadjusted lowess curves for house effects")


### Create multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(g1, g3, g2, g4, cols=2)

### THE END
dev.off()


#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


### PROBLEM 4 - MULTILEVEL REGRESSION AND POSTSTRATIFICATION

# Download the cumulative National Election Study.


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
library ("lme4")
library ("car")
library ("gplots")
library ("sm")
library ("lattice")
library ("base")
library ("ggplot2")
library ("plyr")


### DATA:
# Data is from ANES Time Series Cumulative Data File
# read the data into R
anes.cumulative <- read.dta(file = "anes_cdf.dta")
# how many rows (cases) are in the dataset
nrow(anes.cumulative) # 49760
# names of the columns (variable names)
names(anes.cumulative) 


### RACE:
# table the original variable
table(anes.cumulative$VCF0106a)
mode(anes.cumulative$VCF0106a)
class(anes.cumulative$VCF0106a)
# recode and create new variable    			  
anes.cumulative$RaceWBHO <- NA
anes.cumulative$RaceWBHO[anes.cumulative$VCF0106a == '1. White'] <- 1
anes.cumulative$RaceWBHO[anes.cumulative$VCF0106a == '2. Black'] <- 2
anes.cumulative$RaceWBHO[anes.cumulative$VCF0106a == '5. Hispanic'] <- 3
anes.cumulative$RaceWBHO[anes.cumulative$VCF0106a == '3. Asian'] <- 4
anes.cumulative$RaceWBHO[anes.cumulative$VCF0106a == '4. Native American'] <- 4
anes.cumulative$RaceWBHO[anes.cumulative$VCF0106a == '7. Other'] <- 4
# table new created variable - RaceWBHO
table(anes.cumulative$RaceWBHO)
mode(anes.cumulative$RaceWBHO)
class(anes.cumulative$RaceWBHO)
summary(anes.cumulative$RaceWBHO) # White("1")=40,286 ... Black("2")=5753 ... Hispanic("3")=1957  .... Other("4')=1415 ...  NA's=349


### STATE FIPS CODE:
# table the original variable
table(anes.cumulative$VCF0901a)
mode(anes.cumulative$VCF0901a)
class(anes.cumulative$VCF0901a)
# recode and create new variable    
anes.cumulative$StateCode <- recode (anes.cumulative$VCF0901a, "'0'=NA")
anes.cumulative$StateCode <- as.numeric ( as.character (anes.cumulative$StateCode))
# table new created variable - StateCode
table(anes.cumulative$StateCode)
mode(anes.cumulative$StateCode)
class(anes.cumulative$StateCode)
summary(anes.cumulative$StateCode) # 1805 NAs
anes.cumulative$State <- anes.cumulative$StateCode


### STATE ABBREVIATION:
# table the original variable
table(anes.cumulative$VCF0901b)
mode(anes.cumulative$VCF0901b)
class(anes.cumulative$VCF0901b)
# recode and create new variable  
anes.cumulative$VCF0901b <- as.factor(anes.cumulative$VCF0901b)
anes.cumulative$StateAbb <- recode (anes.cumulative$VCF0901b, "'99'=NA")
# table new created variable - StateAbb
table(anes.cumulative$StateAbb)
mode(anes.cumulative$StateAbb)
class(anes.cumulative$StateAbb)
summary(anes.cumulative$StateAbb) # 1805 NAs


### YEAR:
# table the original variable
table(anes.cumulative$VCF0004)
mode(anes.cumulative$VCF0004)
class(anes.cumulative$VCF0004)
# recode and create new variable            
anes.cumulative$YearFactor <- as.factor (anes.cumulative$VCF0004)
# table new created variable - Year
table(anes.cumulative$YearFactor)
mode(anes.cumulative$YearFactor)
class(anes.cumulative$YearFactor)
# recode and create new variable            
anes.cumulative$YearNumeric <- as.numeric (anes.cumulative$VCF0004)
# table new created variable - Year
table(anes.cumulative$YearNumeric)
mode(anes.cumulative$YearNumeric)
class(anes.cumulative$YearNumeric)


### SEX:
# table the original variable
table(anes.cumulative$VCF0104)
mode(anes.cumulative$VCF0104)
class(anes.cumulative$VCF0104)
# recode and create new variable      		  
anes.cumulative$Female <- NA
anes.cumulative$Female[anes.cumulative$VCF0104 == '1. Male'] <- 0
anes.cumulative$Female[anes.cumulative$VCF0104 == '2. Female'] <- 1
# table new created variable - Female
table(anes.cumulative$Female)
mode(anes.cumulative$Female)
class(anes.cumulative$Female)
summary(anes.cumulative$Female) # male("0")=22,017   female("1")=27640   NA's=103


### LAW:
      # Some people feel that the government in Washington should see to it that every person has a job and a good standard of living. 
      # (1972-1978,1996-LATER: Suppose these people are at one end of a scale, at point 1). 
      # Others think the government should just let each person get ahead on his/their own. 
      # (1972-1978,1996: Suppose these people are at the other end, at point 7. 
      # And, of course, some other people haveopinions somewhere in between, at points 2,3,4,5 or 6.)
      # Where would you place yourself on this scale, or haven't you thought much about this? (7-POINT SCALE SHOWN TO R)
      # 0 and 9 are NA and DK/Haven't thought about it, respectively
# table the original variable
table(anes.cumulative$VCF0809)
mode(anes.cumulative$VCF0809)
class(anes.cumulative$VCF0809)
# recode and create new variable        	  
anes.cumulative$Law <- NA
anes.cumulative$Law[anes.cumulative$VCF0809 == '1'] <- 1
anes.cumulative$Law[anes.cumulative$VCF0809 == '2'] <- 1
anes.cumulative$Law[anes.cumulative$VCF0809 == '3'] <- 1
anes.cumulative$Law[anes.cumulative$VCF0809 == '4'] <- 1
anes.cumulative$Law[anes.cumulative$VCF0809 == '5'] <- 0
anes.cumulative$Law[anes.cumulative$VCF0809 == '6'] <- 0
anes.cumulative$Law[anes.cumulative$VCF0809 == '7'] <- 0
# table new created variable - Law
table(anes.cumulative$Law)
mode(anes.cumulative$Law)
class(anes.cumulative$Law)
summary(anes.cumulative$Law) # 0 = 12,076 ... 1 = 13,235 ... NA's = 24,449


### INTERACTION TERM:
anes.cumulative$InteractionRaceFemale <- factor ( (anes.cumulative$Female * 4 ) + anes.cumulative$RaceWBHO)
table(anes.cumulative$InteractionRaceFemale)
mode(anes.cumulative$InteractionRaceFemale)
class(anes.cumulative$InteractionRaceFemale)
summary(anes.cumulative$InteractionRaceFemale)
# 1=male white(18,182) ... 2=male black(2,224) ... 3=male hispanic(857) ... 4=male other(643)
# 5=female white(22,102) ... 6=female black(3,529) ... 7=female hispanic(1,100) ... 8=female other(772) ... NA's=351


### SUBSETTING THE DATA:
# subset() function
anes.cumulative <- subset(anes.cumulative, select = c(RaceWBHO, StateCode, State, StateAbb, YearFactor, YearNumeric, Female, Law, InteractionRaceFemale))
# how many rows (cases) are in the subset of data
nrow(anes.cumulative) # 49760
# names of the columns in the subset of data (variable names)
names(anes.cumulative)  
anes.cumulative <- anes.cumulative[complete.cases(anes.cumulative)==TRUE, ]
nrow(anes.cumulative) # 25156


#################################################################################################################################################################

# (a) Fit a multilevel logistic regression estimating support for gun control given state, year, sex, and ethnicity (white/black/Hispanic/other).  
          # Use the display() function in R to display the fitted model.  Explain the output in a brief paragraph.

MultilevelLogisticModel <- with ( data = anes.cumulative, 
                                  glmer ( formula = Law ~ (1 | YearFactor) + (1 | InteractionRaceFemale) + (1 | StateAbb), 
                                  family = binomial (link = "logit") ) )
summary (MultilevelLogisticModel)
display (MultilevelLogisticModel)

#################################################################################################################################################################

# (b) Using your model, get estimates of the proportion of people who support gun control, for all 8 demographic groups in each state (excluding Alaska and Hawaii) for the year 2012.  
          # Using the 2010 census, poststratify to get an estimate for each state.


### Create "sstate" variable
sstate<- c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")



### Create vector of state random effects and fill in missing ones
State.Effects <- array(NA,c(51,1))

dimnames(State.Effects) <- list(c(sstate),"effect")
for(i in sstate){
  State.Effects[i,1] <- ranef(MultilevelLogisticModel)$StateAbb[i,1]
}

State.Effects[,1][is.na(State.Effects[,1])] <- 0 
State.Effects

# create dataset with 2 columns
ncol(State.Effects)
State.Effects <- data.frame ( rownames(State.Effects), (State.Effects) )
ncol(State.Effects)
nrow(State.Effects)
# give rownames to State.Effects in the dataset
names(State.Effects)[1] <- "StateAbb"
names(State.Effects)[2] <- "State.Effects"
names(State.Effects)
table(State.Effects$StateAbb)
table(State.Effects$State.Effects)


# obtain random effects for "YearFactor" 
Year.Effects <- data.frame ( ranef(MultilevelLogisticModel)$YearFactor )
nrow(Year.Effects)
names(Year.Effects)
# create dataset with 2 columns
ncol(Year.Effects)
Year.Effects <- data.frame ( rownames(Year.Effects), (Year.Effects) )
ncol(Year.Effects)
nrow(Year.Effects)
# give rownames to Year.Effects in the dataset
names(Year.Effects)[1] <- "YearNumeric"
names(Year.Effects)[2] <- "Year.Effects"
names(Year.Effects)
table(Year.Effects$YearNumeric)
Year.Effects$YearNumeric <- as.numeric(as.character(Year.Effects$YearNumeric))
table(Year.Effects$Year.Effects)


# obtain random effects for "InteractionRaceFemale" 
InteractionRaceFemale.Effects <- data.frame ( ranef(MultilevelLogisticModel)$InteractionRaceFemale )
nrow(InteractionRaceFemale.Effects)
names(InteractionRaceFemale.Effects)
# create dataset with 2 columns
ncol(InteractionRaceFemale.Effects)
InteractionRaceFemale.Effects <- data.frame ( rownames(InteractionRaceFemale.Effects), (InteractionRaceFemale.Effects) )
ncol(InteractionRaceFemale.Effects)
nrow(InteractionRaceFemale.Effects)
# give rownames to InteractionRaceFemale.Effects in the dataset
names(InteractionRaceFemale.Effects)[1] <- "InteractionRaceFemale"
names(InteractionRaceFemale.Effects)[2] <- "InteractionRaceFemale.Effects"
names(InteractionRaceFemale.Effects)
table(InteractionRaceFemale.Effects$InteractionRaceFemale)
table(InteractionRaceFemale.Effects$InteractionRaceFemale.Effects)


### Model the random effects for 2012
YearLinearModel <- lm (data = Year.Effects, formula = Year.Effects ~ YearNumeric)
display(YearLinearModel)

Prediction2012 <- YearLinearModel$coef[1] + ( YearLinearModel$coef[2] * 2012 )
table(Prediction2012) # -0.0113207227912482



### CENSUS DATA:
# Data is from the Census
# read the data into R
census.data <- read.dta(file = "census.dta")
# how many rows (cases) are in the dataset
nrow(census.data) # 51
# names of the columns (variable names)
names(census.data) 
# "P012B001" "P012B002" "P012B026" "SUMLEV"   "STATE"    "P012C001" "P012C002" "P012C026" "P012D001" "P012D002" "P012D026" "P012E001" "P012E002"
# "P012E026" "P012F001" "P012F002" "P012F026" "P012G001" "P012G002" "P012G026" "P012H001" "P012H002" "P012H026" "P012I001" "P012I002" "P012I026"
# "P0120001" "P0120002" "P0120026" "GEOCOMP" 


### CLEAN-UP CENSUS DATA:

# remove Pueto Rico
table(census.data$STATE)
census.data <- subset(census.data, census.data$STATE!=72)

# create "other.men"
other.men <- census.data$P012C002 + census.data$P012D002 + census.data$P012E002 + census.data$P012F002 + census.data$P012G002
other.men <- data.frame(census.data$STATE, 4, 0, other.men, census.data$P0120001)
names(other.men)[4] <- "Frequency.Census"
names(other.men)[2] <- "RaceWBHO.Census"
names(other.men)[3] <- "Female.Census"      

# create "other.women"
other.women <- census.data$P012C026 + census.data$P012D026 + census.data$P012E026 + census.data$P012F026 + census.data$P012G026
other.women <- data.frame(census.data$STATE, 4, 1, other.women, census.data$P0120001)
names(other.women)[4] <- "Frequency.Census"
names(other.women)[2] <- "RaceWBHO.Census"
names(other.women)[3] <- "Female.Census"

# create "black.women"
black.women <- data.frame(census.data$STATE, 2, 1, census.data$P012B026, census.data$P0120001)
names(black.women)[4] <- "Frequency.Census"
names(black.women)[2] <- "RaceWBHO.Census"
names(black.women)[3] <- "Female.Census"

# create "black.men"
black.men <- data.frame(census.data$STATE, 2, 0, census.data$P012B002, census.data$P0120001)
names(black.men)[4] <- "Frequency.Census"
names(black.men)[2] <- "RaceWBHO.Census"
names(black.men)[3] <- "Female.Census"

# create "hispanic.women"
hispanic.women <- data.frame(census.data$STATE, 3, 1, census.data$P012H026, census.data$P0120001)
names(hispanic.women)[4] <- "Frequency.Census"
names(hispanic.women)[2] <- "RaceWBHO.Census"
names(hispanic.women)[3] <- "Female.Census"

# create "hispanic.men"
hispanic.men <- data.frame(census.data$STATE, 3, 0, census.data$P012H002, census.data$P0120001)
names(hispanic.men)[4] <- "Frequency.Census"
names(hispanic.men)[2] <- "RaceWBHO.Census"
names(hispanic.men)[3] <- "Female.Census"

# create "white.women"
white.women <- data.frame(census.data$STATE, 1, 1, census.data$P012I026,census.data$P0120001)
names(white.women)[4] <- "Frequency.Census"
names(white.women)[2] <- "RaceWBHO.Census"
names(white.women)[3] <- "Female.Census"

# create "white.men"
white.men <- data.frame(census.data$STATE, 1, 0, census.data$P012I002, census.data$P0120001)
names(white.men)[4] <- "Frequency.Census"
names(white.men)[2] <- "RaceWBHO.Census"
names(white.men)[3] <- "Female.Census"

# compile "census.file"
census.file <- rbind(other.women, other.men, black.women, black.men, hispanic.women, hispanic.men, white.women, white.men)
names(census.file)[1] <- "StateFips"
names(census.file)[5] <- "FrequencyState.Census"

census.file$InteractionRaceFemale <- ( census.file$Female.Census * 4 ) + census.file$RaceWBHO.Census
census.file$StateFips <- as.numeric(census.file$StateFips)
census.file$RaceWBHO.Census <- factor(census.file$RaceWBHO.Census)
census.file$Female.Census <- factor(census.file$Female.Census)
 

# Create new state totals
State.NewTotal <- aggregate (census.file$Frequency.Census, by = list(census.file$StateFips), FUN = "sum")
names(State.NewTotal)
names(State.NewTotal)[1] <- "StateFips"
names(State.NewTotal)[2] <- "FrequencyState.CensusNew"
names(State.NewTotal)
State.NewTotal

# Join new state total with census.file
census.file <- join (census.file, State.NewTotal, by = "StateFips")
census.file$PercentState.Census <- census.file$Frequency.Census / census.file$FrequencyState.CensusNew
table(census.file$PercentState.Census)

nrow(census.file)
names(census.file)

### Add State Fips Codes
data(state.fips)
state.fips
State.Abb.Fips <- aggregate(as.numeric(state.fips$fips),by=list(state.fips$abb),FUN="mean")
colnames(State.Abb.Fips) <- c("StateAbb","StateFips")
State.Abb.Fips
#c1<-c("AK","HI") # add AK and HI fips (not in map data)
#c2<-c(2,15)
#c3<-cbind(c1,as.numeric(c2))
#colnames(c3) <- c("StateAbb","StateFips")
#State.Abb.Fips<-rbind(State.Abb.Fips,c3)
#State.Abb.Fips


### Join census.file with State.Abb.Fips
census.file <- join (State.Abb.Fips, census.file, by = "StateFips")
nrow(census.file)
names(census.file)

census.file$Intercept <- fixef(MultilevelLogisticModel)["(Intercept)"] # 0.432910017495889 
table(census.file$Intercept)

census.file <- join (census.file, State.Effects, by="StateAbb")
nrow(census.file)
names(census.file)

census.file <- join (census.file, InteractionRaceFemale.Effects, by="InteractionRaceFemale")
nrow(census.file)
names(census.file)

census.file$Prediction2012 <- Prediction2012 # -0.0113207189851455
table(census.file$Prediction2012)


table(census.file$State.Effects)
table(census.file$InteractionRaceFemale.Effects)
table(census.file$Intercept)
table(census.file$Prediction2012)


### Predictions for each cell
census.file$CellPrediction <- invlogit (census.file$State.Effects +
                                        census.file$InteractionRaceFemale.Effects +
                                        census.file$Intercept +
                                        census.file$Prediction2012)
table(census.file$CellPrediction)


### Multiply the cell prediction by the percent of the state in that cell
census.file$CellPredictionWeighted <- census.file$CellPrediction * census.file$PercentState.Census
table(census.file$CellPredictionWeighted)


### State Prediction
StatePrediction <- aggregate (census.file$CellPredictionWeighted, by = list(census.file$StateAbb, census.file$StateFips), FUN = "sum")
StatePrediction
names(StatePrediction)
names(StatePrediction)[1] <- "StateAbb"
names(StatePrediction)[2] <- "StateFips"
names(StatePrediction)[3] <- "Prediction"
names(StatePrediction)
StatePrediction
StatePrediction$Prediction100 <- 100 * StatePrediction$Prediction
StatePrediction

#################################################################################################################################################################

# (c) Make the following five graphs on a 3x2 grid:  
          # (i) a map of estimated gun control support by state in 2012; 
          # (ii) a plot of estimated gun control support vs. Obama vote share in 2012 (indicating each state by its two-letter abbreviation); 
          # (iii) a plot of estimated gun control support in 2012 vs. the raw proportion of respondents in the state from 2012 who supported gun control; 
          # (iv) a plot of estimated gun control support in 2012 vs. the raw proportion of respondents in the state from who supported gun control, pooling data from all years of NES; 
          # (v) a plot of estimated gun control support in 2012 vs. the state-level “random effects” from the fitted multilevel model.

### PLOTS FORMAT
png ("hwk12_problem4a.png", height = 1000, width = 1200)
#par (mfrow=c(3,2))
layout (matrix(c(1,1,2,3,4,5,6,7,8,9), 3, 2, byrow = TRUE))
layout.show(n=5)


### Map of estimated gun control support by state in 2012
# CHOOSE COLORS
# colors = c("darkred", "firebrick3", "mediumvioletred", "darkorchid3", "dodgerblue2", "dodgerblue4")
# colors = cm.colors(6)
colors = c( "lightcyan", "deepskyblue", "dodgerblue1", "royalblue", "mediumblue", "navyblue")
# GET QUARTILES
summary(StatePrediction$Prediction100)
StatePrediction$colorBuckets <- as.numeric(cut(StatePrediction$Prediction100, c(40, 45, 50, 55, 60, 65, 70)))
leg.txt <- c("<45%", "45%-50%", "50-55%", "55-60%", "60-65%", ">65%")
# match the colors to the values in the map using the state abbreviation
data(state.fips)
names(state.fips)
colorsmatched <- StatePrediction$colorBuckets[match(state.fips$abb, StatePrediction$StateAbb)]
# plot the map
map(database="state", fill=TRUE, col=colors[colorsmatched], res=0)
title("Estimated proportion favoring government involvement in 2012")
legend("bottomright", leg.txt, horiz=TRUE, fill=colors)


### Plot of estimated gun control support vs. Obama vote share in 2012 (indicating each state by its two-letter abbreviation)
# Data is Election Results 2012
# read the data into R
VoteShare <- read.dta(file = "statesheet12.dta")
nrow(VoteShare) # 51
names(VoteShare) 
head(VoteShare)
table(VoteShare$Obama_pct)
table(VoteShare$fips)
VoteShare <- subset(VoteShare, select = c(Obama_pct, fips))
names(VoteShare)[1] <- "ObamaVoteShare"
names(VoteShare)[2] <- "StateFips"
names(VoteShare)
VoteShare
VoteShare <- join (StatePrediction, VoteShare, by = "StateFips")
nrow(VoteShare)
names(VoteShare)

plot (x = VoteShare$Prediction, 
      y = VoteShare$ObamaVoteShare, 
      main = "Relationship between Obama vote share and 
      estimated proportion favoring government involvement", 
      xlab = "Estimated proportion of respondents favoring government involvement in 2012", 
      ylab = "Obama Vote Share", 
      pch = ".")
text(x = VoteShare$Prediction, y = VoteShare$ObamaVoteShare, labels = VoteShare$StateAbb)


### Plot of estimated gun control support in 2012 vs. the raw proportion of respondents in the state from 2012 who supported gun control; 
# State Prediction Raw 2012
StatePredictionRaw <- aggregate (census.file$CellPrediction, by = list(census.file$StateAbb, census.file$StateFips), FUN = "mean")
StatePredictionRaw
names(StatePredictionRaw)
names(StatePredictionRaw)[1] <- "StateAbb"
names(StatePredictionRaw)[2] <- "StateFips"
names(StatePredictionRaw)[3] <- "PredictionRaw"
names(StatePredictionRaw)
StatePredictionRaw
StatePredictionRaw$PredictionRaw100 <- StatePredictionRaw$PredictionRaw * 100
StatePredictionRaw
StatePredictionRaw <- join (StatePrediction, StatePredictionRaw, by = "StateFips")
nrow(StatePredictionRaw)
names(StatePredictionRaw)

plot (x = StatePredictionRaw$Prediction, 
      y = StatePredictionRaw$PredictionRaw, 
      main = "Relationship between the 2012 raw and estimated
      proportions favoring government involvement", 
      xlab = "Estimated proportion of respondents favoring government involvement in 2012", 
      ylab = "Raw proportion of respondents favoring government involvement (2012 only)", 
      pch = ".")
text(x = StatePredictionRaw$Prediction, y = StatePredictionRaw$PredictionRaw, labels = StatePredictionRaw$StateAbb)


### Plot of estimated gun control support in 2012 vs. the raw proportion of respondents in the state from who supported gun control, pooling data from all years of NES
# Join data
head(anes.cumulative)
anes.cumulative$StateFips <- anes.cumulative$State
ANES <- subset(anes.cumulative, select = c(StateFips, YearNumeric, InteractionRaceFemale))
head(ANES)
PooledNES <- join (State.Abb.Fips, ANES, by = "StateFips")
nrow(PooledNES)
names(PooledNES)
head(PooledNES)

PooledNES <- join (PooledNES, State.Effects, by="StateAbb")
nrow(PooledNES)
names(PooledNES)
head(PooledNES)

PooledNES <- join (PooledNES, InteractionRaceFemale.Effects, by="InteractionRaceFemale")
nrow(PooledNES)
names(PooledNES)
head(PooledNES)

PooledNES <- join (PooledNES, Year.Effects, by="YearNumeric")
nrow(PooledNES)
names(PooledNES)
head(PooledNES)
PooledNES$Intercept <- fixef(MultilevelLogisticModel)["(Intercept)"] # 0.515370412884632 
table(PooledNES$Intercept)

table(PooledNES$State.Effects)
table(PooledNES$InteractionRaceFemale.Effects)
table(PooledNES$Intercept)
table(PooledNES$Year.Effects)

# Predictions for each cell
PooledNES$CellPrediction <- invlogit (PooledNES$State.Effects +
                                      PooledNES$InteractionRaceFemale.Effects +
                                      PooledNES$Intercept +
                                      PooledNES$Year.Effects)
table(PooledNES$CellPrediction)

# State Prediction Raw Pooled NES
StatePredictionRawPooled <- aggregate (PooledNES$CellPrediction, by = list(PooledNES$StateAbb, PooledNES$StateFips), FUN = "mean")
StatePredictionRawPooled
names(StatePredictionRawPooled)
names(StatePredictionRawPooled)[1] <- "StateAbb"
names(StatePredictionRawPooled)[2] <- "StateFips"
names(StatePredictionRawPooled)[3] <- "PredictionRawPooled"
names(StatePredictionRawPooled)
StatePredictionRawPooled
StatePredictionRawPooled$PredictionRawPooled100 <- 100 * StatePredictionRawPooled$PredictionRawPooled
StatePredictionRawPooled
StatePredictionRawPooled <- join (StatePrediction, StatePredictionRawPooled, by = "StateFips")
nrow(StatePredictionRawPooled)
names(StatePredictionRawPooled)

plot (x = StatePredictionRawPooled$Prediction, 
      y = StatePredictionRawPooled$PredictionRawPooled, 
      main = "Relationship between raw proportions (1972 - 2008) and 
      the 2012 estimated proportion favoring government involvement", 
      xlab = "Estimated proportion of respondents favoring government involvement in 2012", 
      ylab = "Raw proportion of respondents favoring government involvement (1972 - 2008)", 
      pch = ".")
text(x = StatePredictionRawPooled$Prediction, y = StatePredictionRawPooled$PredictionRawPooled, labels = StatePredictionRawPooled$StateAbb)


### Plot of estimated gun control support in 2012 vs. the state-level “random effects” from the fitted multilevel model
StateRandomEffects <- join (StatePrediction, State.Effects, by="StateAbb")
nrow(StateRandomEffects)
names(StateRandomEffects)
head(StateRandomEffects)

plot (x = StateRandomEffects$Prediction, 
      y = StateRandomEffects$State.Effects, 
      main = "Relationship between state-level “random effects” from the fitted multilevel model 
      and estimated proportions favoring government involvement", 
      xlab = "Estimated proportion of respondents favoring government involvement in 2012", 
      ylab = "State-level “random effects” from the fitted multilevel model", 
      pch = ".")
text(x = StateRandomEffects$Prediction, y = StateRandomEffects$State.Effects, labels = StateRandomEffects$StateAbb)


### THE END
dev.off()
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
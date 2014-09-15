#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
### SURVEY ANALYSIS
### HOMEWORK 5 
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################


### PROBLEM 1: RANDOM SAMPLING AND REGRESSION

# Sample 100 random data points x from the normal distribution with mean 10 and standard deviation 5.  Then simulate 100 data points y from the model, y = 2 + 10x – x2 + error, where the errors are normally distributed with mean 0 and standard deviation 1.

# (a) Fit a linear regression to the data and fit a quadratic regression to the data.  Display the fitted regressions (using the display() function).

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


### SIMULATIONS CREATED FROM A NORMAL DISTRIBUTION WITH r(norm) function
		
		# X values = SAMPLE 100 RANDOM DATA POINTS X WITH MEAN 10 AND STANDARD DEVIATION 5
		x <- as.vector (rnorm (n=100, mean=10, sd=5))
		print(x)
		summary(x)
		
		# ERROR values = SAMPLE 100 RANDOM DATA POINTS X WITH MEAN 0 AND STANDARD DEVIATION 1
		error <- as.vector (rnorm (n=100, mean=0, sd=1))
		print(error)
		summary(error)
		
		# Y values = SIMULATE 100 DATA POINTS FROM THE MODEL "y = 2 + 10x - x^2 + error"
		y <- as.vector (2 + (10*x) - (x^2) + error)
		print(y)
		summary(y)

		# CREATE DATASET WITH X, Y and ERROR
		Simulation.Dataset <- as.data.frame (cbind(x, y, error))
		nrow(Simulation.Dataset)
		names(Simulation.Dataset)


### LINEAR REGRESSION
		linear.regression <- lm (y ~ x, data=Simulation.Dataset)
		summary(linear.regression)
		display(linear.regression)
		
		# show the coeffients of the fitted regression
		linear.regression$coef



### QUADRATIC REGRESSION

		### OPTION 1: MAKE THE SQUARED TERM BY HAND
				
				# Create the quadratic term by hand
				Simulation.Dataset$x.squared <- Simulation.Dataset$x * Simulation.Dataset$x
				nrow(Simulation.Dataset)
				names(Simulation.Dataset)
				print(Simulation.Dataset$x.squared)
				
				# Quadratic regression
				quadratic.regression1 <- lm (y ~ x + x.squared, data=Simulation.Dataset)
				summary(quadratic.regression1)
				display(quadratic.regression1)
				
				# show the coeffients of the fitted regression
				quadratic.regression1$coef


		### OPTION 2: POLYNOMIAL REGRESSION
				
				# use poly() function
				quadratic.regression2 <- lm (Simulation.Dataset$y ~ poly(Simulation.Dataset$x, 2, raw=TRUE))
				summary(quadratic.regression2)
				display(quadratic.regression2)
				
				# show the coeffients of the fitted regression
				quadratic.regression2$coef
				
				

#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################

# (b) Make a scatterplot showing the data (using plot()) and the fitted linear and quadratic regression lines (using curve(a + b*x, add=TRUE) and curve(b0+b1*x+b2*x^2,add=TRUE)).

### COEFFIENTS FOR THE LINEAR REGRESSION MODEL
		# linear regression
		linear.regression <- lm (y ~ x, data=Simulation.Dataset)
		# show the coeffients of the fitted regression
		linear.regression$coef
		# INTERCEPT LINEAR COEFFICIENT
		linear.intercept <- linear.regression$coef[1]
		print(linear.intercept)
		# X LINEAR COEFFICIENT
		linear.b1 <- linear.regression$coef[2]
		print(linear.b1)


### COEFFIENTS FOR THE QUADRATIC REGRESSION MODEL
		# quadratic regression
		quadratic.regression <- lm (Simulation.Dataset$y ~ poly(Simulation.Dataset$x, 2, raw=TRUE))
		# show the coeffients of the fitted regression
		quadratic.regression$coef
		# INTERCEPT QUADRATIC COEFFICIENT
		quadratic.intercept <- quadratic.regression$coef[1]
		print(quadratic.intercept)
		# X QUADRATIC COEFFICIENT
		quadratic.b1 <- quadratic.regression$coef[2]
		print(quadratic.b1)
		# X SQUARED QUADRATIC COEFFICIENT
		quadratic.b2 <- quadratic.regression$coef[3]
		print(quadratic.b2)
		

### PLOTS FORMAT
	png ("hwk5_1.png", height=750, width=750)
	par (mfrow=c(1,1))
			
### SCATTERPLOT SHOWING THE DATA
		# use plot() function
		my.plot <- plot (x=x, y=y, xlab="X", ylab="Y", main="Linear versus Quadratic regression predictions")
		# linear curve
		curve(linear.intercept + linear.b1 * x, add=TRUE, col="black", lty="dashed")
		# quadratic curve
		curve(quadratic.intercept + quadratic.b1 * x + quadratic.b2 * x^2, add=TRUE, col="black", lty="solid")
		# R-square value for linear model
		linear.regression <- lm (y ~ x, data=Simulation.Dataset)
		linear.Rsq <- summary(linear.regression)[c("adj.r.squared")]
		print(linear.Rsq)
		# R-squared value for quadratic model
		quadratic.regression <- lm (Simulation.Dataset$y ~ poly(Simulation.Dataset$x, 2, raw=TRUE))
		quadratic.Rsq <- summary(quadratic.regression)[c("adj.r.squared")]
		print(quadratic.Rsq)
		# text for legend
		legend.Rsq <- vector('expression', 2)
		legend.Rsq[1] <- substitute(expression(bold("Linear Curve") ~~ (italic(b)[italic(0)] + italic(b)[italic(1)] * italic(x)) ~~ bolditalic(R)[bolditalic(adj)]^bolditalic(2) == MYVALUE), list(MYVALUE = format(linear.Rsq, digits=3)))[2]
		legend.Rsq[2] <- substitute(expression(bold("Quadratic Curve") ~~ (italic(b)[italic(0)] + italic(b)[italic(1)] * italic(x) + italic(b)[italic(2)] * italic(x)^2) ~~ bolditalic(R)[bolditalic(adj)]^bolditalic(2) == MYVALUE2), list(MYVALUE2 = format(quadratic.Rsq, digits=3)))[2]
		# legend
		legend("bottomleft", legend = legend.Rsq, lty=c("dashed", "solid"), horiz=FALSE, bg="lightgray", inset=0.06)
		
### THE END
dev.off()
		
#################################################################################################################################################################
#################################################################################################################################################################
#################################################################################################################################################################
rm(list = ls())

## Initial exploration of the data

# Step 1: Read in data
library(readxl)
FAA1 <- read_excel("FAA1.xls")
FAA2 <- read_excel("FAA2.xls")

# Step 2: Check structure of each data set
str(FAA1)
str(FAA2)
FAA1$aircraft <- factor(FAA1$aircraft) 
FAA2$aircraft <- factor(FAA2$aircraft)

# Step 3: Merge the two data sets 

# Before merging, will add duration column to FAA2
FAA2$duration <- rep(NA, 150)
FAA <- rbind(FAA1, FAA2)

# Check for duplicates 
sum(duplicated(FAA[-2])) 

# There are 100 duplicates so these should be removed leaving 850 observations
FAA <- FAA[!duplicated(FAA[-2]),]

# Confirm that there are no duplicates
sum(duplicated(FAA[-2])) == 0

# Step 4: Check structure of combined data set
str(FAA)
summary(FAA)
apply(FAA[-1], 2, sd, na.rm = TRUE)

# Step 5: First presentation slide. See notes. 

## Data Cleaning and further exploration

# Step 6:

attach(FAA)

# Take a look at abnormal obervations for each variable
FAA[duration <= 40 & is.na(duration) == F,]
FAA[no_pasg <= 0, ] # sanity check
FAA[speed_ground < 30 | speed_ground > 140, ]
FAA[(speed_air < 30 | speed_air > 140) & is.na(speed_air) == F, ]
FAA[height < 6, ]
FAA[distance > 6000, ] # these are not abnormal

# Keep only rows with normal values
FAA <- FAA[(duration > 40 | is.na(duration) == T) & no_pasg > 0 & (speed_ground >= 30 & speed_ground <= 140) & 
    ((speed_air >= 30 & speed_air <= 140) | is.na(speed_air) == T) & height >= 6,]

attach(FAA)

# Check number of remaining rows
nrow(FAA)

# Step 7: Repeat Step 4
str(FAA)
summary(FAA)
apply(FAA[-1], 2, sd, na.rm = T)

# Step 8: Draw histograms for each variable
par(mfrow = c(2,4))
hist(distance, main = "")
hist(speed_air, main = "")
hist(speed_ground, main = "")
hist(height, main = "")
hist(pitch, main = "")
hist(duration, main = "")
hist(no_pasg, main = "")
hist(as.numeric(aircraft), main = "", xlab = "Airbus=1,Boeing=2")

# Step 9: Presentation slide

## Initial analysis for identifying important factors that impact the response variable "landing distance"

# Step 10: Compute pairwise correlation
cor(FAA[,2:7], FAA[,8], use = "complete.obs")
cor(as.numeric(aircraft), distance, use = "complete.obs")


# Step 11: Scatter plots
plot(speed_air, distance)
plot(speed_ground, distance)
plot(height, distance)
plot(pitch, distance)
plot(duration, distance)
plot(no_pasg, distance)
plot(aircraft, distance) # box plots for both aircraft types
plot(as.numeric(aircraft), distance, xlab = "Airbus=1,Boeing =2")

# Step 12
# Yes

## Regression using a single factor each time

# Step 13: Regress Y (landing distance) on each of the X variables
summary(lm(distance ~ speed_air))
summary(lm(distance ~ speed_ground))
summary(lm(distance ~ height))
summary(lm(distance ~ pitch))
summary(lm(distance ~ duration))
summary(lm(distance ~ no_pasg))
summary(lm(distance ~ aircraft))

# Step 14: Standardize each X variable to X' and then regress Y (landing distance) on each of the X' variables
scaled.FAA <- data.frame(scale(FAA[-1]))
summary(scaled.FAA)
apply(scaled.FAA, 2, sd, na.rm = T)
attach(scaled.FAA)
summary(lm(distance ~ speed_air))
summary(lm(distance ~ speed_ground))
summary(lm(distance ~ height))
summary(lm(distance ~ pitch))
summary(lm(distance ~ duration))
summary(lm(distance ~ no_pasg))
scaled.aircraft <- (as.numeric(aircraft) - mean(as.numeric(aircraft)))/sd(as.numeric(aircraft))
summary(lm(distance ~ scaled.aircraft))
attach(FAA)

# Step 15: Compare Tables 1,2,3. Create Table 0. 

## Check collinearity

# Step 16: Compare the regression coefficients of the three provided models
summary(lm(distance ~ speed_ground))
summary(lm(distance ~ speed_air))
summary(lm(distance ~ speed_ground + speed_air))

cor(speed_ground, speed_air, use = "complete.obs")

# Steps 17, 18 and 19: Fit 6 models according to variable ranking and plot 3 model selection criteria

r.squared <- rep(0,6)
adjusted.r.squared <- rep(0,6)
aic <- rep(0,6)

# Model 1: LD ~ Air speed
model <- lm(distance ~ speed_air)
r.squared[1] <- summary(model)$r.squared
adjusted.r.squared[1] <- summary(model)$adj.r.squared
aic[1] <- AIC(model)

# Model 2: LD ~ Air speed + Ground speed
model <- lm(distance ~ speed_air + speed_ground)
r.squared[2] <- summary(model)$r.squared
adjusted.r.squared[2] <- summary(model)$adj.r.squared
aic[2] <- AIC(model)

# Model 3: LD ~ Air speed + Ground speed + Aircraft type
model <- lm(distance ~ speed_air + speed_ground + aircraft)
r.squared[3] <- summary(model)$r.squared
adjusted.r.squared[3] <- summary(model)$adj.r.squared
aic[3] <- AIC(model)

# Model 4: LD ~ Air speed + Ground speed + Aircraft type + height
model <- lm(distance ~ speed_air + speed_ground + aircraft + height)
r.squared[4] <- summary(model)$r.squared
adjusted.r.squared[4] <- summary(model)$adj.r.squared
aic[4] <- AIC(model)

# Model 5: LD ~ Air speed + Ground speed + Aircraft type + height + pitch
model <- lm(distance ~ speed_air + speed_ground + aircraft + height + pitch)
r.squared[5] <- summary(model)$r.squared
adjusted.r.squared[5] <- summary(model)$adj.r.squared
aic[5] <- AIC(model)

# Model 6:  LD ~ Air speed + Ground speed + Aircraft type + height + duration
model <- lm(distance ~ speed_air + speed_ground + aircraft + height + pitch + duration)
r.squared[6] <- summary(model)$r.squared
adjusted.r.squared[6] <- summary(model)$adj.r.squared
aic[6] <- AIC(model)

# Plots
par(mfrow = c(1,3))
# R squared vs number of parameters 
plot(1:6, r.squared, type = "b", xlab = "No. of parameters")
# Adjusted R squared vs number of parameters 
plot(1:6, adjusted.r.squared, type = "b", xlab = "No. of parameters")
# AIC vs number of parameters 
plot(1:6, aic, type = "b", xlab = "No. of parameters")

##  Variable selection based on StepAIC algorithm

#Step 21: Use the R function "StepAIC" to perform forward variable selection. Compare the result with that in Step 19.

library(MASS)
fit1 <- lm(distance ~ ., na.omit(FAA))
fit2 <- lm(distance ~ 1, na.omit(FAA))
stepAIC(fit1,direction = "backward")$anova
stepAIC(fit2,direction = "forward", scope = list(upper = fit1,lower = fit2))$anova
stepAIC(fit2,direction = "both", scope = list(upper = fit1,lower = fit2))$anova


### Find predicted R squared of the final model

# This function calculates the PRESS

PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)

  return(PRESS)
}

# This function calculates the Predictive r-squared

#' @title Predictive R-squared
#' @author Thomas Hopper
#' @description returns the predictive r-squared. Requires the function PRESS(), which returns
#'              the PRESS statistic.
#' @param linear.model A linear regression model (class 'lm'). Required.
#'
pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1 - PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}

# Call function on the final model
final_model <- lm(distance ~ speed_air + aircraft + height)
pred_r_squared(final_model)

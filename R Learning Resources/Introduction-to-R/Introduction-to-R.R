# Introduction-to-R.R
# -----------------------------------------------------------------------------
# Script to accompany the "Introduction to R" tutorial
# Author: Günter J. Hitsch




# First steps ---------------------------------------------
# Adding two numbers
1 + 4

# Assigning to a variable
x = 2*(4 - 1)
y <- 2*(4 - 1)

# Vector of numbers
v = c(7, -1, 3.6, 1000)

# Character vector
locations = c("Chicago", "Hong Kong", "London")

# Assign the third value in v, 3.6, to a
a = v[3]

# Select the international locations using the c function
international = locations[c(2, 3)]


# Loading/importing data ----------------------------------
women_DF = read.csv("Data/Dating-Women.csv")

# Inspect first and last lines in data
head(women_DF)
tail(women_DF)

# Load men_DF (a data frame) in R data format
load("Data/Dating-Men.RData")
head(men_DF)


# Getting help --------------------------------------------
help(lm)
?lm


# Installing packages ------------------------------------
# Uncomment the line below to install the package!
#install.packages("psych")
library(psych)


# Inspecting the data -------------------------------------
summary(women_DF)
summary(women_DF$age)
table(women_DF$age)

# Individual summary statistics:
sd(women_DF$yrs_education)
mean(women_DF$yrs_education)
median(women_DF$yrs_education)
min(women_DF$yrs_education)
max(women_DF$yrs_education)

# If you have not already done so, install and load psych!
describe(women_DF)

# Histogram of emails
hist(women_DF$emails,
     col  = "lightskyblue1",
     main = "Histogram of first-contact e-mails received",
     xlab = "E-Mails") 


# Regression analysis -------------------------------------

# Scatter plot
plot(women_DF$rating, women_DF$emails,
     pch = 21, lwd = 0.4, bg = "hotpink1",
     main = "E-mails vs. looks rating", 
     xlab = "Looks rating", ylab = "E-mails")

# Regression of emails on rating
lm_fit_1 = lm(emails ~ rating, data = women_DF) 
summary(lm_fit_1) 

# Confidence intervals (95%)
confint(lm_fit_1, level = 0.95)

# Adding regression line to a scatter plot:
plot(women_DF$rating, women_DF$emails, pch = 21, lwd = 0.4, bg = "hotpink1",
     main = "E-mails vs. looks rating",       
     xlab = "Looks rating", ylab = "E-mails")

abline(lm_fit_1, lwd = 1.5, col = "midnightblue")

# Add bmi to the regression
lm_fit_2 = lm(emails ~ rating + bmi, data = women_DF) 
summary(lm_fit_2) 

# Correlation between rating and bmi variables
cor.test(women_DF$rating, women_DF$bmi) 

# Adding all variables excluding age
lm_fit_3 = lm(emails ~ rating + height + bmi + yrs_education + days_active, data = women_DF) 
summary(lm_fit_3)

# As simpler way of writing the above model formula
lm_fit_4 = lm(emails ~ . - age, data = women_DF) 
summary(lm_fit_4) 


# Categorical variables -----------------------------------

# age_1 as a logical variable
women_DF$age_1 = women_DF$age == 1 
head(women_DF)

# Dummies taking 0/1 values
women_DF$age_1 = as.numeric(women_DF$age == 1)
women_DF$age_2 = as.numeric(women_DF$age == 2)
women_DF$age_3 = as.numeric(women_DF$age == 3)

# Add age dummies 2 and 3 to the regression
lm_fit_5 = lm(emails ~ rating + height + bmi + yrs_education + days_active
             + age_2 + age_3, data = women_DF) 
summary(lm_fit_5) 

# Variant: age dummies 1 and 3
lm_fit_6 = lm(emails ~ rating + height + bmi + yrs_education + days_active
              + age_1 + age_3, data = women_DF) 
summary(lm_fit_6)

# Variant: Omit intercept/constant term and add all three age dummies
lm_fit_7 = lm(emails ~ rating + height + bmi + yrs_education + days_active
              + age_1 + age_2 + age_3 - 1, data = women_DF) 
summary(lm_fit_7)

# Variant: use the factor function to create dummies
lm_fit_8 = lm(emails ~ rating + height + bmi + yrs_education + days_active + factor(age),
              data = women_DF) 
summary(lm_fit_8)  

# Remove all manually created age dummies
women_DF$age_1 = NULL
women_DF$age_2 = NULL
women_DF$age_3 = NULL

# Same regression as lm_fit_8 using quick way of creating model formula
lm_fit_9 = lm(emails ~ . - age + factor(age), data = women_DF) 
summary(lm_fit_9)  


#Step1: Download and save insurance.csv file in R
getwd()
setwd("/Users/nirajkc/Desktop/Assignment5")

#step 2:Exploring and preparing the data
#A :Read your csv  file and confirms that the dat is formatted as we had expected.
insurance <- read.csv("insurance.csv", stringsAsFactors =TRUE)
str(insurance)

#B:Show the summary statistics for the dependent variables.
summary(insurance$expenses)

#C: Confirm this visually using a histogram, by using hist function for the 
#dependent variable and give your analysis fron the histogram.
hist(insurance$expenses)

#D: Show the four levels of region variables.
table(insurance$region)

#E:Create a correlation matrix for the four numeric variables in the insurance data frame, and use the cor() command.
cor(insurance[c("age", "bmi", "children", "expenses")])

#step 3: Training a model on the data.
model <- lm(expenses~., data = insurance)
model
 
#step 4: Evaluating model performance
#Evaluate the model performance by using the summary() command on the stored model.
summary(model)

#Step5: Improving model performanace
#Suggest and work with one way to improve model performance

insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
ins_model <- lm(formula = expenses ~ age + age2 + children + bmi + sex + bmi30 * 
            +                        smoker + region, data = insurance)
summary(ins_model)




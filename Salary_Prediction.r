#--------------------------------------------------------------REALM DATA SALARY PREDICCION ---------------------------------------------------------
library(ggplot2)
# dataset file : ds_salaries.csv
#-----------------------1 Read dataset--------------------------------------
ds_salaries <- read.csv("ds_salaries.csv", header = TRUE, sep = ",")
#---------------------------------------------------------------------------
#-----------------------2 Explore dataset----------------------------------
summary(ds_salaries)#We can see the descriptive stadistics for each variable in the dataset
str(ds_salaries) 
cat("The dataset contains", ncol(ds_salaries), "variables.")
"""
-----------------------------------What does our Variables Mean?-----------------------------------
work_year: Year when the salary was paid.
experience_level: Experience level in the job
employment_type: The type of employment for the role
job_title: The role worked in during the year.
salary: The total gross salary amount paid.
salary_currency: The currency of the salary paid as an ISO 4217 currency code.
salary_in_usd: The salary in USD
employee_residence: Employee's primary country of residence in during the work year as an ISO 3166 country code.
remote_ratio: The overall amount of work done remotely
company_location: The country of the employer's main office or contracting branch
company_size: The median number of people that worked for the company during the year
"""
#------------------Unique values of the categorical dataset variables----------------------------
variables <- c("experience_level", "employment_type", "job_title", "salary_currency",
               "employee_residence", "remote_ratio", "company_location", "company_size")

for (variable in variables) {
  unique_values <- unique(ds_salaries[[variable]])
  cat("Unique values of", variable, ":", "\n")
  print(unique_values)
  cat("\n")
}
#--------------------MIN MAX MEAN numerical variables-------------------------- """
variables <- c("work_year", "salary_in_usd", "remote_ratio")
for (variable in variables) {
  max_value <- max(ds_salaries[[variable]])
  min_value <- min(ds_salaries[[variable]])
  mean_value <- mean(ds_salaries[[variable]])
  median_value <- median(ds_salaries[[variable]])
  
  cat("Summary statistics for", variable, ":", "\n")
  cat("Maximum:", max_value, "\n")
  cat("Minimum:", min_value, "\n")
  cat("Mean:", mean_value, "\n")
  cat("Median:", median_value, "\n\n")
}
#We can see that the mean and the median of salary in usd are more or less equal, this can mean that this variable follows a normal distribution

#-------------------How are our variables distribution?(Histogram and boxplots)---------------------

# Variables for histograms (numerical variables)
variables <- c("salary_in_usd", "remote_ratio", "work_year")
par(mfrow = c(1, 3))
# Loop through variables and create histograms
for (variable in variables) {
  # Create histogram
  hist(ds_salaries[[variable]], col = "green",
       main = paste("Histogram of", variable),
       xlab = variable, ylab = "Frequency")
}
par(mfrow = c(1, 1)) #Reset the layout

library(ggplot2)

# Create a subset of the data with only the relevant variables
data_subset <- ds_salaries[, c("work_year", "remote_ratio")]

# Calculate the average remote ratio for each work year
avg_remote_ratio <- aggregate(remote_ratio ~ work_year, data = data_subset, FUN = mean)

# Create the line plot
ggplot(avg_remote_ratio, aes(x = work_year, y = remote_ratio)) +
  geom_line() +
  geom_point(data = subset(avg_remote_ratio, remote_ratio == max(remote_ratio)), color = "red", size = 3) +
  labs(x = "Work Year", y = "Average Remote Ratio", title = "Change in Remote Work over Work Years")


# Barplots of Categorical Variables (Here there is only some of them as job_title, employee residence and company location has a lot of variables in them)
variables <- c("company_size", "work_year", "experience_level")
# Create a layout for displaying multiple plots
par(mfrow = c(1, 3))
# Loop through each variable and create the corresponding barplot
for (variable in variables) {
  barplot(table(ds_salaries[[variable]]), 
          main = variable, 
          xlab = variable, 
          ylab = "Frequency", 
          col = "green",
          las = 2)
}
par(mfrow = c(1, 1)) # Reset de layout
#-------------------------------------------------------------------------------

#---------------Interesting PLots---------------------
# Top 5 professions with highest salary
mean_salary <- aggregate(salary_in_usd ~ job_title, data = ds_salaries, FUN = mean)
sorted_jobs <- mean_salary[order(mean_salary$salary_in_usd, decreasing = TRUE),]
top_10_salary <- head(sorted_jobs, n = 10)
top_10_salary
barplot(top_10_salary$salary_in_usd, names.arg = top_10_salary$job_title,
        main = "Top 5 Professions with Highest Salary",
        xlab = "Job", ylab = "Mean Salary", las = 2,col="green")

# Top 5 Cities with highest salary
mean_salary_city <- aggregate(salary_in_usd ~ company_location, data = ds_salaries, FUN = mean)
sorted_city <- mean_salary_city[order(mean_salary_city$salary, decreasing = TRUE),]
top_10_city <- head(sorted_city, n = 10)
top_10_city
barplot(top_10_city$salary, names.arg = top_10_city$company_location,
        main = "Top 5 Countries with Highest Salary",
        xlab = "Country", ylab = "Mean Salary",col="green")

# Company sizes with highest salary
mean_salary_company <- aggregate(salary_in_usd ~ company_size, data = ds_salaries, FUN = mean)
sorted_company <- mean_salary_company[order(mean_salary_company$salary_in_usd, decreasing = TRUE),]
top_5_company <- head(sorted_company, n = 5)
top_5_company
barplot(top_5_company$salary_in_usd, names.arg = top_5_company$company_size,
        main = "Top Company Sizes with Highest Salary",
        xlab = "Company Size", ylab = "Mean Salary",col="green")

# Relation between experience and salary
mean_salary_experience <- aggregate(salary_in_usd ~ experience_level, data = ds_salaries, FUN = mean)
sorted_experience <- mean_salary_experience[order(mean_salary_experience$salary),]
barplot(sorted_experience$salary, names.arg = sorted_experience$experience_level,
        main = "Mean Salary by Experience Level",
        xlab = "Experience Level", ylab = "Mean Salary", col = "green")
sorted_experience
#---------------------------------------------------------------------------------------------------------------------------------------


#----------------3. Perform a multiple linear regression with salary in usd as the response and all-------------------------------------
#Is salary needed (salary in currency units)
correlation <- cor(ds_salaries$salary, ds_salaries$salary_in_usd)
correlation
#In this dataset we have three variables that are related with the salary and I want to predict it with the same current units
#So in order to simplify my model and focus in those variables that hace stronger impact on the target variable
ds_salaries <- subset(ds_salaries, select = -c(salary, salary_currency))#subset without salary ans salary_currency
head(ds_salaries) #first lines of the dataset
#------------------------------backward selection------------------------------
library(MASS)
lm_initial <- lm(salary_in_usd~ ., data = ds_salaries)
lm_backward <- stepAIC(lm_initial, direction = "backward")
anova(lm_backward)
#-------------------------------------------------------------------------------
#----------------------forward selection----------------------------------------
null_model <- lm(salary_in_usd ~ 1, data = ds_salaries) 
full_model <-lm(salary_in_usd~ ., data = ds_salaries)

# Running forward stepwise selection with AIC to choose the best model
library("MASS")
model_forward <- stepAIC(null_model, direction="forward", scope=list(lower=null_model, upper=full_model), trace=TRUE)
model_forward
anova(model_forward)
#-----------------------------------------------------------------------------
#----------------------Stepwise selection------------------------------------
null_model <- lm(salary_in_usd ~ 1, data = ds_salaries)
full_model <- lm(salary_in_usd ~ ., data = ds_salaries)
# Running stepwise selection with AIC to choose the best model
library("MASS")
model_stepwise <- stepAIC(null_model, direction="both", scope=list(lower=null_model, upper=full_model), trace=TRUE)
model_stepwise
anova(model_stepwise)
#---------------------------------------------------------------------------


#---------------------------4 MODEL STUDY -----------------------------------
model=lm(salary_in_usd~ job_title + company_size + work_year + experience_level + employee_residence,data=ds_salaries)
summary(model)
#-----------------------COEFFICIENT EXAMINATION--------------------------
# Get the coefficients
coefficients <- coef(model)
# Get the 95% confidence intervals for the coefficients
conf_intervals <- confint(model)
# Get the p-values 
p_values <- summary(model)$coefficients[, 4]
# Create a data frame with coefficients, confidence intervals, and p-values
coef_summary <- data.frame(
  Coefficient = coefficients,
  Lower_CI = conf_intervals[, 1],
  Upper_CI = conf_intervals[, 2],
  p_value = p_values
)
# Order the coefficients by absolute value
coef_summary <- coef_summary[order(-abs(coef_summary$Coefficient)), ]
print(coef_summary)
#Possitive coefficients = If predictor variable increases, the response variable
                        #is also expexted to increase.
#Negative coefficients = If predictor variable decrease, the response variable
                        #is also expected to decrease.
#----------------------------------------------------------------------
#-----------------Significant Predictor Variables-----------------
# Get the p-values associated with the coefficients
p_values <- summary(model)$coefficients[, 4]

# Create a vector with the indices sorted according to the p-values
sorted_indices <- order(p_values)

# Sort the coefficients and p-values based on the indices
sorted_coefficients <- coefficients[sorted_indices]
sorted_p_values <- p_values[sorted_indices]

# Create a data frame with the sorted coefficients and p-values
sorted_coef_summary <- data.frame(Coefficient = sorted_coefficients, 
                                  p_value = sorted_p_values)

# Display the data frame with the sorted coefficients and p-values
print(sorted_coef_summary)

#------------------------------------------------------------------------
#-------------------------RESIDUALS--------------------------------------
plot(residuals(model),type = "b",ylim = range(residuals(model, type = "pearson")))
abline(h = 0)
hist(residuals(model), breaks = 100, col = "green",main="Residuals Histogram")
#In order to have more information about our first model, we did a plot of the residuals 
#If the model is good we would expect the plot to show a random scatter of points around the horizontal line at 0. 
#We also know that any trends in the residuals indicates a bad model to fit the data.

#To have evidence that the model does not fit good the data, is important to do a residual histogram and see if the resiudals are normally distributed or not.
#If they are, that means that our model is a good fit.
#As the histogram is slightly skewed to the left, which means that the model might be overestimating the number of affected individuals for some time points. 
#However, the skewness is not too extreme and that is a good sign.


#It seems that there are outliers, so I do cooks distance, cooks distance is a mesure used to identify influencial observations in a regression analysis.
#If the outliers is influential, it will be excluded from the model.
#-----------------------Cooks distance----------------------------------------

model <- lm(salary_in_usd ~ job_title + company_size + work_year + experience_level + employee_residence, data = ds_salaries)
# Calculate Cook's distance
cooksd <- cooks.distance(model)
# Identify the observations with high Cook's distance (considered as outliers)
outliers <- cooksd > 4 / length(cooksd) # Adjust the threshold value as needed
# Exclude the outliers from the dataset
ds_salaries_no_outliers <- ds_salaries[!outliers, ]
# Refit the model without the outliers
new_model <- lm(salary_in_usd ~ job_title + company_size + work_year + experience_level + employee_residence, data = ds_salaries_no_outliers)
plot(residuals(new_model),type="b")
hist(residuals(new_model), breaks = 100, col = "green",main="Residuals Histogram")

#----------------------------------------------------------------------------
#----------------------PREDICTIONS-------------------------------------------
#We can make a comparison between the predictions with outliers and with not ouliers.
pred_model<-predict(model)

# Check if all predictions are positive
all_positive <- all(pred_model > 0)
if (all_positive) {
  print("All predictions are positive.")
} else {
  print("Not all predictions are positive.")
}
#There are negative values in the predictions but salaries can't be negative, 
#We can compute logarithms to the predictions.

#------------------------LOG SALARY MODEL---------------------------------
ds_salaries_no_outliers$log_salary <- log(ds_salaries_no_outliers$salary_in_usd)#new variable
# Fit the regression model using the transformed salary variable
model_log <- lm(log_salary ~ job_title + company_size + work_year + experience_level + employee_residence, data =  ds_salaries_no_outliers)
# Generate predictions using the model
log_predictions <- predict(model_log)
hist(log_predictions)

#Check if all log predictions are possitive
all_positive <- all(log_predictions > 0)
if (all_positive) {
  print("All predictions are positive.")
} else {
  print("Not all predictions are positive.")
}
#All predictions are possitive

# Reverse the logarithmic transformation to obtain predictions in the original scale
scaled_log_pred <- exp(log_predictions)
#Check if all log predictions are possitive
all_positive <- all(scaled_log_pred > 0)
if (all_positive) {
  print("All predictions are positive.")
} else {
  print("Not all predictions are positive.")
}

#Mean of the predicted values of both models
mean_pr_model <- mean(pred_model)
mean_pr_nmodel <- mean(pred_new_model)
mean_log_model <- mean(scaled_log_pred)
mean_models <- c(mean_pr_model, mean_pr_nmodel, mean_log_model)
sorted_means <- sort(mean_models)
model_names <- c("pred_model", "pred_new_model", "scaled_log_pred")
sorted_models <- model_names[order(mean_models)]
result <- data.frame(Model = sorted_models, Mean = sorted_means)
result


#OBSERVATION:
#The new model without outliers produces higher mean predictions compared to the model with outliers.
#This suggests that the outliers were influencing the average predicted salaries downward. 

#------------------------------Making Predictions and Comparing them-------------------------------------------------------------------
#PREDICTION 1
values1 <- data.frame(job_title = 'Data Scientist', company_size = 'L', work_year = 2023, experience_level = 'EX',employee_residence='US')
# Predict using the original model
prediction <- predict(model, newdata = values1)
cat("Model: Original Model\n")
cat("Prediction:", prediction, "\n\n")

# Predict using the new model without outliers
predictionnew <- predict(new_model, newdata = values1)
cat("Model: New Model without Outliers\n")
cat("Prediction:", predictionnew, "\n\n")

# Predict using the model with logarithm transformation
log_pred <- exp(predict(model_log, newdata = values1))
cat("Model: Model with Logarithm Transformation\n")
cat("Prediction:", log_pred, "\n\n")

#PREDICTION 2
values2 <- data.frame(job_title = 'Data Scientist', company_size = 'M', work_year = 2023, experience_level = 'EX', employee_residence = 'US')
# Predict using the original model
prediction <- predict(model, newdata = values2)
cat("Model: Original Model\n")
cat("Prediction:", prediction, "\n\n")

# Predict using the new model without outliers
predictionnew <- predict(new_model, newdata = values2)
cat("Model: New Model without Outliers\n")
cat("Prediction:", predictionnew, "\n\n")

# Predict using the model with logarithm transformation
log_pred <- exp(predict(model_log, newdata = values2))
cat("Model: Model with Logarithm Transformation\n")
cat("Prediction:", log_pred, "\n\n")

#PREDICTION 3
values3 <- data.frame(job_title = 'Data Engineer', company_size = 'S', work_year = 2023, experience_level = 'SE', employee_residence = 'US')
# Predict using the original model
prediction <- predict(model, newdata = values3)
cat("Model: Original Model\n")
cat("Prediction:", prediction, "\n\n")

# Predict using the new model without outliers
predictionnew <- predict(new_model, newdata = values3)
cat("Model: New Model without Outliers\n")
cat("Prediction:", predictionnew, "\n\n")

# Predict using the model with logarithm transformation
log_pred <- exp(predict(model_log, newdata = values3))
cat("Model: Model with Logarithm Transformation\n")
cat("Prediction:", log_pred, "\n\n")

#-----------------------------------------------------------------------------------------

#--------------------------------NON PARAMETRIC BOOOSTRAP (MODEL NO OUTLIERS)--------------------------------
predicted_salaries<-predict(new_model)
mean_salary=mean(predicted_salaries)
# Create a vector to store the bootstrap results
bootstrap_results <- vector("numeric", 10000)
# Loop through the bootstrap iterations
for(i in 1:10000){
  # Sample with replacement from the predicted salaries
  bootstrap_sample <- sample(predicted_salaries, size = length(predicted_salaries), replace = TRUE)
  # Calculate the mean of the bootstrap sample
  bootstrap_mean <- mean(bootstrap_sample)
  # Store the bootstrap mean in the bootstrap_results vector
  bootstrap_results[i] <- bootstrap_mean
}

#-------------------------- Comparison of distributions NON PARAMETRIC (MODEL NO OUTLIERS)-----------------------------
bootstrap_mean <- mean(bootstrap_results)
bootstrap_mean#137847.6
mean_salary#137848.2
#The means are mostly equal, lets see how they are distributed.
# Histogram and density plots (data with no outliers vs nonparametric Bootrsap)
par(mfrow = c(1, 2))
hist((predicted_salaries), main = "Model prediction salary distribution", xlab = "Media", col = "green",breaks=50)
hist((bootstrap_results), main = "Bootstrap salary prediction distribution", xlab = "Media", col = "green",breaks=50)
#We can see that there are negative predictions on the models distribution and
#that it doesn't follow a normal distribution around the mean value
#The histogram of bootrsap results, we can see that there are no negative predictions,
#and that it follows a normal distribution.
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))


#  ------------------------------Condidence Interval NON PARAMETRIC BOOTSTRAP (NO OUTLIERS MODEL)------------------------
alpha <- 0.05  
sorted_results <- sort(bootstrap_results)
lower_limit <- sorted_results[round(alpha/2 * length(sorted_results))]
upper_limit <- sorted_results[round((1 - alpha/2) * length(sorted_results))]
cat("Confidence Interval:", lower_limit, "-", upper_limit, "\n")
#we can say the same looking at their density plot.
plot(density(predicted_salaries), main = "Model mean distribution", xlab = "Media", col = "green")
plot(density(bootstrap_results), main = "Bootstrap mean distribution", xlab = "Media", col = "green")
abline(v=upper_limit,col="red",lty=2)
abline(v=lower_limit,col="red",lty=2)
abline(v=bootstrap_mean,col="blue",lty=1)
legend_text <- c("Confidence Interval (2.5% - 97.5%)", paste("Bootstrap Mean =", round(bootstrap_mean, 2)))
legend("topright", legend = legend_text, col = c("red", "blue"), lty = c(2, 1))
par(mfrow = c(1, 1))


# --------------------------Confidence interval of THE ORGINAL DATA------------------------------------
se <- sd(predicted_salaries) / sqrt(length(predicted_salaries))
# Calcula el valor crítico de la distribución t para el nivel de confianza del 95%
t <- qt(0.975, df = length(predicted_salaries) - 1)
# Confidence Interval
ci_lower <- mean(predicted_salaries) - t * se
ci_upper <- mean(predicted_salaries) + t * se
cat("Confidence Interval:", ci_lower, "-", ci_upper, "\n")
#----------------------------------------------------------------------------------------------------
#the bootstrap confidence interval is narrower than the original confidence interval, 
#it suggests that the bootstrap method has provided a more precise estimate of the population parameter.




#-----------------------THE SAME STUDY FOR THE LOG MODEL------------------------------------------------
predicted_salaries<-exp(predict(model_log))
mean_salary=mean(predicted_salaries)
# Create a vector to store the bootstrap results
bootstrap_results <- vector("numeric", 10000)
# Loop through the bootstrap iterations
for(i in 1:10000){
  # Sample with replacement from the predicted salaries
  bootstrap_sample <- sample(predicted_salaries, size = length(predicted_salaries), replace = TRUE)
  # Calculate the mean of the bootstrap sample
  bootstrap_mean <- mean(bootstrap_sample)
  # Store the bootstrap mean in the bootstrap_results vector
  bootstrap_results[i] <- bootstrap_mean
}

#-------------------------- Comparison of distributions NON PARAMETRIC (LOG MODEL)-----------------------------
bootstrap_mean <- mean(bootstrap_results)
bootstrap_mean#131837.9
mean_salary#131847.3
#The means are mostly equal, lets see how the are distributed.
# Histograms (data with no outliers vs nonparametric Bootrsap)
par(mfrow = c(1, 2))
hist((predicted_salaries), main = "Log Model prediction salary distribution", xlab = "Media", col = "green",breaks=80)
hist((bootstrap_results), main = "Bootstrap salary prediction distribution", xlab = "Media", col = "green",breaks=80)
#We can see that there are NO negative predictions in any model distribution due to the log transformation
#The predicred_salaries does NOT FOLLOW a normal distribution
#The bootstrap results follows a normal distribution.
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))

#  ------------------------------Condidence Interval NON PARAMETRIC BOOTSTRAP (LOG MODEL)------------------------
alpha <- 0.05  
sorted_results <- sort(bootstrap_results)
lower_limit <- sorted_results[round(alpha/2 * length(sorted_results))]
upper_limit <- sorted_results[round((1 - alpha/2) * length(sorted_results))]
cat("Confidence Interval:", lower_limit, "-", upper_limit, "\n")


plot(density(predicted_salaries), main = "Log Model mean distribution", xlab = "Media", col = "green")
plot(density(bootstrap_results), main = "Bootstrap mean distribution", xlab = "Media", col = "green")

abline(v=upper_limit,col="red",lty=2)
abline(v=lower_limit,col="red",lty=2)
abline(v=bootstrap_mean,col="blue",lty=1)

legend_text <- c("Confidence Interval (2.5% - 97.5%)", paste("Bootstrap Mean =", round(bootstrap_mean, 2)))
legend("topright", legend = legend_text, col = c("red", "blue"), lty = c(2, 1))
par(mfrow = c(1, 1))




#---------------------------PARAMETRIC BOOTSTRAP--------------------------------------------------------------
#-----------PARAMETRIC BOOSTRAP NORMAL DISTRIBUTION (NO OUTLIERS MODEL)-----------------------------------------
predicted_salaries<-predict(new_model)
model <- lm(salary_in_usd ~ job_title + company_size + work_year + experience_level + employee_residence, data = ds_salaries_no_outliers)
coefficients <- coef(model)
sqrt_residual_variance <- sigma(model)
bootstrap_results <- vector("numeric", 1000)

for (i in 1:1000) {
  bootstrap_sample <- rnorm(length(predicted_salaries), mean = predicted_salaries, sd =sqrt_residual_variance)
  bootstrap_mean <- mean(bootstrap_sample)
  bootstrap_results[i] <- bootstrap_mean
}
bootstrap_mean <- mean(bootstrap_results)
bootstrap_mean

# --------------------------------Comparison of distributions (NO OUTLIERS MODEL)-------------------------------------------------------------------
par(mfrow = c(1, 2))
hist((predicted_salaries), main = "Model salary prediction distribution", xlab = "Mean", col = "green",breaks=80)
#The predicted values does not follow a normal distribution.
hist((bootstrap_results), main = "", xlab = "Mean", col = "green",breaks=80)
#Bootstrap Results follow more or less a normal distribution
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))
#  ------------------------------Condidence Interval PARAMETRIC BOOTSTRAP (NO OUTLIERS MODEL)------------------------
alpha <- 0.05  
sorted_results <- sort(bootstrap_results)
lower_limit <- sorted_results[round(alpha/2 * length(sorted_results))]
upper_limit <- sorted_results[round((1 - alpha/2) * length(sorted_results))]
cat("Confidence Interval:", lower_limit, "-", upper_limit, "\n")
plot(density(predicted_salaries), main = "Model distribution", xlab = "Mean", col = "green")
plot(density(bootstrap_results), main = "Bootstrap distribution", xlab = "Mean", col = "green")
abline(v=upper_limit,col="red",lty=2)
abline(v=lower_limit,col="red",lty=2)
abline(v=bootstrap_mean,col="blue",lty=1)
legend_text <- c("Confidence Interval (2.5% - 97.5%)", paste("Bootstrap Mean =", round(bootstrap_mean, 2)))
legend("topright", legend = legend_text, col = c("red", "blue"), lty = c(2, 1))
par(mfrow = c(1, 1))




#------------------------------------------SAME STUDY FOR LOG MODEL----------------------------------------
mean_salary=mean(predicted_salaries)
coefficients <- coef(model_log)
sqrt_residual_variance <- sigma(model)
bootstrap_results <- vector("numeric", 1000)

for (i in 1:1000) {
  bootstrap_sample <- rnorm(length(predicted_salaries), mean = predicted_salaries, sd =sqrt_residual_variance)
  bootstrap_mean <- mean(bootstrap_sample)
  bootstrap_results[i] <- bootstrap_mean
}

#-------------------------- Comparison of distributions NON PARAMETRIC (LOG MODEL)-----------------------------
bootstrap_mean <- mean(bootstrap_results)
bootstrap_mean#131866
mean_salary#131847.3
#The means are mostly equal, lets see how they are distributed.
# Histograms (data with no outliers vs nonparametric Bootrsap)
par(mfrow = c(1, 2))
hist((predicted_salaries), main = "Log Model prediction salary distribution", xlab = "Media", col = "green",breaks=80)
hist((bootstrap_results), main = "Bootstrap salary prediction distribution", xlab = "Media", col = "green",breaks=80)
#We can see that there are NO negative predictions in any model distribution due to the log transformation
#The predicred_salaries does NOT FOLLOW a normal distribution
#The bootstrap results follows a normal distribution.
par(mfrow = c(1, 1))
par(mfrow = c(1, 2))

#  ------------------------------Condidence Interval NON PARAMETRIC BOOTSTRAP (LOG MODEL)------------------------
alpha <- 0.05  
sorted_results <- sort(bootstrap_results)
lower_limit <- sorted_results[round(alpha/2 * length(sorted_results))]
upper_limit <- sorted_results[round((1 - alpha/2) * length(sorted_results))]
cat("Confidence Interval:", lower_limit, "-", upper_limit, "\n")


plot(density(predicted_salaries), main = "Log Model mean distribution", xlab = "Media", col = "green")
plot(density(bootstrap_results), main = "Bootstrap mean distribution", xlab = "Media", col = "green")

abline(v=upper_limit,col="red",lty=2)
abline(v=lower_limit,col="red",lty=2)
abline(v=bootstrap_mean,col="blue",lty=1)

legend_text <- c("Confidence Interval (2.5% - 97.5%)", paste("Bootstrap Mean =", round(bootstrap_mean, 2)))
legend("topright", legend = legend_text, col = c("red", "blue"), lty = c(2, 1))
par(mfrow = c(1, 1))















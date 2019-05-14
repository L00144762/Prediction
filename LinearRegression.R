# example uses "women" dataset
# that contains height and weight of 15 women aged 30 - 39

str(women)


simple_linear_model <- lm(weight ~ height, data = women) # dependant variable is height (x-axis)
simple_linear_model
# shows the intercept and beta coefficient for height variable
# ie weight = -87.52 + 3.45 x height

plot(women$height,
     women$weight, 
     xlab = "Height (inches)",
     ylab = "Weight (lbs)",
     main = "Scatter plot showing regression line for weight
     predicted from height")
     


abline(simple_linear_model)

summary(simple_linear_model)

confint(simple_linear_model)

# cor measures the level of association between two variables
# and ranges from -1 (perfect negative correlation) to +1 (perfectly positive correlation)
# a value cose to 0 indicates a weak relationship
# a low correlation (-.02 < 0 < 0.2) suggests that muich of the variation
# of the outcome variable is not explained by the predictor
# in such case, we should then look at better predictor variables
cor(women$height, women$weight)

# Cars dataset
library(car)
head(cars)

# Visualise if there is any relationship (linear) between 
# the independant and dependant variables
scatter.smooth(x = cars$speed, 
               y = cars$dist,
               main = 'distance vs speed',
               xlab = 'car speed',
               ylab = 'stopping distance')

# two charts side by side
par(mfrow = c(1,2)) #c(1,2) = 1 row two charts, divides graph into 2 columns

boxplot(cars$speed, main = "Car Speed", 
        sub = paste("Outlier rows"),
        boxplot.stats(cars$speed)$outliers)
# cant see any outliers

boxplot(cars$dist, main = "Stopping Distance", 
        sub = paste("Outlier rows"),
        boxplot.stats(cars$dist)$outliers)
# one outlier

# there's a linear relationship; as one increases so does the other 
# if there wasn't, then the plot line would just run straight across

install.packages("e1071")
library(e1071)


plot(density(cars$speed), 
     main = "Density plot for speed",
     ylab = "Freq")
polygon(density(cars$speed), col = "red")




par(mfrow = c(1,2))  #dividing into two columns

plot(density(cars$speed), 
     main = "Density plot for speed (with skewness)",
     ylab = "Freq",
       sub = paste("Skewness : ", round(e1071::skewness(cars$speed), 2))) #::skewness - using 
                                                                        #skewness function from e1071
# fill in the area under the chart with colour
polygon(density(cars$speed), col = "red")

plot(density(cars$dist), 
     main = "Density plot for dist (with skewness)",
     ylab = "Freq",
     sub = paste("Skewness : ", round(e1071::skewness(cars$dist), 2))) #::skewness - using 
                                                                      #skewness function from e1071
polygon(density(cars$dist), col = "red")

cor(cars$speed, cars$dist)

# Build linear relation model on full data

linear_model <- lm(dist ~ speed, data = cars)
print(linear_model)

model_summary <- summary(linear_model)

#  model coefficients 
model_coefficients <- model_summary$coefficients
model_coefficients

#****** use t value stats if using linear regr. for CA4 ****

AIC(linear_model)
BIC(linear_model)

# -------------------------------------------------------------
# set.seed sets the "randomness"; a way of unrandomising the randomness
set.seed(200)

# select a random sample
# from 1:all records in cars, with 80% of rows
total_records <- sample(1:nrow(cars), 
                           0.8 * nrow(cars))

trainin_data <- cars[total_records,]
trainin_data

testing_data <- cars[-total_records,] #takes the 20% not used in total_records
testing_data

# build model using training data
lr_model <- lm(dist ~ speed, data = trainin_data)

# model summary
summary(lr_model)

# predict distance from testing data
distance_predicted <- predict(lr_model, testing_data)

# make an actual vs predicted dataframe
actuals_preds <- data.frame(actuals = testing_data$dist,
                            predicted = distance_predicted)
actuals_preds

correlation_accuracy <- cor(actuals_preds$actuals, actuals_preds$predicted)
# ~0.90 correlation which is fairly good

# min - max accuracy 
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / 
                           apply(actuals_preds, 1, max))
min_max_accuracy



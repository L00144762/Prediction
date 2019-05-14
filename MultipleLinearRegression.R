# Multiple linear regression

# We use the " : " to indicate an interaction between pedictor variables
# predictor variables 
multiple_linear_model <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(multiple_linear_model)

# We can visualise interaction using the effects() function
# This means we can change the values for wt 
# and view the changes graphically

install.packages("effects")
library("effects")

plot(effect("hp:wt",
            multiple_linear_model,, # two commas because there's an option not being used
            list(wt = c(2.2, 3.2, 4.2))),
     multiline = TRUE) 

# Evaluate the stastical assumption using the plot() function

par(mfrow = c(1, 4))
plot(multiple_linear_model)


# Global Validation of Linear Model Assumption

install.packages("gvlma")
library(gvlma)
gv_model <- gvlma(multiple_linear_model)
summary(gv_model) # p-value is looking good. model is ready to go 

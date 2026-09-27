  

#libraries and packages
library(AER)
library(dplyr)
library(lmtest)
library(car)
library(ggplot2)
library(GGally)
library(MASS)
install.packages("MLmetrics")
library(MLmetrics)


#Loading the dataset

setwd("C:/Users/adamr/OneDrive - University of Edinburgh/Year 3/Predictive Analytics/PAB-Coursework1")

oj <- read.csv("C:/Users/adamr/OneDrive - University of Edinburgh/Year 3//Predictive Analytics/PAB-Coursework1/oj.csv")

#Incase plots break after clearing plot output
graphics.off()

head(oj)

oj$brand <- as.factor(oj$brand) 
oj$store <- as.factor(oj$store)
oj$feature <- as.factor(oj$feature)

# Question 1

dim(oj)
#summary of entire dataset
summary(oj)
#summary of each brand
summary(oj[oj$brand == "dominicks", ])
summary(oj[oj$brand == "minute.maid", ])
summary(oj[oj$brand == "tropicana", ])
#summary for each store
summary(oj[oj$store == 12, ])
summary(oj[oj$store == 9, ])
summary(oj[oj$store == 8, ])
summary(oj[oj$store == 5, ])
summary(oj[oj$store == 2, ])



# Question 2


pdf("Q2_Histogram_Logmove.pdf", width = 8, height = 6)


h <- hist(oj$logmove,
          col = "steelblue",
          main = "Histogram of Logmove (Log of number of units sold)",
          xlab = "Log of Unit Sales",
          breaks = 12)

# Generate x values
x_vals <- seq(min(oj$logmove), max(oj$logmove), length = 100)

# Add properly scaled normal curve for dist comparison
lines(x_vals,
      dnorm(x_vals,
            mean = mean(oj$logmove),
            sd = sd(oj$logmove)) *
        length(oj$logmove) *
        diff(h$breaks)[1],
      col = "red",
      lwd = 2)

dev.off()




# histogram of raw sales data in order to compare skewedness


pdf("Q2_Histogram_Logmove_rawsales(optional).pdf", width = 8, height = 6)

hist(exp(oj$logmove), 
     col = "darkred", 
     main = "Histogram of Units Sold (Original Scale)", 
     xlab = "Actual Units Sold", 
     breaks = 30)

dev.off()







# Question 3


store2_data <- subset(oj, store ==2)

tapply(store2_data$logmove, store2_data$brand, summary)
tapply(store2_data$logmove, store2_data$brand, sd)


pdf("Q3_Logmove_by_Brand_Store2.pdf", width = 8, height = 6)

plot(as.factor(store2_data$brand), store2_data$logmove,
     main = "Logmove by Brand for Store 2",
     xlab = "Brand",
     ylab = "Logmove",
     col = c( "Orange", "red" , "yellow"))

dev.off()



#the histogram to investigate skewness and support box median line claim
summary(store2_data) # for finding optimal bin number, using same rule as in Q2

#Dominick's Histogram
pdf("Hist_Dominicks_Store2.pdf", width = 5, height = 4)
dom_store2_data <- subset(store2_data, brand == "dominicks")
hist(dom_store2_data$logmove,
     main = "Dominick's Logmove Distribution",
     xlab = "Logmove",
     col = "orange",
     breaks = 8)
dev.off()

#Minute Maid Histogram
pdf("Hist_MinuteMaid_Store2.pdf", width = 5, height = 4)
mm_store2_data <- subset(store2_data, brand == "minute.maid")
hist(mm_store2_data$logmove,
     main = "Minute Maid Logmove Distribution",
     xlab = "Logmove",
     col = "red",
     breaks = 8)
dev.off()

#Tropicana Histogram
pdf("Hist_Tropicana_Store2.pdf", width = 5, height = 4)
trop_store2_data <- subset(store2_data, brand == "tropicana")
hist(trop_store2_data$logmove,
     main = "Tropicana Logmove Distribution",
     xlab = "Logmove",
     col = "yellow",
     breaks = 8)
dev.off()







# Question 4: Simple Linear regression 


oj$logprice <- log(oj$price)


model_simple <- lm( logmove ~ logprice, data = oj)

#Checking assumptions:

pdf("Q4_Simple_Model_Diagnostics.pdf", width = 8, height = 8)

par(mfrow=c(2,2))
plot(model_simple)

dev.off()

par(mfrow=c(1,1))

#Supporing linearity plot

pdf("Q4_Simple_Model_Linearity.pdf", width = 8, height = 8)
ggplot(oj, aes(x = logprice, y = logmove)) +
  geom_point(alpha = 1, color = "black") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Linear line
  geom_smooth(method = "loess", color = "blue", se = FALSE) + 
  labs(title = "Linearity Check: Logmove vs Logprice",
       subtitle = "Red = Linear Model, Blue = Loess Smoother") 
dev.off()


#Checking for normality
shapiro.test(oj$logmove)
#checking for autocorrelation

dwtest(model_simple)

#Visually checking autocorrelation

pdf("Q4_Simple_Model_ACF.pdf")

acf(model_simple$residuals)

dev.off()

# Zero Conditional mean:

pdf("Q4_Simple_Model_Residual_Plots.pdf")
residualPlots(model_simple)# also produces the Tukey and Curvature tests

dev.off()
residualPlots(model_simple)

summary(model_simple)




# Question 5
# a 
# i need to introduce k-1 dummy vars where k is the unique stores and brands
#as.factor() is a data structure used to categorize data, R automatically creates N-1 dummy variable used earlier in code
# for categorical variables with N levels

# Check the levels of the Brand variable
levels(oj$brand)

levels(oj$store)
# View the dummy coding matrix for Brand
contrasts(oj$brand)
contrasts(oj$store)

numberOfBrands <- length(unique(oj$brand))
numberOfStores <- length(unique(oj$store))


#k-1 req
dum_brand <- numberOfBrands - 1
dum_store <- numberOfStores - 1

print(paste(("Number of Brands"),numberOfBrands, "Number of Dummy Variables for Brands", dum_brand))
print(paste(("Number of Stores"),numberOfStores, "Number of Dummy Variables for Stores", dum_store))


#b
set.seed(123) # set to seed to ensure i get the same results every time



train_ind <- sample.int(n = nrow(oj), size = floor(.75*nrow(oj)), replace = F) #sample takes samples randomly according to seed, replace F makes sure they're disjoint

#creating the training and validation set
oj_train = oj[train_ind, ]
oj_test = oj[-train_ind, ] #contains all entries not in the training set

#checking dimensions 
dim(oj)
dim(oj_train)
dim(oj_test)


#c



model_mlr_oj <- lm(logmove ~ logprice + feature + week + brand + store, data= oj_train)

#Checking Ols assumptions

#plotting model with diagnostic plots
levels(oj_train$feature)

pdf("Q5c_Multiple_Model_Diagnostics.pdf", width = 8, height = 8)

par(mfrow=c(2,2))
plot(model_mlr_oj)

dev.off()

par(mfrow=c(1,1))
crPlots(model_mlr_oj)

#investigating linearity in the continuous variables

# Component-plus-residual plot for logprice
pdf("Q5c_CRPlot_logprice.pdf", width = 6, height = 6)
crPlot(model_mlr_oj, "logprice", main = "Component-plus-Residual Plot: logprice")
dev.off()

# Component-plus-residual plot for week
pdf("Q5c_CRPlot_week.pdf", width = 6, height = 6)
crPlot(model_mlr_oj, "week", main = "Component-plus-Residual Plot: week")
dev.off()


#Checking Independence of Errors ( autocorrelation)

dwtest(model_mlr_oj)


pdf("Q5c_Multiple_Model_ACF.pdf", width = 8, height = 8)
acf(model_mlr_oj$residuals)
dev.off()

#Checking for MultiColinearity


#using VIF
vif(model_mlr_oj)
#Visually

x <- (oj_train[,-4])

pdf("Q5c_Multiple_Model_ggpairs.pdf", width = 8, height = 8)

ggpairs(x)

dev.off()

#Checking for Zero Conditional Mean of Errors assumption
pdf("Q5c_Multiple_Model_ResidualPlots.pdf", width = 8, height = 8)

residualPlots(model_mlr_oj) # also produces the Tukey and Curvature tests

dev.off()


#model results


summary(model_mlr_oj)





#d testing for non-linear effects

model_mlr_oj_nl <- lm(logmove ~ logprice + I(logprice^2) + feature + week + brand + store, data= oj_train)

summary(model_mlr_oj_nl)

# Anova test to compare quadratic model to with the original model
anova( model_mlr_oj,model_mlr_oj_nl)



# e: adding the interaction terms

model_mlr_oj_it <- lm(logmove ~ logprice * brand + feature + week  + store, data= oj_train)

summary(model_mlr_oj_it)

anova(model_mlr_oj,model_mlr_oj_it)

#f: performing a step-wise AIC



model_null <- lm(logmove ~1, data=oj_train) #defining the starting null model

model_full <- lm(logmove ~ logprice + feature + week + brand + store, data = oj_train)

#performing the stepwise AIC

step_forward <- stepAIC(model_null, 
                        scope = list(lower = model_null, upper = model_full), 
                        direction = "forward")

summary(step_forward)


#g 


pred_train = predict(model_mlr_oj, oj_train)



mse_train <- MSE(y_pred = pred_train, y_true = oj_train$logmove)
print(paste("Training MSE:", mse_train))

pdf("Q5g_Actual_vs_Predicted_Training.pdf", width = 8, height = 6)

plot_actual_predicted <- plot(x =oj_train$logmove,
                              y= pred_train,
                              main ="Actual vs predicted logmove from the training set",
                              xlab="Actual logmove",
                              ylab="Predicted logmove",
                              pch=20,
                              col = "steelblue")

# reference line
abline(0, 1, col = "red", lwd = 2)


dev.off()

# h 


# Ensure factor levels match exactly what was used in training
oj_test$brand <- as.factor(oj_test$brand)
oj_test$store <- as.factor(oj_test$store)
oj_test$feature <- as.factor(oj_test$feature)

#checking all levels are present
levels(oj_test$brand)
levels(oj_test$store)
levels(oj_test$feature)

#retrianing the model from Q4 on the Testing set
model_simple_train <- lm( logmove ~ logprice, data = oj_train)

#Accuracy for Simple Model (from Question 4)
pred_simple <- predict(model_simple_train, oj_test)
mse_simple <- MSE(y_pred = pred_simple, y_true = oj_test$logmove)

#Accuracy for Multiple Model (from Question 5c), validation MSE
pred_multiple <- predict(model_mlr_oj, oj_test)
mse_multiple <- MSE(y_pred = pred_multiple, y_true = oj_test$logmove)

# Compute training MSEs
pred_simple_train <- predict(model_simple_train, oj_train)
pred_multiple_train <- predict(model_mlr_oj, oj_train)

mse_simple_train <- MSE(y_pred = pred_simple_train, y_true = oj_train$logmove)
mse_multiple_train <- MSE(y_pred = pred_multiple_train, y_true = oj_train$logmove)

# Get Adjusted R-squared from model summaries
adjr2_simple <- summary(model_simple_train)$adj.r.squared
adjr2_multiple <- summary(model_mlr_oj)$adj.r.squared

#building table for results
model_comparison <- data.frame(
  Model = c("Simple (Model 2)", "Multiple (Model 4)"),
  Train_MSE = round(c(mse_simple_train, mse_multiple_train), 4),
  Val_MSE = round(c(mse_simple, mse_multiple), 4),
  Train_RMSE = round(c(sqrt(mse_simple_train), sqrt(mse_multiple_train)), 4),
  Val_RMSE = round(c(sqrt(mse_simple), sqrt(mse_multiple)), 4),
  Adj_R2 = round(c(adjr2_simple, adjr2_multiple), 4)
)

#comparison results table
print(model_comparison)

#setting y scale to be the same for each for better side by side comparison 

y_min <- min(c(pred_simple, pred_multiple, oj_test$logmove))
y_max <- max(c(pred_simple, pred_multiple, oj_test$logmove))

#plots
pdf("Q5g_Actual_vs_Predicted_Validation_Simple.pdf", width = 8, height = 6)

plot(x = oj_test$logmove,
     y = pred_simple,
     ylim = c(y_min, y_max),
     main = paste("Actual vs Predicted logmove - Simple Model On The Validation Set\nMSE =", round(mse_simple, 3)),
     xlab = "Actual logmove",
     ylab = "Predicted logmove",
     pch = 20,
     col = "steelblue")

# Reference line
abline(0, 1, col = "red", lwd = 2)

dev.off()

# Plot actual vs predicted for Multiple Model
pdf("Q5g_Actual_vs_Predicted_Validation_Multiple.pdf", width = 8, height = 6)

plot(x = oj_test$logmove,
     y = pred_multiple,
     ylim = c(y_min, y_max),
     main = paste("Actual vs Predicted logmove - Multiple Model On The Validation Set\nMSE =", round(mse_multiple, 3)),
     xlab = "Actual logmove",
     ylab = "Predicted logmove",
     pch = 20,
     col = "darkgreen")

# Reference line
abline(0, 1, col = "red", lwd = 2)

dev.off()






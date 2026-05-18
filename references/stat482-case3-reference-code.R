remove(list=ls())#clear objects from previous session

#Set working directory
wk_cs3 <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(wk_cs3)

#Dataset: Generating the IT Hardware Sales Data
# Set seed for reproducibility
set.seed(42)

# Generate 48 months of IT hardware sales data
# Starting from January 2020
months <- 48
time_points <- 1:months

# Components of the time series:
# 1. Baseline level
baseline <- 650

# 2. Linear growth trend (increasing demand over time)
trend <- 4.5 * time_points

# 3. Seasonal pattern (quarterly cycles)
# Q4 typically higher due to budget cycles
seasonal <- 120 * sin(2 * pi * time_points / 12 + pi/2)

# 4. Random noise
noise <- rnorm(months, mean = 0, sd = 25)

# Combine all components
sales <- baseline + trend + seasonal + noise

# Create data frame
it_data <- data.frame(
 Month = seq(as.Date("2020-01-01"), 
             by = "month", 
             length.out = months),
 Sales = round(sales, 2)
)

# Display first few rows
head(it_data, 10)

# Summary statistics
summary(it_data$Sales)

#Loading and Preparing IT Dataset
#--------------------------------
# Load required libraries
library(forecast)
library(ggplot2)
library(tseries)

# Use the it_data generated from previous slide
# Convert to time series object
it_ts <- ts(it_data$Sales, 
            start = c(2020, 1), 
            frequency = 12)

# Display time series structure
print(it_ts)
str(it_ts)

# Basic summary
cat("Time Series Range:", 
    min(it_ts), "to", max(it_ts), "\n")
cat("Mean Sales:", round(mean(it_ts), 2), "\n")

#Initial Time Series Visualization
#---------------------------------
# Create comprehensive time plot
autoplot(it_ts) +
 ggtitle("IT Hardware Sales Over Time") +
 xlab("Month") +
 ylab("Sales (USD Thousands)") +
 theme_minimal() +
 geom_smooth(method = "loess", se = TRUE, color = "#1A2D7A")

# Summary statistics
summary(it_ts)

# Check for missing values
sum(is.na(it_ts))

#Exploratory Analysis
#--------------------
#Decomposing Time Series Components
#----------------------------------
# Additive decomposition
# Use when seasonal variation is constant
decomp_add <- decompose(it_ts, 
                        type = "additive")
autoplot(decomp_add) +
 ggtitle("Additive Decomposition")

# Multiplicative decomposition
# Use when seasonal variation increases
# with trend level
decomp_mult <- decompose(it_ts, 
                         type = "multiplicative")
autoplot(decomp_mult) +
 ggtitle("Multiplicative Decomposition")

#Choosing Between Additive and Multiplicative Models
# Test which model fits better
# Lower standard deviation suggests better fit
sd(decomp_add$random, na.rm = TRUE)
sd(decomp_mult$random, na.rm = TRUE)

#Testing for Stationarity
#------------------------
# Augmented Dickey-Fuller test
# H0: Series has a unit root (non-stationary)
# H1: Series is stationary
adf_test <- adf.test(it_ts, alternative = "stationary")
print(adf_test)

# KPSS test (complementary)
# H0: Series is stationary
# H1: Series is non-stationary
kpss_test <- kpss.test(it_ts)
print(kpss_test)

# Visual check: ACF and PACF plots
par(mar=c(1,1,1,1))
par(mfrow = c(2, 1))
acf(it_ts, main = "ACF Plot")
pacf(it_ts, main = "PACF Plot")
par(mfrow = c(1, 1))

#Differencing to Achieve Stationarity
#------------------------------------
# First-order differencing
# Removes linear trend
it_diff1 <- diff(it_ts, differences = 1)

# Plot differenced series
autoplot(it_diff1) +
 ggtitle("First-Order Differenced Series") +
 ylab("Change in Sales")

# Test differenced series for stationarity
adf.test(it_diff1, alternative = "stationary")

# Seasonal differencing (lag = 12)
# Removes seasonal pattern
it_diff_seasonal <- diff(it_ts, lag = 12)

# Combined: seasonal + first-order
it_diff_both <- diff(it_diff_seasonal, 
                     differences = 1)
#Model Development
#-----------------
#Fitting ARIMA Models
#--------------------
# Automatic model selection
# auto.arima() uses AIC to find optimal p,d,q
arima_auto <- auto.arima(it_ts,
                         seasonal = TRUE,
                         stepwise = FALSE,
                         approximation = FALSE,
                         trace = TRUE)

# Display selected model
summary(arima_auto)

# Manual specification (if needed)
arima_manual <- Arima(it_ts,
                      order = c(1, 1, 1),
                      seasonal = c(1, 1, 1))

# Compare models using AIC and BIC
AIC(arima_auto, arima_manual)
BIC(arima_auto, arima_manual)

#Fitting Exponential Smoothing (ETS) Models
#------------------------------------------
# Automatic ETS model selection
# ETS framework: Error, Trend, Seasonal
ets_model <- ets(it_ts, model = "ZZZ")

# Display model details
summary(ets_model)

# Extract components
autoplot(ets_model) +
 ggtitle("ETS Model Components")

# Specific model types:
# "ANN" = Simple exponential smoothing
# "AAN" = Holt's linear trend
# "AAA" = Holt-Winters additive
# "MAM" = Holt-Winters multiplicative

# Fit specific model if needed
ets_specific <- ets(it_ts, model = "MAM")
summary(ets_specific)

#Diagnostic Checking: Residual Analysis
#-------------------------------------
# Comprehensive residual diagnostics
checkresiduals(arima_auto)

# Ljung-Box test
# H0: Residuals are independently distributed
# p-value > 0.05 suggests good fit
Box.test(residuals(arima_auto), 
         lag = 20, 
         type = "Ljung-Box")

# Normality test
shapiro.test(residuals(arima_auto))

# Plot residuals vs fitted
plot(fitted(arima_auto), 
     residuals(arima_auto),
     main = "Residuals vs Fitted")

#Comparing ARIMA vs. ETS Performance
#----------------------------------
# Extract accuracy metrics
accuracy(arima_auto)
accuracy(ets_model)

# Time series cross-validation
# Test forecast accuracy on rolling windows
arima_cv <- tsCV(it_ts, 
                 function(x, h) {
                  forecast(auto.arima(x), h = h)
                 }, 
                 h = 6)

ets_cv <- tsCV(it_ts,
               function(x, h) {
                forecast(ets(x), h = h)
               },
               h = 6)

# Calculate RMSE for both
sqrt(mean(arima_cv^2, na.rm = TRUE))
sqrt(mean(ets_cv^2, na.rm = TRUE))

#Creating Short-Term Forecasts
#----------------------------
# Generate 6-month forecast from ARIMA
arima_forecast <- forecast(arima_auto, 
                           h = 6,
                           level = c(80, 95))

# Display forecast values
print(arima_forecast)

# Visualize with prediction intervals
autoplot(arima_forecast) +
 ggtitle("ARIMA 6-Month Forecast") +
 xlab("Month") +
 ylab("Sales (USD Thousands)") +
 theme_minimal()

# Generate ETS forecast
ets_forecast <- forecast(ets_model, 
                         h = 6,
                         level = c(80, 95))

autoplot(ets_forecast) +
 ggtitle("ETS 6-Month Forecast")

#Extracting Forecast Values
#-------------------------
# Access forecast point estimates
forecast_values <- as.numeric(arima_forecast$mean)
print(forecast_values)

# Create forecast data frame with intervals
forecast_df <- data.frame(
 Month = seq(as.Date("2024-01-01"), 
             by = "month", 
             length.out = 6),
 Forecast = forecast_values,
 Lower_80 = arima_forecast$lower[, 1],
 Upper_80 = arima_forecast$upper[, 1],
 Lower_95 = arima_forecast$lower[, 2],
 Upper_95 = arima_forecast$upper[, 2]
)

print(forecast_df)

# Export to CSV for stakeholder review
write.csv(forecast_df, 
          "it_sales_forecast.csv", 
          row.names = FALSE)

#Model Validation
#----------------
#Calculating RMSE and MAPE
#-------------------------
# Split data: train on first 42 months
# test on last 6 months
train_ts <- window(it_ts, 
                   end = c(2023, 6))
test_ts <- window(it_ts, 
                  start = c(2023, 7))

# Fit model on training data
model_train <- auto.arima(train_ts)

# Forecast test period
forecast_test <- forecast(model_train, 
                          h = 6)

# Calculate accuracy metrics
accuracy(forecast_test, test_ts)

#Interpreting Accuracy Metrics
#----------------------------
# Manual MAPE calculation
actual <- as.numeric(test_ts)
predicted <- as.numeric(forecast_test$mean)

mape <- mean(abs((actual - predicted) / actual)) * 100

cat("MAPE:", round(mape, 2), "%\n")

# Manual RMSE calculation
rmse <- sqrt(mean((actual - predicted)^2))

cat("RMSE:", round(rmse, 2), "USD Thousands\n")

# Visual comparison
comparison_df <- data.frame( Month = 1:6, Actual = actual,
                             Predicted = predicted
)

#--------------------------------
#Complete Reproducible Workflow
#--------------------------------
# Step 1: Generate the dataset (from slide 3)
set.seed(42)
months <- 48
time_points <- 1:months
baseline <- 650
trend <- 4.5 * time_points
seasonal <- 120 * sin(2 * pi * time_points / 12 + pi/2)
noise <- rnorm(months, mean = 0, sd = 25)
sales <- baseline + trend + seasonal + noise
it_data <- data.frame(
 Month = seq(as.Date("2020-01-01"), by = "month", length.out = months),
 Sales = round(sales, 2)
)

# Step 2: Create time series object
it_ts <- ts(it_data$Sales, start = c(2020, 1), frequency = 12)

# Step 3: Fit models
arima_auto <- auto.arima(it_ts, seasonal = TRUE)
ets_model <- ets(it_ts)

# Step 4: Generate forecasts
forecast_arima <- forecast(arima_auto, h = 6)
forecast_ets <- forecast(ets_model, h = 6)

# Step 5: Visualize
autoplot(forecast_arima) + 
 ggtitle("6-Month IT Hardware Sales Forecast")

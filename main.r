#load libraries
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tseries)
library(gridExtra)


# load data
data <- read.delim("CACONDE.txt", header = FALSE, sep="\n")

colnames(data) <- c("value")
start_date <- as.Date("2015-01-01")
data$date <- seq.Date(from = start_date, by = "month", length.out = nrow(data))

ts_data <- ts(data$value, start = c(2015,1), frequency = 12)

# 1. Use visual exploration tools to understand the dataset. Investigate aspects
# such as time dependence, seasonality, trends, and data behavior.
summary(data)

# time series plot
ggplot(data, aes(x = date, y = value)) +
  geom_line(color = "blue") +
  labs(title = "Time Series Plot", x = "Date", y = "Useful water %") +
  theme_minimal()


# seasonality?
data$month <- month(data$date, label = TRUE)
data$year <- year(data$date)

spectrum(ts_data, main = "Periodogram of Series", col = "purple", log = "no")
# spectogram indicates strong long-term seasonality (yearly)
# but no short-term seasonality (month to month)

# monthly average visually
monthly_avg <- data %>%
  group_by(month) %>%
  summarise(avg_value = mean(value, na.rm = TRUE))

ggplot(monthly_avg, aes(x = month, y = avg_value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Seasonality - Monthly Average", x = "Month", y = "Avg useful water %") +
  theme_minimal()


# trend?
ggplot(data, aes(x = date, y = value)) +
  geom_line(alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.2, color = "darkred") +
  labs(title = "Trend Analysis with LOESS", x = "Date", y = "Useful water %") +
  theme_minimal()

# 2. Test the series for stationarity.
n <- nrow(data)
part1 <- data$value[1:(n/3)]
part2 <- data$value[(n/3 + 1):n]
part3 <- data$value[(n/3 + 2):n]

# mean
mean(part1); mean(part2); mean(part3)
# suggests stationary

# variance
var(part1); var(part2); var(part3)
# suggests stationary

# autocorrelation
acf(data$value, main = "Autocorrelation")
# may suggest non-stationary, could be due to strong seasonality

# Augmented Dickey-Fuller test
adf.test(data$value)
# strongly suggests stationary
# p < 0.05 => reject null hypothesis (non-stationary)
# data is stationary at the 5% significance level
# series is statistically stationary


# 3. Split the dataset into training and validation sets by reserving the last 6
# observations for forecasting evaluation.
train <- window(ts_data, end = c(2023, 8))
test <- window(ts_data, start = c(2023, 9))

# 4. Estimate an Autoregressive (AR) model using the dynamic regression ap-
# proach discussed in class. Select the appropriate number of covariates
# based on the data time dependence.
pacf(data$value, main = "Partial Autocorrelation") # suggests AR(1) or AR(2) will be good

ar_model <- Arima(train, order = c(2,0,0))

ar_forecast <- forecast(ar_model, h = 6)
plot(ar_forecast)


# 5. Estimate an ARMA model including seasonal components (e.g., sine and/or
# cosine terms) to account for seasonality.

# Create cos regressor matrix
cos_t <- cos(2 * pi * (1:length(data$value))/12)
xreg_train <- matrix(cos_t[1:length(train)], ncol = 1)
xreg_test  <- matrix(cos_t[(length(train) + 1):length(data$value)], ncol = 1)

# Fit ARMA with cosine regressor
arma_model <- Arima(train, order = c(2,0,2), xreg = xreg_train)
summary(ar_model)  # check if the regressor was included

# Forecast using new regressor values
arma_forecast <- forecast(arma_model, h = 6, xreg = xreg_test)
plot(arma_forecast)


# 6. Estimate a SARMA model.
sarma_model <- Arima(train, order = c(2,0,2),
                     seasonal = list(order = c(1,0,1), period = 12)) # try 2,0,2 order?

sarma_forecast <- forecast(sarma_model, h = 6)
plot(sarma_forecast)

# 7. Compare the three models. Which model is the most suitable for this
# dataset? Keep in mind that a model can only be used for prediction if
# it has been properly validated. To compare the forecasting performance,
# evaluate both in-sample and out-of-sample predictions using visual tools
# and performance metrics.

# Compare AIC and BIC
model_comparison <- data.frame(
  Model = c("AR", "ARMA", "SARMA"),
  AIC = c(AIC(ar_model), AIC(arma_model), AIC(sarma_model)),
  BIC = c(BIC(ar_model), BIC(arma_model), BIC(sarma_model))
)

print(model_comparison) # AR has lowest BIC, SARMA has lowest AIC


# Function to calculate MAPE (Mean Absolute Percentage Error)
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

# Compute errors for each model

results <- data.frame(
  Model = c("AR", "ARMA", "SARMA"),

  # In-sample (train) errors
  MSE_Train = c(
    mean((fitted(ar_model) - train)^2),
    mean((fitted(arma_model) - train)^2),
    mean((fitted(sarma_model) - train)^2)
  ),
  MAPE_Train = c(
    mape(train, fitted(ar_model)),
    mape(train, fitted(arma_model)),
    mape(train, fitted(sarma_model))
  ),

  # Out-of-sample (test) errors
  MSE_Test = c(
    mean((ar_forecast$mean - test)^2),
    mean((arma_forecast$mean - test)^2),
    mean((sarma_forecast$mean - test)^2)
  ),
  MAPE_Test = c(
    mape(test, ar_forecast$mean),
    mape(test, arma_forecast$mean),
    mape(test, sarma_forecast$mean)
  )
)

# Round only numeric columns
results[,2:5] <- round(results[,2:5], 2)

# Now print
print(results)


# Residual Analysis
par(mfrow=c(2,3))  # Arrange plots in 2 rows and 3 columns
plot(ar_model$residuals, main="AR Residuals", ylab="Residuals")
plot(arma_model$residuals, main="ARMA Residuals", ylab="Residuals")
plot(sarma_model$residuals, main="SARMA Residuals", ylab="Residuals")

acf(ar_model$residuals, main="AR ACF of Residuals")
acf(arma_model$residuals, main="ARMA ACF of Residuals")
acf(sarma_model$residuals, main="SARMA ACF of Residuals")


# Plot Forecasts
p1 <- autoplot(ar_forecast, main="AR Forecast")
p2 <- autoplot(arma_forecast, main="ARMA Forecast")
p3 <- autoplot(sarma_forecast, main="SARMA Forecast")

grid.arrange(p1, p2, p3, ncol=1)


# 8. Are there any other models that could be applied?
#load libraries
library(ggplot2)
library(lubridate)
library(tidyverse)
library(tseries)


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
# suggests non-stationary, could be due to strong seasonality

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


# 5. Estimate an ARMA model including seasonal components (e.g., sine and/or
# cosine terms) to account for seasonality.


# 6. Estimate a SARMA model.


# 7. Compare the three models. Which model is the most suitable for this
# dataset? Keep in mind that a model can only be used for prediction if
# it has been properly validated. To compare the forecasting performance,
# evaluate both in-sample and out-of-sample predictions using visual tools
# and performance metrics.


# 8. Are there any other models that could be applied?
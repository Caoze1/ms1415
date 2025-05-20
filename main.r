#load libraries
library(ggplot2)
library(lubridate)
library(tidyverse)


# load data
data <- read.delim("CACONDE.txt", header = FALSE, sep="\n")

colnames(data) <- c("value")
start_date <- as.Date("2015-01-01")
data$date <- seq.Date(from = start_date, by = "month", length.out = nrow(data))

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

# monthly average
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
# mean

# variance

# covariance / autocovariance

# 3. Split the dataset into training and validation sets by reserving the last 6
# observations for forecasting evaluation.


# 4. Estimate an Autoregressive (AR) model using the dynamic regression ap-
# proach discussed in class. Select the appropriate number of covariates
# based on the data time dependence.


# 5. Estimate an ARMA model including seasonal components (e.g., sine and/or
# cosine terms) to account for seasonality.


# 6. Estimate a SARMA model.


# 7. Compare the three models. Which model is the most suitable for this
# dataset? Keep in mind that a model can only be used for prediction if
# it has been properly validated. To compare the forecasting performance,
# evaluate both in-sample and out-of-sample predictions using visual tools
# and performance metrics.


# 8. Are there any other models that could be applied?
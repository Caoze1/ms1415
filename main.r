# 1. Use visual exploration tools to understand the dataset. Investigate aspects
# such as time dependence, seasonality, trends, and data behavior.


# 2. Test the series for stationarity.


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
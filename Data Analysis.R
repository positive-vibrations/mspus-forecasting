install.packages(c("fpp2","dplyr","readxl"))

library(fpp2)
library(readxl)
library(dplyr)

# import mean sale price data and create time series
mspus.data <- read.csv("MSPUS.csv", header = TRUE)
  ts.mspus <- ts(data = mspus.data$MSPUS, start = 2010, frequency = 4)

# import margin statistics (millions of dollars)
margin.statistics.data <- read_xlsx("margin-statistics.xlsx")
margin.statistics.data <- arrange(margin.statistics.data, margin.statistics.data$`Year-Month`)

ts.margin.data <- ts(margin.statistics.data$`Debit Balances in Customers' Securities Margin Accounts`, 
                     start = 1997, frequency = 12)

# format monthly data as quarterly and create time series
x <- margin.statistics.data$`Debit Balances in Customers' Securities Margin Accounts`
date <- seq(as.Date('1997-01-01'), as.Date('2022-03-01'), by = '1 month')
df1 <- data.frame(date, x)

margin.statistics.quarterly <- df1 %>% 
    group_by(quarter = quarters(date), year = lubridate::year(date)) %>% 
    summarise(x = mean(x)) %>% 
    arrange(year)

  ts.margin.stat <- ts(data = margin.statistics.quarterly$x, start = 1997, frequency = 4) 
  ts.margin <- window(ts.margin.stat, start = 2010)

  
  
  
  
# Graphs:
# Time Plots
autoplot(ts.mspus) +
  xlab("Year") + ylab("Dollars") +
  ggtitle("Median Sale Price of Houses in US")

autoplot(ts.margin.data) +
  xlab("Year") + ylab("Dollars") +
  ggtitle("US Margin Debt (Shown in Millions of Dollars)")

fit <- tslm(ts.mspus~ts.margin)
autoplot(ts.mspus) +
  autolayer(fitted(fit))
# spurious relationship?

dm <- diff(ts.mspus)

da <- diff(ts.margin, lag = 1)

autoplot(dm)

# difference using diff() and then scatterplot

qplot(dm,da) +
  xlab("Median Sale Price of Housing") + ylab("US Margin Debt(Shown in Millions of Dollars") +
  ggtitle("Differenced Scatter Plot")


# season plot
ggseasonplot(ts.mspus) +
  xlab("Year") + ylab("Dollars") +
  ggtitle("Season Plot for Median Sale Price of Houses in US")

# seasonal subseries plot
ggsubseriesplot(ts.mspus) +
  xlab("Year") + ylab("Dollars") +
  ggtitle("Seasonal Subseries Plot for Median Sale Price of Houses in US")

# scatterplot
qplot(ts.mspus, ts.margin) +
  xlab("Median Sale Price of Houses in US (Dollars)") + ylab("US Margin Debt (Dollars)")

# ACF plot
ggAcf(ts.mspus) +
  ggtitle("ACF Plot of Median Sale Price of Houses in US")



# TS Decomposition
# Classical
mspus.dc <- decompose(ts.mspus, type = c("multiplicative"))

margin.dc <- decompose(ts.margin, type = c("multiplicative"))

autoplot(mspus.dc) +
  ggtitle("Classical Time Series Decomposition: Median Sale Price of Houses in US")

autoplot(margin.dc) +
  ggtitle("Classical Time Series Decomposition: US Margin Debt (Shown in Millions of Dollars")

autoplot(seasadj(mspus.dc))

# STL
mspus.stl <- stl(ts.mspus,
    s.window = 13,
    t.window = 13)

margin.stl <- stl(ts.margin,
    s.window = 13,
    t.window = 13)

autoplot(margin.stl) +
  ggtitle("STL Time Series Decomposition: US Margin Debt (Shown in Millions of Dollars)")

autoplot(mspus.stl) +
  ggtitle("STL Time Series Decomposition: Median Sale Prices of Houses in US")






# Subset testing and training set for mspus
mspus.train <- window(ts.mspus, end = c(2018, 4))
mspus.test <- window(ts.mspus, start = 2019)

margin.train <- window(ts.margin, end = c(2018, 4))
margin.test <- window(ts.margin, start = 2019)

autoplot(ts.mspus) +
  autolayer(mspus.train, series = "Train") +
  autolayer(mspus.test, series = "Test") +
  xlab("Year") + ylab("Dollars") +
  ggtitle("MSPUS: Training and Test Sets")


# Simple forecasting methods

# naive
fit.mspus.naive <- rwf(mspus.train)

# seasonal naive
fit.mspus.snaive <- snaive(mspus.train, h = 13)

# drift
fit.mspus.drift <- rwf(mspus.train, h = 13, drift = TRUE)

# Simple forecast plots
autoplot(ts.mspus) +
  autolayer(fit.mspus.naive, 
            series = "Naive", PI = FALSE) +
  autolayer(fit.mspus.snaive,
            series = "Seasonal Naive", PI = FALSE) +
  autolayer(fit.mspus.drift,
            series = "Drift", PI = FALSE) +
  guides( color = guide_legend(title = "Forecasts")) + 
  xlab("Year") + ylab("Dollars") +
  ggtitle("Simple Forecasting Methods: Median Sales Price of Houses in US")



# Linear Regression Model: MSPUS ~ Margin Debt
fit1.mspus.lm <- tslm(mspus.train ~ margin.train + season)
summary(fit1.mspus.lm)

fit2.mspus.lm <- tslm(mspus.train ~ trend + season)
summary(fit2.mspus.lm)

fit.mspus.lm <- tslm(ts.mspus ~ ts.margin + season)

# Forecasts
fc1.mspus.lm <- forecast(fit1.mspus.lm, newdata = margin.test, h = 13)
fc2.mspus.lm <- forecast(fit2.mspus.lm, h = 13)


# Increase Scenario
margin.drift.up <- rwf(
  ts.margin, h = 13, drift = TRUE
  )

fc.data <- data.frame(
  ts.margin = margin.drift.up$mean
  )

fc_up.mspus.lm <- forecast(fit.mspus.lm, newdata = fc.data, h = 13)



# Decrease Scenario
margin.drift.down <- rwf(
  window(ts.margin, start = c(2021, 3)),
  drift = TRUE, h = 13
  )

fc.data <- data.frame(
  ts.margin = margin.drift.down$mean
  )

fc_down.mspus.lm <- forecast(fit.mspus.lm, newdata = fc.data, h = 13)

# Margin Debt Scenarios Plot
autoplot(ts.margin) +
  autolayer(margin.drift.down,
            series = "Decrease", PI = FALSE) +
  autolayer(margin.drift.up,
            series = "Increase", PI = FALSE) +
  xlab("Year") + ylab("Dollars") +
  ggtitle("US Margin Debt Scenarios (Shown in Millions of Dollars)") +
  guides(color = guide_legend(title = "Scenario"))

# Linear Regression Plots
autoplot(ts.mspus) +
  autolayer(fc1.mspus.lm, PI = FALSE, series = "Margin + Season") +
  autolayer(fc2.mspus.lm, PI = FALSE, series = "Trend + season") +
  xlab("Year") + ylab("Dollars") +
  guides(color = guide_legend(title = "Formula")) +
  ggtitle("Regression Forecasts of MSPUS")

autoplot(ts.mspus) +
  autolayer(fc_down.mspus.lm,
            series = "decrease", PI = TRUE) +
  autolayer(fc_up.mspus.lm,
            series = "increase", PI = TRUE) +
  xlab("Year") + ylab("Dollars") +
  guides(color = guide_legend(title = "Scenario")) +
  ggtitle("Scenario-Based Regression Forecast of MSPUS: US Margin Debt and Seasonality")







# Exponential Smoothing
fc.mspus.holt <- holt(mspus.train, h = 13)
fc.mspus.holt_damped <- holt(mspus.train, damped = TRUE, h = 13)
fc.mspus.hw <- hw(mspus.train, h = 13, seasonal = c("multiplicative"))


fc.mspus.holt[["model"]]
# Holt's method 
# 
# Call:
#  holt(y = ts.mspus, h = 8) 
# 
#   Smoothing parameters:
#     alpha = 0.534 
#     beta  = 0.3324 
# 
#   Initial states:
#     l = 218140.4425 
#     b = 3453.2294 
# 
#   sigma:  8945.187
# 
#      AIC     AICc      BIC 
# 1088.216 1089.611 1097.675 

fc.mspus.holt_damped[["model"]]
# Damped Holt's method 
# 
# Call:
#  holt(y = ts.mspus, h = 8, damped = TRUE) 
# 
#   Smoothing parameters:
#     alpha = 0.4609 
#     beta  = 0.4212 
#     phi   = 0.8965 
# 
#   Initial states:
#     l = 218140.1142 
#     b = 3956.0394 
# 
#   sigma:  8956.743
# 
#      AIC     AICc      BIC 
# 1089.241 1091.241 1100.592

fc.mspus.hw[["model"]]
# Holt-Winters' multiplicative method 
# 
# Call:
#  hw(y = ts.mspus, h = 13, seasonal = c("multiplicative")) 
# 
#   Smoothing parameters:
#     alpha = 0.6075 
#     beta  = 0.3199 
#     gamma = 1e-04 
# 
#   Initial states:
#     l = 218278.2866 
#     b = 3374.8637 
#     s = 1.0095 0.9987 0.9942 0.9977
# 
#   sigma:  0.03
# 
#      AIC     AICc      BIC 
# 1089.787 1094.402 1106.813 

autoplot(ts.mspus) +
  autolayer(fc.mspus.holt, PI = FALSE, series = "Holt's Method") +
  autolayer(fc.mspus.holt_damped, PI = FALSE, series = "Damped Holt's Method") +
  autolayer(fc.mspus.hw, PI = FALSE, series = "Holt-Winter's Method") +
  guides( color = guide_legend(title = "Forecast")) +
  xlab("Year") + ylab("Dollars") +
  ggtitle("Exponential Smoothing: Forecasts of Median Sale Price")



# Arima
fit.mspus.arima <- auto.arima(mspus.train,
           seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# Series: mspus.train 
# ARIMA(1,1,0) with drift 
# 
# Coefficients:
#   ar1     drift
# -0.4754  3015.680
# s.e.   0.1517   805.776
# 
# sigma^2 = 51397352:  log likelihood = -359.47
# AIC=724.95   AICc=725.72   BIC=729.62

fc.mspus.arima <- forecast(fit.mspus.arima, h = 13)
autoplot(ts.mspus) +
  autolayer(fc.mspus.arima, PI = FALSE) +
  xlab("Year") + ylab("Dollars") +
  ggtitle("Forecast from Arima(1,1,0) with Drift")


# Measures of Forecast Accuracy
accuracy(fc1.mspus.lm, mspus.test)
accuracy(fc.mspus.holt, mspus.test)
accuracy(fc.mspus.holt_damped, mspus.test)
accuracy(fc.mspus.hw, mspus.test)
accuracy(fc.mspus.arima, mspus.test)



# Residual Analysis
res.fc <- residuals(fc1.mspus.lm)

checkresiduals(res.fc)


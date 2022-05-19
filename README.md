
# mspus-forecasting
Final project for EC 4435 Economic Forecasting. Project involved analyzing housing sale price data from FRED, and evaluating, choosing, and applying a suitable forecasting method in R.

Methods evaluated:
* Simple methods (naive, seasonal naive, drift)
* Linear regression
* Exponential smoothing
* Arima

The chosen method involved building a simple model regressing the data set against US margin debt.
Data was forecast based on two scenarios for US margin debt, a decrease following recent trends, or an increase based on historical trends.


![RegressionForecast](https://github.com/positive-vibrations/mspus-forecasting/blob/main/assets/images/mspus-regression-forecast.png?raw=true)

Forecast data suggests:

* In increase scenario, correction to historical trend.
* In decrease scenario, significant devaluing of housing market.

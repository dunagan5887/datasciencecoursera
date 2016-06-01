library(lubridate) # For year() function below
library(forecast)

dat = read.csv("./gaData.csv")

#training = dat[year(dat$date) < 2012,]
#testing = dat[(year(dat$date)) > 2011,]

#tstrain = ts(training$visitsTumblr)
#ts1Train <- window(tstrain,start=131,end=365)

#tsTest = ts(testing$visitsTumblr)
#ts1Test <- window(tsTest,start=1,end=235)



ga_ts = ts(dat$visitsTumblr)
ts1Train <- window(ga_ts,start=1,end=365)
ts1Test <- window(ga_ts,start=366)



batsModel <- bats(ts1Train)

confidence_levels <- c(95)
forecastedPredictions <- forecast(batsModel, level = confidence_levels, h=235)

forecastAccuracy <- accuracy(forecastedPredictions,ts1Test)

lower_bounds <- forecastedPredictions$lower
upper_bounds <- forecastedPredictions$upper

in_interval = 0

for (i in 1:235)
{
  lower = lower_bounds[i]
  upper = upper_bounds[i]
  test_value = ts1Test[i]
  if ((test_value <= upper) && (test_value >= lower))
  {
    in_interval = in_interval + 1
  }
}

percentage_in_interval = in_interval / 235.0

# Answer .9617

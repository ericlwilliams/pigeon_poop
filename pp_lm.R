# Testing basic linear regressions predicting 3-1-1 pigeon poop response times (to extend to full data)

library(ggplot2)
library(data.table)

# id, Zip, MeanIncome, Created.Date,Closed.Date, Num.Calls, Resolution.Date, Mean.Resp.Time.By.Zip
# Log.Mean.Income, NumPeople, Call.Density
pp.data <- data.table(read.csv("./data/pp_income_data.csv",stringsAsFactors=FALSE))
browser()
# Testing basic linear regressions predicting 3-1-1 pigeon poop response times (to extend to full data)

library(ggplot2)
library(data.table)

# pp.data:
#  [1] "Zip"                      "Num.People.By.Zip"
#  [3] "Mean.Income.By.Zip"       "id"
#  [5] "Created.Date"             "Closed.Date"
#  [7] "Resolution.Date"          "Resolution.Time"
#  [9] "Num.Calls.By.Zip"         "Call.Density.By.Zip"
# [11] "Log.Mean.Income.By.Zip"   "Log10.Mean.Income.By.Zip"
# [13] "Mean.Res.Time.By.Zip"

 pp.data <- data.table(read.csv("./data/pp_income_data.csv",stringsAsFactors=FALSE))
 # fix up classes
 pp.data[,`:=`(Created.Date=as.Date(Created.Date),
 	Resolution.Date=as.Date(Resolution.Date),
 	Closed.Date=as.Date(Closed.Date),

 	# Zip=as.factor(Zip),
 	id=as.character(id))]

browser()
# Print uni-variate R^2
print("lm.r2:")

# Zip
lm.fit <- lm(Mean.Res.Time.By.Zip ~ Zip,data=pp.data)
lm.r2 <- summary(lm.fit)$r.squared
print(sprintf("Zip: %.3f",lm.r2))
# print(summary(lm.fit))


# Created.Date
lm.fit <- lm(Resolution.Time ~ Created.Date,data=pp.data)
lm.r2 <- summary(lm.fit)$r.squared
print(sprintf("Created.Date: %.3f",lm.r2))

# Num.Calls.By.Zip
lm.fit <- lm(Mean.Res.Time.By.Zip ~ Num.Calls.By.Zip,data=pp.data)
lm.r2 <- summary(lm.fit)$r.squared
print(sprintf("Num.Calls.By.Zip: %.3f",lm.r2))

# Log.Mean.Income.By.Zip
lm.fit <- lm(Mean.Res.Time.By.Zip ~ Log.Mean.Income.By.Zip,data=pp.data)
lm.r2 <- summary(lm.fit)$r.squared
print(sprintf("Log.Mean.Income.By.Zip: %.3f",lm.r2))


# Num.People.By.Zip
lm.fit <- lm(Mean.Res.Time.By.Zip ~ Num.People.By.Zip,data=pp.data)
lm.r2 <- summary(lm.fit)$r.squared
print(sprintf("Num.People.By.Zip: %.3f",lm.r2))

# Call.Density.By.Zip
lm.fit <- lm(Mean.Res.Time.By.Zip ~ Call.Density.By.Zip,data=pp.data)
lm.r2 <- summary(lm.fit)$r.squared
print(sprintf("Call.Density.By.Zip: %.3f",lm.r2))

# Created.Date
lm.fit <- lm(Resolution.Time ~ Created.Date,data=pp.data)
lm.r2 <- summary(lm.fit)$r.squared
print(sprintf("Created.Date: %.3f",lm.r2))

cat("\n")
cat("\n")
print("Complete Model By Zip")

lm.fit <- lm(Mean.Res.Time.By.Zip ~ 
	Log.Mean.Income.By.Zip + Num.Calls.By.Zip + Num.People.By.Zip + Call.Density.By.Zip,
	data=pp.data)
lm.r2 <- summary(lm.fit)$r.squared
print(sprintf("Complete: %.3f",lm.r2))
print(summary(lm.fit))
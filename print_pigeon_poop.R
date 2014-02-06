library(ggplot2)
library(data.table)
library(scales)

# Zip, MeanIncome, Created.Date,Closed.Date, NumComplaints
pp.data <- data.table(read.csv("./data/pp_income_data.csv",stringsAsFactors=FALSE))
 # fix up classes

 pp.data[,`:=`(Created.Date=as.Date(Created.Date),
 	Resolution.Date=as.Date(Resolution.Date),
 	Closed.Date=as.Date(Closed.Date),
 	id=as.character(id))]
#   [1] "Zip"                      "Num.People.By.Zip"
#  [3] "Mean.Income.By.Zip"       "id"
#  [5] "Created.Date"             "Closed.Date"
#  [7] "Resolution.Date"          "Resolution.Time"
#  [9] "Num.Calls.By.Zip"         "Call.Density.By.Zip"
# [11] "Log.Mean.Income.By.Zip"   "Log10.Mean.Income.By.Zip"
# [13] "Mean.Resp.Time.By.Zip"


scat.pp.income <- ggplot(pp.data, aes(x=Log.Mean.Income.By.Zip, y=Call.Density.By.Zip)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)+
    xlab("Log Median Income")+
    ylab("311 Pigeon Poop Complaints Density per Zip")

ggsave(plot=scat.pp.income,filename= file.path("./figures/pp_stats/scat_pp_income.pdf"))

# Plot resolution time vs ZIP income
scat.pp.resp.t.income <- ggplot(pp.data, aes(x=Log.Mean.Income.By.Zip,y=Mean.Resp.Time.By.Zip)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)+
    xlab("Log Median Income")+
    ylab("Avg Time (days) to Resolution Action")

ggsave(plot=scat.pp.resp.t.income,filename= file.path("./figures/pp_stats/scat_pp_resp_t_income.pdf"))

# Complaints over time
h_pp_t <- ggplot(pp.data,aes(x=Created.Date))
h_pp_t <- h_pp_t + geom_histogram(binwidth=30,color="white") +
	scale_x_date(labels = date_format("%Y-%b"),
		breaks = seq(min(pp.data$Created.Date),max(pp.data$Created.Date),30)) +
	ylab("Pigeon Poop Complaints") + xlab("Year and Month") +
	theme_bw() + theme(axis.text.x = element_text(angle=90)) + labs(title = "NYC 311 pigeon poop complaints over time")
	
ggsave("./figures/pp_stats/h_all_pp_t.pdf")

# Investigate pigeon-poop-complaint decline

# For each month, across years, fit line
pp.data[,Year:=as.numeric(format(Created.Date,"%Y"))]
pp.data[,Month:=as.numeric(format(Created.Date,"%m"))]



month.count.by.year <- data.table(data.frame(table(Month=pp.data$Month,Year=pp.data$Year)))
month.count.by.year$Month <- as.integer(month.count.by.year$Month)
# temp hack
month.count.by.year$Year <- as.integer(month.count.by.year$Year)+min(pp.data$Year)-1
month.count.by.year[,Date:=as.Date(paste(as.character(Year),"-",as.character(Month),"-01",sep=""),format="%Y-%m-%d")]
month.count.by.year <- month.count.by.year[which(Freq != 0),]
pp.coefs = data.frame()

for(i in 1:max(month.count.by.year$Month))
{
	 cur_lm <- lm(Freq ~ as.numeric(Date),data=subset(month.count.by.year,Month == i))
	 cur_stats <- unclass(summary(cur_lm))$coefficients

	 pp.coefs <- rbind(pp.coefs,data.frame(B0 = cur_stats["(Intercept)","Estimate"], B0_SE = cur_stats["(Intercept)","Std. Error"],
	 	B1 = cur_stats["as.numeric(Date)","Estimate"], B1_SE = cur_stats["as.numeric(Date)","Std. Error"]))

	# cur_coefs <- coef(lm(Freq ~ Year,data=subset(month.count.by.year,Month == i)))
	# pp.coefs <- rbind(pp.coefs,data.frame(B0 = cur_coefs["(Intercept)"], B1 = cur_coefs["Year"]))
}
rownames(pp.coefs) <- 1:12

print("Regression coeficients by month")
print(pp.coefs)

pp.decline <- data.frame(B0 = with(pp.coefs,sum(B0*(1/B0_SE))/sum((1/B0_SE))), B1=with(pp.coefs,sum(B1*(1/B1_SE))/sum((1/B1_SE))));
# Combine coefficients by weighting on 1/SE
pp.decline$B0_SE <- with(pp.coefs,sqrt(sum((B0_SE)^2)))
pp.decline$B1_SE <- with(pp.coefs,sqrt(sum((B1_SE)^2)))

print("Average coefficients:")
print(pp.decline)

# Complaints over time with fitted lies
cur.colors <- rainbow(12)
h_pp_decl_t <- ggplot(data=month.count.by.year) +
	geom_bar(aes(x=Date, y=Freq, fill=factor(Month)), stat="identity") +
		scale_x_date(limits = c(min(month.count.by.year$Date)-1,max(month.count.by.year$Date)+1)) +
		 geom_smooth(aes(x=Date, y=Freq,color=factor(Month)),linetype="dotdash",method="lm",se=FALSE,size=1)  + 
		 scale_color_manual(values=cur.colors) +
		 theme_bw() + theme(axis.text.x = element_text(angle=90)) + labs(title = "NYC 311 pigeon poop complaints over time") +
		 ylab("Pigeon Poop Complaints") + xlab("Year and Month")

ggsave("./figures/pp_stats/h_all_pp_decl_t.pdf")

# Combine all declines
# month.count.by.year$Decline.Estimates <- sapply(as.numeric(month.count.by.year$Date), function(d) pp.decline$B0 + (pp.decline$B1)*d)

h_pp_sum_decl_t <- ggplot(data=month.count.by.year) +
	geom_bar(aes(x=Date, y=Freq), stat="identity") +
	# geom_abline(aes(intercept=pp.decline$B0, slope=pp.decline$B1)) +
	geom_smooth(aes(x=Date, y=Freq),linetype="dotdash",method="lm",se=T,size=1,color="red") +
	scale_x_date(limits = c(min(month.count.by.year$Date)-3,max(month.count.by.year$Date)+3), 
		breaks = "1 month", 
		labels=date_format("%Y-%b")) +
		 # scale_color_manual(values=cur.colors) +
		 theme_bw() + theme(axis.text.x = element_text(angle=90)) + labs(title = "NYC 311 pigeon poop complaints over time") +
		 ylab("Pigeon Poop Complaints") + xlab("Year and Month")

	# 

ggsave("./figures/pp_stats/h_all_pp_sum_decl_t.pdf")

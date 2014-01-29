library(ggplot2)
library(data.table)

# Zip, MeanIncome, Created.Date,Closed.Date, NumComplaints
pp.data <- data.table(read.csv("./data/pp_income_data.csv",stringsAsFactors=FALSE))

scat.pp.income <- ggplot(pp.data, aes(x=Log.Mean.Income, y=Complaint.Density)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm)+
    xlab("Log Median Income")+
    ylab("311 Pigeon Poop Complaints Density per Zip")

ggsave(plot=scat.pp.income,filename= file.path("./figures/latest/scat_pp_income.pdf"))

browser()
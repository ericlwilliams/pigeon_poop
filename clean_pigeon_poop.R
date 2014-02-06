# # File-Name:       clean_pigeon_poop.R
# Author:          Eric Williams
# Purpose:         Combines NYC 311 'unsanitary pigeon condition' data with irs.gov income tax information by zip
# Data Used:       https://data.cityofnewyork.us/
#                  http://www.irs.gov/uac/Tax-Stats-2

library(data.table)
# library(ggplot2)
library(plyr)

pp311.data <- data.table(read.csv("./data/pigeon_poop_2010_to_present.csv",stringsAsFactors=FALSE,header=TRUE))
income.data <- data.table(read.csv("./data/ny_tax_stats_zip_clean.csv",stringsAsFactors=FALSE,header=TRUE))

fix.date <- function(date.str){

	date.str <- strsplit(date.str," ")
	date.str <- sapply(date.str, function(d) d[1])
	date <- as.Date(strptime(date.str,format="%m/%d/%y"))
	return(date)
}
# thin

pp311.data <- pp311.data[,list(Zip=as.numeric(Incident.Zip),id=Unique.Key,
	Created.Date=fix.date(Created.Date),Closed.Date=fix.date(Closed.Date),Resolution.Date=fix.date(Resolution.Action.Updated.Date),Borough=Borough)]
pp311.data <- pp311.data[which(!is.na(Zip)),]
browser()
# Days to resolution
# 0 - same day
pp311.data[,Resolution.Time:=difftime(Resolution.Date,Created.Date,units="days")]
# negative days to resolution for repeat calls? could group by address, ignoring for now (~5 calls)
pp311.data <- pp311.data[which(Resolution.Time>=0),]

setkey(pp311.data,Zip)
make.num <- function(num.str){
	num.str <- gsub("[ |,]","",num.str)
	# replace . and - with 0
	num.str <- gsub("\\D","0",num.str)

}

# tmp <- ggplot(pp311.data,aes(x=Created.Date,y=Resolution.Time))+geom_point()
# ggsave(plot=tmp,filename= file.path("./figures/latest/tmp.pdf"))
# browser()


income.data <- income.data[which(Size.of.Adjusted.Gross.Income=="" & !is.na(Zip)),]


income.data <- income.data[,list(Zip=as.numeric(Zip),Num.People.By.Zip=as.numeric(make.num(Number.of.Returns)),
	Mean.Income.By.Zip=as.numeric(make.num(Taxable.Income))/as.numeric(make.num(Number.of.Returns)))]
setkey(income.data,Zip)

combined.data <- income.data[pp311.data,]
# Remove any entries that don't have income data
combined.data <- combined.data[which(!is.na(Num.People.By.Zip)),]

# test.data<-ddply(combined.data,.(Zip),sum,NumComplaints=ncol(Mean.Income.By.Zip))
zip.tbl <-table(combined.data$Zip)
zip.freq.dt <- as.data.frame(zip.tbl,stringsAsFactors=FALSE)
names(zip.freq.dt) <- c("Zip","Num.Calls.By.Zip")
zip.freq.dt <- data.table(zip.freq.dt)
zip.freq.dt[,Zip:=as.numeric(Zip)]
setkey(zip.freq.dt,Zip)

combined.data<-combined.data[zip.freq.dt,]

combined.data[,Call.Density.By.Zip:=Num.Calls.By.Zip/Num.People.By.Zip]


# Transform mean income

combined.data[,Log.Mean.Income.By.Zip:=log(Mean.Income.By.Zip)]
combined.data[,Log10.Mean.Income.By.Zip:=log10(Mean.Income.By.Zip)]

# Get Avg. response time per zip
combined.data[,Mean.Resp.Time.By.Zip:=sum(Resolution.Time)/Num.Calls.By.Zip,by=Zip]

# Something funky about one entry with Zip=10000,  dumping
combined.data <- combined.data[which(Zip!=10000),]

# TODO - Transform median based on groups
# http://people.umass.edu/biep540w/pdf/Grouped%20Data%20Calculation.pdf

write.csv(combined.data,"./data/pp_income_data.csv",row.names=FALSE)

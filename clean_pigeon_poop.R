# # File-Name:       clean_pigeon_poop.R
# Author:          Eric Williams
# Purpose:         Combines NYC 311 'unsanitary pigeon condition' data with irs.gov income tax information by zip
# Data Used:       https://data.cityofnewyork.us/
#                  http://www.irs.gov/uac/Tax-Stats-2

library(data.table)

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

pp311.data <- pp311.data[,list(Zip=as.numeric(Incident.Zip),Created.Date=fix.date(Created.Date),Closed.Date=fix.date(Closed.Date))]
pp311.data <- pp311.data[which(!is.na(Zip)),]
setkey(pp311.data,Zip)
make.num <- function(num.str){
	num.str <- gsub("[ |,]","",num.str)
	# replace . and - with 0
	num.str <- gsub("\\D","0",num.str)

}


income.data <- income.data[which(Size.of.Adjusted.Gross.Income=="" & !is.na(Zip)),]
income.data <- income.data[,list(Zip=as.numeric(Zip),NumPeople=as.numeric(make.num(Number.of.Returns)),MeanIncome=as.numeric(make.num(Taxable.Income))/as.numeric(make.num(Number.of.Returns)))]
setkey(income.data,Zip)

combined.data <- income.data[pp311.data,]

# test.data<-ddply(combined.data,.(Zip),sum,NumComplaints=ncol(MeanIncome))
zip.tbl <-table(combined.data$Zip)
zip.freq.dt <- as.data.frame(zip.tbl,stringsAsFactors=FALSE)
names(zip.freq.dt) <- c("Zip","NumComplaints")
zip.freq.dt <- data.table(zip.freq.dt)
zip.freq.dt[,Zip:=as.numeric(Zip)]
setkey(zip.freq.dt,Zip)

combined.data<-combined.data[zip.freq.dt,]

combined.data[,Complaint.Density:=NumComplaints/NumPeople]

# Transform mean income

combined.data[,Log.Mean.Income:=log(MeanIncome)]
combined.data[,Log10.Mean.Income:=log10(MeanIncome)]


write.csv(combined.data,"./data/pp_income_data.csv",row.names=FALSE)

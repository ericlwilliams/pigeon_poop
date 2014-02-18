# need_umbrella.R  
# Input date ('yyyy-mm-dd') and zip
# Output: chance of pigeon poop, safest ZIP, and plots (e.g. complaints vs time for given zip)
# 
# Call with e.g.  > Rscript need_umbrella.R 2010-05-27 10021

library(data.table)
library(ggplot2)
library(scales)

# Zip, MeanIncome, Created.Date,Closed.Date, NumComplaints
pp.data <- data.table(read.csv("./data/pp_income_data.csv",stringsAsFactors=FALSE))
 # fix up classes

pp.data[,`:=`(Created.Date=as.POSIXct(Created.Date),
	Resolution.Date=as.Date(Resolution.Date),
	Closed.Date=as.Date(Closed.Date),
 	id=as.character(id))]

# chooo




browser()

# options(echo=TRUE)

args <- commandArgs(trailingOnly = TRUE)

if(length(args)!= 2)
{
	stop("Need date and zip input to predict pigeon poop probability (PPP)")
}

cur_date <- as.Date(args[[1]])
cur_zip <- as.integer(args[[2]])

is_future = FALSE
is_today = FALSE
is_past = FALSE

if(cur_date<as.Date("2010-01-02")) # earliest complaint
{
	stop("Pigeon's only pooping since 2010-01-02")
}else if(cur_date>Sys.Date())
{
	print("Calculating future pigeon poop probabilities...")
	is_future=TRUE
}else if(cur_date==Sys.Date())
{
	print("Calculating current pigeon poop probabilities...")
	is_today=TRUE
}else if(cur_date<Sys.Date())
{
	print("Calculating past pigeon poop probabilities...")
	is_past=TRUE
}

# PPP is given zip: historical average # complaints of that day / historical average of # complaints 
zip.pp.data <- pp.data[which(Zip==cur_zip),]

# If no observations in zip, expand to +- zip, try again
if(nrow(zip.pp.data)==0){
	print(sprintf('Error: %d not a valid zip',cur_zip))
	stop("TODO: print list of boroughs/zips")
}

# The ol' switch-a-roo, use borough data rather than zip due to lack of statistics
cur_borough <- zip.pp.data[1,Borough]

zip.pp.data <- pp.data[which(Borough==cur_borough),]

h_pp_t <- ggplot(zip.pp.data,aes(x=Created.Date))
h_pp_t <- suppressWarnings(h_pp_t + geom_histogram())
h_pp_t <- h_pp_t +theme_bw() +
	scale_x_datetime(breaks = date_breaks("1 month"),
		labels = date_format("%Y-%b"),
		limits = c(min(zip.pp.data$Created.Date),
			max(zip.pp.data$Created.Date)))
h_pp_t <- h_pp_t + xlab(NULL) + opts(axis.text.x  = theme_text(angle=45,
                                                                  hjust = 1,
                                                                  vjust = 1))
ggsave("./figures/need_umbrella/h_pp_t.pdf")




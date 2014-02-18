# setwd("/Users/elw/Dropbox/eMacs/projects/nyc311/pp_reco/")
# need_umbrella.R  
# Input date ('yyyy-mm-dd') and zip
# Output: chance of pigeon poop, safest ZIP, and plots (e.g. complaints vs time for given zip)
# 
# Call with e.g.  > Rscript need_umbrella.R 2010-05-27 10021

# library(data.table)
library(ggplot2)
library(scales)
library(rpart)        # rpart()
library(rattle)       # fancyRpartPlot()
library(randomForest) # randomForest()
library(ada)          # ada()
library(ROCR)         # prediction()
library(party)        # ctree() and cforest()



# Zip, MeanIncome, Created.Date,Closed.Date, NumComplaints
# pp.data <- data.table(read.csv("./data/pp_income_data.csv",stringsAsFactors=FALSE))
pp.data <- data.frame(read.csv("./data/pp_income_data.csv",stringsAsFactors=FALSE))

 # fix up classes

# pp.data[,`:=`(Created.Date=as.POSIXct(Created.Date),
# 	Resolution.Date=as.Date(Resolution.Date),
# 	Closed.Date=as.Date(Closed.Date),
#  	id=as.character(id))]

pp.data$Created.Date <- as.POSIXct(pp.data$Created.Date)

cmonth <- format(pp.data$Created.Date,'%b')
pp.data$Created.Month <- factor(cmonth,levels=unique(cmonth),ordered=TRUE)


pp.data$Resolution.Date <- as.Date(pp.data$Resolution.Date)

rmonth <- format(pp.data$Resolution.Date,'%b')
pp.data$Resolution.Month <- factor(rmonth,levels=unique(rmonth),ordered=TRUE)

pp.data$Closed.Date <- as.Date(pp.data$Closed.Date)
pp.data$id <- as.character(pp.data$id)
pp.data$Is.Closed <- as.factor(pp.data$Is.Closed)
pp.data$Borough <- as.factor(pp.data$Borough)
# choose variables of interest
# target <- "Num.Calls.By.Zip" # mean response time by zip
target <- "Is.Closed" #
feat <- names(pp.data)
ignore <- c("Log10.Mean.Income.By.Zip","id","Closed.Date","Mean.Resp.Time.By.Zip","Closed.Date","Created.Date","Resolution.Date","Resolution.Time")



# variables that have unique value for every observation - candidate for ignoring
(ids   <- names(which(sapply(pp.data, function(x) length(unique(x))) == nrow(pp.data))))
ignore <- union(ignore, ids)

## ----ignore_missing_variables--------------------------------------------

mvc <- sapply(data.frame(pp.data)[feat], function(x) sum(is.na(x)))
# all missing values
mvn <- names(which(mvc == nrow(pp.data)))
ignore <- union(ignore, mvn)


## ----ignore_mostly_missing_variables-------------------------------------
mvn <- names(which(mvc >= 0.7*nrow(pp.data)))
ignore <- union(ignore, mvn)

feat <- setdiff(feat, ignore)

# Remove Missing Target
if(sum(is.na(pp.data[target]))>0)
{
	pp.data <- pp.data[!is.na(pp.data[target]),]
}

## ----fig.height=4--------------------------------------------------------
p <- ggplot(pp.data, aes_string(x=target))
p <- p + geom_bar(width=0.2)
print(p)

# ds[target] <- as.factor(ds[[target]])
# table(ds[target])

## ----identify_variables--------------------------------------------------
(inputs  <- setdiff(feat, target))
(nobs    <- nrow(pp.data))

## ----identify_variable_types---------------------------------------------
numi       <- which(sapply(pp.data[inputs], is.numeric))
numerics   <- names(numi)
# categorical indices
cati       <- which(sapply(pp.data[inputs], is.factor))
categorics <- names(cati)

## ----construct_the_formula-----------------------------------------------
(form <- formula(paste(target, "~ .")))


## ----set_seed------------------------------------------------------------
# (seed <- sample(1:1000000, 1))
seed <- 123
set.seed(seed)

## ----build_training_testing_datasets-------------------------------------
length(train <- sample(nobs, 0.7*nobs))
length(test  <- setdiff(seq_len(nobs), train))

## ------------------------------------------------------------------------
actual.train <- pp.data[train, target]
actual       <- pp.data[test, target]


## ----build_model, out.lines=NULL-----------------------------------------
# various paramters that control aspects of the rpart fit
ctrl <- rpart.control(maxdepth=5)
system.time(model <- m.rp <- rpart(form, pp.data[train, feat], control=ctrl))
mtype <- "rpart" # Record the type of the model for later use.


## ----echo=FALSE, message=FALSE-------------------------------------------
# Do this to avoid the messages taking up space on the
# page so we can fit the plot.
library(rpart.plot)
library(RColorBrewer)


## ----plot_model, out.width='0.5\\textwidth'------------------------------
fancyRpartPlot(model)

## ----evaluate_train_accuracy---------------------------------------------
head(cl <- predict(model, pp.data[train, feat],type="class"))


## ------------------------------------------------------------------------
head(actual.train)


## ------------------------------------------------------------------------
(acc <- sum(cl == actual.train, na.rm=TRUE)/length(actual.train))


## ------------------------------------------------------------------------
(err <- sum(cl != actual.train, na.rm=TRUE)/length(actual.train))


## ----evaluate_train_auc--------------------------------------------------
pr <- predict(model, pp.data[train, feat],type="prob")[,2]
pred <- prediction(pr, pp.data[train, target])
(atr <- attr(performance(pred, "auc"), "y.values")[[1]])


## ----evaluate_test_accuracy----------------------------------------------
# Evaluate on TEST now
cl <- predict(model, pp.data[test, feat], type="class")
(acc <- sum(cl == actual, na.rm=TRUE)/length(actual))


## ------------------------------------------------------------------------
(err <- sum(cl != actual, na.rm=TRUE)/length(actual))


## ----evaluate_test_auc---------------------------------------------------
pr <- predict(model, pp.data[test, feat], type="prob")[,2]
pred <- prediction(pr, pp.data[test, target])
(ate <- attr(performance(pred, "auc"), "y.values")[[1]])


## ------------------------------------------------------------------------
#  Confusion matrix
cl <- predict(model, pp.data[train, feat], type="class")
round(100*table(actual.train, cl, dnn=c("Actual", "Predicted"))/length(actual.train))


## ------------------------------------------------------------------------
cl <- predict(model, pp.data[test, feat], type="class")
round(100*table(actual, cl, dnn=c("Actual", "Predicted"))/length(actual))


## ----riskchart, message=FALSE--------------------------------------------
# riskchart(pr, pp.data[test, target], pp.data[test, risk])
# riskchart(pr, pp.data[test, target], rep(1,length(test)))
riskchart(pr, pp.data[test, target], rep(1,length(test)))

## ----src.top=25, src.bot=14----------------------------------------------
experi <- function(form, ds, dsname, target, modeller, details="", 
                   n=100, control=NULL,
                   keep=FALSE, # Keep the last model built.
                   prob="prob",
                   class="class",
                   log="experi.log")
{
  suppressPackageStartupMessages(require(pROC))

  user <- Sys.getenv("LOGNAME")
  node <- Sys.info()[["nodename"]]

  wsrpart.model <- modeller=="wsrpart"
  
  numclass <- length(levels(ds[,target]))

  start.time <- proc.time()
  
  seeds <- cors <- strs <- aucs <- accs <- NULL
  for (i in seq_len(n))
  {
    loop.time <- proc.time()

    seeds  <- c(seeds, seed <- sample(1:1000000, 1))
    set.seed(seed)
    
    train  <- sample(nrow(ds), 0.7*nrow(ds))
    test   <- setdiff(1:nrow(ds), train)
    actual <- ds[test, target]
    
    args   <- append(list(form, data=ds[train,]), control)

    model  <- do.call(modeller, args)
    
    if (numclass==2)
    {
      if (modeller %in% c("ctree", "cforest"))
        pr <- do.call("rbind", predict(model, newdata=ds[test,], type=prob))[,2]
      else
        pr <- predict(model, newdata=ds[test,], type=prob)[,2]
      # For small samples we can get all the same class...
      # Should really ensure we sample both classes
      if (length(unique(actual)) == 1)
        aucs <- c(0.0, aucs)
      else
        aucs <- c(auc(actual, pr), aucs)
    }
    if ("ada" %in% class(model)) class <- "vector"
    if (modeller %in% c("ctree", "cforest")) class <- "response"

    #compute cirrelation and strength for mrpart
    if (wsrpart.model)
    {
      cors <- c(correlation(model, ds, form), cors)
      strs <- c(strength(model, ds, form), strs)
    }
    
    cl <- predict(model, newdata=ds[test,], type=class)
    accs <- c(sum(cl==actual, na.rm=TRUE)/length(actual), accs)

    if (! is.null(log))
    {
      require(lubridate)
      if (! file.exists(log))
        write.table(data.frame(user=NA, node=NA, ts=NA, ds=NA, model=NA,
                               acc=NA, auc=NA, cor=NA, str=NA,
                               user=NA, elapsed=NA),
                    file=log, sep=",", row.names=FALSE)
      write.table(data.frame(user=user, node=node, ts=now(),
                           ds=dsname, model=modeller,
                           acc=round(accs[1], 4),
                           auc=round(aucs[1], 4),
                           cor=ifelse(wsrpart.model, round(cors[1], 4), NA),
                           str=ifelse(wsrpart.model, round(strs[1], 4), NA),
                           user=round((proc.time()-loop.time)['user.self'], 2),
                           elapsed=round((proc.time()-loop.time)['elapsed'],2)),
                file=log, sep=",", append=TRUE, col.names=FALSE, row.names=FALSE)
    }
  }
  
  result <- data.frame(modeller=paste0(modeller, "_", details),
                       auc=ifelse(numclass==2, mean(aucs), NA),
                       auc.sd=ifelse(numclass==2, sd(aucs), NA),
                       cor=ifelse(wsrpart.model, mean(cors), NA),
                       cor.sd=ifelse(wsrpart.model , sd(cors), NA),
                       str=ifelse(wsrpart.model, mean(strs), NA),
                       str.sd=ifelse(wsrpart.model , sd(strs), NA),
                       acc=mean(accs), acc.sd=sd(accs), n=n,
                       user=(proc.time()-start.time)['user.self'],
                       elapsed=(proc.time()-start.time)['elapsed'])
  if (wsrpart.model)
    if (numclass==2)
      result[-1]   <- round(result[-1], 2)
  else
    result[-c(1:3)]   <- round(result[-c(1:3)], 2)
  else
    if (numclass==2)
      result[-c(1,4:7)]   <- round(result[-c(1,4:7)], 2)
  else
    result[-c(1:7)]   <- round(result[-c(1:7)], 2)
  
  row.names(result) <- NULL

  if (keep)
  {
    if (numclass==2) 
    {
      attr(result, "pr") <- pr
      attr(result, "test") <- test
    }
    attr(result, "model") <- model
  }

  return(result)
}



## ----run_experiments-----------------------------------------------------
n <- 10

ds <- pp.data
vars <- feat
dsname <- "pp.data"
## ----run_rp, message=FALSE-----------------------------------------------
ex.rp <- experi(form, ds[vars], dsname, target, "rpart", "1", n=n, keep=TRUE)



## ----run_ad--------------------------------------------------------------
ex.ad <- experi(form, ds[vars], dsname, target, "ada", "50", n=n, keep=TRUE)
## ----run_rf--------------------------------------------------------------


omit <- NULL
mo <- attr(na.omit(ds[vars]), "na.action")
omit <- union(omit, mo)
if (length(omit)) ds <- ds[-omit,]

# ds[vars] <- na.roughfix(ds[vars])
ex.rf <- experi(form, ds[vars], dsname, target, "randomForest", "500", n=n, keep=TRUE,
                control=list(na.action=na.omit))

## ----run_ct--------------------------------------------------------------
# ex.ct <- experi(form, ds[vars], dsname, target, "ctree", "1", n=n, keep=TRUE)

browser()

## ----run_cf, eval=FALSE--------------------------------------------------
## # Generates: error code 1 from Lapack routine 'dgesdd'
## ex.cf <- experi(form, ds[vars], dsname, target, "cforest", "500", n=n, keep=TRUE)


## ----show_results, out.lines=NULL----------------------------------------
results <- rbind(ex.rp, ex.rf, ex.ad)
rownames(results) <- results$modeller
results$modeller <- NULL
print(results)

## ----experi_riskchart_rp-------------------------------------------------
ex <- ex.rp
pr <- attr(ex, "pr")
test <- attr(ex, "test")
riskchart(pr, ds[test, target])


## ----experi_riskchart_rf-------------------------------------------------
ex <- ex.rf
pr <- attr(ex, "pr")
test <- attr(ex, "test")
riskchart(pr, ds[test, target])


## ----experi_riskchart_ad-------------------------------------------------
ex <- ex.ad
pr <- attr(ex, "pr")
test <- attr(ex, "test")
riskchart(pr, ds[test, target])


## ----experi_riskchart_ct-------------------------------------------------
# ex <- ex.ct
# pr <- attr(ex, "pr")
# test <- attr(ex, "test")
# riskchart(pr, ds[test, target], ds[test, risk])

# # options(echo=TRUE)

# args <- commandArgs(trailingOnly = TRUE)

# if(length(args)!= 2)
# {
# 	stop("Need date and zip input to predict pigeon poop probability (PPP)")
# }

# cur_date <- as.Date(args[[1]])
# cur_zip <- as.integer(args[[2]])

# is_future = FALSE
# is_today = FALSE
# is_past = FALSE

# if(cur_date<as.Date("2010-01-02")) # earliest complaint
# {
# 	stop("Pigeon's only pooping since 2010-01-02")
# }else if(cur_date>Sys.Date())
# {
# 	print("Calculating future pigeon poop probabilities...")
# 	is_future=TRUE
# }else if(cur_date==Sys.Date())
# {
# 	print("Calculating current pigeon poop probabilities...")
# 	is_today=TRUE
# }else if(cur_date<Sys.Date())
# {
# 	print("Calculating past pigeon poop probabilities...")
# 	is_past=TRUE
# }

# # PPP is given zip: historical average # complaints of that day / historical average of # complaints 
# zip.pp.data <- pp.data[which(Zip==cur_zip),]

# # If no observations in zip, expand to +- zip, try again
# if(nrow(zip.pp.data)==0){
# 	print(sprintf('Error: %d not a valid zip',cur_zip))
# 	stop("TODO: print list of boroughs/zips")
# }

# # The ol' switch-a-roo, use borough data rather than zip due to lack of statistics
# cur_borough <- zip.pp.data[1,Borough]

# zip.pp.data <- pp.data[which(Borough==cur_borough),]

# h_pp_t <- ggplot(zip.pp.data,aes(x=Created.Date))
# h_pp_t <- suppressWarnings(h_pp_t + geom_histogram())
# h_pp_t <- h_pp_t +theme_bw() +
# 	scale_x_datetime(breaks = date_breaks("1 month"),
# 		labels = date_format("%Y-%b"),
# 		limits = c(min(zip.pp.data$Created.Date),
# 			max(zip.pp.data$Created.Date)))
# h_pp_t <- h_pp_t + xlab(NULL) + opts(axis.text.x  = theme_text(angle=45,
#                                                                   hjust = 1,
#                                                                   vjust = 1))
# ggsave("./figures/need_umbrella/h_pp_t.pdf")




#  Course           : CS 513
#  Team Member 1    : Bryan Gardner (10369193)
#  Team Member 2    : Michael Langford (10387693)
#  Purpose          : Predict trends and values in the S&P 500 using data mining techniques

#install.packages("neuralnet")
#install.packages("Quandl")

rm(list=ls())
set.seed(3982)

library(Quandl)
library(neuralnet)
library(randomForest)

start_date <- as.Date("2010-01-01")
today <- Sys.Date()

dates <- seq.Date(from=as.Date("2010-01-01"), to=Sys.Date(), by=1)
head(dates)

# generic wrapper to get stock data
getStockData <- function(ticker) {
  return(Quandl.datatable("WIKI/PRICES",
                          ticker=c(ticker),
                          date.gte="2010-01-01",
                          date.lte=Sys.Date(),
                          paginate=TRUE,
                          qopts.columns=c("Date","ticker", "close")
                          )
         )
}

# normalization function
mmnorm <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# denormalization function
denorm <- function(x, min, max) {
  return(x * (max - min) + min)
}

# get gold prices
gold_prices <- Quandl("WGC/GOLD_DAILY_USD", start_date="2010-01-01", end_date=today)
View(gold_prices)

# get treasury yield
treasury_yield <- Quandl("USTREASURY/YIELD", start_date="2010-01-01", end_date=today)
View(treasury_yield)

# get stock pricse
exxon <- getStockData("XOM")
ge <- getStockData("GE")
jpm <- getStockData("JPM")

# get oil prices
oil <-Quandl("OPEC/ORB", date.gte="2010-01-01", date.lte=today)
View(oil)

# read in SP data
sp_data = read.csv("GSPC.csv")
sp_data[,1] <- as.Date(sp_data[,1], format="%m/%d/%Y")
View(sp_data)

# Add the next day S&P close to the current day
time_series_sp_data<-NULL
for (s in seq(from=1, to=nrow(sp_data)-1, by=1)) {
  time_series_sp_data<-rbind(time_series_sp_data, cbind(sp_data[s,], Next_Close=sp_data[s+1,5]))
}
View(time_series_sp_data)

# Join all the datasets on date
joined_data <- NULL
for (d in as.list(dates)) {
   gold_ind <- which(gold_prices[,1] == d, arr.ind=TRUE)
   exxon_ind <- which(exxon[,1] == d, arr.ind=TRUE)
   ge_ind <- which(ge[,1] == d, arr.ind=TRUE)
   jpm_ind <- which(jpm[,1] == d, arr.ind=TRUE)
   sp_ind <- which(time_series_sp_data[,1] == d, arr.ind=TRUE)
   treasury_ind <- which(treasury_yield[,1] == d, arr.ind=TRUE)
   oil_ind <- which(oil[,1] == d, arr.ind=TRUE)
   
   if (length(gold_ind) > 0 && length(exxon_ind) > 0 && length(ge_ind) > 0 && length(jpm_ind) > 0 && length(sp_ind) > 0 && length(treasury_ind) > 0 && length(oil_ind) > 0) {
     joined_data<-rbind(joined_data,
                        data.frame(
                          Date=as.Date(d),
                          Gold_Close=gold_prices[gold_ind, 2],
                          XOM=exxon[exxon_ind, 3],
                          GE=ge[ge_ind, 3],
                          JPM=jpm[jpm_ind, 3],
                          TEN_YEAR=treasury_yield[treasury_ind, 10],
                          ONE_YEAR=treasury_yield[treasury_ind, 5],
                          THREE_YEAR=treasury_yield[treasury_ind, 7],
                          OIL=oil[oil_ind, 2],
                          SP_CLOSE=time_series_sp_data[sp_ind, 5],
                          NEXT_SP=time_series_sp_data[sp_ind, 8]))
   }
}
View(joined_data)

min <- min(joined_data$NEXT_SP)
max <- max(joined_data$NEXT_SP)
normalized<-cbind(Date=joined_data[,1], as.data.frame(lapply(joined_data[,!names(joined_data) %in% c("Date")], mmnorm)))
View(normalized)

ind <- seq(from=1, to=nrow(normalized), by=5)

norm_test <- normalized[ind, !names(normalized) %in% c("Date", "NEXT_SP")]
norm_training <- normalized[-ind, !names(joined_data) %in% c("Date")]
head(norm_test)
View(norm_test)
head(norm_training)

actual_test<-joined_data[ind, !names(joined_data) %in% c("Date", "NEXT_SP")]
actual_training<-joined_data[-ind, !names(joined_data) %in% c("Date")]
head(actual_test)
head(actual_training)

View(norm_training)
View(actual_training)

head(norm_training)
nn<-neuralnet(NEXT_SP~Gold_Close+XOM+GE+JPM+TEN_YEAR+ONE_YEAR+THREE_YEAR+OIL+SP_CLOSE, data=norm_training, hidden=c(5,5))
plot(nn)

head(norm_test)
nn_pred <- compute(nn, norm_test)$net.result
nn_denorm_results <- denorm(nn_pred, min, max)

nn_results <- cbind(joined_data[ind,], PREDICTED_NEXT_SP=nn_denorm_results)
View(nn_results)

correct_moves <- sum((nn_results$SP_CLOSE < nn_results$NEXT_SP & nn_results$SP_CLOSE < nn_results$PREDICTED_NEXT_SP) | 
      (nn_results$SP_CLOSE > nn_results$NEXT_SP & nn_results$SP_CLOSE > nn_results$PREDICTED_NEXT_SP))

correct_moves/nrow(nn_results)

sum(abs(nn_results$NEXT_SP - nn_results$PREDICTED_NEXT_SP))/nrow(nn_results)



head(actual_training)

rf<-randomForest(NEXT_SP~Gold_Close+TEN_YEAR+ONE_YEAR+THREE_YEAR+OIL+SP_CLOSE+XOM+JPM+GE, data=actual_training, importance=TRUE, na.action=na.omit)

plot(rf)
varImpPlot(rf)
importance(rf)

head(actual_test)
rf_pred <- predict(rf,actual_test)

rf_results <- cbind(joined_data[ind,], PREDICTED_NEXT_SP=rf_pred)
View(rf_results)
rf_correct_moves <- sum((rf_results$SP_CLOSE < rf_results$NEXT_SP & rf_results$SP_CLOSE < rf_results$PREDICTED_NEXT_SP) | 
                          (rf_results$SP_CLOSE > rf_results$NEXT_SP & rf_results$SP_CLOSE > rf_results$PREDICTED_NEXT_SP))

rf_correct_moves/nrow(rf_results)
sum(abs(rf_results$NEXT_SP - rf_results$PREDICTED_NEXT_SP))/nrow(rf_results)

#install.packages("neuralnet")
#install.packages("Quandl")

rm(list=ls())

library(Quandl)
library(neuralnet)

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

normalized<-as.data.frame(lapply(joined_data[,!names(joined_data) %in% c("Date")], mmnorm))
View(normalized)

ind <- seq(from=1, to=nrow(normalized), by=5)
test <- normalized[ind,]
training <- normalized[-ind,]
View(normalized)

View(training)
nn<-neuralnet(NEXT_SP~Gold_Close+XOM+GE+JPM+TEN_YEAR+ONE_YEAR+THREE_YEAR+OIL+SP_CLOSE, data=training, hidden=c(5,5))
plot(nn)

head(test)
results <- compute(nn, test[,!names(test) %in% c("NEXT_SP")])$net.result
denorm_results <- denorm(results, min, max)

pred <- cbind(joined_data[ind,], PREDICTED_NEXT_SP=denorm_results)
View(pred)

correct_moves <- sum((pred$SP_CLOSE < pred$NEXT_SP & pred$SP_CLOSE < pred$PREDICTED_NEXT_SP) | 
      (pred$SP_CLOSE > pred$NEXT_SP & pred$SP_CLOSE > pred$PREDICTED_NEXT_SP))

correct_moves/nrow(pred)

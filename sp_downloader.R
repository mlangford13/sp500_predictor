#install.packages("rvest")
#install.packages("xts")
#install.packages("Quandl")

library(xml2)
library(rvest)
library(xts)
library(Quandl)
library(neuralnet)

today <- Sys.Date()

dates <- seq.Date(from=as.Date("1990-01-01"), to=Sys.Date(), by=1)
View(dates)

getStockData <- function(ticker) {
  return(Quandl.datatable("WIKI/PRICES", ticker=c(ticker), date.gte="1990-01-01", date.lte=Sys.Date(), paginate=TRUE, qopts.columns=c("ticker", "date", "close")))
}

mmnorm <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

denorm <- function(x, min, max) {
  return(x * (max - min) + min)
}


gold_prices <- Quandl("WGC/GOLD_DAILY_USD", date.gte="1990-01-01", date.lte=today)
View(gold_prices)

exxon <- getStockData("XOM")
View(exxon)

apple <- getStockData("AAPL")
google <- getStockData("GOOG")

sp_data = read.csv("GSPC.csv")
sp_data[,1] <- as.Date(sp_data[,1])
typeof(sp_data[,4])
View(sp_data)


joined_data <- NULL
for (d in as.list(dates)) {
   gold_ind <- which(gold_prices[,1] == d, arr.ind=TRUE)
   exxon_ind <- which(exxon[,2] == d, arr.ind=TRUE)
   apple_ind <- which(apple[,2] == d, arr.ind=TRUE)
   google_ind <- which(google[,2] == d, arr.ind=TRUE)
   sp_ind <- which(sp_data[,1] == d, arr.ind=TRUE)
   
   if (length(gold_ind) > 0 && length(exxon_ind) > 0 && length(apple_ind) > 0 && length(google_ind) > 0 && length(sp_ind) > 0) {
     joined_data<-rbind(joined_data,
                        data.frame(
                          Date=as.Date(d),
                          Gold_Close=gold_prices[gold_ind, 2],
                          XOM=exxon[exxon_ind, 3],
                          AAPL=apple[apple_ind, 3],
                          GOOG=google[google_ind, 3],
                          SP=sp_data[sp_ind, 5]))
   }
}

min <- min(joined_data$SP)
max <- max(joined_data$SP)

normalized<-as.data.frame(lapply(joined_data[,-1], mmnorm))
ind <- seq(from=1, to=nrow(normalized), by=5)
test <- normalized[ind,]
training <- normalized[-ind,]
View(normalized)

nn<-neuralnet(SP~Gold_Close+XOM+AAPL+GOOG, data=training, hidden=c(10))
plot(nn)

results <- compute(nn, test[,-5])$net.result
denorm_results <- denorm(results, min, max)

View(cbind(joined_data[-ind, 6], denorm_results))

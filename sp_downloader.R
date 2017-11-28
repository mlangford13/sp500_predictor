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

treasury_yield <- Quandl("USTREASURY/YIELD", date.gte="1990-01-01", date.lte=today)
View(treasury_yield)

exxon <- getStockData("XOM")
View(exxon)

apple <- getStockData("AAPL")
jpm <- getStockData("JPM")

sp_data = read.csv("GSPC.csv")
sp_data[,1] <- as.Date(sp_data[,1])
typeof(sp_data[,4])
View(sp_data)


joined_data <- NULL
for (d in as.list(dates)) {
   gold_ind <- which(gold_prices[,1] == d, arr.ind=TRUE)
   exxon_ind <- which(exxon[,2] == d, arr.ind=TRUE)
   apple_ind <- which(apple[,2] == d, arr.ind=TRUE)
   jpm_ind <- which(jpm[,2] == d, arr.ind=TRUE)
   sp_ind <- which(sp_data[,1] == d, arr.ind=TRUE)
   treasury_ind <- which(treasury_yield[,1] == d, arr.ind=TRUE)
   
   if (length(gold_ind) > 0 && length(exxon_ind) > 0 && length(apple_ind) > 0 && length(google_ind) > 0 && length(sp_ind) > 0 && length(treasury_ind)) {
     joined_data<-rbind(joined_data,
                        data.frame(
                          Date=as.Date(d),
                          Gold_Close=gold_prices[gold_ind, 2],
                          XOM=exxon[exxon_ind, 3],
                          AAPL=apple[apple_ind, 3],
                          GOOG=google[google_ind, 3],
                          TEN_YEAR=treasury_yield[treasury_ind, 10],
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

nn<-neuralnet(SP~Gold_Close+XOM+AAPL+GOOG+TEN_YEAR, data=training, hidden=c(10))
plot(nn)

results <- compute(nn, test[,-6])$net.result
denorm_results <- denorm(results, min, max)

View(cbind(joined_data[-ind, 7], denorm_results))

#install.packages("rvest")
#install.packages("xts")
#install.packages("Quandl")

library(xml2)
library(rvest)
library(xts)
library(Quandl)

today <- Sys.Date()

sp500_wiki <- read_html(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")

symbols_table <- sp500_wiki %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()
symbols_table <- symbols_table[[1]]
symbols <-as.character(symbols_table$`Ticker symbol`)

Quandl.api_key("")

for (s in symbols) {
  tryCatch({
    s_data <- Quandl.datatable("WIKI/PRICES", ticker=c(s), date.gte="1997-01-01", date.lte=today)
    rownames(s_data) <- as.Data(s_data$date)
    s_data <- s_data[, "close", drop=FALSE]
  })
}

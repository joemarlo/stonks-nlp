library(rvest)
library(httr)
library(tidyverse)
tickers <- list()
for (i in 1:26) {
  link <- paste0('https://www.poweropt.com/optionable.asp?fl=',LETTERS[i])
  url <- read_html(link)
  table <- url %>% 
    html_node(xpath = '//*[@id="example"]') %>% 
    html_table()
  
  table <- table %>% select(X1)
  
  table <- data.frame(table[-1,])
  names(table) <- "ticker"
  table$ticker <- trimws(table$ticker)
  table$ticker <- str_remove(table$ticker, "\r\n")
  table <- as.data.frame(str_split_fixed(table$ticker, " ", n = 2))
  table <- table %>% map_df(trimws)
  names(table) <- c("ticker", "name")
  table$ticker <- str_remove(table$ticker, "[(]")
  table$ticker <- str_remove(table$ticker, "[)]")
  tickers[[i]] <- table
}

tickers <- bind_rows(tickers)
write_csv(tickers, "Data/tickers.csv")


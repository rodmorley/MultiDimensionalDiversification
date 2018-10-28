##
##
##      Multi Dimensional Diversification            Dr Rufus G Rankin 
##
##  https://www.amazon.com/Multi-Dimensional-Diversification-Improving-Portfolio-Selection/dp/153317850X?keywords=multidimensional+diversification&qid=1540729199&sr=8-1-fkmrnull&ref=sr_1_fkmrnull_1
##
##

require(quantmod)
require(purrr)

###
###  get asset class returns.
###

getStockReturns3 <- function(symbols, from = "2004-01-01", output = returns){
  
  symbols <- c("SPY",  "IWM", "EFA", "EEM", "IYR", "FIREX", "AGG", "PFORX",  "JNK", "TLT", "IEF",  "SHY", "PCRIX", "GLD")
  prices.daily         <- getSymbols(symbols, src = 'yahoo', 
                                     from = from, 
                                     auto.assign = TRUE, 
                                     warnings = FALSE) %>%
    map(~Ad(get(.))) %>% 
    reduce(merge) %>%
    `colnames<-`(symbols)
  
  prices         <- to.monthly(prices.daily, indexAt = 'lastof', OHLC = FALSE)
  returns        <- ROC(prices, type = "discrete")[-1,]
  return(output)
}

rets <- getStockReturns3()

rets_names <- c("SP500","SmallCap","MSCI_EAFE","EmergingMarkets",
                "US_RealEstate","Intl_RealEstate", "USAggBonds","ForeignBonds",
                "USHighYield","USLongBond","UST10yr","UST3yr","BbgCommodity","Gold")

names(rets) <- rets_names

###
### get Eurekahedge long short equity returns.
###

eurekahedge_url <- "http://www.eurekahedge.com/df/Eurekahedge_indices.zip"
download.file(eurekahedge_url, destfile = "EurekaHedge_Indices.zip", mode = 'wb')
unzip("EurekaHedge_Indices.zip")
eh <- read.csv("EurekaHedge_Indices.csv", header = TRUE, stringsAsFactors = F)

# eh %>% filter(str_detect(Index.Name, pattern = "Europe Long Short Equities Hedge Fund Index")) %>% select(Index.Name, NAV, Date, EHIID)

ELSEHFI     <- eh %>% filter(EHIID == 86) %>% mutate(Return = NAV/100) %>% select(Date, Return) %>% arrange(Date)
ELSEHFI.xts <- xts(ELSEHFI$Return, order.by = ELSEHFI$Date)



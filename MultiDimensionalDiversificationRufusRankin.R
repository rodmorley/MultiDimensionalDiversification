##
##
##    Multi Dimensional Diversification            Dr Rufus G Rankin 
##
##  https://www.amazon.com/Multi-Dimensional-Diversification-Improving-Portfolio-Selection/dp/153317850X?keywords=multidimensional+diversification&qid=1540729199&sr=8-1-fkmrnull&ref=sr_1_fkmrnull_1
##
##

require(quantmod)
require(purrr)
require(magrittr)
require(stringr)
require(ggcorrplot)
require(psych)

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
### get Eurekahedge long short equity , Event Driven and CTA returns.
###

eurekahedge_url <- "http://www.eurekahedge.com/df/Eurekahedge_indices.zip"
download.file(eurekahedge_url, destfile = "EurekaHedge_Indices.zip", mode = 'wb')
unzip("EurekaHedge_Indices.zip")
eh <- read.csv("EurekaHedge_Indices.csv", header = TRUE, stringsAsFactors = F)
eh$Date <- as.Date(eh$Date, format = "%d-%b-%Y")

str(eh)

eh %>% filter(str_detect(Index.Name, pattern = "Europe Long Short Equities Hedge Fund Index")) %>% select(Index.Name, NAV, Date, EHIID)
eh %>% filter(str_detect(Index.Name, pattern = "Event Driven Hedge Fund Index")) %>% group_by(EHIID, Index.Name) %>% summarise(total.count=n())
eh %>% filter(str_detect(Index.Name, pattern = "CTA")) %>% group_by(EHIID, Index.Name) %>% summarise(total.count=n())

LongShortEquity     <- eh %>% filter(EHIID == 86) %>% mutate(Return = NAV/100) %>% select(Date, Return) %>% arrange(Date)
LongShortEquity.xts <- xts(LongShortEquity$Return, order.by = LongShortEquity$Date)
LongShortEquity.xts <- LongShortEquity.xts[-1]
colnames(LongShortEquity.xts)<- "LongShortEquity"

EventDriven     <- eh %>% filter(EHIID == 478) %>% mutate(Return = NAV/100) %>% select(Date, Return) %>% arrange(Date)
EventDriven.xts <- xts(EventDriven$Return, order.by = EventDriven$Date)
EventDriven.xts <- EventDriven.xts[-1]
colnames(EventDriven.xts)<- "EventDriven"

CTA     <- eh %>% filter(EHIID == 476) %>% mutate(Return = NAV/100) %>% select(Date, Return) %>% arrange(Date)
CTA.xts <- xts(CTA$Return, order.by = CTA$Date)
CTA.xts <- CTA.xts[-1]
colnames(CTA.xts)<- "CTA"

combined_returns <- na.omit(merge(rets, EventDriven.xts, LongShortEquity.xts, CTA.xts))


returns_all_colours = c(rep("red", 4),rep("orange",2),rep("green",2),rep("brown",1), rep("darkgrey",3),rep("plum4",2),rep("blue", 3))
chart.RiskReturnScatter(rets, colorset = returns_all_colours, cex.lab = 1.3, cex.main = 2)


ggcorrplot(cor(combined_returns), type = "lower", legend.title = 'correlation')

###
### and PCA with varimax using psych package
###

pca_all_assets <- principal(cor(combined_returns), nfactors = 8, rotate = 'varimax')
print(pca_all_assets)
fa.diagram(pca_all_assets, cut = 0.3)
pca_all_assets$Structure[1:length(pca_all_assets$values), 1:pca_all_assets$factors] %>% d3heatmap(dendrogram = "row", colors = 'Blues')

###
### how many PCs to explain > 75% of variance -> how many 'bets' in portfolio.
###

pca_all_assets$Vaccounted["Cumulative Var",] > 0.75
grep(TRUE, pca_all_assets$Vaccounted["Cumulative Var",] > 0.75) %>% min()


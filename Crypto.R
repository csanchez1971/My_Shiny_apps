#Packages
library(quantmod)

#Data structure that contains stock quote objects
ETF_Data <- new.env()

#Assign dates to set range for stock quotes
sDate <- as.Date("2020-01-01", "%Y-%m-%d")

eDate <- as.Date("2021-12-31", "%Y-%m-%d")

#Assign a vector of ticker symbols.
ticker_symbol <- c("DOGE-USD","ETH-USD","ETC-USD")

getSymbols(ticker_symbol, env=ETF_Data, from=sDate, to=eDate)

col <- c("Ethereum", "DogeCoin", "Ethereum Classic")

Daily_Value <- do.call(merge, eapply(ETF_Data, Ad))
colnames(Daily_Value) <- col

Monthly_Value <- Daily_Value[endpoints(Daily_Value,'months')]
colnames(Monthly_Value) <- col


#Buying price

ETH = 3165
ETC = 93
DOGE1 = 0.38822
DOGE2 = 0.5651

Number_ETH <- 0.0322
Number_ETC <- 1.312
Number_DOGE <- 813.7

Total <- as.numeric(Number_ETH*last(Daily_Value$Ethereum) + Number_ETC*last(Daily_Value$`Ethereum Classic`) + Number_DOGE*last(Daily_Value$DogeCoin))
Total


Number_ETH*(last(Daily_Value$Ethereum)-ETH)
Number_ETC*(last(Daily_Value$`Ethereum Classic`)-ETC)
Number_DOGE*last(Daily_Value$DogeCoin)-DOGE1*602.4-DOGE2*211.3




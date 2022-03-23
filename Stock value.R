#Packages
library(quantmod)
library(ggplot2)
library(plotly)

#Data structure that contains stock quote objects
ETF_Data <- new.env()

#Assign dates to set range for stock quotes
sDate <- as.Date("2018-01-01", "%Y-%m-%d")

eDate <- as.Date("2022-12-31", "%Y-%m-%d")

#Assign a vector of ticker symbols.
ticker_symbol <- c("ACN","NVS","ALC", "EURUSD=X", "EURCHF=X","CHFUSD=X")


#Loop and retrieve each ticker symbols quotes from Yahoo's API 
getSymbols(ticker_symbol, env=ETF_Data, from=sDate, to=eDate)
# Extract the Adjusted column from all objects,
# then merge all columns into one object
Daily_Value <- do.call(merge, eapply(ETF_Data, Ad))
Daily_Value <- Daily_Value[,c(1,2,6,5,3,4)]
# then extract the monthly endpoints
Monthly_Value <- Daily_Value[endpoints(Daily_Value,'months')]
# Month <- getSymbols(ticker_symbol, from = sDate, src = 'yahoo', periodicity = 'monthly')


ggplot(Monthly_Value, aes(x=Index, y=CHFUSD.X.Adjusted))+geom_line()




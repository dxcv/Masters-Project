#clean workspace 
rm(list=ls()[! ls() %in% c("object1","object2")]) #remove all but the objects you define e.g. object1 and object2
#load required packages
library(RQuantLib)
library(quantstrat) 
library(quantmod)
library(PerformanceAnalytics)
#library(tidyquant)
#library(tidyverse)
#Download FRED data for 10Yr US Bonds
start.date <- as.Date("1970-01-31")
end.date <- as.Date("2016-12-31")

getSymbols("INTGSTZAM193N", src="FRED") #download data

Tbill.yield <- to.monthly(INTGSTZAM193N,indexAt='lastof',drop.time=TRUE)[,4] #convert to monthly
Tbill.yield <- Tbill.yield[paste(start.date,end.date,sep="/")] #subset to required period

rm(start.date,end.date)
#save as CSV files
write.zoo(Tbill.yield, file="CSV files/SATBillYields.csv", sep=",")
TRCalcs <- Tbill.yield
TRCalcs$Yield <- Tbill.yield/12/100
TRCalcs$Index <- 100

for (i in 2:NROW(TRCalcs)-1){
  TRCalcs$Index[i+1,1] <- (1+TRCalcs$Yield[i])*TRCalcs$Index[i]
}

TBillIndex <- TRCalcs[,3]
colnames(TBillIndex) <- c("TBILLS.Close")

Returns <- Return.calculate(TBILLTR["1972-12-31::2016-12-31"])[-1,]
Returns <- Returns["1972-12-31::2012-12-31"]
Return.annualized(Returns,scale = 12)*100
To do:
  
  check this is right way to calculate TR index for Rf
Can this be done better with a mutate?
#save as CSV files
write.zoo(TBillIndex, file="CSV files/TBILLS.csv", sep=",")
new calculation method

getSymbols("DTB3", src="FRED") #download data
Tbill.yield <- to.monthly(DTB3,indexAt='lastof',drop.time=TRUE)[,4] #convert to monthly
Tbill.return <- Tbill.yield #set this up to hold price returns
Tbill.return[1,1]<-0
colnames(Tbill.return)<-"PriceReturn"

for (i in 1:(NROW(Tbill.yield)-1)) {
  Tbill.return[i+1,1]<-FixedRateBondPriceByYield(yield=Tbill.yield[i+1,1]/100,issueDate=Sys.Date(),
                                                 maturityDate= advance("TARGET", Sys.Date(), 3, 2),
                                                 rates=Tbill.yield[i,1]/100,period=2)[1]/100-1
}

#Total return will be the price return + the previous months yield for one month
Tbill.totalreturn<-Tbill.return+lag(Tbill.yield,k=1)/12/100
Tbill.totalreturn[1,1]<-0
colnames(Tbill.totalreturn)<-"Total Return"

#Now we need to create a price index with base 100
TBILLTR <- cumprod(1 + Tbill.totalreturn) * 100
colnames(TBILLTR) <- c("TBILLTR.Close")

Returns <- Return.calculate(TBILLTR["1972-12-31::2016-12-31"])[-1,]
Returns <- Returns["1972-12-31::2012-12-31"]
Return.annualized(Returns,scale = 12)*100
AllAssets <- TBILLTR["1972-12-31::2016-12-31"]

AllAssets.ret <- Return.calculate(AllAssets)[-1,]

#subset to required dates
Returns <- AllAssets.ret["1972-12-31::2012-12-31"]
stats <- rbind(Return.annualized(Returns,scale = 12)*100, 
               StdDev.annualized(Returns,scale = 12)*100, 
               SharpeRatio.annualized(Returns,Rf = Returns),
               maxDrawdown(Returns,scale = 12,invert = FALSE)*100,
               c(4.30))

rownames(stats) <- c("Return","Volatility","Sharpe","MaxDD","Inflation CAGR")

round(stats,2)
Return.annualized(Returns, scale = 12)

Return.annualized(Returns, geometric = FALSE)

SharpeRatio.annualized(Returns["1972-12-31::2012-12-31"], scale = 12, Rf = Returns["1972-12-31::2012-12-31"],geometric = TRUE)
TBILLTR doesn't equal expected need to check calcs

managers example

data(managers) TbillMonthlyTReturns <- managers[,10] Return.annualized(TbillMonthlyTReturns["1998//2002"]) SharpeRatio.annualized(TbillMonthlyTReturns["1998//2002"],TbillMonthlyTReturns["1998//2002"])

AllAssets.ret <- Return.calculate(AllAssets)[-1,]

subset to required dates

Returns <- AllAssets.ret["1972-12-31::2012-12-31"] stats <- rbind(Return.annualized(Returns,scale = 12)100, StdDev.annualized(Returns,scale = 12)100, SharpeRatio.annualized(Returns,Rf = Returns), maxDrawdown(Returns,scale = 12,invert = FALSE)*100, c(4.30))

rownames(stats) <- c("Return","Volatility","Sharpe","MaxDD","Inflation CAGR")

round(stats,2)



```{r}
Return.annualized(Returns, scale = 12)
Return.annualized(Returns, geometric = FALSE)
SharpeRatio.annualized(Returns, scale = 12, Rf = Returns["1972-12-31::2012-12-31"],geometric = TRUE)
library(quantmod)
require(zoo)
library(xts)
library(ggplot2)
library(tseries)
library(gridExtra)
library(rugarch)
library(e1071)
library(stats)
library(stargazer)
options(scipen=999)

#index data
# download daily sp500 closing prices over the last 31 years

start <- as.Date('1990-01-03')
end <- as.Date('2021-04-30')
sp500 <-new.env()
getSymbols("^GSPC",env=sp500, src="yahoo", from = start, to = end)
getSymbols("^GSPC", src="yahoo", from = start, to = end)
dtax<-sp500[["GSPC"]]
dtax=GSPC
colnames(dtax) <- c("Open","High","Low","Close","Volumn","Adj.Close")
par(mfrow=c(2,1))
plot(dtax$Close, main="SP500 daily closing prices")
acf(dtax$Close, main= "ACF of SP500 daily closing prices")

#stocks data
#read csv that contains info on market capitalization of each sp500 stock
#this will allow us to sample 4 stocks, one for each quartile of the market cap distribution

sp_stocks= read.csv("sp500_marketcap.csv", header = TRUE, sep = ";" )
colnames(sp_stocks)= c("Rank","Ticker", "Company", "Industry", "Market_cap" )
sp_stocks[,6]=NULL
summary(sp_stocks$Market_cap)
sp_stocks$Market_cap

bp=ggplot(sp_stocks, aes(x="", y=Market_cap))+
  geom_boxplot()
bp

#data is ordered by market cap, so we can easily sample 1 stock for each quartile

set.seed(1)
sample(1:126,1)
#121 SO
sample(127:252,1)
#194 AIG
sample(253:378,1)
#291 DTE
sample(379:504,1)
#379 CMS

#download daily closing prices for the 4 randomly selected stocks

myStocks <-lapply(c("SO", "AIG", "DTE", "CMS"), function(x) {getSymbols(x, 
                                                                        from = "1990/01/01", 
                                                                        to = "2021/04/30",
                                                                        periodicity = "daily",
                                                                        auto.assign=FALSE)} )
# plots of closing prices and ACF

myStocks[[1]]$SO.Adjusted
plot(myStocks[[1]]$SO.Adjusted, main="Southern daily closing prices")
acf(myStocks[[1]]$SO.Adjusted, main= "ACF of Southern daily closing prices")

myStocks[[2]]
plot(myStocks[[2]]$AIG.Adjusted, main="AIG daily closing prices")
acf(myStocks[[2]]$AIG.Adjusted, main= "ACF of AIG daily closing prices")

myStocks[[3]]
plot(myStocks[[3]]$DTE.Adjusted, main="DTE Energy daily closing prices")
acf(myStocks[[3]]$DTE.Adjusted, main= "ACF of DTE Energy daily closing prices")

myStocks[[4]]$CMS.Adjusted
plot(myStocks[[4]]$CMS.Adjusted, main="CMS Energy daily closing prices")
acf(myStocks[[4]]$CMS.Adjusted, main= "ACF of CMS Energy daily closing prices")

#index returns
#we compute returns from adjusted closing prices, produce plots and runs statistical tests

dtax$ret<-100*diff(log(dtax$Adj.Close))
dtax<-dtax[complete.cases(dtax),]
plot(dtax$ret, main="SP500 daily returns")
acf(dtax$ret, main="ACF of SP500 daily returns")

par(mfrow=c(1,1))

g1= ggplot(dtax, aes(x = ret)) +
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5,binwidth =0.05) +
  geom_density(colour = 'blue') + xlab(expression(bold('Return'))) +
  ylab(expression(bold('Frequency')))+
  ggtitle("SP500 daily returns")
plot(g1)

qqnorm(dtax$ret, main="Normal Q-Q Plot of SP500 returns")
qqline(dtax$ret, col = "steelblue", lwd = 2)


bp_index=ggplot(dtax, aes(x="", y=ret))+
  geom_boxplot()+
  ylab("%")+
  ggtitle("SP500 daily returns")
bp_index #chart 1

#normality test for index returns
library(nortest)
ad.test(dtax$ret) #normality rejected at 1% confidence level

#Augmented Dickey–Fuller Test
adf.test(dtax$ret)
adf.test(dtax$Adj.Close)

#stocks returns
#we compute stock returns, produce plots and run statistical tests

myStocks[[1]]$ret<-100*diff(log(myStocks[[1]]$SO.Adjusted))
myStocks[[1]]<-myStocks[[1]][complete.cases(myStocks[[1]]),]
plot(myStocks[[1]]$ret, main="Southern daily returns")
acf(myStocks[[1]]$ret, main="ACF of Southern daily returns")

g2= ggplot(myStocks[[1]], aes(x = ret)) +
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5,binwidth =0.05) +
  geom_density(colour = 'blue') + xlab(expression(bold('Return'))) +
  ylab(expression(bold('Frequency')))
plot(g2)

qqnorm(myStocks[[1]]$ret, main="")
qqline(myStocks[[1]]$ret, col = "steelblue", lwd = 2)

myStocks[[2]]$ret<-100*diff(log(myStocks[[2]]$AIG.Adjusted))
myStocks[[2]]<-myStocks[[2]][complete.cases(myStocks[[2]]),]
plot(myStocks[[2]]$ret, main="AIG daily returns")
acf(myStocks[[2]]$ret, main="ACF of AIG daily returns")

g3= ggplot(myStocks[[2]], aes(x = ret)) +
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5,binwidth =0.05) +
  geom_density(colour = 'blue') + xlab(expression(bold('Return'))) +
  ylab(expression(bold('Frequency')))
plot(g3)

qqnorm(myStocks[[2]]$ret, main="")
qqline(myStocks[[2]]$ret, col = "steelblue", lwd = 2)

myStocks[[3]]$ret<-100*diff(log(myStocks[[3]]$DTE.Adjusted))
myStocks[[3]]<-myStocks[[3]][complete.cases(myStocks[[3]]),]
plot(myStocks[[3]]$ret, main="DTE Energy daily returns")
acf(myStocks[[3]]$ret, main="ACF of DTE Energy daily returns")

g4= ggplot(myStocks[[3]], aes(x = ret)) +
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5,binwidth =0.05) +
  geom_density(colour = 'blue') + xlab(expression(bold('Return'))) +
  ylab(expression(bold('Frequency')))
plot(g4)

qqnorm(myStocks[[3]]$ret, main="")
qqline(myStocks[[3]]$ret, col = "steelblue", lwd = 2)

myStocks[[4]]$ret<-100*diff(log(myStocks[[4]]$CMS.Adjusted))
myStocks[[4]]<-myStocks[[4]][complete.cases(myStocks[[4]]),]
plot(myStocks[[4]]$ret, main="CMS Energy daily returns")
acf(myStocks[[4]]$ret, main="ACF of CMS Energy daily returns")

g5= ggplot(myStocks[[4]], aes(x = ret)) +
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5,binwidth =0.05) +
  geom_density(colour = 'blue') + xlab(expression(bold('Return'))) +
  ylab(expression(bold('Frequency')))
plot(g5)

qqnorm(myStocks[[4]]$ret, main="")
qqline(myStocks[[4]]$ret, col = "steelblue", lwd = 2)

stocks_df= cbind(myStocks[[1]]$ret, myStocks[[2]]$ret, myStocks[[3]]$ret, myStocks[[4]]$ret, dtax$ret)
colnames(stocks_df)= c("Southern", "AIG", "DTE Energy", "CMS Energy", "SP500")

#volatility: stylized facts
#volatility persistence

par(mfrow=c(2,1))
plot(stocks_df$SP500^2, main="SP500 daily squared returns")
acf(stocks_df$SP500^2, na.action = na.pass, main= "ACF of SP500 daily squared retruns")
Box.test(stocks_df$SP500^2, lag=1, type = "Ljung-Box" )
Box.test(stocks_df$Southern^2, lag=1, type = "Ljung-Box" )
Box.test(stocks_df$AIG^2, lag=1, type = "Ljung-Box" )
Box.test(stocks_df$`DTE Energy`^2, lag=1, type = "Ljung-Box" )
Box.test(stocks_df$`CMS Energy`^2, lag=1, type = "Ljung-Box" )
#leverage effect

par(mfrow=c(3,2))
ccf(as.numeric(stocks_df$SP500^2),as.numeric(stocks_df$SP500), main="SP500 - CCF between returns and squared returns", na.action = na.pass)
ccf(as.numeric(stocks_df$Southern^2),as.numeric(stocks_df$Southern), main="Southern", na.action = na.pass)
ccf(as.numeric(stocks_df$AIG^2),as.numeric(stocks_df$AIG), main="AIG", na.action = na.pass)
ccf(as.numeric(stocks_df$`DTE Energy`^2),as.numeric(stocks_df$`DTE Energy`), main="DTE Energy", na.action = na.pass)
ccf(as.numeric(stocks_df$`CMS Energy`^2),as.numeric(stocks_df$`CMS Energy`), main="CMS Energy", na.action = na.pass)

#simulation of stochastic processes

set.seed(1) 

par(mfrow=c(1,3))

ar1 = arima.sim(model = list(ar = 0.8), n = 200)
plot(ar1, main= "Simulation of AR(1) process", xlab="t", ylab="")
acf(ar1, main= "ACF of AR(1) process with phi = 0.8")
pacf(ar1, main= "PACF of AR(1) process with phi = 0.8")
adf.test(ar1)

ma1 = arima.sim(model = list(ma = 0.8), n = 200)
plot(ma1, main= "Simulation of MA(1) process", xlab="t", ylab="")
acf(ma1, main= "ACF of MA(1) process with theta = 0.8")
pacf(ma1, main= "PACF of MA(1) process with theta = 0.8")
adf.test(ma1)

arma11 = arima.sim(model = list(ar = 0.4, ma = 0.5), n = 200)
plot(arma11, main= "Simulation of ARMA(1,1) process", xlab="t", ylab="")
acf(arma11, main= "ACF of ARMA(1,1) process with phi = 0.4 and theta = 0.5")
pacf(arma11, main= "PACF of ARMA(1,1) process with phi = 0.4 and theta = 0.5")
adf.test(arma11)

wn= arima.sim(model = list(order = c(0, 0, 0)), n = 200)
plot(wn, main= "Simulation of White Noise process", xlab="t", ylab="")
acf(wn, main= "ACF of White Noise process")
pacf(wn, main= "PACF of White Noise process")
adf.test(wn)

rw= arima.sim(model = list(order = c(0, 1, 0)), n = 200)
plot(rw, main= "Simulation of Random Walk process", xlab="t", ylab="")
acf(rw, main= "ACF of Random Walk process")
pacf(rw, main= "PACF of Random Walk process")
adf.test(rw)

#garch implementation on sp500 data
library(rugarch)
library(FinTS)

plot(stocks_df$SP500^2)

acf(stocks_df$SP500, na.action = na.pass)
ArchTest(ret,lags=12, demean=F)

ret= stocks_df$SP500 - mean(stocks_df$SP500, na.rm= TRUE)
ret<-ret[complete.cases(ret),]

acf(ret$SP500)

spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), 
                   variance.model= list(model = "sGARCH", garchOrder = c(1, 1), 
                                        submodel = NULL, external.regressors = NULL, 
                                        variance.targeting = FALSE), distribution.model ="norm")


fit <- ugarchfit(spec, data = ret )
show(fit)
fit@fit$fitted.values
plot(fit)
par(mfrow=c(2,2))
plot(fit, which=8)
plot(fit, which=9)
plot(fit, which=10)
plot(fit, which=12)

spec1 <- ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), 
                    variance.model= list(model = "sGARCH", garchOrder = c(1, 1), 
                                         submodel = NULL, external.regressors = NULL, 
                                         variance.targeting = FALSE), distribution.model ="std")

fit1 <- ugarchfit(spec1, data = ret )
show(fit1)
fit@fit$fitted.values

par(mfrow=c(2,2))
plot(fit1, which=8)
plot(fit1, which=9)
plot(fit1, which=10)
plot(fit1, which=12)

par(mfrow=c(1,3))


#check of residulas
library(forecast)
checkresiduals(fit@fit$residuals)
fit@fit$sigma
plot(fit@fit$sigma)
fitted(fit)

#fitted variance

ret$hhat<-fit@fit$sigma^2
plot(ret$hhat, main="Fitted conditional variance")
ret$actuals=ret$SP500^2

g5= ggplot(ret, aes(Index)) + 
  geom_line(aes(y = actuals, colour = "actuals")) + 
  geom_line(aes(y = hhat, colour = "fitted"))
g5

modelroll=ugarchroll (
  spec=spec, data=ret$SP500, n.ahead = 1,
  n.start = 1000, refit.every = 50, refit.window = c("recursive"),
  window.size = NULL, solver = "hybrid", fit.control = list(),
  solver.control = list(), calculate.VaR = FALSE, VaR.alpha = c(0.01,
                                                                0.05),
  cluster = NULL, keep.coef = TRUE
)

plot(modelroll)
modelroll@forecast

plot(modelroll@forecast$density$Sigma, type="l")
plot(modelroll)


models <- c("sGARCH", "eGARCH", "gjrGARCH", "apARCH", "csGARCH")
distributions <- c("norm", "std", "ged", "snorm", "sstd", "sged", "jsu", "ghyp")
spec.comp <- list()

for( m in models ) {
  for( d in distributions ) {
    spec.comp[[paste( m, d, sep = "-" )]] <-
      ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                 variance.model = list(model = m, garchOrder = c(1, 1)),
                 distribution.model=d) }}

specifications <- names( spec.comp )

roll.comp <- list()

for( s in specifications ){
  roll.comp[[s]] <-  ugarchroll(spec = spec.comp[[s]], data= ret$SP500, n.ahead = 1, n.start = 1000, refit.every = 50, refit.window = c("recursive"),
                                window.size = NULL, solver = "hybrid", fit.control = list(),
                                solver.control = list(), calculate.VaR = FALSE, VaR.alpha = c(0.01,0.05),
                                cluster = NULL, keep.coef = TRUE )}
library(MCS)

#MCS procedure
#QLIKE loss function

Loss_QLIKE_1 <- do.call(cbind,lapply(specifications,
                                     function(s) LossVol(realized=roll.comp[[s]]@forecast$density$Realized,
                                                         evaluated=roll.comp[[s]]@forecast$density$Sigma, which = "QLIKE")))
colnames(Loss_QLIKE_1) <- specifications

SSM_QLIKE_1 <- MCSprocedure(Loss = Loss_QLIKE_1, alpha = 0.2, B = 5000, statistic = "TR")


#MSE loss function

Loss_MSE_SD_1 <- do.call(cbind,lapply(specifications,
                                      function(s) LossVol(realized=roll.comp[[s]]@forecast$density$Realized,
                                                          evaluated=roll.comp[[s]]@forecast$density$Sigma, which = "SE1")))
colnames(Loss_MSE_SD_1) <- specifications

SSM_MSE_SD_1 <- MCSprocedure(Loss = Loss_MSE_SD_1, alpha = 0.2, B = 5000, statistic = "TR")

## print table
library(xtable)

out1=as.data.frame(SSM_QLIKE_1@show)
out1$Rank_R=NULL
out1$v_R=NULL     
out1$MCS_R=NULL
out1
xtable(out1)

##write outputs to csv

lql1= as.data.frame(Loss_QLIKE_1)
write.csv(lql1, "lql1.csv", row.names = TRUE)
write.csv(ret, "data.csv", row.names = TRUE)


b1=roll.comp$`apARCH-norm`@forecast$density$Sigma
b2=roll.comp$`apARCH-norm`@forecast$density$Realized

plot(b1, type="l")

b_out=as.data.frame(b1)
b_out$realized=b2
colnames(b_out)=c("sigma", "realized_ret")
write.csv(b_out, "b_out.csv", row.names = TRUE)

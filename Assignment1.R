
#########################################
##Exploratory Data Analysis-ALI NAFEES##
#########################################

##At first you would need to install all the packages and call the libraries as shown below, required to perform your task

library(moments) 
library(pdfetch)
library(graphics)
library(psych)
library(tidyquant)
options(digits=4)

tq_index("SP500")

tickers=c("AAPL","TSLA","AMZN","MSFT","TWTR") ##this will create the list of tickers for five stocks

dat=pdfetch_YAHOO(tickers,fields="adjclose",from="2015-01-01", to="2020-12-31", interval= "1wk")
##this will fetch the data from Yahoo Finance and stored that into table named "dat".

##Computing Continously compounding log returns (a)
aapl_ccret <- dat[,1]
log_aapl <- diff(log(aapl_ccret), lag=1)
log_aapl.na=as.vector(na.omit(log_aapl))    # remove missing values
tsla_ccret <- dat[,2]
log_tsla <- diff(log(tsla_ccret), lag=1)
log_tsla.na=as.vector(na.omit(log_tsla))    # remove missing values
amzn_ccret <- dat[,3]
log_amzn <- diff(log(amzn_ccret), lag=1)
log_amzn.na=as.vector(na.omit(log_amzn))    # remove missing values
msft_ccret <- dat[,4]
log_msft <- diff(log(msft_ccret), lag=1)
log_msft.na=as.vector(na.omit(log_msft))    # remove missing values
twtr_ccret <- dat[,5]
log_twtr <- diff(log(twtr_ccret), lag=1)
log_twtr.na=as.vector(na.omit(log_twtr))    # remove missing values

#plotting the time series plot (b1)


plot(ts(log_aapl.na))
plot(ts(log_tsla.na))
plot(ts(log_amzn.na))
plot(ts(log_msft.na))
plot(ts(log_twtr.na))

#plotting the histograms (b2)

#AAPL histogram
hist(log_aapl.na,breaks=50,main="Hist of Index's return")
hist(log_aapl.na,prob=T,breaks=50,main="Hist of Index's return")
curve(dnorm(x,mean=mean(log_aapl.na),sd=sd(log_aapl.na)),add=T,col="red")
#TSLA histogram
hist(log_tsla.na,breaks=50,main="Hist of Index's return")
hist(log_tsla.na,prob=T,breaks=50,main="Hist of Index's return")
curve(dnorm(x,mean=mean(log_tsla.na),sd=sd(log_tsla.na)),add=T,col="red")
#AMZN histogram
hist(log_amzn.na,breaks=50,main="Hist of Index's return")
hist(log_amzn.na,prob=T,breaks=50,main="Hist of Index's return")
curve(dnorm(x,mean=mean(log_amzn.na),sd=sd(log_amzn.na)),add=T,col="red")
#MSFT histogram
hist(log_msft.na,breaks=50,main="Hist of Index's return")
hist(log_msft.na,prob=T,breaks=50,main="Hist of Index's return")
curve(dnorm(x,mean=mean(log_msft.na),sd=sd(log_msft.na)),add=T,col="red")
#TWTR histogram
hist(log_twtr.na,breaks=50,main="Hist of Index's return")
hist(log_twtr.na,prob=T,breaks=50,main="Hist of Index's return")
curve(dnorm(x,mean=mean(log_twtr.na),sd=sd(log_twtr.na)),add=T,col="red")

#estimating the mean return and standard deviation of returns c(1)

mandsd_aapl = c(mean(log_aapl.na), sd(log_aapl.na))
mandsd_aapl

mandsd_tsla = c(mean(log_tsla.na), sd(log_tsla.na))
mandsd_tsla

mandsd_amzn = c(mean(log_amzn.na), sd(log_amzn.na))
mandsd_amzn

mandsd_msft = c(mean(log_msft.na), sd(log_msft.na))
mandsd_msft

mandsd_twtr = c(mean(log_twtr.na), sd(log_twtr.na))
mandsd_twtr

#simulating 100 days forward of stock price movement for each stock c(2)

r_aapl = rnorm(100,mean=mean(log_aapl.na)/253,sd=sd(log_aapl.na)/sqrt(253))
r_tsla = rnorm(100,mean=mean(log_tsla.na)/253,sd=sd(log_tsla.na)/sqrt(253))
r_amzn = rnorm(100,mean=mean(log_amzn.na)/253,sd=sd(log_amzn.na)/sqrt(253))
r_msft = rnorm(100,mean=mean(log_msft.na)/253,sd=sd(log_msft.na)/sqrt(253))
r_twtr = rnorm(100,mean=mean(log_twtr.na)/253,sd=sd(log_twtr.na)/sqrt(253))


#Repeating the simulation in c(2) 20 times and saved them in matrix with 100 rows and 20 col

#AAPL simulation
niter = 20
matrix_aapl <- matrix(0,100,20)

for (i in niter:1) {
  r_fore_aapl = rnorm(100,mean=mean(log_aapl.na)/7,sd=sd(log_aapl.na)/sqrt(7))
  Price_aapl = as.numeric(aapl_ccret[313,1]) * exp(cumsum(r_fore_aapl))
  matrix_aapl[,i]=Price_aapl
}
mean(matrix_aapl)

#TSLA simulation
niter = 20
set.seed(1)
for (i in niter:1) {
  r_fore_tsla = r_tsla
  logPrice_tsla = mean(tsla_ccret) + cumsum(r_fore_tsla)
}
matrix_tsla <- matrix(logPrice_tsla,100,20)
mean(matrix_tsla)

#AMZN simulation
niter = 20
set.seed(1)
for (i in niter:1) {
  r_fore_amzn = r_amzn
  logPrice_amzn = mean(amzn_ccret) + cumsum(r_fore_amzn)
}
matrix_amzn <- matrix(logPrice_amzn,100,20)
mean(matrix_amzn)

#MSFT simulation
niter = 20
set.seed(1)
for (i in niter:1) {
  r_fore_msft = r_msft
  logPrice_msft = mean(msft_ccret) + cumsum(r_fore_msft)
}
matrix_msft <- matrix(logPrice_msft,100,20)
mean(matrix_msft)

#TWTR simulation
niter = 20
set.seed(1)
for (i in niter:1) {
  r_fore_twtr = r_twtr
  logPrice_twtr = mean(twtr_ccret) + cumsum(r_fore_twtr)
}
matrix_twtr <- matrix(logPrice_twtr,100,20)
mean(matrix_twtr)

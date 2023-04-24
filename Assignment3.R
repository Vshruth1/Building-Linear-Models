
##Sampling and Hypothesis Testing - Shruthi Venkataraman


## Call the libraries as shown below, required to perform your task

library(moments) 
library(pdfetch)
library(graphics)
library(psych)
library(tidyquant)
options(digits=4)

tq_index("SP500")

tickers=c("GOOG","AMZN","FB","NFLX","SMSN.IL") ##this will create the list of tickers for five stocks

##Fetching the Stock Data

##this will fetch the data from Yahoo Finance for five years with 1 week interval 
#and stored that into table named "dat".

dat=pdfetch_YAHOO(tickers,fields="adjclose",from="2016-01-01", to="2020-12-31", interval= "1wk")


##Calculating Log returns

#Calculating log returns
returns = na.omit(diff(log(dat)))
print(head(returns))
dims = dim(returns)
n = dims[1]
l = dims[2]

no_of_weeks = nrow(returns)/2 #this has determine how many observations we have.
no_of_weeks


##Sampling and It's Mean


#the below function will compute the 100 samples for each stock for sample size 39 and
#then it will also compute the means for each 100 samples.
#the result will be saved in sample_mean_100

sample_mean_100=matrix(0,nrow=100,ncol=5)
colnames(sample_mean_100)=colnames(dat)
for (i in 1:5)
{
  sample_mean_100[,i] = replicate(100,mean(sample(returns[,i],39,replace = TRUE))) #I got to 39 by no_of_weeks= 130 * 30%=39
}

#Below command, for each stock, will draw the histogram of the means of returns for 100 samples.

par(mfrow=c(2,2))
for (i in 1:5) {
  names=colnames(sample_mean_100)
  hist(sample_mean_100[,i],breaks=50,main=names[i])
  hist(sample_mean_100[,i],prob=T,breaks=50,main=names[i])
  curve(dnorm(x,mean=mean(sample_mean_100[,i]),sd=sd(sample_mean_100[,i])),add=T,col="blue")
}

#this below command will conduct the hypothesis testing for each pair of stocks. i.e. Ho: Ui=Uj)
#First we have to conduct the variance test to see if both the stocks returns are equal or not.



##Conducting Variance test


#this command will compute the variance test for the pairs of stock to dtermine if we use var.equal "true" or "false"
#while doing t-test. The results will be stored in the matrix called Var_Test.

Var_Test=matrix(0,nrow=5,ncol=5)
colnames(Var_Test) <- c("GOOG","AMZN","FB","NFLX","SMSN.IL")
rownames(Var_Test) <- c("GOOG","AMZN","FB","NFLX","SMSN.IL")
for (i in 1:5) 
{
  for (k in 1:5) 
  {
    Variance_test <-  var.test(returns[,i],returns[,k])
    Var_Test[i,k] = Variance_test$p.value
  }
}



##Conducting Hypothesis

#this command will conduct the t-test using the p value computed in the var.test. if p.value in above step < 0.05
#then var.equal will be "True" otherwise "False"
#P-value < 0.05 means it indicates strong evidence against the null hypothesis and is considered to be significantly
#significant.

for (i in 1:5) 
{
  for (k in 1:5) 
  {
    if (Var_Test[i,k]>0.05)
    {
      Hypo_test <-  t.test(as.vector(returns[,i]),as.vector(returns[,k]),paired = TRUE,var.equal = TRUE)
      print(Hypo_test)
    } else {
      Hypo_test <-  t.test(as.vector(returns[,i]),as.vector(returns[,k]),paired = TRUE,var.equal = FALSE)
      print(Hypo_test)
    }
  }
}


#To evaluate the result of the hypothesis result we will compare the aplha value i.e. 0.05 with p value.
#if pvalue is greater than alpha, you accept the null hypothesis means you reject the alternative hypothesis.
#if p value is less than alpha, you reject the null hypothesis means you accept the alternative hypothesis.
# In this case, the null hypothesis is accepted.

##THANK YOU

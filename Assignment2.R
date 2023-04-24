#############################################################
##Exploratory Data Analysis-Shruthi Venkataraman:Homework 2##
#############################################################

#a)setting the working directory to be used in the program

setwd("C:/Users/USER/OneDrive/Desktop/hw2/")

data_dir="C:/Users/USER/OneDrive/Desktop/hw2/" #data_dir must be first defined 
data_in <- paste(data_dir)

#Creating a separate input

use_case_dir <- "Weekly database"
data_out <- use_case_dir
output <- paste0(use_case_dir)

#b)Creating a new folder for output on computer 

create_output_if_doesnt_exist<- function(folder){
  if (!dir.exists(folder)){
    dir.create(folder)
    print(paste(folder,"folder created."))}
  else{
    print(paste(folder,"folder already exists"))}
}
create_output_if_doesnt_exist(output)

#c)Cleaning and Analyzing the Fama-French data

#Loading data of fama/french 3 factors
ff_3 <-  read.csv("F-F_Research_Data_Factors_weekly.csv", sep=",", skip = 3,header = T,nrows = 4961) 

#Parsed with column specifications
library(tidyverse)
cols(
  x = col_character(),
  'MKT.RF' = col_double(),
  SMB = col_double(),
  HML = col_double(),
  RF = col_double()
)


#To change the Column Names
colnames(ff_3) <- paste(c("date","mkt_excess","smb","hml","rf"))


#Changing date format that are in Year/month format parsed as int.


library(lubridate)
library(tidyquant)
library(timetk)
library(dplyr)

ff_3$date <- as.Date.character(ff_3$date, "%Y%m%d") #formatting the date in fama french 3 file
ff_3 <- ff_3 %>%
  mutate_at(vars(-date), function(x) x/100)
#We are almost done cleaning

#Now we will fetch the data for the 5 stocks and compute the retunrs for that


##Analysing and Cleaning Stock Returns Data

library(moments) 
library(pdfetch)
library(graphics)
library(psych)
library(tidyquant)


options(digits=4)

tq_index("SP500")

tickers=c("GOOG","AMZN","FB","NFLX","SMSN.IL") ##this will create the list of tickers for five stocks

dat=pdfetch_YAHOO(tickers,fields="adjclose",from="2016-01-01", to="2020-12-31", interval= "1wk")
##this will fetch the data from Yahoo Finance for five years with 1 week interval 
#and stored that into table named "dat".##

##Computing Continuously compounding log returns
goog_ret <- dat[,1]                         # Choosing the first column of the data set and assigning it.
log_goog <- diff(log(goog_ret), lag=1)      # Applying the log function to find the log returns

amzn_ret <- dat[,2]                         # Choosing the second column of the data set and assigning it.
log_amzn <- diff(log(amzn_ret), lag=1)      # Applying the log function to find the log returns

fb_ret <- dat[,3]                           # Choosing the third column of the data set and assigning it.
log_fb <- diff(log(fb_ret), lag=1)          # Applying the log function to find the log returns

nflx_ret <- dat[,4]                         # Choosing the fourth column of the data set and assigning it.
log_nflx <- diff(log(nflx_ret), lag=1)      # Applying the log function to find the log returns

smsn_ret <- dat[,5]                         # Choosing the fifth column of the data set and assigning it.
log_smsn <- diff(log(smsn_ret), lag=1)      # Applying the log function to find the log returns

Date_range <- data.frame("date" = seq(as.Date("2016-01-01"), as.Date("2020-12-25"), by="weeks")) #Specifying the date range to include it as column on Dataframe.

stock_returns<-data.frame(cbind(Date_range,log_goog,log_amzn,log_fb,log_nflx,log_smsn)) #this has created the data frame for the stock returns.

# Cleaning the stock return data

stock_returns$date= stock_returns$date + days(1)

##Merging the Two datasets now i.e. Fama French and Stock Returns

#merge(ff_3,stock_returns, by = 'date', all = T)

Market_analysis <- data.frame(merge(ff_3,stock_returns, by = 'date', all = T))
Market_analysis_clean <- data.frame(na.omit(Market_analysis)) #removing missing values

#e)Selecting Subset B/w 1st and 3rd quantile of Market Excess Return

f_quan = quantile(stock.ret.clean$mkt_excess,0.25) #calculating First Quantile
f_quan

th_quan = quantile(stock.ret.clean$mkt_excess,0.75) #Calculating Third Quantile
th_quan

#selecting subset >1st quantile and <3rd quantile of Market excess return

final_output <- stock.ret.clean %>% filter((mkt_excess > f_quan)&(mkt_excess < th_quan)) %>% 
  select(date, mkt_excess,GOOG,AMZN, FB,NFLX,SMSN.IL)

write.csv(final_output, paste0("Assignment_2_Shruthi_V.csv"),row.names = F) #exporting final output into the computer. It will be saved on the working directory.


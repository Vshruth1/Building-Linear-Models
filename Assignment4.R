############################################
####Assignment - 5: Shruthi Venkataraman####
############################################

#Load the packages#
library(tidyverse)
library(grid)
library(viridis)
library(dplyr)
library(pastecs)
library(janitor)
library(ggplot2)

# SETTING WORKING DIRECTORY
setwd("C:/Users/Admin/OneDrive/Desktop/da_case_studies-0.7.1")

# SETTING DATA DIRECTORY
data_dir="C:/Users/Admin/OneDrive/da_case_studies-0.7.1" #data_dir must be first defined 
data_in <- paste(data_dir,"airbnb", sep = "/")

# GETTING DATA AND SAMPLE SELECTION

hotels <- read_csv(paste(data_in,"airbnb_hackney_workfile.csv", sep = "/"))


#filtering the raw data to include only columns required for analysis.

hotel_data <- airbnb_hackney_workfile %>% 
  select(n_accommodates,n_beds,f_property_type,f_room_type,n_days_since,price,n_review_scores_rating)

summary(hotel_Data)

#based on the summary stats, we found out four columns have Na's, as follows;
# 1) n_beds, 2)n_days_since 3)price 4)n_review_scores_rating
#we are going to replace the missing values by the means of the remaining values for each column separately.


##### QUESTION A #####

#1) Replacing Na's in column n_beds with the mean of the remaining values

hotel_Data$n_beds[is.na(hotel_Data$n_beds)] <- mean(hotel_Data$n_beds,na.rm = TRUE)

#2) Replacing Na's in column n_days_since with the mean of the remaining values

hotel_Data$n_days_since[is.na(hotel_Data$n_days_since)] <- mean(hotel_Data$n_days_since,na.rm = TRUE)

#3) Replacing Na's in column price with the mean of the remaining values

hotel_Data$price[is.na(hotel_Data$price)] <- mean(hotel_Data$price,na.rm = TRUE)

#4) Replacing Na's in column n_review_scores_rating with the mean of the remaining values

hotel_Data$n_review_scores_rating[is.na(hotel_Data$n_review_scores_rating)] <- mean(hotel_Data$n_review_scores_rating,na.rm = TRUE)


##### QUESTION B #####

# HISTOGRAM OF PRICE DISTRIBUTION

hist(hotel_Data$price,
     main = "Distribution of Room Prices",
     xlab = "Prices",
     border = "green",
     col = "blue",
     breaks = 50)

# BOXPLOT TO COMPARE PRICE DISTRIBTUION BETWEEN ROOM TYPES
boxplot(price ~ f_room_type, data = hotel_Data,
        xlab = "Room Type", 
        ylab = "Price",
        main = "Price Distribution Between Room Types")
#considering price as dependent variable, conducting the multiple regression analysis.

#To do the regression we have to break the f_property-type and f_room_type into the dummy variables.

#for f-property type we have to create the two dummy variables.

hotel_Data$f_property_type_Apt <- ifelse(hotel_Data$f_property_type == "Apartment",0,1)

#for f_room_type we have to create three dummy variables
hotel_Data$f_room_type_EH <- ifelse(hotel_Data$f_room_type == "Entire home/apt",0,1)
hotel_Data$f_room_type_PR <- ifelse(hotel_Data$f_room_type == "Private room",0,1)


##### QUESTION C #####

# MULTIPLE REGRESSION WITH PRICE AS DEPENDENT VARIABLE

model_L <- lm(price ~ n_accommodates + n_beds + f_property_type_Apt + f_room_type_EH +
                f_property_type_PR + n_days_since , data = hotel_data)
print(model_L)

# Output of our model
summary(model_L)

anova(model_L)

##### QUESTION D #####

#to identify the overall model goodness of fit we will evaluate the R2 value. In our model, the R2 value is 0.521, which is considered
#to be a strong evidence for the validity of the model. Had R2 value was been close to 0 then we would have said that our model is not good. Also,
#in order to determine the over all fitness of the test F-test is the good indicator. In which if it was zero then that means there is no
#dependency of dependent variable on independent variables. Also if P-Value < 0.05 that means are model is accurate.

#to specify the significance of variable, we would consider the p values. if p value is < 0.05, then we can say variables are
#statistically significant. So in model_1 all variables are significant except f_property_type_Apt and f_room_type_PR.

#Diagnostics of regression model
par(mfrow = c(2,2))
plot(model_L)

##### QUESTION E #####
# BP Test - Heteroscedasticity Test

library(lmtest)
bptest(model_L)

#there are violations of the assumptions such as
#Non-normal; as you can see in Normal Q-Q plot the data does not follows the straight line.
#Heteroscedasticity; As the P-value in Breusch-Pagan test < 0.05.As well, Scale-Location plot is indicating a pattern.

##### QUESTION F #####

####Repeating the regression analysis with log price####

#considering price as dependent variable, conducting the multiple regression analysis

model_Llog <- lm(log(price)~n_accommodates+n_beds+f_property_type_Apt+f_room_type_EH
                +f_room_type_PR+n_days_since, data = Analysis_Data)
print(model_Llog)

#output of our model.
summary(model_Llog)

#to identify the overall model goodness of fit we will evaluate the R2 value. In our model, the R2 value is 0.6431, which is considered
#to be a strong evidence for the validity of the model. Had the R2 value been close to 0 then we would have said our model is not good. Also,
#the p-value of the F-statistic is < 0.05 (significant level) which means the model is accurate.

#to specify the significance of variable, we would consider the p values. if p value is < 0.05, then we can say variables are
#statistically significant. So in our model all variables except n_beds and f_room_type_PR are significant.

#Diagnostics of regression model
par(mfrow = c(2,2))
plot(model_Llog)

##Breusch-Pagan Test##
bptest(model_Llog)

#there are violations of the assumptions such as
#Non-Normal; as you can see in Normal Q-Q plot the data does not follows the straight line.
#Heteroskedasticity is there as Breusch-Pagan test P value is < 0.05. However, taking the log of DV have reduced the heteroskedasticity in
#he model_log. As you can see P- value has improved and scale-location graph is not showing a specific pattern.

#############
##THANK YOU##
#############

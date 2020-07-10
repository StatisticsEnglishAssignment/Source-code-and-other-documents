library(dplyr)
library(tidyverse)
library(descr)
library(ggplot2)


Housing_data<- read.csv(file.choose(),  header = T)
Housing_data$bathrooms <-round(Housing_data$bathrooms,0)
Housing_data$floors <-round(Housing_data$floors,0)
Housing_prices_filter<- filter(Housing_data, Housing_data$price<=2000000)

# code for box plots
#1-price
boxplot(Housing_prices_filter$price/1000, main="Distribution of Prices of houses",ylab ="Prices of houses ('000)",notch=TRUE, 
        col=(c("orange")))
#2-sqft_living
boxplot(Housing_prices_filter$sqft_living, main="Floor area of houses",ylab ="Area of houses ('000)",notch=TRUE, 
        col=(c("Yellow")))
#3-yr_built
boxplot(Housing_prices_filter$yr_built, main="Houses based on the year of construction",ylab ="Year constructed (In decades)",notch=TRUE, 
        col=(c("Red")))

# code for histogram
#4-prices
ggplot(data = Housing_prices_filter)+ 
  geom_histogram(mapping = aes(x = price), binwidth = 100000,fill="blue",color = "light blue") + 
  labs(title = "Distribution of houses prices",x = "Prices of houses", y = "Number of Houses")+theme_light()

#5-sqft living
ggplot(data = Housing_prices_filter)+ 
  geom_histogram(mapping = aes(x = sqft_living), binwidth = 800,fill="red", color = "yellow") + 
  labs(title = "Distribution of house sizes",x = "Area of houses", y = "Number of Houses")+theme_minimal()

#6-bedrooms
ggplot(data = Housing_prices_filter)+ 
  geom_histogram(mapping = aes(x = bedrooms), binwidth = 1, fill="dark green",color= "green") + 
  labs(title = "Total Number of bedrooms",x = "Number of Bedrooms", y = "Number of Houses")+theme_minimal()

#7-bathrooms
ggplot(data = Housing_prices_filter)+ 
  geom_histogram(mapping = aes(x = bathrooms), binwidth = 1, fill="dark green",color= "green") + 
  labs(title = "Total Number of bathrooms",x = "Number of Bathrooms", y = "Number of Houses")+theme_minimal()

#8-condition
ggplot(data = Housing_prices_filter)+ 
  geom_histogram(mapping = aes(x = condition), binwidth = 1, fill="orange",color= "black") + 
  labs(title = "Total Number of houses based on condition levels",x = "Condition level", y = "Number of Houses")+theme_minimal()

#9-grade
ggplot(data = Housing_prices_filter)+ 
  geom_histogram(mapping = aes(x = grade), binwidth = 1, fill="purple",color= "black") + 
  labs(title = "Total Number of houses based on grade",x = "Grade", y = "Number of Houses")+theme_minimal()

#10- yrbuilt
ggplot(data = Housing_prices_filter)+ 
  geom_histogram(mapping = aes(x = yr_built), binwidth = 10, fill="red",color= "black") + 
  labs(title = "Total Number of houses based on year which they were built",x = "Decade", y = "Number of Houses")+theme_minimal()

 
                                                       
#pie chart
#11- floors                                                      
slices1<- as.integer(table(Housing_data$floors)) 
lbls1<- c("One floor","Two floors", "Three floors", "Four Floors")
cols <- c("red", "orange", "grey", "black")
pie(slices1, labels=lbls1, main="Pie chart for the floors in houses", col=cols) + legend("topright")
legend("topright", c("One floor","Two floors", "Three floors", "Four Floors"), cex =0.7 ,fill=cols)


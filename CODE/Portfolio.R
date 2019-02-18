
#The code portfolio includes practice code from previous weeks. The topics covered in week 5, 6, 7, 9 and 12. 

## The first thing you will do is read or import the file. There could be many ways of
## importing the data file, but for practice purposes we will be using .csv file. 
## The package has been installed in the library earlier so now we will go ahead and write the command to import & view the file.

library(readxl)
schooldata <-read_excel("HarrisburghUni Doc's/Semester 2/506- Olga/Lecture Slides/schooldata.xlsx")
View(schooldata)

##Upon viewing it, it shows us that it has 10320 observations and 8 variables. 

##Matrices and Dataframes: Creating a sample matrix and data frame. 

x <- 10:50
y <- 60:100
z <- 110:150

##Creating a matrix with x,y,z as columns 
cbind(x,y,z)

##Creating a matrix where x,y,z are rows
rbind(x,y,z)

# Creating a matrix of integers 100:120 with 4 rows and 2 columns
## we could create a similar matrix with 2 rows and 4 columns by switching the command 
## for nrow and ncol function.

matrix(data = 100:140, nrow=5, ncol=2)

## Creating a dataframe 
test <- data.frame("index" = c(1,2,3,4,5,6,7), "school"=c("a","b","a","a","b","b","a"), "age" =c(9,10,8,9,10,8,9)) 
## We could also explore the data using some descriptive statistics.
summary(Customer_Data)
str(Customer_Data)
list(Customer_Data)

##Pre-processing some of the data from numerical to categorical 
##Others into numerical 
library(float)
fl(Customer_Data$TotalCharges)
##Now we shall replace the option of "No Internet Service" to "NO" for the column
## and replace it with 1 and 0 with Yes & No for SeniorCitizen
Customer_Data[Customer_Data == 'No internet service'] <- 'No'
Customer_Data$SeniorCitizen[Customer_Data$SeniorCitizen ==1] <- 'YES'
Customer_Data$SeniorCitizen[Customer_Data$SeniorCitizen ==0] <- 'NO'

##Now we will split the churn and non-churn using the filter function from dplyr
churn <-filter(Customer_Data, Churn == "Yes")
non_churn <-filter(Customer_Data, Churn == "No")

##Exploratory Data Analysis 
library(ggplot2)
library(scales)
library(dplyr)
library(plyr)

##Now we will built plots to analyze our data 
partner <-filter(Customer_Data, partner=="Yes")
single <-filter(Customer_Data, partner=="No")
partnership_plot <-ggplot(Customer_Data=partner, aes(x=churn)) + geom_bar(position = 'dodge', stat = 'count', fill="red") + geom_text(aes(label = paste0(round(prop.table(..count...)*100,2), '%')), 
                                                                                                                                      stat = 'count', 
                                                                                                                                      position = position_dodge(1), 
                                                                                                                                      size = 4
                                                                                                                                      vjust = 0.5) +
  theme_minimal()+ggtitle('Customer with partners')+ xlab('churn')+ ylab('clients')
  

##PLotting a histogram from customer data for the period of tenure and total chargers incurred by the customer in that period 
hist(Customer_Data, Customer_Data$tenure, Customer_Data$TotalCharges, main = "Tenure & Total Charges", xlab = "tenure")
##PLotting a histogram by churn and non-churn percentage 
data%>% 
  group_by(churn) %>% 
  summarize(n=n()) %>% 
  mutate(
    percentage = round(n / sum(n), 3),
    n = NULL
  ) %>%
  ggplot(aes(x = churn, y = percentage)) + geom_col(aes(fill = churn)) +
  theme +
  geom_text(
    aes(x = churn, y = percentage, label = paste(percentage*100, "%", sep = ""))
  )



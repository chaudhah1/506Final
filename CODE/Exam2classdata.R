library(readxl)
schooldata <- read_excel("~/HarrisburghUni Doc's/Semester 2/510-Ashby/schooldata.xlsx")
View(schooldata)

## Factoring variables ## 

schooldata$districtcode <- factor(schooldata$districtcode)
schooldata$schoolcode <- factor(schooldata$schoolcode)
schooldata$gender <- factor(schooldata$gender)
schooldata$classcode <- factor(schooldata$classcode)

## Finding Skewness & Outliers ##

install.packages("moments")
library(moments)
agostino.test(schooldata$getotal) 

install.packages("outliers")
library(outliers)
outlier(schooldata$getotal)

## Plotting graphs & using log to remove skewness ## 
plot(density(schooldata$getotal))
agostino.test(log(schooldata$getotal+20))
plot(density(schooldata$getotal+20))

## Creating a new column after taking log ## 
schooldata$getotal_1 <- log(schooldata$getotal+20)

## Building a model 
library(lmerTest)
model1 <-lmer(getotal_1 ~ classsize + age + gender + classcode + (1|districtcode/schoolcode),data= schooldata)
model1 

## Shapiro test & QQ 
shapiro.test(resid(model1))
qqnorm(resid(model1))
qqline(resid(model1))

## Summarize 
tapply(schooldata$getotal_1, schooldata$classsize, mean)
tapply(schooldata$getotal_1, schooldata$classsize, sd)


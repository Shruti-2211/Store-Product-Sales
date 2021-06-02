library(readxl)
library(funModeling)
library(Hmisc)
library(tidyverse)
library(dplyr)

#Read csv data
sales_data <- read.csv("E:/Shruti/Study/MBA/Sem-2/Analytics/R Programming/Data/sales_data_csv.csv")
View(sales_data)


#Data Analysis
data_analysis <- function (data)
{
  glimpse(data)
  df_status(data)
  freq(data)
  profiling_num(data)
  plot_num(data)
  describe(data)
}
data_analysis(sales_data)

#Linear Regreassion model between SKU.01 (dependent variable) & SKU.03 (independent variable)
linear.reg.relation <- lm(sales_data$SKU.01 ~ sales_data$SKU.02)
print(linear.reg.relation)
print(summary(linear.reg.relation))

#Discontinued Product <- Product which has lesser sales

SKU_data <- select(sales_data, 3:23)
SKU_data <- colSums(SKU_data)
print(SKU_data)

#Identify which two product should be stopped
sort(SKU_data)[1:2]


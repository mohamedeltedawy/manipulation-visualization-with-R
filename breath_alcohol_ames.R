library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)


#import and read data set
ba_data <- read_csv("breath_alcohol_ames.csv")
glimpse(ba_data)

#count by year to calc tests were administered in each year 
ba_year <- ba_data %>% count(year)
glimpse(ba_year)

#count by location which department administers more breathalyzer tests
pds <- ba_data %>% count(location)
glimpse(pds)

#count and visul to calc the number of tests by hour
hourly <- ba_data %>% count(hour) %>% arrange(desc(hour))
glimpse(hourly)
ggplot(hourly , aes(x= hour , weight = n)) + 
  geom_bar()

#determine which time of the year has the most breathalyzer tests
monthly <- ba_data %>% count(month) %>% arrange(desc(month))
#make month a factor
monthly$month <- as.factor(monthly$month)

ggplot(monthly, aes(x = month ,weight =n))+
  geom_bar()

#compare results for men and women
ba_data %>% count(gender)

#clean data from NA in gender 
clean_gender <- ba_data %>% filter(!is.na(gender)) 

#mean tests 
mean_bas <- clean_gender %>% mutate(meanRes = (Res1 + Res2) /2)

ggplot(mean_bas , aes(x = gender , y = meanRes))+
  geom_boxplot()

#determine the percent of the breathalyzer tests above the legal limit
#duis = driving under the influence

duis <- ba_data %>% filter(Res1 > 0.08 | Res2 > 0.08)

#p_dui proportion of all tests that would have resulted in a DUI
p_dui <- nrow(duis) / nrow(ba_data)

#determine the week in the year each test occurred
ba_data <- ba_data %>% mutate(date = ymd(paste(year, month,day)))

#create week variable
ba_data <- ba_data %>% mutate(week = week(date))
head(ba_data , n = 10)


#create time series plot to compare weeks across years
weekly <- ba_data %>% count(week , year)
weekly %>% ungroup()

weekly %>% mutate(year = as.factor(year))

head(weekly , n = 10)
# create the time series plot with one line for each year
ggplot(weekly, aes(x = week, y = n)) + 
  geom_line(aes(x = week, y = n , color = year) ) + 
  geom_point(aes(color = year)) +  # included to make the plot more readable 
  scale_x_continuous(breaks = seq(0,52,2))  # to make the x-axis more readable




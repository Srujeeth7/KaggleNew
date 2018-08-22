setwd('C:\\Users\\chink\\Desktop\\Data Science\\Kaggle\\Avacado')

library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)

avocado <- read.csv('avocado.csv',stringsAsFactors = F)

avocado$Date <- as.POSIXct(avocado$Date)

avocado %>% group_by(Date) %>%
  summarise(avgprice = mean(AveragePrice)) %>%
  ggplot(aes(Date, avgprice)) + geom_line() 


avocado %>% group_by(region) %>% 
  summarise(mean_price = mean(AveragePrice)) %>% 
  arrange(desc(mean_price)) %>% head(10) %>%
  ggplot(aes(y=mean_price,x=region,fill=region)) + 
  geom_bar(stat='identity') +
  labs(title = 'Top 10 Performing Markets over the years',x='Regions',y='Mean Average Price') +
  theme(axis.text.x = element_text(angle=90))


# Total volume over the years

avocado %>% group_by(year, region) %>% summarise(avg_volume = mean(Total.Volume)) %>%
  arrange(desc(avg_volume)) %>% slice(1:5) %>% 
  arrange(desc(year, avg_volume)) %>%
  ggplot(aes(year, avg_volume, fill = region)) + 
  geom_bar(stat = 'identity') +
  facet_grid(year ~ region) + scale_y_continuous() +
  labs(title = 'Avg volumes of Top 5 regions over the years' )

## Conventional vs Organic over the years

avocado %>% group_by(type,year) %>% summarise(avg_price = mean(AveragePrice)) %>% 
  ggplot(aes(year,avg_price,color=type)) + geom_line()
  

most_volatile <- avocado %>% group_by(region,type,year) %>% summarise(sd=sd(AveragePrice),min_pr = min(AveragePrice),max_pr =max(AveragePrice),spread=max_pr-min_pr) %>% arrange(desc(sd))

most_volatile_org <-
  most_volatile %>% filter(type == 'organic') %>% mutate(
    sd = dollar(sd),
    min_pr = dollar(min_pr),
    max_pr = dollar(max_pr),
    spread = dollar(spread)
  )
most_volatile_con <-
  most_volatile %>% filter(type == 'conventional') %>% mutate(
    sd = dollar(sd),
    min_pr = dollar(min_pr),
    max_pr = dollar(max_pr),
    spread = dollar(spread)
  )

average_price <- avocado %>% filter(region == 'Chicago') %>% mutate(qtr = quarter(Date,with_year='F'))
date_ranges <- paste(min(avocado$Date) , '-',max(avocado$Date))



ggplot(average_price,
       aes(as.factor(qtr), AveragePrice, group = as.factor(qtr)),
       color = 'type') +
  geom_boxplot(
    fill = 'white',
    alpha = .3,
    outlier.shape = NA,
    color = 'red',
    outlier.colour = 'blue'
  ) +
  geom_jitter(stat = 'identity', width = 0.25, size = 1.8) + theme_bw() +
  facet_grid(type ~ year, scales = 'free') +
  scale_y_continuous(breaks = seq(0, 3, .2)) + scale_x_discrete(drop = F) +
  labs(
    title = 'Avg price of single avocado : Chicago',
    subtitle = paste('By Quarter of the year |', ' Dates from ', date_ranges),
    x='Quarter of the year',
    y= 'Average Price'
  )




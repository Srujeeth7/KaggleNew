setwd('C:/Users/chink/Desktop/Data Science/Kaggle/Apple_Apps/')

Apple <- read.csv('AppleStore.csv')
Apple_desc <- read.csv('appleStore_description.csv')

library(tidyverse)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(corrplot)

AppData <- Apple %>% left_join(Apple_desc,by=c('id','id'))
AppData <- AppData %>% select(-c(track_name.y,size_bytes.y))

rm(list=c('Apple','Apple_desc'))

## Popular Genres

AppData %>% group_by(prime_genre) %>% summarise(popular_app = mean(rating_count_tot*user_rating)) %>% 
  arrange(desc(popular_app)) %>% 
  ggplot(aes(reorder(prime_genre,popular_app),popular_app,fill=prime_genre)) + geom_bar(stat='identity') + coord_flip() +
  labs(title = 'Top Genres in AppStore', x = 'Genres',y = 'Popularity (Rating count vs user rating)') + 
  scale_y_continuous(breaks= seq(0,180000,20000))



# Top genres overall

temp <-
  AppData %>% group_by(prime_genre) %>%  summarise(count=length(prime_genre)) %>%arrange(desc(count))

wordcloud(
  words = temp$prime_genre,
  max.words = 23,
  min.freq = 100,
  freq =temp$count,
  scale=c(6,1),
  random.color = F,
  random.order = F,
  rot.per = 0.35,
  colors = brewer.pal(20,'Dark2')
)
text(x=.5,y=1.0,'Genres Frequency',cex =2,offset=.7)


## Apps Frequency

AppData %>% group_by(prime_genre) %>% summarise(count=n()) %>% arrange(desc(count)) %>% 
  ggplot(aes(reorder(prime_genre,desc(count)),count,fill=prime_genre)) + geom_bar(stat = 'identity',width = 0.4) + 
  theme(axis.text.x = element_text(angle=90),legend.position = 'None')+
  scale_y_continuous(breaks = seq(0,4000,300))  + labs(title = 'Frequency of Apps vs Genres',x = 'Genres',y='Frequency of Apps')

## Free vs Paid 

table(AppData$price)

AppData$Type[AppData$price==0] <- 'Free'
AppData$Type[AppData$price>0] <- 'Paid'

AppData %>% group_by(Type) %>% summarise(count = n()) %>% ggplot(aes(Type,count,fill=Type)) + geom_bar(stat='identity') +
  labs(title= 'Free vs Paid Apps comparision in Apple Store',subtitle = paste('Free Apps Comprises about ',round(nrow(AppData[AppData$Type=='Free',])/nrow(AppData)*100,2),'%',sep = ' '))

## Apps count vs content rating 

AppData %>% group_by(cont_rating,prime_genre) %>% summarise(count = n(),dis = unique(cont_rating)) %>%
  ggplot(aes(reorder(prime_genre,count),count,fill=cont_rating)) + geom_bar(stat='identity')+ 
  labs(title = 'Frequency of Apps',x='Prime genre') + facet_grid(~cont_rating,drop = T,space = "free") +coord_flip()

# Top App in each Genre

temp_top_app <- data.frame(AppData %>% group_by(prime_genre) %>% arrange(desc(rating_count_tot)) %>% 
  select(track_name.x,rating_count_tot) %>% slice(1:1) %>%
  arrange(desc(rating_count_tot))) 

temp_top_app$prime_genre <- as.character(temp_top_app$prime_genre)
temp_top_app$track_name.x <- as.character(temp_top_app$track_name.x)


# wordcloud(
#   words = temp_top_app$track_name.x,
#   freq = temp_top_app$rating_count_tot,
#   min.freq = 10000,
#   max.words = 20,
#   random.order = F,
#   random.color = F,
#   colors = brewer.pal(8,'Dark2'),
#   scale=c(5,0.3)
# )

ggplot(temp_top_app,aes(prime_genre,rating_count_tot,color=track_name.x)) + geom_point(stat='identity',cex=2 )+ 
  theme(legend.position = 'None') + geom_text(aes(label=track_name.x),hjust=-0.1,vjust=-0.15) + coord_flip() +
  scale_y_continuous(breaks = seq(0,30000000,500000)) + labs(title='Top App in Each Genre',x='Genre', y= 'Ratings count')

## Ratings Distributions

AppData %>% group_by(user_rating) %>% summarise(count =n()) %>%
  ggplot(aes(user_rating,count,fill=user_rating)) + geom_bar(stat='identity') +
  scale_x_continuous(breaks = seq(0,5,0.5)) + scale_y_continuous(breaks=seq(0,2700,300)) +
  labs(title = 'Ratings vs Frequency of Apps' , x= 'user ratings',y='Frequency')

## Free vs Paid apps ratings distribution

AppData %>% group_by(Type,user_rating) %>% summarise(count = n()) %>%
  ggplot(aes(user_rating,count,fill=Type)) + geom_bar(stat='identity') + 
  facet_grid(Type~.) + scale_x_continuous(breaks=seq(0,5,0.5)) + scale_y_continuous(breaks = seq(0,1500,300))
  
## Correlation 

corx <- cor(select_if(AppData[,5:19],is.numeric))

corrplot(corx)

## Language supported vs rating distribution

AppData %>% group_by(lang.num) %>%  summarise(mean_rating=mean(user_rating)) %>% 
  ggplot(aes(lang.num,mean_rating)) + geom_line()


## cont rating vs price

AppData %>% group_by(cont_rating) %>% filter(price <= 40, price > 0) %>%
  ggplot(aes(cont_rating,price,color=cont_rating)) +geom_boxplot() + theme(legend.position = 'None')

table(AppData$cont_rating)


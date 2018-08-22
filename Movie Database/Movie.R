Sys.setenv(JAVA_HOME = 'C:/Program Files/Java/jre1.8.0_161')

rm(list = ls())
setwd("C:/Users/chink/Desktop/Edureka/Kaggle/Movie Database/")

library(plyr) #data manipulation
library(tidyverse)# data manipulation
library(formattable)# table
library(splitstackshape) # split columns
library(jsonlite) #JSON format
library(wordcloud) #wordcloud
library(RColorBrewer) # Color Theme
library(ggthemes) #Themes for plot
library(tm) # Sentiment Analysis
library(RSentiment) # Sentiment Analysis
library(zoo) # Time
library(stringr)

movie <- read.csv("tmdb_5000_movies.csv", stringsAsFactors = F)
credits <- read.csv("tmdb_5000_credits.csv", stringsAsFactors = F)

glimpse(movie)
summary(movie)

genredf <-
  movie %>% filter(nchar(movie$genres) > 2) %>% mutate(js = lapply(genres, fromJSON)) %>% unnest(js) %>% select(id, title, genre =
                                                                                                                  name)

slice(genredf)

temp <-
  genredf %>% group_by(genre) %>% summarise(count = length(genre)) %>% arrange(desc(count))

wordcloud(
  words = temp$genre,
  freq = temp$count,
  min.freq = 100,
  max.words = 20,
  random.color = F,
  random.order = F,
  rot.per = 0.35,
  colors = brewer.pal(20, 'Dark2'),
  scale = c(5, 0.2)
)


# Movie with Highest Budget

budget_mv <-
  movie %>% select(original_title, budget) %>% drop_na(original_title) %>% arrange(desc(budget)) %>% head(10)

ggplot(budget_mv, aes(original_title, budget, fill = original_title)) + geom_bar(stat =
                                                                                   'identity') + scale_y_continuous(labels = scales::comma) +
  theme(axis.text = element_text(angle = 90))

# Highest Grossing Movies

Gross_mv <-
  movie %>% select(original_title, revenue) %>% drop_na(original_title) %>% arrange(desc(revenue)) %>% head(10)

ggplot(Gross_mv, aes(original_title, revenue, fill = original_title)) + geom_histogram(stat = 'identity') +
  theme(axis.text = element_text(angle = 90)) + scale_y_continuous(labels =
                                                                     scales::comma)

## Most Profitable Company


company <-
  movie %>% filter(nchar(production_companies) > 2) %>% mutate(profit = lapply(production_companies, fromJSON)) %>% unnest(profit) %>% select(Production = name , original_title, revenue, budget) %>% group_by(Production) %>% summarise(profit = sum(as.numeric(revenue -
                                                                                                                                                                                                                                                                    budget))) %>% arrange(desc(profit)) %>% head(10)



ggplot(company, aes(reorder(Production, profit), profit, fill = Production)) + geom_bar(stat = 'identity') + scale_y_continuous(labels = scales::comma) + theme(axis.text = element_text(angle =
                                                                                                                                                                                           90))

## Exploring the tagline

corpus <- Corpus(VectorSource(list(movie$tagline)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
dtm_tag <-
  DocumentTermMatrix(VCorpus(VectorSource(corpus[1]$content)))
freq_tag <- colSums(as.matrix(dtm_tag))

sent_tag <-
  calculate_sentiment(names(freq_tag)) %>% cbind(as.data.frame(freq_tag))

positive_tag <- sent_tag[sent_tag$sentiment == "Positive", ]
negative_tag <- sent_tag[sent_tag$sentiment == "Negative", ]

cat(
  'Postive sentiment',
  sum(positive_tag$freq_tag),
  "Negative Sentiment",
  sum(negative_tag$freq_tag),
  sep = "\n"
)

par(mfrow = c(1, 2))

wordcloud(
  positive_tag$text,
  positive_tag$freq_tag,
  min.freq = 10,
  rot.per = 0,
  max.words = 50,
  random.order = F,
  random.color = F
  
)
text(x = 0.5, y = 1.05, "Postive")

wordcloud(
  negative_tag$text,
  negative_tag$freq_tag,
  min.freq = 10,
  random.order = F,
  random.color = F,
  max.words = 50
)
text(x = 0.5, y = 1.05, "Negative")

## Movies year by year analysis

class(movie$release_date)

movie$release_date <- as.Date(movie$release_date, "%Y-%m-%d")

year_analysis <-
  movie %>% drop_na(original_title) %>% mutate(Year = as.factor(format(release_date, "%Y", trim =
                                                                         F)),
                                               Date = as.factor(format(release_date, "%d", trim = F)),
                                               Month = month.abb[as.factor(format(release_date, "%m", trim = F))]) %>% select(original_title, Year, Month, Date , vote_average) %>% group_by(Month) %>% drop_na(Month) %>% summarise(count =
                                                                                                                                                                                                                                       n())

ggplot(year_analysis, aes(reorder(Month, count), count, fill = Month)) + geom_bar(stat =
                                                                                    'identity') + coord_flip() + geom_label(aes(label = count)) + theme(legend.position = 'None') + labs(x =
                                                                                                                                                                                           "Months", y = "Number of movies released")


movie <-
  movie %>% drop_na(original_title) %>% mutate(Year = as.factor(format(release_date, "%Y", trim =
                                                                         F)),
                                               Date = as.factor(format(release_date, "%d", trim = F)),
                                               Month = month.abb[as.factor(format(release_date, "%m", trim = F))])


movie$Normalized_rating <- movie$vote_average * movie$vote_count
movie$Normalized_rating <-
  (movie$Normalized_rating - mean(movie$Normalized_rating)) / sd(movie$Normalized_rating)

movie %>% drop_na(Month) %>%  group_by(Month)  %>%  ggplot(aes(Month, Normalized_rating, fill =
                                                                 Month)) + geom_boxplot(na.rm = T, outlier.colour = 'red') + coord_flip() +
  theme(legend.position = 'None')

## Artist Comparision

#View(credits)

Credits_cast <-
  credits %>% filter(nchar(cast) > 2) %>% mutate(js = lapply(cast, fromJSON)) %>% unnest(js) %>% select(title, name)

Credits_cast %>% group_by(name) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(10) %>% ggplot(aes(name, count, fill =
                                                                                                                  name)) + geom_bar(stat = 'identity') + coord_flip() + geom_label(aes(label =
                                                                                                                                                                                         count)) + theme(legend.position = 'None')

# Budget Revenue for Actor

db = movie %>% left_join(credits, by = c("id" = "movie_id"))
db_credit <-
  db %>% filter(nchar(cast) > 2) %>% mutate(js = lapply(cast, fromJSON)) %>% unnest(js)


revenue <- function(df, col_name) {
  temp_df <-
    df %>% filter(name == col_name) %>% arrange(desc(revenue)) %>% head(10)
  df_plot <-
    ggplot(temp_df, aes(original_title, revenue, fill = original_title)) +
    geom_bar(stat = 'identity') + theme(legend.position = 'None')  + labs(
      title = paste(col_name, "'s Top 10 Movies", sep = ""),
      x
      =
        'Movies',
      y = 'Revenue'
    ) + theme(axis.text.x  = element_text(angle = 90),
              plot.title = element_text(hjust = 0.5, size = 15)) + scale_y_continuous(labels = scales::comma)
  print(df_plot)
}

revenue(db_credit, 'Tom Cruise')

budget <- function(df, col_names) {
  temp_df <-
    df %>% filter(name == col_names) %>% arrange(desc(budget)) %>% head(10)
  df_plot <-
    ggplot(temp_df, aes(original_title, budget , fill = original_title)) +
    geom_bar(stat = 'identity') + theme(legend.position = 'None') + labs(
      title = paste(col_names, "'s Top 10 budgeted Films", sep = ""),
      y = "Budget",
      x = "Movies"
    ) + theme(plot.title = element_text(hjust = 0.5, size = 15),
              axis.text.x = element_text(angle = 90)) + scale_y_continuous(labels = scales::comma)
  print(df_plot)
}

budget(db_credit, 'Tom Cruise')


## Top 10 box office stars 

top_stars <- db_credit %>% filter(order==0) %>%group_by(name) %>% summarise(sum_revenue= sum(revenue)) %>% arrange(desc(sum_revenue)) %>% head(10)

ggplot(top_stars, aes(reorder(name,sum_revenue), sum_revenue, fill = name)) +
  geom_bar(stat =  'identity') + theme(axis.text.x = element_text(angle = 90),
                                       legend.position = 'None',plot.title = element_text(hjust=0.5,size = 15)) +
  labs(title = 'Top 10 Actors Revenue', x = 'Actors', y = 'Revenue') +
  scale_y_continuous(labels = scales::comma)

## Budget vs Popularity

temp <- db_credit %>% select(budget,popularity) %>% distinct()

ggplot(temp,aes(budget,popularity))+
  stat_bin_hex(bins = 15)+
  scale_fill_distiller(palette = "Spectral")+
  stat_smooth(method = lm,color="red",size=2)+scale_x_continuous(labels=scales::comma)

popularity_cor <-cor(data.frame(temp$budget,temp$popularity))

# For Tom Cruise 

Tom <- db_credit %>% filter(name=='Tom Cruise') %>% subset(!duplicated(original_title)) %>% arrange(desc(budget)) %>% head(20) %>% select(budget,original_title,popularity)


ggplot(Tom, aes(budget, popularity, col = as.factor(original_title))) +
  geom_point() + theme(axis.text.x = element_text(angle = 90),
                       legend.position = 'bottom') +
  stat_smooth(method = "lm",color='red',size=2) +scale_x_continuous(labels=scales::comma)


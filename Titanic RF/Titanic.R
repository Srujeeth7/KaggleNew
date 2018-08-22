setwd("C:\\Users\\chink\\Desktop\\Edureka\\Kaggle\\Titanic")

library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
library("randomForest")

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

full <- bind_rows(train, test)

str(full)

################## Feature Engineering ################################

# To check and remove the title from the passengers name
full$Title <- gsub('(.*,)|\\s|(\\..*)', '', full$Name)

table(full$Title)
table(full$Sex, full$Title)

# Now assign the rare values as one title others as it is for ex : miss,mrs,mr,master are relavent in count compared to others

rare_title <-
  c(
    "Capt",
    "Col",
    "Don",
    "Dona",
    "Dr",
    "Jonkheer",
    "Lady",
    "Major",
    "Sir",
    "theCountess",
    "Rev"
  )

full$Title[full$Title == "Mlle"] <- 'Miss'
full$Title[full$Title == "Mme"] <- 'Mrs'
full$Title[full$Title == "Ms"] <- 'Miss'
full$Title[full$Title %in% rare_title] <- 'Rare Title'

table(full$Sex, full$Title)

full$surname <-
  sapply(full$Name, function(x)
    strsplit(x, split = '[,.]')[[1]][1])

# or we can use the below
full$surname <- gsub('(,.*)', '', full$Name)

# Grouping the family members

full$Fsize <- full$SibSp + full$Parch + 1

full$family <- paste(full$surname, full$Fsize, sep = "_")

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = 'count', position = 'dodge') +
  scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme()

# We can observe here that singles and family size greater than 5 survival rate is less 
# We can group the families by survival

full$FsizeD[full$Fsize==1] <- 'Single'
full$FsizeD[full$Fsize > 1 & full$Fsize <5 ] <- 'Small'
full$FsizeD[full$Fsize > 4 ] <- 'Large'

mosaicplot(table(full$FsizeD,full$Survived),shade = T,main = "Family Size by Survival")

#################### Missing Values ####################

#Embarked has missing values for passenger ID 62 and 830

embark_fare <- full %>% filter(PassengerId != 62 & PassengerId !=830)

ggplot(embark_fare, aes(
  x = Embarked,
  y = Fare,
  fill = factor(Pclass)
)) + geom_boxplot() + geom_hline(aes(yintercept = 80), color = 'red', linetype = 'dashed') +
  scale_y_continuous(labels = dollar_format()) + theme_few()

full[c(62,830),]

# So on comparision we can replace the values with C

full[c(62,830),'Embarked'] <- 'C'

#Passenger ID 1044 fare is null

full[full$PassengerId==1044,] # is from class 3 and embarked from S


ggplot(full[full$Pclass == 3 &
              full$Embarked == 'S', ], aes(x = Fare)) + geom_density(fill = "blue", alpha = 0.1) +
  geom_vline(
    aes(xintercept = median(Fare, na.rm = T)),
    colour = "red",
    linetype = "dashed",
    lwd = 1
  ) + scale_x_continuous(breaks = seq(0, 100, by = 8), labels = dollar_format()) +
  theme_few()

full$Fare[full$PassengerId==1044] <- 8.05

## Predictive Imputation

sum(is.na(full$Age))

#install.packages("mice")
library("mice")

factor_var <- c('PassengerId','Pclass','Sex','Embarked','Title','surname','family','FsizeD')

full[factor_var] <- lapply(full[factor_var],function(x) as.factor(x))

set.seed(7)

mice_mod <- mice(full[,!names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')],method = "rf")
mice_output <- complete(mice_mod)

# Plot original vs mice output 

par(mfrow = c(1,2))

hist(full$Age,freq= F,main = "Age : Original Data" ,col = 'darkgreen',ylim = c(0,0.04))
hist(mice_output$Age,freq= F,main = "Age : Original Data" ,col = 'lightgreen',ylim = c(0,0.04))

full$Age <- mice_output$Age

ggplot(full[1:891,], aes(Age , fill = factor(Survived))) + geom_histogram() + facet_grid(. ~
                                                                                           Sex) + theme_few()
full$Ischild[full$Age < 18 ] <- 'Child' 
full$Ischild[full$Age >= 18 ] <- 'Adult' 

table(full$Ischild,full$Survived)

full$Mother <- "No"
full$Mother[full$Sex == "female" & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Yes'

full$Mother <- as.factor(full$Mother)
full$Ischild <- as.factor(full$Ischild)

table(full$Mother,full$Survived)

md.pattern(full)

train <- full[1:891,]
test <- full[892:1309,]

set.seed(67)

rf_model <- randomForest(factor(Survived)~Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Ischild + Mother , data = train)

plot(rf_model,ylim=c(0,0.36))
legend('topright',colnames(rf_model$err.rate),col=1:3,fill=1:3)

importance <- importance(rf_model)
var_importance <-data.frame(variables =row.names(importance),Importance =round(importance[,'MeanDecreaseGini'],2))

rank_importance <- var_importance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rank_importance, aes(x = reorder(variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

prediction <- predict(rf_model,test)

solution <-data.frame(PassengerId = test$PassengerId,Survived = prediction)

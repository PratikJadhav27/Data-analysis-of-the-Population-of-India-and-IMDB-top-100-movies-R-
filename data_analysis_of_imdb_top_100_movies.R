library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


# Load the data
library(readr)
movies <- read_csv("movies - movies.csv")
names(movies)

# Split the genre column into separate columns
movies_01 <- movies %>%
  separate(genre, c("genre_1", "genre_2", "genre_3"), sep = ", ")

view(movies_01)


#Numeric Summary
summary(movies)
table(movies_01$movie_name)    

table(movies_01$movie_name)
?dim

head(movies_01)
tail(movies_01)
str(movies_01)

median(movies_01$imdb_rating)
mean(movies_01$imdb_rating)
min(movies_01$imdb_rating)
range(movies_01$imdb_rating)
var(movies_01$imdb_rating)

quantile(movies_01$imdb_rating,0.5)
quantile(movies_01$imdb_rating,0.25)
quantile(movies_01$imdb_rating,0.75)

quantile(movies_01$imdb_rating,0.5)
quantile(movies_01$imdb_rating,0.25)
quantile(movies_01$imdb_rating,0.75)

IQR(movies_01$imdb_rating)

1:20
seq(1,20,2)

quantile(movies_01$imdb_rating,0.25)
seq(0,1,0.25)

quantile(movies_01$imdb_rating, seq(0,1,0.25))
quantile(movies_01$imdb_rating, seq(0,1,0.05))
quantile(movies_01$imdb_rating, seq(0,1,0.25),1)
round(quantile(movies_01$imdb_rating, seq(0,1,0.25)),1)
round(quantile(movies_01$imdb_rating, seq(0,1,0.05)),1)



#Data Visualization of one variable
#Histogram of imdb rating

hist(movies$imdb_rating,
     breaks = 15,
     main = "Histogram of IMDB Rating",
     xlab = "IMDB Rating",
     col = "steelblue")


#bar plot of genre

barplot_movies <- table(movies_01$genre_1)  
barplot(barplot_movies, main = "Movie Genre Distribution", 
        xlab = "Genre", ylab = "Frequency")
plot(barplot_movies)  


# Data Visualization for two variables

#Duration Vs. Year
g1<-ggplot(data= movies_01,
           aes(x=year_of_release,y = run_time_in_min))
g1

g1+geom_point()

g1+geom_point(alpha= 0)
g1+geom_point(alpha= 1, color = 'steelblue')

g1+geom_boxplot(alpha= 1)
g1+geom_boxplot(alpha= 1, color = 'red')

g1+geom_jitter(alpha = 0.9, color = "green")
g1+geom_boxplot(alpha = 0.9)+geom_jitter(alpha = 0.8)


#genre vs imdb rating
g2<-ggplot(data= movies_01,
           aes(x= genre_1,y = imdb_rating))
g2

g2+geom_point()

g2+geom_point(alpha= 0)
g2+geom_point(alpha= 1, color = 'steelblue')

g2+geom_boxplot(alpha= 1)
g2+geom_boxplot(alpha= 1, color = 'red')

g2+geom_jitter(alpha = 0.9, color = "green")
g2+geom_boxplot(alpha = 0.9)+geom_jitter(alpha = 0.8)


#imdb rating vs. votes
g3<-ggplot(data= movies_01,
           aes(x= imdb_rating,y = votes))
g3

g3+geom_point()

g3+geom_point(alpha= 0)
g3+geom_point(alpha= 1, color = 'steelblue')

g3+geom_boxplot(alpha= 1)
g3+geom_boxplot(alpha= 1, color = 'red')

g3+geom_jitter(alpha = 0.9, color = "green")
g3+geom_boxplot(alpha = 0.9)+geom_jitter(alpha = 0.8)

#Data Visualization for three and three plus variables

ggplot(movies_01, aes(x =imdb_rating , y = gross_total_in_million_dollars/10^5)) +
  geom_point(aes(colour = genre_1)) + facet_wrap(~genre_1)+
  xlab("IMDB Rating") +
  ylab("Budget") +
  ggtitle("IMDB Rating Vs. Budget")

ggplot(movies_01, aes(x =imdb_rating , y = gross_total_in_million_dollars/10^5)) +
  geom_point(aes(colour = category)) + facet_wrap(~genre_1)+
  xlab("IMDB Rating") +
  ylab("Budget") +
  ggtitle("IMDB Rating Vs. Budget")






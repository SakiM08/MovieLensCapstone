# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# If using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Number of rows

nrow(edx)

# Number of columns

ncol(edx)

# Column names

colnames(edx)

# First 6 rows 

head(edx)

# Summary of the dataset

summary(edx)

# Loading necessary packages

library(ggplot2)
library(tidyr)
library(dplyr)
library(lattice)
library(grid)
library(lubridate)
library(ggthemes)
library(gridExtra)
library(scales)

# Ratings Frequency

ggplot(edx, aes(x = as.vector(rating))) + geom_histogram(binwidth = 0.25, color = "black", fill = "gray") + 
  scale_y_continuous(breaks = c(500000, 1000000, 1500000, 2000000, 2500000), name = "Count") + 
  scale_x_continuous(breaks = c(.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), name = "Rating") + ggtitle("Rating Frequency") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) 

# Top rated genres

top10genres <- edx %>% select(genres, rating) %>% mutate(genres = as.factor(genres)) %>% group_by(genres) %>% 
  summarize(NumberOfRatings = n()) %>% filter(NumberOfRatings >= 135000)

top10genres %>% ggplot(aes(genres, NumberOfRatings, fill = genres)) + 
  geom_bar(stat = "identity", width = 0.5, color = "black") + 
  ggtitle("Top 10 most rated genres") + scale_x_discrete(name = "Genre") + 
  scale_y_continuous(name = "Rating Count", breaks = c(100000, 200000, 300000, 400000, 500000, 600000, 700000), 
                     labels = comma_format(big.mark = ",")) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
  scale_fill_discrete(name = "Genres") 

# Bar Plot of the top 5 rated movies (Filtered to have atleast 100 ratings)

top5movies <- edx %>% group_by(title, movieId) %>% summarize(AvgRating = mean(rating), NumberOfRatings = n()) %>% 
  arrange(desc(AvgRating)) %>% filter(NumberOfRatings >= 100)


top5ratedmovies <- head(top5movies, 5)

plottop5ratedmovies <-top5ratedmovies %>% ggplot(aes(reorder(title, -AvgRating), AvgRating, fill = title)) + 
  geom_bar(stat = "identity", width = 0.75, color = "black", alpha = 0.6) + ggtitle("Avg Rating for the Top 5 Rated Movies") + 
  scale_x_discrete(name = "Movie Title") + scale_y_continuous(name = "Average Rating", breaks = c(0, 1, 2, 3, 4, 5)) + 
  geom_text(aes(label = round(AvgRating, 2)), vjust = 2) + 
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) + 
  scale_fill_discrete(name = "Movie Title")


# Bar plot of the bottom 5 rated movies (Filtered to have atleast 300  ratings)

bottom5movies <- edx %>% group_by(title, movieId) %>% summarize(AvgRating = mean(rating), NumberOfRatings = n()) %>% 
  arrange(AvgRating) %>% filter(NumberOfRatings >= 300) 

bottom5ratedmovies <- head(bottom5movies, 5)

plotbottom5ratedmovies <- bottom5ratedmovies %>% ggplot(aes(reorder(title, AvgRating), AvgRating, fill = title)) + 
  geom_bar(stat = "identity", width = 0.75, color = "black", alpha = 0.6) + ggtitle("Avg Rating for the Bottom 5 Rated Movies") + 
  scale_x_discrete(name = "Movie Title") + geom_text(aes(label = round(AvgRating, 2)), vjust = -0.5) + 
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) + scale_fill_discrete(name = "Movie Title") + 
  ylim(0, 4) + ylab("Average Rating")

# Plots for Top 5 and Bottom 5 Rated movies side by side

plottop5ratedmovies

plotbottom5ratedmovies

# Box Plot for movies with the highest number of ratings

bptop5 <- edx %>% filter(title %in% c("Pulp Fiction (1994)", "Forrest Gump (1994)", 
                                      "Silence of the Lambs, The (1991)", "Jurassic Park (1993)", 
                                      "Shawshank Redemption, The (1994)")) %>% select(title, rating)

bptop5 %>% group_by(title) %>% ggplot(aes(reorder(title, rating), rating, fill = title)) + 
  geom_boxplot(width = 0.75, color = "black", alpha = 0.5) + coord_flip()  + scale_y_continuous(name = "Rating") + 
  scale_x_discrete(name = "Movie Title") + scale_fill_discrete(name = "Movie Title") + ggtitle("Boxplot for Top 5 Rated Movies ") + 
  theme(plot.title = element_text(hjust = 0.5))

# Number Of Ratings Given Throughout The Years

library(stringr)

# Mutate edx datatset to include column year

edxwyear <- edx %>% mutate(yearId = as.numeric(str_sub(title, -5, -2)))

edxwyear %>% select(yearId, rating) %>% mutate(rating = as.numeric(rating)) %>% 
  group_by(yearId, rating) %>% summarize(rating = n()) %>% filter(yearId >= 1920) %>% 
  ggplot(aes(x = yearId, y = rating, color = yearId)) + geom_line() + 
  scale_x_continuous(breaks = c(1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010), name = "Year") + 
  ylab("Number of Ratings") + ggtitle("Number of Ratings Per Year") + theme_classic() + 
  scale_color_continuous(name = "Year") 

# Genre popularity throughout the years (Used random sample generator to choose 8 genres)

samplegenres <- sample(edxwyear$genres, replace = FALSE, 8)

print("The 8 genres are Adventure, Action, Comedy, Drama, Fantatsy, Horror, Romance, Sci-Fi")

edxwyear %>% select(yearId, genres) %>% mutate(genres = as.character(genres)) %>% 
  group_by(yearId, genres) %>% 
  filter(genres %in% c("Adventure", "Action", "Comedy", "Drama", "Fantasy", "Horror", "Romance", "Sci-Fi")) %>% 
  summarize(count = n()) %>% filter(yearId >= 1920) %>% ggplot(aes(x = yearId, y = count, color = genres)) + 
  geom_line(alpha = .9) + theme_economist() + scale_color_discrete(name = "Genre") + ylab("Number of Ratings") + 
  xlab("Year") + ggtitle("Genre Popularity Through the Years")



# Now lets make a recommendation for 3 random users based on their past viewing preferences

sampleusers <- sample(edx$userId, replace = FALSE, 3)

print("The 3 randomly chosen userIds are 28291, 58469, 21149")


user1 <- edx %>% select(userId, movieId, rating, title, genres) %>% filter(userId == 28291)

user1 %>% select(genres, rating) %>% group_by(genres) %>% summarize(NumberOfRatings = n(), AvgRating = mean(rating)) %>% 
  arrange(desc(NumberOfRatings))

user1table <- user1 %>% select(movieId, title, rating, genres) %>% arrange(desc(rating))
head(user1table, 10)

user2 <- edx %>% select(userId, movieId, rating, title, genres) %>% filter(userId == 58469)

user2 %>% select(genres, rating) %>% group_by(genres) %>% summarize(NumberOfRatings = n(), AvgRating = mean(rating)) %>% 
  arrange(desc(NumberOfRatings))

user2table <- user2 %>% select(movieId, title, rating, genres) %>% arrange(desc(rating))
head(user2table)

user3 <- edx %>% select(userId, movieId, rating, title, genres) %>% filter(userId == 21149)

user3 %>% select(genres, rating) %>% group_by(genres) %>% summarize(NumberOfRatings = n(), AvgRating = mean(rating)) %>% 
  arrange(desc(NumberOfRatings))



# Defining the RMSE function


rmse <- function(validation, y_hat){
  sqrt(mean((validation-y_hat)^2))
}


# Analysis/Accuracy of the report's models will now be assesed.

# Model 1: Ratings predicted/approximated by the mean ratings in the edx dataset

mu <- mean(edx$rating)
mu

model1 <- rmse(validation$rating, mu)
model1

# Model 2: Ratings predicted/approximated based on individual genres; some genres are rated higher/lower than others.

genreratingavgs <- edx %>% select(genres, rating) %>% group_by(genres) %>% summarize(b_i = mean(rating - mu))

model2table <- validation %>% left_join(genreratingavgs, by = "genres") %>% mutate(prediction = mu + b_i) %>% summarize(prediction)
model2 <- rmse(validation$rating, model2table$prediction)
model2

# Model 3: Ratings predicted/approximated based on individual movies; some movies are rated higher/lower than others.

movieratingavgs <- edx %>% select(movieId, rating) %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))

model3table <- validation %>% left_join(movieratingavgs, by = "movieId") %>% mutate(prediction = mu + b_i) %>% summarize(prediction)
model3 <- rmse(validation$rating, model3table$prediction)
model3

# Model 4: Ratings predicted/approximated based on individual users (adding onto Model 3); some users give higher/lower ratings for specific movies than do others.

userratingavgs <- edx %>% left_join(movieratingavgs, by = "movieId") %>% group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))

model4table <- validation %>% left_join(movieratingavgs, by = "movieId") %>% left_join(userratingavgs, by = "userId") %>% 
  mutate(prediction = mu+ b_i + b_u) %>% summarize(prediction)
model4 <- rmse(validation$rating, model4table$prediction)
model4



# Refining model 4

# Adding a number of ratings per movie (numberofratingspermovie = norpm)

lambdas <- seq(0, 10, 0.25)

ratingspermovie <- edx %>% group_by(movieId) %>% summarize(total = sum(rating - mu), norpm = n())

finalrmsevalues <- sapply(lambdas, function(l) {
  model5 <- validation %>% left_join(ratingspermovie, by = "movieId") %>% left_join(userratingavgs, by = "userId") %>% 
    mutate(b_i = total/(norpm+l)) %>% mutate(prediction = mu + b_i + b_u) %>% summarize(prediction)
  return(rmse(validation$rating, model5$prediction))
  
})


# RMSE Values Plot

rmseplot <- qplot(lambdas, finalrmsevalues, main = "RMSE Values vs Lambda", xlab = "Lambda", ylab = "RMSE Value") + theme_clean() + theme(plot.title = element_text(hjust = 0.5))

rmseplot

# Final RMSE value

min(finalrmsevalues)

# Lambda Value at which it occurs:

lambdas[which.min(finalrmsevalues)]




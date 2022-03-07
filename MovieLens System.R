## Made Ots
## 07.March 2022
## MovieLens System
## HarvardX: PH125.9x - Capstone Project
## https://github.com/Veronicaots/Harvard-CAPSTONE

if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(scales)) install.packages("scales")
if(!require(DescTools)) install.packages("DescTools")

library(DescTools)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(ggrepel)
library(ggthemes)
library(scales)


##########################################################
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


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
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


###################### EXPLORE THE DATA SET #########################

# Head
head(edx) %>%
  print.data.frame()

# Data set structure
str(edx)

# Rows & columns
dim(edx) 

# Missing values
anyNA(edx)

# Users & Movies
summary(edx)

# Extract the movie release year
edx <- edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))),title = str_remove(title, "[/(]\\d{4}[/)]$"))

# Extract the date from edx and validation sets
edx <- edx %>% 
  mutate(year = as.integer(str_extract(str_extract(title,"\\((\\d{4})\\)$"),"\\d{4}")))
validation <- validation %>% 
  mutate(year = as.integer(str_extract(str_extract(title,"\\((\\d{4})\\)$"),"\\d{4}")))

# Genres
edx %>% group_by(genres) %>% 
  summarise(n=n())%>%
  head()

# Number of movies per genre
tibble(count = str_count(edx$genres, fixed("|")), genres = edx$genres) %>% 
  group_by(count, genres) %>%
  summarise(n = n()) %>%
  arrange(-count) %>% 
  head()

# Counting the ratings
edx %>% group_by(rating) %>% summarize(n=n())

# Number of unique movies and users in edx data set 
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

# Most rated movies
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, binwidth=0.2, color="black", show.legend = FALSE, aes(fill = cut(n, 100))) + 
  scale_x_log10() + 
  ggtitle("Movies Rated")

# Distribution of ratings
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Distribution of ratings")

# Number of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")

# Number of ratings by users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings by users")

# Mean movie ratings by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  theme_light()

# Training set distribution
edx %>% 
  ggplot(aes(rating)) + 
  geom_histogram(binwidth=0.2, color="darkblue", fill="lightblue") + 
  ggtitle("Rating Distribution (Training")

# Validation set distribution
validation %>% 
  ggplot(aes(rating)) + 
  geom_histogram(binwidth=0.2, color="darkblue", fill="lightblue") +  
  ggtitle("Rating Distribution (Validation")

# Sets are similar

###################### MODELING #########################

# Summarize the data
edx %>% summarize(n_movies = n_distinct(movieId), n_users = n_distinct(userId), n_years = n_distinct(year))

# Create the Training Set & Test Set from Validation set
set.seed(1996)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2,list = FALSE)
train_set <- edx[-test_index,]
test_set_tmp <- edx[test_index,]
test_set <- test_set_tmp %>% 
  semi_join(train_set, by = "movieId") %>% semi_join(train_set, by = "userId")%>% semi_join(train_set, by = "year")
train_set <- rbind(train_set, anti_join(test_set_tmp, test_set))
rm(test_set_tmp)

## Mean
# Overall average rating on the training data set
mu <- mean(edx$rating)

# Predict unknown ratings with mu 
RMSE(train_set$rating, mu)

# Applying basic model on training data set:
mu <- mean(edx$rating)
mu
lambdas <- seq(0,10,0.25)

# Evaluating with basic RMSE function
RMSE <- function(true_ratings, predicted_ratings){ 
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## Naive-Bayes
mu <- mean(edx$rating)
naive_rmse <- RMSE(test_set$rating,mu)
rmse_results <- data_frame(Method = "Just the average", RMSE = naive_rmse)

# Movie Rating Effect
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  pull(b_i)
movie_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Method = "Movie effect", 
                                     RMSE = movie_effect_rmse))

# User rating Effect
user_avgs <- train_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  left_join(user_avgs, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% 
  pull(pred)
user_effect_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data_frame(Method = "User effect", 
                                     RMSE = user_effect_rmse))
# Rating Year Effect
year_avgs <- train_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  left_join(user_avgs, by = "userId") %>% 
  group_by(year) %>% 
  summarize(b_y = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  left_join(user_avgs, by = "userId") %>% 
  left_join(year_avgs, by = "year") %>% 
  mutate(pred = mu + b_i + b_u + b_y) %>% 
  pull(pred)
RMSE(predicted_ratings, test_set$rating)


## Regularized Movie and User Effect method with Year
# Calculate the biases
lambdas <- seq(0,10,0.25)

rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating -  mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by = "movieId") %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- train_set %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    group_by(year) %>% 
    summarize(b_y = sum(rating - b_u - b_i - mu)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    left_join(b_y, by = "year") %>%
    mutate(pred = mu + b_i + b_u + b_y) %>% 
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

# RMSEs versus lambdas
qplot(lambdas, rmses)
best_lambda <- lambdas[which.min(rmses)]
best_lambda
min(rmses)
rmse_results <- bind_rows(rmse_results, 
                          data_frame(Method = "Regularized movie + user + year effect", 
                                     RMSE = min(rmses)))

## Testing model against edX set
b_i_tuned <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i_tuned = sum(rating - mu)/(n() + best_lambda))
b_u_tuned <- edx %>% 
  left_join(b_i_tuned, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u_tuned = sum(rating - b_i_tuned - mu)/(n() + best_lambda))
b_y_tuned <- edx %>% 
  left_join(b_i_tuned, by = "movieId") %>% 
  left_join(b_u_tuned, by = "userId") %>% 
  group_by(year) %>% 
  summarize(b_y_tuned = sum(rating - b_i_tuned - b_u_tuned - mu)/(n() + best_lambda))


## Evaluating the model against validation set
predicted_ratings <- validation %>% 
  left_join(b_i_tuned, by = "movieId") %>% 
  left_join(b_u_tuned, by = "userId") %>% 
  left_join(b_y_tuned, by = "year") %>% 
  mutate(pred = mu + b_i_tuned + b_u_tuned + b_y_tuned) %>% 
  pull(pred)

final_rmse <- RMSE(predicted_ratings, validation$rating)
final_rmse
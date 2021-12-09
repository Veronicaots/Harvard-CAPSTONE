## Made Ots
## Movie Recimmendation System
## HarvardX: PH125.9x - Capstone Project
## https://github.com/Veronicaots/Harvard-CAPSTONE


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

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Head
head(edx) %>%
  print.data.frame()

# Users & Movies
summary(edx)

install.packages("DescTools")
library(DescTools)

# Rows & Columns in the dataset
dim(edx)

# How many zeros were given as ratings in the edx dataset?
edx %>% filter(rating == 0) %>% nrow()

# How many threes were given as ratings in the edx dataset?
edx %>% filter(rating == 3) %>% nrow()

# How many different movies are in the edx dataset?
edx %>% group_by(movieId) %>% summarize(count = n())

# How many different users are in the edx dataset?
edx %>% group_by(userId) %>% summarize(count = n())

# How many movie ratings are in each of the following genres in the edx
#  dataset?
edx %>% filter(genres %like% "%Drama%") %>% summarize(n())
edx %>% filter(genres %like% "%Comedy%") %>% summarize(n())
edx %>% filter(genres %like% "%Thriller%") %>% summarize(n())
edx %>% filter(genres %like% "%Romance%") %>% summarize(n())

# Which movie has the most ratings?
edx %>% group_by(title) %>%
  summarize(n_ratings = n()) %>%
  arrange(desc(n_ratings))

# What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>%
  summarize(n_ratings = n()) %>%
  arrange(desc(n_ratings))

# True or False, In general, half star ratings are less common than whole star ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
edx %>% group_by(rating) %>%
  summarize(n_ratings = n()) %>%
  arrange(desc(n_ratings))





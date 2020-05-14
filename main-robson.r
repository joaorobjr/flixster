## load packages
require(arules)
require(arulesSequences)
require(arulesViz)
require(dplyr)
require(e1071)
require(forcats)
require(igraph)
require(plotrix)
require(recommenderlab)
require(stringr)
require(text2vec)
require(tidyverse)
require(tm)
require(funModeling)
require(gridExtra)

library(lubridate)
library(caret)

options(scipen=999)

## load data
ratings.orig = read.delim(
  '../data/ratings.timed.txt',
  sep = "\t",
  skipNul = T,
  # skip NULL values
  col.names = c("userid", "movieid", "rating", "date"),
)
movies.orig = read.delim('../data/movie-names.txt', stringsAsFactors = FALSE)
profiles.orig = read.csv(
  '../data/profile.txt',
  skipNul = T,
  na.strings = c('N/A', ''),
  stringsAsFactors = FALSE
)

# working data frames
ratings.raw =  data.frame(ratings.orig)
movies.raw =   data.frame(movies.orig)
profiles.raw = data.frame(profiles.orig)


##### DATA PREPARATION ------------------------------------------------------

### DATA CLEANING -----------------------------------------------------------

### Clean ratings data set --------------------------------------------------

# remove time tag from columns
ratings.raw$date = ratings.raw$date %>% str_replace(' 00:00:00', "")

# Adjust data types
ratings.raw = ratings.raw %>% mutate(date = as.Date(date))

# Set correct memberfor date for dates before date.flixster
ratings.raw = ratings.raw %>% 
  mutate(date = if_else(date < date.flixster, date.flixster, date))

### Clean profile data set --------------------------------------------------

# remove time tag from columns
profiles.raw$memberfor = profiles.raw$memberfor %>% str_replace(' 00:00:00', "")

# Adjust data types
profiles.raw = profiles.raw %>%
  mutate(age = as.integer(age)) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(memberfor = as.Date(memberfor))

# add new level into gender factor
levels(profiles.raw$gender) = c(levels(profiles.raw$gender), "Other")

# replace NA values into gender with "unknown"
profiles.raw = profiles.raw %>% mutate(gender = replace_na(gender, "Other"))

# Flixster's founding date)
date.flixster = as.Date("2006-01-20")

# Set correct memberfor date for dates before date.flixster
profiles.raw = profiles.raw %>% 
  mutate(memberfor = if_else(memberfor < date.flixster, date.flixster, memberfor))

# Add columns in `mean_ratings_user` and `q_ratings_user` into profile data set
# The `mean_ratings_user` column contains the mean value of ratings per user
# The `q_ratings_user` columns contains the total of rantings per user

# calculate the mean value and total of ratings per user
user.ratings = ratings.raw %>% group_by(userid) %>% 
  summarise(mean_ratings_user = mean(rating), q_ratings_user = n())

# join user.ratings with profile.raw
profiles.raw = left_join(profiles.raw, user.ratings, by = "userid")

# replace NA values into `mean_ratings_user` and `q_ratings_user` with 0
profiles.raw = profiles.raw %>%
  mutate(q_ratings_user = replace_na(q_ratings_user, 0)) %>%
  mutate(mean_ratings_user = replace_na(mean_ratings_user, 0))

rm(user.ratings)

profiles.raw = select(profiles.raw, -c(profileview))

### Clean movies data set ---------------------------------------------------

# remove ASCII codes
movies.raw$moviename = movies.raw$moviename %>% str_replace_all("&#233;", "é")
movies.raw$moviename = movies.raw$moviename %>% str_replace_all("&amp;", "&")
movies.raw$moviename = movies.raw$moviename %>% str_replace_all("&#\\d*;", "")

# Add columns in `mean_ratings_movie` and `q_ratings_movie` into movies data set.
# The `mean_ratings_movie` column contains the mean value of ratings per movie
# The `q_ratings_movie` columns contains the total of rantings per movie

# Calculate the mean value and total of ratings per movie
movie.ratings = ratings.raw %>% group_by(movieid) %>% 
  summarise(mean_ratings_movie = mean(rating), q_ratings_movie = n())

# join movie.ratings with movies.raw
movies.raw = left_join(movies.raw, movie.ratings, by = "movieid")

# replace NA values into `mean_ratings_movie` and `q_ratings_movie` with 0
movies.raw = movies.raw %>%
  mutate(q_ratings_movie = replace_na(q_ratings_movie, 0)) %>%
  mutate(mean_ratings_movie = replace_na(mean_ratings_movie, 0))

rm(movie.ratings)

# tibble version of data frames
movies = tbl_df(movies.raw)
profiles = tbl_df(profiles.raw)
ratings = tbl_df(ratings.raw)

rm(movies.orig, movies.raw, ratings.orig, ratings.raw, profiles.orig, profiles.raw)

#### EXPLORATORY DATA ANALYSIS ----------------------------------------------

# Variable age
summary(age)
percentile_var=quantile(age, c(0.01, 0.9953), na.rm = T)
dfp=data.frame(value=percentile_var, percentile=c("1th", "99.5th"))
ggplot(profiles) +
  geom_histogram(aes(x = age, fill = gender), stat = "count", position = "dodge") +
  labs(x = "Age (yrs)", y = "Total", 
       title = "Distribuition of User Per Age and Gender", 
       subtitle = "The distribution is right skewed") +
  geom_vline(data=dfp, aes(xintercept=value, colour = percentile), show.legend = T, linetype="dashed") +
  theme_bw()

rm(percentile_var, dfp)

# Distribution of Ratings Per Users
ggplot(profiles) +
  geom_histogram(aes(q_ratings_user), color = "white", binwidth = 0.2) +
  scale_x_log10() + 
  ggtitle("Distribution of Ratings Per Users", 
          subtitle = "The distribution is right skewed") +
  xlab("Number of Ratings") +
  ylab("Number of Users") + 
  theme_bw()

#Popularity: nTop=20 more active users
head(profiles %>% select(userid, gender, age, memberfor, q_ratings_user) %>% arrange(desc(q_ratings_user)), 20)

# Distribution of Ratings Per Movies
ggplot(movies) +
  geom_histogram(aes(x=q_ratings_movie), color = "white", binwidth = 0.2) +
  scale_x_log10() + 
  ggtitle("Distribution of Ratings Per Movies", 
          subtitle = "The distribution is right skewed") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") + 
  theme_bw()

#Popularity: nTop=20 more movies rated
head(movies %>% arrange(desc(q_ratings_movie)), 20)

#The rating period was collected over almost ~3.82 years.
as.duration(interval(date.flixster, max(ratings$date)))

#Distribution of rating per years
ratings %>% mutate(year = year(date)) %>%
  ggplot(aes(x=year)) +
  geom_histogram(color = "white", binwidth = 0.2) + 
  ggtitle("Rating Distribution Per Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  theme_bw()

#Distribution of rating per month
ratings %>% mutate(month = month(date, label = T)) %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(x=month, fill=year)) +
  geom_histogram(stat = "count", binwidth = 0.2, color = "white", position = "dodge") + 
  ggtitle("Rating Distribution Per Month") +
  xlab("Month") +
  ylab("Number of Ratings") +
  theme_bw()


profiles = select(profiles, -c(lastlogin, location))

# Create a common set of ratings/movies/users
flixster = ratings %>% left_join(profiles, by = "userid") %>% left_join(movies, by = "movieid")


# Get movies rated by less active users and with a low number of ratings.
movies.relevants = flixster %>% filter(q_ratings_movie > 5, q_ratings_user > 10)
glimpse(movies.relevants) #7843526 obs of 12 variables

movies.no.relevants = anti_join(flixster, movies.relevants)
glimpse(movies.no.relevants) #352551 obs of 12 variables

# Get less active users with age = NA
user.low.ratings = flixster %>% filter(is.na(age), q_rating_user < 10) %>%
  select(userid, q_rating_user, gender, age, memberfor) %>%
  arrange(userid)

####------------------------------------------------------------------------------

# 'test_set' will be 30% of flixster datase
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = flixster$rating, times = 1, p = 0.3, list = FALSE)

train_set <- flixster[-test_index,]
test_set <- flixster[test_index,]


#Create ratings matrix. Rows = userId, Columns = movieId
#ratingsmat <- dcast(ratings, userid~movieid, value.var = "rating", na.rm=FALSE)
#ratingsmat <- as.matrix(ratingmat[,-1]) #remove userIds
#ratings.mat <- as(train_set, "realRatingMatrix")







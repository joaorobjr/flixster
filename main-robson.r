## load packages
require(arules)
require(arulesSequences)
require(arulesViz)
require(dplyr)
require(e1071)
require(igraph)
require(recommenderlab)
require(stringr)
require(text2vec)
require(tidyverse)
require(tm)

## load data
ratings.orig= read.delim(
  'data/ratings.timed.txt',
  sep= "\t",
  skipNul= T, # skip NULL values
  col.names= c("userid", "movieid", "rating", "date")
)
movies.orig= read.delim('data/movie-names.txt')
profiles.orig= read.csv(
  'data/profile.txt',
  skipNul= T,
  na.strings= c('N/A', '')
)

# working data frames
ratings.raw=  data.frame(ratings.orig)
movies.raw=   data.frame(movies.orig)
profiles.raw= data.frame(profiles.orig)

### Clean ratings.raw

# remove time tag from columns
ratings.raw$date= ratings.raw$date %>% str_replace(' 00:00:00', "")


### Clean profile.raw

# remove time tag from columns
profiles.raw$memberfor = profiles.raw$memberfor %>% str_replace(' 00:00:00', "")

# add new level into gender factor
levels(profiles.raw$gender) = c(levels(profiles.raw$gender), "Unknown")
# replace NA values into gender with "unknown"
profiles.raw <- profiles.raw %>% mutate(gender = replace_na(gender, "Unknown"))

# add column in profile  with the total ranting per user
# calculate the total of ratings and mean of ratings per user
tot.user.ratings = ratings.raw %>% group_by(userid) %>% summarise(mean_ratings_user = mean(rating), q_rating_user = n())
# join tot.user.ratings with profile.raw
profiles.raw = left_join(profiles.raw, tot.user.ratings, by = "userid")
# replace NA values into q_rating_user with 0
profiles.raw = profiles.raw %>% 
  mutate(q_rating_user = replace_na(q_rating_user, 0)) %>% 
  mutate(mean_ratings_user = replace_na(mean_ratings_user, 0))

rm(tot.user.ratings)

### Clean movies.row

# remove ASCII encondes
movies.raw$moviename = movies.raw$moviename %>% str_replace_all("&#233;", "é")
movies.raw$moviename = movies.raw$moviename %>% str_replace_all("&amp;", "&")
movies.raw$moviename = movies.raw$moviename %>% str_replace_all("&#\\d*;", "")

# add column in movie with the total ranting per movie
# calculate the total of ratings per movie
tot.movie.ratings = ratings.raw %>% group_by(movieid) %>% summarise(mean_ratings_movie = mean(rating), q_rating_movie = n())
# join tot.movie.ratings with movies.raw
movies.raw = left_join(movies.raw, tot.movie.ratings, by = "movieid")
# replace NA values into q_rating_movie with 0
movies.raw = movies.raw %>% 
  mutate(q_rating_movie = replace_na(q_rating_movie, 0)) %>% 
  mutate(mean_ratings_movie = replace_na(mean_ratings_movie, 0))

rm(tot.movie.ratings)

# conversions
profiles.raw$age= as.integer(profiles.raw$age)
profiles.raw$gender= as.factor(profiles.raw$gender)
profiles.raw$memberfor= as.Date(profiles.raw$memberfor)
profiles.raw$profileview= as.integer(profiles.raw$profileview)
ratings.raw$date= as.Date(ratings.raw$date)

# tibble version of data frames
movies= tbl_df(movies.raw)
profiles= tbl_df(profiles.raw)
ratings= tbl_df(ratings.raw)

# Remove no relevant variables
profiles = profiles %>%  select(-c(profileview))

# Create a common set of ratings/movies/users 
flisxter = ratings %>% left_join(profiles, by = "userid") %>% left_join(movies, by = "movieid")

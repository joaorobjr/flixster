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
require(reshape2)
require(lubridate)
require(caret)
require(ggplot2)

options(scipen=999)

### LOAD RAW DATA --------------------------------------------------------
# Ratings ----------------------------------------------------------------
ratings.orig = read.delim(
  '../data/ratings.timed.txt',
  sep = "\t",
  skipNul = T,
  # skip NULL values
  col.names = c("userid", "movieid", "rating", "date"),
)
# Movies ----------------------------------------------------------------
movies.orig = read.delim('../data/movie-names.txt', 
                         stringsAsFactors = FALSE)
# Profile ----------------------------------------------------------------
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

# DATA PREPARATION -------------------------------------------------------

# DATA CLEANING ----------------------------------------------------------

# Clean Ratings Data Set -------------------------------------------------

# remove time tag from columns
ratings.raw$date = ratings.raw$date %>% str_replace(' 00:00:00', "")

# Adjust data types
ratings.raw = ratings.raw %>% mutate(date = as.Date(date))

# Flixster's founding date)
date.flixster = as.Date("2006-01-20")

# Set correct memberfor date for dates before date.flixster
ratings.raw = ratings.raw %>% 
  mutate(date = if_else(date < date.flixster, date.flixster, date))

# Clean Profile Data Set -------------------------------------------------

# remove time tag from columns
profiles.raw$memberfor = profiles.raw$memberfor %>% 
  str_replace(' 00:00:00', "")

# Adjust data types
profiles.raw = profiles.raw %>%
  mutate(age = as.integer(age)) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(memberfor = as.Date(memberfor))

# add new level into gender factor
levels(profiles.raw$gender) = c(levels(profiles.raw$gender), "Other")

# replace NA values into gender with "unknown"
profiles.raw = profiles.raw %>% mutate(gender = replace_na(gender, "Other"))

# Set correct memberfor date for dates before date.flixster
profiles.raw = profiles.raw %>% 
  mutate(memberfor = if_else(memberfor < date.flixster, date.flixster, 
                             memberfor))

# Add columns in `mean_ratings_user` and `q_ratings_user` into profile 
# data set. The `mean_ratings_user` column contains the mean value of 
# ratings per user and the `q_ratings_user` columns contains the total
# of rantings per user

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

### Clean Movies data set ------------------------------------------------

# remove ASCII codes
movies.raw$moviename = movies.raw$moviename %>% 
  str_replace_all("&#233;", "é")
movies.raw$moviename = movies.raw$moviename %>% 
  str_replace_all("&amp;", "&")
movies.raw$moviename = movies.raw$moviename %>% 
  str_replace_all("&#\\d*;", "")

# Add columns in `mean_ratings_movie` and `q_ratings_movie` into movies 
# data set. Tthe `mean_ratings_movie` column contains the mean value of 
# ratings per movie and the `q_ratings_movie` columns contains the total
# of rantings per movie

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

rm(movies.orig, movies.raw, ratings.orig, ratings.raw, profiles.orig, 
   profiles.raw)

#### EXPLORATORY DATA ANALYSIS -------------------------------------------

# Variable age
summary(age)
percentile_var=quantile(age, c(0.01, 0.9953), na.rm = T)
dfp=data.frame(value=percentile_var, percentile=c("1th", "99.5th"))
ggplot(profiles) +
  geom_histogram(aes(x = age, fill = gender), stat = "count", 
                 position = "dodge") +
  labs(x = "Age (yrs)", y = "Total", 
       title = "Distribuition of User Per Age and Gender", 
       subtitle = "The distribution is right skewed") +
  geom_vline(data=dfp, aes(xintercept=value, colour = percentile), 
             show.legend = T, linetype="dashed") +
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
head(profiles %>% select(userid,gender,age,memberfor,q_ratings_user) %>% 
       arrange(desc(q_ratings_user)), 20)

# Distribution of Ratings Per Movies
ggplot(movies) +
  geom_histogram(aes(x=q_ratings_movie), color = "white", binwidth = 0.2)+
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
  geom_histogram(stat = "count", binwidth = 0.2, color = "white", 
                 position = "dodge") + 
  ggtitle("Rating Distribution Per Month") +
  xlab("Month") +
  ylab("Number of Ratings") +
  theme_bw()

#Popularity: Distribution Top 20 Rated Movies
movies.ratings = left_join(ratings, movies, by = "movieid")
movies.ratings %>% 
  group_by(moviename) %>% 
  summarise(count = n()) %>% 
  top_n(20, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(moviename, count), y = count))+
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Top 20 Rated Movie", title = "Most Rated Movie") +
  coord_flip() +
  theme_grey(base_size = 12)
rm(movies.ratings)

#Popularity: Distribution Top 20 Active Users
ratings %>% 
  group_by(userid) %>% 
  summarise(count = n()) %>% 
  top_n(20, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(userid, count), y = count))+
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Top 20 More Active Users", title = "Most Acive Users") +
  coord_flip() +
  theme_grey(base_size = 12)

# Create a common set of ratings/movies/users
flixster = ratings %>% left_join(profiles, by = "userid") %>% 
  left_join(movies, by = "movieid")

# 2776 movies which represent 89.74% of total de ratings
freq.movies = freq(data = flixster, input="movieid", plot = F) %>% 
  filter(percentage > 0) #2776 Obs.

# 5169 users which represent 63.61% of total de ratings
freq.user = freq(data = flixster, input="userid", plot = F) %>% 
  filter(percentage > 0) #5169 Obs.

movies.no.relevants = anti_join(flixster, flixster.sample)
glimpse(movies.no.relevants) #4499277 obs of 14 variables

# Get less active users with age = NA
user.low.ratings = flixster %>% filter(is.na(age), q_rating_user < 10) %>%
  select(userid, q_rating_user, gender, age, memberfor) %>%
  arrange(userid)

# SAVE DATA SETS----------------------------------------------------------
save(flixster, file = "flixster.RData")
save(movies, file = "movies.RData")
save(profiles, file = "profiles.RData")
save(ratings, file = "ratings.RData")

# EXTRACT SAMPLE ---------------------------------------------------------

# Get movies rated by more active users and with a high number of ratings.
flixster.sample = flixster %>% 
  filter(q_ratings_movie > 410, q_ratings_user > 410)

glimpse(flixster.sample) #3696800 obs of 14 variables

rm(flixster)

#Remove variables
flixster.sample = select(flixster.sample, 
                         c(userid, movieid, rating, date))

# # 'test_set' will be 30% of flixster datase
# set.seed(755)
# test_index <- createDataPartition(y = flixster.sample$rating, 
# times = 1, p = 0.3, list = FALSE)
# 
# train_set <- flixster.sample[-test_index,] #2587758 obs
# test_set <- flixster.sample[test_index,] #1109042
# 
# #To make sure we don’t include users and movies in the test set 
# #that do not appear in the training set
# test_set <- test_set %>% semi_join(train_set, by = "movieid") %>%
#   semi_join(train_set, by = "userid")
# 
# 
# save(train_set, file = "train-set.RData")
# save(test_set, file = "test-set.RData")

# LOAD DATA SETS (.RData) ------------------------------------------------
load("data/flixster.tiny.rdata")
load("data/movies.rdata")

flixster.sample = sample_n(flixster.tiny, 10000)

# BUILD MODELS AND RECOMMENDATIONS --------------------------------------- 

# BINARY APPROACH --------------------------------------------------------

# Some recommendation models work on binary data, so it might be useful to 
# binarize the data, that is, define a table containing only 0s and 1s. 
# The 0s will be either treated as missing values or as bad ratings.
# 
# In our case, define a matrix having 1 if the rating is above or equal to 
# a definite threshold (for example, 0.5), and 0 otherwise. In this case, 
# giving a bad rating to a movie is equivalent to not having rated it.

binarymodels = recommenderRegistry$get_entries(
  dataType ="binaryRatingMatrix")

nobinarymodels = recommenderRegistry$get_entries(
  dataType ="realRatingMatrix")

# Prepare Rating Matrix --------------------------------------------------

#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat = dcast(flixster.sample, userid~movieid, value.var="rating")
ratingmat = as.matrix(ratingmat)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat = as(ratingmat, "realRatingMatrix")

#Create a binary rating matrix
binaryratingmat = binarize(ratingmat, minRating = 0.5)

ntop = 10 # the number of top movies to recommend to each user

# Prepapre Training and Test data set ------------------------------------
which_train = sample(x = c(TRUE, FALSE), 
                      size = nrow(binaryratingmat),
                      replace = TRUE, 
                      prob = c(0.8, 0.2))

train_binary = binaryratingmat[which_train, ]
test_binary = binaryratingmat[!which_train, ]

# IBCF (Binary Approach) -------------------------------------------------

binarymodels$IBCF_binaryRatingMatrix$parameters

binarymodelIBCF = Recommender(data = train_binary, method = "IBCF")

binaryrecIBCF = predict(object = binarymodelIBCF, 
                        newdata = test_binary, n = ntop)

#Recommendations for the first user:
recuser1 = binaryrecIBCF@items[[1]] # recommendation for the first user
moviesuser1 = binaryrecIBCF@itemLabels[recuser1]
moviesrecIBCF = moviesuser1
for (i in 1:ntop){
  moviesrecIBCF[i] = as.character(
    subset(movies, movies$movieid == moviesuser1[i])$moviename)
}
moviesrecIBCF

# UBCF (Binary Approach) -------------------------------------------------

binarymodels$UBCF_binaryRatingMatrix$parameters

binarymodelUBCF = Recommender(data = train_binary, method = "UBCF")

binaryrecUBCF = predict(object = binarymodelUBCF, 
                        newdata = test_binary,n = ntop)

#Recommendations for the first user:
recuser1 = binaryrecUBCF@items[[1]] # recommendation for the first user
moviesuser1 = binaryrecUBCF@itemLabels[recuser1]
moviesrecUBCF = moviesuser1
for (i in 1:10){
  moviesrecUBCF[i] = as.character(
    subset(movies, movies$movieid == moviesuser1[i])$moviename)
}
moviesrecUBCF

# POPULAR (Binary Approach) ----------------------------------------------

binarymodels$POPULAR_binaryRatingMatrix$parameters

binarymodelPOP = Recommender(data = train_binary, method = "POPULAR")

binaryrecPOP = predict(object = binarymodelPOP, 
                       newdata = test_binary,  n = ntop)

#Recommendations for the first user:
recuser1 = binaryrecPOP@items[[1]] # recommendation for the first user
moviesuser1 = binaryrecPOP@itemLabels[recuser1]
moviesrecPOP = moviesuser1
for (i in 1:10){
  moviesrecPOP[i] = as.character(
    subset(movies, movies$movieid == moviesuser1[i])$moviename)
}
moviesrecPOP

# NO-BINARY APPROACH -----------------------------------------------------

# Prepapre Training and Test data set ------------------------------------
which_train <- sample(x = c(TRUE, FALSE), 
                      size = nrow(ratingmat),
                      replace = TRUE, 
                      prob = c(0.8, 0.2))

train <- ratingmat[which_train, ]
test <- ratingmat[!which_train, ]

# IBCF (No-Binary Approach) ----------------------------------------------

nobinarymodels$IBCF_realRatingMatrix$parameters

modelIBCF = Recommender(data = train, method = "IBCF")

recIBCF = predict(object = modelIBCF, newdata = test, n = ntop)

#Recommendations for the first user:
recuser1 = recIBCF@items[[1]]
moviesuser1 = recIBCF@itemLabels[recuser1]
moviesrecIBCF = moviesuser1
for (i in 1:ntop){
  moviesrecIBCF[i] = as.character(
    subset(movies, movies$movieid == moviesuser1[i])$moviename)
}
moviesrecIBCF

# UBCF (No-Binary Approach) ----------------------------------------------

nobinarymodels$UBCF_realRatingMatrix$parameters

modelUBCF = Recommender(data = train, method = "UBCF")

recUBCF = predict(object = modelUBCF, newdata = test, n = ntop)

#Recommendations for the first user:
recuser1 = recUBCF@items[[1]]
moviesuser1 = recUBCF@itemLabels[recuser1]
moviesrecUBCF = moviesuser1
for (i in 1:10){
  moviesrecUBCF[i] = as.character(
    subset(movies, movies$movieid == moviesuser1[i])$moviename)
}
moviesrecUBCF

# POPULAR (No-Binary Approach) -------------------------------------------

nobinarymodels$POPULAR_realRatingMatrix$parameters

modelPOP = Recommender(data = train, method = "POPULAR")

recPOP = predict(object = modelPOP, newdata = test, n = ntop)

#Recommendations for the first user:
recuser1 = recPOP@items[[1]]
moviesuser1 = recPOP@itemLabels[recuser1]
moviesrecPOP = moviesuser1
for (i in 1:10){
  moviesrecPOP[i] = as.character(
    subset(movies, movies$movieid == moviesuser1[i])$moviename)
}
moviesrecPOP

# SAVE MODELS AND RECOMMENDATIONS ----------------------------------------

save(binarymodels, train_binary, test_binary, binaryratingmat, 
     binarymodelIBCF, binarymodelUBCF, binarymodelPOP, binaryrecIBCF, 
     binaryrecUBCF, binaryrecPOP, ntop, nobinarymodels,train, test, 
     ratingmat, modelIBCF, modelUBCF, modelPOP, recIBCF, recUBCF, recPOP,
     file = "data/models-recommendations.rdata")

# ASSOCIATION RULES (Binary Approach) --------------------------------------------

# trainsetAR = train_set[, -c(2,3,5,6,7)]
# trainsetAR = dcast(test_set, userid~movieid, value.var="rating", na.rm=FALSE)
# bm = as(as.matrix(trainsetAR), "realRatingMatrix")
# bm = binarize(bm, minRating = 0.5)
# modelAR = Recommender(bm, method = "AR")
# getModel(modelAR)
# rules <- getModel(modelAR)$rule_base
# inspect(rules)

binary.models$AR_binaryRatingMatrix$parameters

flixster.sample.ar = select(flixster.sample, c(userid, movieid))

ratingmatAR = dcast(flixster.sample, userid~movieid , value.var="rating", na.rm=FALSE)

#ratingmat <- dcast(flixster.sample.ar, userid~movieid, fun.aggregate = count())

ratingmat <- as.matrix(flixster.sample)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")
#Create a binary rating matrix
ratingmat <- binarize(ratingmat, minRating = 0.5)

which_train <- sample(x = c(TRUE, FALSE), 
                      size = nrow(ratingmat),
                      replace = TRUE, 
                      prob = c(0.8, 0.2))

train <- ratingmat[which_train, ]
test <- ratingmat[!which_train, ]

modelAR = Recommender(data = train, method = "AR", param=list(confidence=0.2))
getModel(modelAR)
rules <- getModel(modelAR)$rule_base
inspect(rules)

# COMPARE MODELS ---------------------------------------------------------

load("data/models-recommendations.rdata")

# In order to compare different models, I define them as a following list:
# 
# - Binary Approach  
# * Item-based Collaborative Filtering
# * User-based Collaborative Filtering
# * Popularity
# 
# - No-Binary Approach  
# * Item-based Collaborative Filtering
# * User-based Collaborative Filtering
# * Popularity

# PREPARE EVALUATION -----------------------------------------------------
models_to_evaluate <- list(
  IBCF = list(name = "IBCF"),
  UBCF = list(name = "UBCF"), 
  POP  = list(name = "POPULAR")
)

topN = c(1, 2, 5)

# EVALUATE BINARY MODELS -------------------------------------------------
evalBinary <- evaluationScheme(data = binaryratingmat, 
                              method = "cross-validation",
                              k = 4, 
                              given = 1, 
                              goodRating = 3)

results.binary <- evaluate(x = evalBinary, 
                    method = models_to_evaluate, 
                    n = topN )
results.binary

#Results for IBCF model using binary approach: ")
cmIBCFbin = getConfusionMatrix(results.binary$IBCF)
cmUBCFbin = getConfusionMatrix(results.binary$UBCF)
cmPOPbin = getConfusionMatrix(results.binary$POP)
#cmARbin = getConfusionMatrix(results.binary$AR)

plot(results.binary, annotate = TRUE, legend = "topleft", main = "ROC Curve") 

plot(results.binary, annotate = TRUE, "prec/rec", legend = "topleft", 
     main = "Precision-Recall")

# EVALUATE NO-BINARY MODELS -------------------------------------------------

eval <- evaluationScheme(data = ratingmat, 
                              method = "cross-validation",
                              k = 4, 
                              given = 1, 
                              goodRating = 3)

results <- evaluate(x = eval, 
                    method = models_to_evaluate, 
                    n = topN )
results

#Results for IBCF model using binary approach: ")
cmIBCF = getConfusionMatrix(results$IBCF)
cmUBCF = getConfusionMatrix(results$UBCF)
cmPOP = getConfusionMatrix(results$POP)
#cmAR = getConfusionMatrix(results$AR)

plot(results, annotate = TRUE, legend = "topleft", main = "ROC Curve") 

plot(results, annotate = TRUE, "prec/rec", legend = "topleft", 
     main = "Precision-Recall")

save(models_to_evaluate, topN, evalBinary, results.binary, eval, results, 
     file = "data/eval-results.rdata")


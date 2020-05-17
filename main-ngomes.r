# HEADER ------------------------------------------------------------------
###
## Practical Assignment of Advanced Topics in Data Science
## By Nuno Gomes (up199300242) and João Robson Júnior (up201)
## May 2020
###


# PACKAGES ----------------------------------------------------------------
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



# FUNCTIONS ---------------------------------------------------------------
# compute the mode
getmode= function(arr) {
  uniq.vals= unique(arr)
  uniq.vals[which.max(tabulate(match(arr, uniq.vals)))]
}


# SETUP -------------------------------------------------------------------
movies.path= file("../data/movie-names.txt")
profiles.path= file("../data/profile.txt")
ratings.path= file("../data/ratings.timed.txt")

movies.orig= read.delim(movies.path)
profiles.orig= read.csv(
  profiles.path,
  skipNul= T,
  na.strings= c("N/A", "")
)
ratings.orig= read.delim(
  ratings.path,
  sep= "\t",
  skipNul= T, # skip NULL values
  col.names= c("userid", "movieid", "rating", "date")
)

on.exit(close(movies.path, profiles.path, ratings.path))

## working data frames
movies.raw= data.frame(movies.orig)
profiles.raw= data.frame(profiles.orig)
ratings.raw= data.frame(ratings.orig)


# DATA ENGINEERING --------------------------------------------------------
## remove time tag from columns
ratings.raw$date= ratings.raw$date %>% str_replace(" 00:00:00", "")
profiles.raw$memberfor= profiles.raw$memberfor %>% str_replace(" 00:00:00", "")
## conversions
profiles.raw$age= as.integer(profiles.raw$age)
profiles.raw$gender= as.factor(profiles.raw$gender)
profiles.raw$memberfor= as.Date(profiles.raw$memberfor)
profiles.raw$profileview= as.integer(profiles.raw$profileview)
ratings.raw$date= as.Date(ratings.raw$date)
## remove ascii codes from movies
movies.raw$moviename= movies.raw$moviename %>%
  str_replace_all("&#233;", "é")
movies.raw$moviename= movies.raw$moviename %>%
  str_replace_all("&amp;", "&")
movies.raw$moviename= movies.raw$moviename %>%
  str_replace_all("&#\\d*;", "")


## tibble version of data frames
movies= tbl_df(movies.raw)
profiles= tbl_df(profiles.raw)
ratings= tbl_df(ratings.raw)

## remove profileview from profiles tibble
profiles= profiles %>% select(-"profileview")


# VARIABLES ---------------------------------------------------------------
age= profiles$age
dates= ratings$date
gender= fct_explicit_na(profiles$gender, "Other")
lastlogin= profiles$lastlogin
location= profiles$location
memberfor= profiles$memberfor #
movieid.movies= movies$movieid
movieid.ratings= ratings$movieid
moviename= movies$moviename
rating= ratings$rating
userid.ratings= ratings$userid
userid.profiles= profiles$userid


# EXPLORATORY DATA ANALYSIS -----------------------------------------------
par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))
# age ---------------------------------------------------------------------

# Variable age
summary(age)
percentile.age= quantile(age, c(0.01, 0.9951), na.rm= T)
dfp.age=data.frame(value= percentile.age, percentile= c("1st", "99.5th"))
ggplot(data.frame(age= age, gender= gender)) +
  geom_histogram(
    aes(x= age, fill= gender),
    stat= "count",
    position= "dodge"
  ) +
  labs(x= "Age (yrs)", y= "Total", 
       title= "Distribuition of users per age and gender"#, 
#       subtitle= "(right skewed distribution)"
  ) +
  geom_vline(
    data= dfp.age,
    aes(xintercept= value, colour= percentile),
    show.legend= T,
    linetype= "dashed") +
  theme_bw()
rm(percentile.age, dfp.age)

idx.nas.age= which(is.na(age)) # 25.5% of all age values
age= na.omit(age)
var(age) # 107.5378
skewness(age) # 2.444335
hist(age,
  breaks= seq(round(min(age)) - 1, round(max(age)) + 1, by= 1),
  xlab= "Age (yrs)"
)
boxplot(age,
  yaxt= "n",
  main= "Boxplot of age"
)
axis(2, las= 2)

### remove outliers
age.out= boxplot.stats(age, coef= 3.9)$out
age.no.out.idx= !(age %in% age.out)
age.no.out= age[age.no.out.idx]
summary(age.no.out) # max= 71 (99.5% of values)
ggplot(data.frame(age= age.no.out)) +
  geom_histogram(
    aes(x= age),
    stat= "count",
    position= "dodge"
  ) +
  labs(x= "Age (yrs)", y= "Total", 
       title= "Histogram of ages"#, 
  ) +
  theme_bw()

hist(age.no.out,
  breaks= seq(round(min(age.no.out)) - 1, round(max(age.no.out)) + 1, by= 1),
  xlab= "Age (yrs)",
  main= "Histogram of age"
)
boxplot(age.no.out, outline= F, yaxt= "n")
axis(2, las= 2)
mtext(
  side= 3, line= 2, at= 1, cex= 1.2,
  expression(paste("Boxplot of age"))
)
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")
## Most of the individuals are between 18 and 24 years old;
## the youngest is 12 and the oldest 113 (outlier);
## the distribution is right skewed, as expected (Flixster customers are
## typically from young generations).

#save(age, file= "../data/age.rdata")
#load("../data/age.rdata")
#save(age.no.out, file= "../data/age.no.out.rdata")
#load("../data/age.no.out.rdata")

# memberfor ---------------------------------------------------------------
## correct wrong dates
## correct all wrong dates, i.e., dates before 2006-01-20 (Flixster's founding
##  date)
date.flixster= as.Date("2006-01-20")
idx.memberfor.wrong= which(profiles$memberfor < date.flixster) # 57722
profiles$memberfor[idx.memberfor.wrong]= date.flixster
summary(profiles$memberfor) # min= 2006-01-20; NAs= 203
memberfor= profiles$memberfor
summary(memberfor)
idx.nas.memberfor= which(is.na(memberfor)) # 203
uids.memberfor.nas= profiles$userid[idx.nas.memberfor] # 203
# fill in NAs in memberfor with earliest rating date
# join profiles and ratings tables
users.ratings= tbl_df(merge(profiles, ratings, by= "userid"))
for (i in 1:nrow(users.ratings)) {
  if (is.na(users.ratings$memberfor[i])) {
    users.ratings$memberfor[i]= users.ratings$date[i]
  }
}
#write.csv(users.ratings, file= '../data/users-ratings.csv')
#users.ratings= read.csv("../data/users-ratings.csv")
#summary(as.Date(users.ratings$memberfor)) # min= 2006-01-20
# write.csv(memberfor, "../data/memberfor.csv")
memberfor= as.Date(users.ratings$memberfor)
save(memberfor, file= "../data/memberfor.rdata")
tmp= read.csv("../data/memberfor.csv")
memberfor= as.Date(tmp$x)
summary(memberfor)

# dates -------------------------------------------------------------------
summary(dates) # min= 1941-12-07
idx.date.wrong= which(ratings$date < date.flixster) # 2998
sum(idx.date.wrong %in% idx.memberfor.wrong) # 35
# So, not all rating dates before date.flixster are in the profiles dates set.
summary(ratings$date[idx.date.wrong]) # min= 1941-12-07
ratings$date[idx.date.wrong]= date.flixster
summary(ratings$date) # min= 2006-01-20
dates= ratings$date
#save(dates, file= "../data/dates.rdata")
load("../data/dates.rdata")

# gender ------------------------------------------------------------------
#save(gender, file= "../data/gender.rdata")
#load("../data/gender.rdata")
n.gender= length(gender)
gender.woman= gender[gender == "Female"]
n.woman= length(gender.woman)
gender.woman.pct= round(n.woman / n.gender * 100)
gender.man= gender[gender == "Male"]
n.man= length(gender.man)
gender.man.pct= round(n.man / n.gender * 100)
gender.other= gender[gender == "Other"]
n.other= length(gender.other)
gender.other.pct= round(n.other / n.gender * 100)
gender.labels= c(
  paste('Women:', gender.woman.pct),
  paste('Men:',   gender.man.pct),
  paste('Other:', gender.other.pct))
gender.labels= paste0(gender.labels, '%')
gender.colours= c("#FA9FB5", "#74A9CF", "#2ECC71")
par(mfrow= c(1, 1), oma= c(0, 0, 0, 0))
pie3D(c(n.woman, n.man, n.other), theta= pi/3,
      labels= gender.labels, labelcex= 1.5,
      col= gender.colours,
      start= pi/4, explode= 0.08)
mtext("Gender spread", side= 3, line= -4, outer= T, cex= 2)
# More women (65%) than men (35%) were considered in this study.

# lastlogin ---------------------------------------------------------------
summary(lastlogin) # min= 0; max= 177278; NAs= 57925
idx.nas.lastlogin= which(is.na(lastlogin)) # 5.7% of all values
lastlogin= na.omit(lastlogin)
var(lastlogin) # 102829.2
skewness(lastlogin) # 217.3447
hist(lastlogin,
  breaks= seq(round(min(lastlogin)) - 1, round(max(lastlogin)) + 1),
  xlab= "Last login"
)
boxplot(lastlogin,
  yaxt= "n",
  main= "Boxplot of lastlogin"
)
axis(2, las= 2)
### remove outliers
lastlogin.out= boxplot.stats(lastlogin, coef= 20)$out
lastlogin.no.out.idx= !(lastlogin %in% lastlogin.out)
lastlogin.no.out= lastlogin[lastlogin.no.out.idx]
summary(lastlogin.no.out) # max= 46 (~99% of the values)
hist(lastlogin.no.out,
  breaks= seq(round(min(lastlogin.no.out)) - 1, round(max(lastlogin.no.out))),
  xlab= "Last login",
  main= "Histogram of lastlogin"
)
boxplot(lastlogin.no.out, outline= F, yaxt= "n")
axis(2, las= 2)
mtext(
  side= 3, line= 2, at= 1, cex= 1.2,
  expression(paste("Boxplot of lastlogin"))
)
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")
## We don't know exactly what represents this variable. We suspect it is the
## total number of logins per user. Assuming that is the case, most of the users
## logged in to Flixster between approximately four to 30 times;
## the minimum of lastlogin is 0, meaning the user never logged in, and the
## maximum is 177278;
## the distribution is strongly right skewed.
## This variable does not seem to be relevant for any of our analyses, so it
## will be discarded.


# location ----------------------------------------------------------------
summary(location) # min= 0; max= 1617.0; NAs= 203
idx.nas.location= which(is.na(location)) # ~0.02% of all values
loca= na.omit(location)
var(loca) # 81802.41
skewness(loca) # 1.036804
hist(location,
  breaks= seq(round(min(loca)) - 1, round(max(loca)) + 1),
  xlab= "Location"
)
boxplot(loca,
  yaxt= "n",
  main= "Boxplot of location"
)
axis(2, las= 2)
## There is a huge difference between the number of zeros and the remainder of
##  location numbers. This might mean that zero is the most typical region (in
##  case the numbers indicate regions) or zero might represent an outlier.

## remove NAs
## Since there is no information about the variable and the latter is an
##  integer, we will imput the mode (0)
location[idx.nas.location]= 0
summary(location) # min= 0; max= 1617.0

## remove outliers
## Assuming zero is an outlier, we will look to the distribution of the
##  remainder of the numbers.
remove= c(0)
location= location[!(location %in% remove)]
location.out= boxplot.stats(location)$out
location.no.out.idx= !(location %in% location.out)
location.no.out= location[location.no.out.idx]
summary(location.no.out) # max= 1173 (~99% of the values)
hist(location.no.out,
  breaks= seq(round(min(location.no.out)) - 1, round(max(location.no.out))),
  xlab= "Location",
  main= "Histogram of location"
)
boxplot(location.no.out, outline= F, yaxt= "n")
axis(2, las= 2)
mtext(
  side= 3, line= 2, at= 1, cex= 1.2,
  expression(paste("Boxplot of location"))
)
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")
## Removing location zero and outliers above 1200, we verify that most of the
##  values are between 180 and 300. Since we do not have information about this
##  variable and it is irrelevant for our analysis, it will be removed.

# movieid.movies ----------------------------------------------------------
summary(movieid.movies) # min= 1; max= 66730
## Apparently, there are 66730 movies in the data set; there are no NAs.
## This is just the number ID of each movie. No relevant statistics to compute.
#save(movieid.movies, file= "../data/movieid.movies.rdata")
#load("../data/movieid.movies.rdata")

# movieid.ratings ---------------------------------------------------------
summary(movieid.ratings) # min= 1; max= 66730
## No NAs.
par(mfrow= c(1, 1), oma= c(0, 0, 0, 0))
hist(movieid.ratings,
  breaks= seq(round(min(movieid.ratings)) - 1, round(max(movieid.ratings)) + 1),
  xlab= "Movie ID (ratings)"
)
ggplot(data.frame(x= movieid.ratings)) +
  geom_histogram(
    aes(x= x),
    stat= "count",
    position= "dodge"
  ) +
  labs(x= "Movie ID (ratings)", y= "Total", 
       title= "Histogram of movieid.ratings"#, 
  ) +
  theme_bw()

## From the histogram, we can see that there are films with more ratings than
##  others. In particular, there is a movie with ID greater than 60000 with
##  around 35000 ratings.
#save(movieid.ratings, file= "../data/movieid.ratings.rdata")
#load("../data/movieid.ratings.rdata")

# rating ------------------------------------------------------------------
par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))

summary(rating) # min= 0.5; max= 5.0
var(rating) # 1.192404
skewness(rating) # -0.7054742
hist(rating,
  breaks= seq(round(min(rating)) -0.5, round(max(rating)) +0.5, by= 0.5),
  xlab= "Ratings"
)
boxplot(rating,
  yaxt= "n",
  main= "Boxplot of ratings"
)
axis(2, las= 2)
## The most common ratings are 3.0, 3.5, and 5.0.
##  The distribution of ratings is left skewed, which is expected in this kind
##  of data set, i.e., in a sufficient large data set of films, it is expected
##  that most of the ratings will lie above the medium value of the scale.
#save(rating, file= "../data/rating.rdata")
#load("../data/rating.rdata")



# remove unnecessary variables --------------------------------------------
profiles= select(profiles, -c(lastlogin, location, memberfor))

# remove NAs --------------------------------------------------------------
profiles$gender= gender
profiles= profiles %>%
  filter(! profiles$age %in% profiles$age[is.na(profiles$age)])
profiles= profiles %>% 
  filter(profiles$age %in% age.no.out)

#save(movies, file= "../data/movies.rdata")
#load("../data/movies.rdata")
#save(profiles, file= "../data/profiles.rdata")
#load("../data/profiles.rdata")
#save(ratings, file= "../data/ratings.rdata")
#load("../data/ratings.rdata")

# add extra rating columns ------------------------------------------------
## mean_ratings: mean value of ratings per user
## total_ratings: total number of ratings per user
user.ratings= ratings %>%
  group_by(userid) %>%
  summarise(
    mean.ratings.user= mean(rating),
    total.ratings.user= n()
  )
## join user.ratings with profiles
profiles.left= left_join(profiles, user.ratings, by= "userid")

profiles.inner= profiles %>% 
  inner_join(user.ratings, by= "userid")
sum(apply(profiles.inner, 1, anyNA)) # no NAs
## THIS IS THE JOIN WE WANT TO PERFORM!
save(profiles.inner, file= "../data/profiles.inner.rdata")
load("../data/profiles.inner.rdata")

profiles.outer= profiles %>% 
  full_join(user.ratings, by= "userid")

## replace NA values in `mean_ratings` and `total_ratings` by 0
profiles.left= profiles.left %>%
  mutate(mean_ratings= replace_na(mean_ratings, 0)) %>% 
  mutate(total_ratings= replace_na(total_ratings, 0))
rm(user.ratings)

## mean_ratings: mean value of ratings per film
## total_ratings: total number of ratings per movie
movie.ratings= ratings %>%
  group_by(movieid) %>%
  summarise(
    mean.ratings.movie= mean(rating),
    total.ratings.movie= n()
  )
## join movie.ratings with movies
movies.left= left_join(movies, movie.ratings, by= "movieid")

movies.inner= movies %>% 
  inner_join(movie.ratings, by= "movieid")
## THIS IS THE JOIN WE WANT TO PERFORM!
save(movies.inner, file= "../data/movies.inner.rdata")
load("../data/movies.inner.rdata")

## replace NA values in `mean_ratings` and `total_ratings` with 0
movies= movies %>%
  mutate(mean_ratings= replace_na(mean_ratings, 0)) %>% 
  mutate(total_ratings= replace_na(total_ratings, 0))
rm(movie.ratings)

## join movies, profiles, and ratings into flixster tibbles
flixster.inner= ratings %>% 
  inner_join(profiles.inner, by= "userid") %>% 
  inner_join(movies.inner, by= "movieid")
sum(apply(flixster.inner, 1, anyNA)) # no NAs, 6,131,346 rows
## THIS IS THE JOIN WE WANT TO PERFORM!
save(flixster.inner, file= "../data/flixster.inner.rdata")
load("../data/flixster.inner.rdata")

flixster.left= ratings %>%
  left_join(profiles, by= "userid") %>%
  left_join(movies, by= "movieid")
# 8140604 NAs, 8196077 rows

flixster.full= ratings %>% 
  full_join(profiles, by= "userid") %>% 
  full_join(movies, by= "movieid")
# 12093916 NAs, 8851973 rows

#flixster= flixster.inner %>% 
#  rename(
#    mean_ratings_user= mean_ratings.x,
#    mean_ratings_movie= mean_ratings.y,
#    total_ratings_user= total_ratings.x,
#    total_ratings_movie= total_ratings.y 
#  )

flixster= flixster.inner %>%
  select(
    userid, gender, age, total.ratings.user, mean.ratings.user,
    movieid, moviename, rating, date, mean.ratings.movie, total.ratings.movie
  )
save(flixster, file= "../data/flixster.rdata")
load("../data/flixster.rdata")

flixster.tiny= flixster %>% 
  select(
    userid,
    movieid,
    rating,
    date
  )
sum(apply(flixster.tiny, 1, anyNA)) # 0 NAs
save(flixster.tiny, file= "../data/flixster.tiny.rdata")
load("../data/flixster.tiny.rdata")

flixster.small= flixster %>% 
  select(
    userid, gender, age,
    movieid, moviename,
    rating, date
  )
sum(apply(flixster.small, 1, anyNA)) # 0 NAs
save(flixster.small, file= "../data/flixster.small.rdata")
load("../data/flixster.small.rdata")

#remove(movies, profiles, ratings)


# data exploration --------------------------------------------------------
# distribution of ratings per users
users= length(unique(flixster$userid))

flixster %>% 
  group_by(userid) %>% 
  summarise(n= n()) %>% 
  arrange(n) %>% 
  head()

users.less.10.ratings= flixster[flixster$total.ratings.user <= 10, ] %>% 
  group_by(userid) %>% 
  summarise(n= n()) %>% 
  nrow()

users.less.10.ratings / length(unique(flixster$userid))

users.more.1000.ratings= flixster[flixster$total.ratings.user >= 1000, ] %>% 
  group_by(userid) %>% 
  summarise(n= n()) %>% 
  nrow()

users.more.1000.ratings / length(unique(flixster$userid))

flixster %>%
  group_by(userid) %>% 
  summarise(n= n()) %>% 
ggplot() +
  geom_histogram(
    aes(n),
    color= "white",
    binwidth= 0.2
  ) +
  scale_x_log10() + 
  ggtitle("Distribution of ratings per users") + 
  xlab("Number of ratings") +
  ylab("Number of users") + 
  theme_bw()





# TESTING (to remove) -----------------------------------------------------
# ```{r}
# knitr::opts_chunk$set(echo= F)
# ```

#ggplot(data.frame(age= age.no.out)) +
#  geom_histogram(
#    aes(x= age),
#    stat= "count",
#    position= "dodge"
#  ) +
#  labs(x= "Age (yrs)", y= "Total", 
#       title= "Histogram of ages"#, 
#  ) +
#  theme_bw()



# idx.date= which(ratings$date == min(ratings$date)) # before: 2; now: 3571
# uids= ratings$userid[idx.date] %>% unique() # 220
# memberfor[unique(idx.date)]
# profiles[idx.date, ]
# profiles[profiles$userid == uids, ]
# idx= which(profiles$userid == uids)
# profiles[idx, ]
# ratings$date[idx.date]= profiles[idx, ]$memberfor
# summary(ratings$date)

# There are a significant number of _NA's_ in variables _gender_ (67529 or
# around 6.73% of total) and _age_ (255618 or around 25.49% of total).
# We adopt the aproach the replacement the empty values into a new value to
# treatmet of missing values. The empty values into variable _gender_ will be
# replaced for new value `Other`. For the empty values into _age_ variable will
# be set the value `0`.

## Setting 'gender' as factors and 'age' as integer
profiles <- profiles %>% mutate(gender = as.factor(gender)) %>% mutate(age = as.integer(age))

## remove NAs
#sum(is.na(movies.raw)) # 0
#sum(is.na(profiles.raw)) # 637096 (63.5% of rows contain NAs)
#sum(is.na(ratings.raw)) # 0
## profiles.raw= profiles.raw %>% na.omit()
# sum(is.na(profiles.raw)) # 0




# TO-DO -------------------------------------------------------------------
# [ ] Distribution of ratings per year
# [ ] Films with more ratings (top 10 or top 20)
# [ ] Number of ratings per user
# [ ] Avoid to remove users rich in information (if they have a lot of ratings)
# [ ] Create a common set of ratings/movies/users
# [ ] Remove html tags from moviename
# Create column in profiles with number of ratings per userid
# Create column with ratings/no.views
# Create column with ratings/movie
# Check if movies with low number of ratings were rated by active users or not
# Normalised version of ratings? I.e., remove the mean rating from each user.
# Weighted arithmetic mean of ratings with number of views? Maybe the previous
#  idea is better.
# Remove movies with low number of ratings? If yes, we have to justify our
#  criteria. Keep those that were made by active users.
# Popularity: has been seen, not been seen, now many times. 20 most viewed
#  movies.
# Popularity might be an important strategy during the Oscar's season.
# Recomendations: compare them with precision, recall, mean average
#  precision... (select two, confusion matrix) + ROC + AUC.
# Sliding window: train for three months and test; train for the next three
#  months and test, etc. I.e., the recommendations have to be updated regularly.
#  The predictions will be the recommendations, and we will check if the users
#  have seen the movies recommended by us or not. Hits: when the user saw our
#  recommendations; misses: we recommended but the users did not see them.
#  Test different sliding window strategies and indicate the most relevant: last
#  three months for the next month, or last six months for the next two months,
#  etc.
#  Note: the training data set (width of the window) has to be the same for all
#  strategies when comparing strategies in order not to benefit any of them.
# Link analysis between users and movies they have seen: age, gender, popularity
#  per age, popularity per gender (these are extra)



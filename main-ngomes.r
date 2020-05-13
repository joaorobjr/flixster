#####
# Practical Assignment of Advanced Topics in Data Science
# By Nuno Gomes (up199300242) and João Robson Júnior (up201)
# May 2020
#####


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
  '../data/ratings.timed.txt',
  sep= "\t",
  skipNul= T, # skip NULL values
  col.names= c("userid", "movieid", "rating", "date")
)
movies.orig= read.delim('../data/movie-names.txt')
profiles.orig= read.csv(
  '../data/profile.txt',
  skipNul= T,
  na.strings= c('N/A', '')
)


# working data frames
movies.raw=   data.frame(movies.orig)
profiles.raw= data.frame(profiles.orig)
ratings.raw=  data.frame(ratings.orig)


# remove time tag from columns
ratings.raw$date= ratings.raw$date %>% str_replace(' 00:00:00', "")
profiles.raw$memberfor = profiles.raw$memberfor %>% str_replace(' 00:00:00', "")


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


## remove profileview from profiles tibble
profiles= profiles %>% select(-"profileview")


## variables
age=             profiles$age
dates=           ratings$date
gender=          profiles$gender
lastlogin=       profiles$lastlogin
location=        profiles$location
memberfor=       profiles$memberfor # 
movieid.movies=  movies$movieid
movieid.ratings= ratings$movieid
moviename=       movies$moviename
rating=          ratings$rating
userid.ratings=  ratings$userid
userid.profiles= profiles$userid


## distributions and outliers
par(mfrow= c(1, 2), oma= c(0, 2, 3, 1))

### age
summary(age) # min= 12; max= 113; NAs= 255618
idx.nas.age= which(is.na(age)) # 25.5% of all age values
age= na.omit(age)
var(age) # 107.5378
skewness(age) # 2.444335
hist(age,
     breaks= seq(round(min(age))-1, round(max(age))+1, by= 1),
     xlab= "Age (yrs)")
boxplot(age, yaxt= 'n',
        main= "Boxplot of age")
axis(2, las= 2)
# remove outliers
age.out= boxplot.stats(age, coef= 4)$out
age.no.out.idx= !(age %in% age.out)
age.no.out= age[age.no.out.idx]
summary(age.no.out) # max= 71 (99.5% of values)
hist(age.no.out,
     breaks= seq(round(min(age.no.out))-1, round(max(age.no.out))+1, by= 1),
     xlab= "Age (yrs)",
     main= "Histogram of age")
boxplot(age.no.out, outline= F, yaxt= 'n')
axis(2, las= 2)
mtext(side= 3, line= 2, at= 1, cex= 1.2,
      expression(paste("Boxplot of age")))
mtext(side= 3, line= 1, at= 1, cex= 0.7, "Outliers removed")


#### correct all wrong dates, i.e., dates before 2006-01-20 (Flixster's founding
####  date)
date.flixster= as.Date("2006-01-20")

### memberfor
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
summary(users.ratings$memberfor) # min= 2006-01-20

### date
summary(dates) # min= 1941-12-07
idx.date.wrong= which(ratings$date < date.flixster) # 2998
sum(idx.date.wrong %in% idx.memberfor.wrong) # 35
# So, not all rating dates before date.flixster are in the profiles dates set.
summary(ratings$date[idx.date.wrong]) # min= 1941-12-07
ratings$date[idx.date.wrong]= date.flixster
summary(ratings$date) # min= 2006-01-20
dates= ratings$date



#idx.date= which(ratings$date == min(ratings$date)) # before: 2; now: 3571
#uids= ratings$userid[idx.date] %>% unique() # 220
#memberfor[unique(idx.date)]
#profiles[idx.date, ]
#profiles[profiles$userid == uids, ]
#idx= which(profiles$userid == uids)
#profiles[idx, ]
#ratings$date[idx.date]= profiles[idx, ]$memberfor
#summary(ratings$date)


## remove NAs
sum(is.na(movies.raw)) # 0
sum(is.na(profiles.raw)) # 637096 (63.5% of rows contain NAs)
sum(is.na(ratings.raw)) # 0
#profiles.raw= profiles.raw %>% na.omit()
#sum(is.na(profiles.raw)) # 0





## to-do
# Avoid to remove users rich in information (if they have a lot of ratings)
# Create a common set of ratings/movies/users
# Remove html tags from moviename
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
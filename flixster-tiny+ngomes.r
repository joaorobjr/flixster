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
require(funModeling)
require(igraph)
require(lubridate)
require(plotrix)
require(recommenderlab)
require(reshape2)
require(stringr)
require(text2vec)
require(tidyverse)
require(tm)



# load data ---------------------------------------------------------------

load("../data/flixster.u50m500.rdata")
flixster.go= flixster.u50m500 %>%
  select(userid, movieid, moviename, rating)
# tibble: 4,752,926 x 4
save(flixster.go, file= "../data/flixster.go.rdata")
load("../data/flixster.go.rdata")



### Rating matrix non-normalised
### binary + non-binary
### ibcf, ubcf, popular
### ROC + precision-recall



# build recommendation models ---------------------------------------------
rec.mod.bin=   recommenderRegistry$get_entries(dataType= "binaryRatingMatrix")
rec.mod.nobin= recommenderRegistry$get_entries(dataType= "realRatingMatrix")


# rating matrices ---------------------------------------------------------
FLIXST= flixster.go
TOPN= 10 # number of recommendations to each user

ratingmat.go.nobin= dcast(
  FLIXST,
  userid ~ movieid,
  value.var= "rating"
  )
ratingmat.go.nobin= as.matrix(ratingmat.go.nobin)

# convert rating matrix to recommenderlab sparse matrix
ratingmat.go.nobin= as(ratingmat.go.nobin, "realRatingMatrix")

# create binary rating matrix
ratingmat.go.bin= binarize(ratingmat.go.nobin, minRating= 0.5)

save(ratingmat.go.nobin, file= "../data/ratingmat.go.nobin.rdata")
save(ratingmat.go.bin, file= "../data/ratingmat.go.bin.rdata")



# train/test split --------------------------------------------------------
set.seed(42)
idx_train_test= sample(
  c(TRUE, FALSE),
  size= nrow(ratingmat),
  replace= T,
  prob= c(0.7, 0.3)
)

train.go.nobin= ratingmat.go.nobin[  idx_train_test, ]
test.go.nobin=  ratingmat.go.nobin[! idx_train_test, ]

train.go.bin= ratingmat.go.bin[  idx_train_test, ]
test.go.bin=  ratingmat.go.bin[! idx_train_test, ]

save(train.go.nobin, file= "../data/train.go.nobin.rdata")
save(test.go.nobin, file= "../data/test.go.nobin.rdata")
save(train.go.bin, file= "../data/train.go.bin.rdata")
save(test.go.bin, file= "../data/test.go.bin.rdata")



# ibcf (binary approach) --------------------------------------------------
## item-based collaborative filtering model
rec.mod.bin$IBCF_binaryRatingMatrix$parameters
rec.mod.go.ibcf.bin= Recommender(
  data= train.go.bin,
  method= "IBCF"
)
rec.mod.go.ibcf.bin.pred= predict(
  object= rec.mod.go.ibcf.bin,
  newdata= test.go.bin,
  n= TOPN
)

# recommendations for the first user
rec.mod.go.ibcf.bin.user.1= rec.mod.go.ibcf.bin.pred@items[[1]]
movies.go.ibcf.bin.user.1=
  rec.mod.go.ibcf.bin.pred@itemLabels[rec.mod.go.ibcf.bin.user.1]
movies.rec.mod.go.ibcf.bin= movies.go.ibcf.bin.user.1
for (i in 1:TOPN) {
  movies.rec.mod.go.ibcf.bin[i]= as.character(
    subset(
      FLIXST,
      FLIXST$movieid == movies.go.ibcf.bin.user.1[i])$moviename
    )
}
movies.rec.mod.go.ibcf.bin
save(
  movies.rec.mod.go.ibcf.bin,
  file= "../data/movies.rec.mod.go.ibcf.bin.rdata"
)



# ubcf (binary approach) -------------------------------------------------
## user-based collaborative filtering model
rec.mod.bin$UBCF_binaryRatingMatrix$parameters
rec.mod.go.ubcf.bin= Recommender(
  data= train.go.bin,
  method= "UBCF"
)
rec.mod.go.ubcf.bin.pred= predict(
  object= rec.mod.go.ubcf.bin,
  newdata= test.go.bin,
  n= TOPN
)

# recommendations for the first user
rec.mod.go.ubcf.bin.user.1= rec.mod.go.ubcf.bin.pred@items[[1]]
movies.go.ubcf.bin.user.1=
  rec.mod.go.ubcf.bin.pred@itemLabels[rec.mod.go.ubcf.bin.user.1]
movies.rec.mod.go.ubcf.bin= movies.go.ubcf.bin.user.1
for (i in 1:TOPN) {
  movies.rec.mod.go.ubcf.bin[i]= as.character(
    subset(
      FLIXST,
      FLIXST$movieid == movies.go.ubcf.bin.user.1[i])$moviename
    )
}
movies.rec.mod.go.ubcf.bin
save(
  movies.rec.mod.go.ubcf.bin,
  file= "../data/movies.rec.mod.go.ubcf.bin.rdata"
)



# popular (binary approach) ----------------------------------------------
## popular recommendation method recommends the popular films to users;
## the popularity of a film is determined by the number or ratings received
rec.mod.bin$POPULAR_binaryRatingMatrix$parameters
rec.mod.go.pop.bin= Recommender(
  data= train.go.bin,
  method= "POPULAR"
)
rec.mod.go.pop.bin.pred= predict(
  object= rec.mod.go.pop.bin,
  newdata= test.go.bin,
  n= TOPN
)

# recommendations for the first user
rec.mod.go.pop.bin.user.1= rec.mod.go.pop.bin.pred@items[[1]]
movies.go.pop.bin.user.1=
  rec.mod.go.pop.bin.pred@itemLabels[rec.mod.go.pop.bin.user.1]
movies.rec.mod.go.pop.bin= movies.go.pop.bin.user.1
for (i in 1:TOPN) {
  movies.rec.mod.go.pop.bin[i]= as.character(
    subset(
      FLIXST,
      FLIXST$movieid == movies.go.pop.bin.user.1[i])$moviename
    )
}
movies.rec.mod.go.pop.bin
save(
  movies.rec.mod.go.pop.bin,
  file= "../data/movies.rec.mod.go.pop.bin.rdata"
)



# ibcf (non-binary approach) ---------------------------------------------
## Item-based collaborative filtering, a recommender method with non-
## normalised data, with distance between items computed using cosine
## similarity.
rec.mod.nobin$IBCF_realRatingMatrix$parameters
rec.mod.go.ibcf.nobin= Recommender(
  data= train.go.nobin,
  method= "IBCF"
)
rec.mod.go.ibcf.nobin.pred= predict(
  object= rec.mod.go.ibcf.nobin,
  newdata= test.go.nobin,
  n= TOPN
)

# recommendations for the first user
rec.mod.go.ibcf.nobin.user.1= rec.mod.go.ibcf.nobin.pred@items[[1]]
movies.go.ibcf.nobin.user.1=
  rec.mod.go.ibcf.nobin.pred@itemLabels[rec.mod.go.ibcf.nobin.user.1]
movies.rec.mod.go.ibcf.nobin= movies.go.ibcf.nobin.user.1
for (i in 1:TOPN) {
  movies.rec.mod.go.ibcf.nobin[i]= as.character(
    subset(
      FLIXST,
      FLIXST$movieid == movies.go.ibcf.nobin.user.1[i])$moviename
    )
}
movies.rec.mod.go.ibcf.nobin
save(
  movies.rec.mod.go.ibcf.nobin,
  file= "../data/movies.rec.mod.go.ibcf.nobin.rdata"
)



# ubcf (non-binary approach) -------------------------------------------------
## user-based collaborative filtering model
rec.mod.nobin$UBCF_realRatingMatrix$parameters
rec.mod.go.ubcf.nobin= Recommender(
  data= train.go.nobin,
  method= "UBCF"
)
rec.mod.go.ubcf.nobin.pred= predict(
  object= rec.mod.go.ubcf.nobin,
  newdata= test.go.nobin,
  n= TOPN
)

# recommendations for the first user
rec.mod.go.ubcf.nobin.user.1= rec.mod.go.ubcf.nobin.pred@items[[1]]
movies.go.ubcf.nobin.user.1=
  rec.mod.go.ubcf.nobin.pred@itemLabels[rec.mod.go.ubcf.nobin.user.1]
movies.rec.mod.go.ubcf.nobin= movies.go.ubcf.nobin.user.1
for (i in 1:TOPN) {
  movies.rec.mod.go.ubcf.nobin[i]= as.character(
    subset(
      FLIXST,
      FLIXST$movieid == movies.go.ubcf.nobin.user.1[i])$moviename
    )
}
movies.rec.mod.go.ubcf.nobin
save(
  movies.rec.mod.go.ubcf.nobin,
  file= "../data/movies.rec.mod.go.ubcf.nobin.rdata"
)



# popular (non-binary approach) -------------------------------------------
rec.mod.nobin$POPULAR_realRatingMatrix$parameters
rec.mod.go.pop.nobin= Recommender(
  data= train.go.nobin,
  method= "POPULAR"
)
rec.mod.go.pop.nobin.pred= predict(
  object= rec.mod.go.pop.nobin,
  newdata= test.go.nobin,
  n= TOPN
)

# recommendations for the first user
rec.mod.go.pop.nobin.user.1= rec.mod.go.pop.nobin.pred@items[[1]]
movies.go.pop.nobin.user.1=
  rec.mod.go.pop.nobin.pred@itemLabels[rec.mod.go.pop.nobin.user.1]
movies.rec.mod.go.pop.nobin= movies.go.pop.nobin.user.1
for (i in 1:TOPN) {
  movies.rec.mod.go.pop.nobin[i]= as.character(
    subset(
      FLIXST,
      FLIXST$movieid == movies.go.pop.nobin.user.1[i])$moviename
    )
}
movies.rec.mod.go.pop.nobin
save(
  movies.rec.mod.go.pop.nobin,
  file= "../data/movies.rec.mod.go.pop.nobin.rdata"
)



# save models and recommendations -----------------------------------------
save(
  rec.mod.bin,
  ratingmat.go.bin,
  train.go.bin, test.go.bin,
  rec.mod.go.ibcf.bin, rec.mod.go.ibcf.bin.pred, movies.rec.mod.go.ibcf.bin,
  rec.mod.go.ubcf.bin, rec.mod.go.ubcf.bin.pred, movies.rec.mod.go.ubcf.bin,
  rec.mod.go.pop.bin, rec.mod.go.pop.bin.pred, movies.rec.mod.go.pop.bin,
  file= "../data/recommender-system-binary-tiny.rdata"
)

save(
  rec.mod.nobin,
  ratingmat.go.nobin,
  train.go.nobin, test.go.nobin,
  rec.mod.go.ibcf.nobin,
  rec.mod.go.ibcf.nobin.pred,
  movies.rec.mod.go.ibcf.nobin,
  rec.mod.go.ubcf.nobin,
  rec.mod.go.ubcf.nobin.pred,
  movies.rec.mod.go.ubcf.nobin,
  rec.mod.go.pop.nobin,
  rec.mod.go.pop.nobin.pred,
  movies.rec.mod.go.pop.nobin,
  file= "../data/recommender-system-no-binary-tiny.rdata"
)



# compare models ----------------------------------------------------------
models= list(
  ibcf= list(name= "IBCF"),
  ubcf= list(name= "UBCF"),
  pop= list(name= "Popular")
)

TOP.N= c(1, 2, 5, seq(10, 100, 10)) # top N recommendations
ITEMS= 1 # number of films to generate recommendations
RAT.THRESH= 3 # minimum rating to be considered a good option
N.FOLDS= 4 # number of folds/samples to run evaluation to (for the CV)

## select most relevant data
## rowCounts(r) ## number of ratings per user
## colCounts(r) ## number of ratings per item
## colMeans(r) ## average item rating
ratings.go.bin= ratingmat.go.bin[
  rowCounts(ratingmat.go.bin) > 50,
  colCounts(ratingmat.go.bin) > 500
]

ratings.go.nobin= ratingmat.go.nobin[
  rowCounts(ratingmat) > 50,
  colCounts(ratingmat) > 500
]



# compare binary models ---------------------------------------------------
eval.go.bin= evaluationScheme(data= ratings.go.bin,
                              method= "cross-validation",
                              k= N.FOLDS,
                              given= ITEMS,
                              goodRating= RAT.THRESH)

results.go.bin= evaluate(x= eval.go.bin,
                         method= models,
                         n= TOP.N )
results.go.bin

evals.go.bin.size= sapply(eval.go.bin@runsTrain, length)
evals.go.bin.size

# training data for the runs
eval.go.bin.train= getData(eval.go.bin, "train")

# known ratins used for prediction for test data
eval.go.bin.known= getData(eval.go.bin, "known")

# ratings used for evaluation for test data
eval.go.bin.unknown= getData(eval.go.bin, "unknown")

eval.go.bin.train.df= data.frame(
  x= rowCounts(eval.go.bin.train),
  type= "train"
)
eval.go.bin.known.df= data.frame(
  x= rowCounts(eval.go.bin.known),
  type= "known"
)
eval.go.bin.unknown.df= data.frame(
  x= rowCounts(eval.go.bin.unknown),
  type= "unknown"
)

evals.go.bin.all= rbind(
  eval.go.bin.train.df,
  eval.go.bin.known.df,
  eval.go.bin.unknown.df
)
evals.go.bin= rbind(
  eval.go.bin.train.df,
  eval.go.bin.unknown.df
)

ggplot(
  evals.go.bin,
  aes(x= x, fill= type)
) +
  geom_histogram(
    binwidth= 20,
    alpha= 0.5
  ) +
  labs(
    x= "Number of ratings per row",
    y= "Count",
    title= "Data used for the runs"
  ) +
  theme_bw()


# confusion matrices
cm.go.ibcf.bin= getConfusionMatrix(results.go.bin$ibcf)
cm.go.ubcf.bin= getConfusionMatrix(results.go.bin$ubcf)
cm.go.pop.bin=  getConfusionMatrix(results.go.bin$pop)

# condense results of all folds
columns.to.sum.go= c("TP", "FP", "FN", "TN", "precision", "recall", "TPR", "FPR")
cm.go.sum.ibcf.bin= Reduce(
  "+",
  getConfusionMatrix(results.go.bin$ibcf)
)[, columns.to.sum.go]
cm.go.sum.ubcf.bin= Reduce(
  "+",
  getConfusionMatrix(results.go.bin$ubcf)
)[, columns.to.sum.go]
cm.go.sum.pop.bin= Reduce(
  "+",
  getConfusionMatrix(results.go.bin$pop)
)[, columns.to.sum.go]


plot(
  results.go.bin,
  lw= 3,
  annotate= T,
  legend= "topleft",
  main= "ROC curve"
)

plot(
  results.go.bin,
  lw= 3,
  annotate= T,
  "prec/rec",
  legend= "topright",
  main= "Precision-Recall"
)

eval.go.rec.bin= Recommender(
  data= eval.go.bin.train,
  method= "IBCF",
  parameter= NULL
)

ITEMS.REC= 10

eval.go.pred.bin= predict(
  object= eval.go.rec.bin,
  newdata= eval.go.bin.known,
  n= ITEMS.REC,
  type= "ratings"
)

# distribution of ratings per user in the matrix of predictions
ggplot(data.frame(x= rowCounts(eval.go.pred.bin))) +
  geom_histogram(
    aes(x),
    bins= 20
  ) +
  labs(
    x= "Number of ratings per user",
    y= "Count",
    title= "Distribution of ratings per user"
  )

### THIS YIELDS AN ERROR, BECAUSE IT ONLY WORKS FOR THE NON-BINARY CASE
eval.go.accu= calcPredictionAccuracy(
  x= eval.go.pred.bin,
  data= eval.go.bin.unknown,
  byUser= F
)
### THIS YIELDS AN ERROR, BECAUSE IT ONLY WORKS FOR THE NON-BINARY CASE

save(
  eval.go.bin,
  results.go.bin,
  eval.go.bin.train, eval.go.bin.known, eval.go.bin.unknown,
  eval.go.rec.bin, eval.go.pred.bin,
  file= "../data/evals.go.binary-tiny.rdata"
)



# compare non-binary models -----------------------------------------------
eval.go.nobin= evaluationScheme(data= ratings.go.nobin,
                                method= "cross-validation",
                                k= N.FOLDS,
                                given= ITEMS,
                                goodRating= RAT.THRESH)

results.go.nobin= evaluate(x= eval.go.nobin,
                           method= models,
                           n= TOP.N )
results.go.nobin

evals.go.nobin.size= sapply(eval.go.nobin@runsTrain, length)
evals.go.nobin.size

# training data for the runs
eval.go.nobin.train= getData(eval.go.nobin, "train")

# known ratins used for prediction for test data
eval.go.nobin.known= getData(eval.go.nobin, "known")

# ratings used for evaluation for test data
eval.go.nobin.unknown= getData(eval.go.nobin, "unknown")

eval.go.nobin.train.df= data.frame(
  x= rowCounts(eval.go.nobin.train),
  type= "train"
)
eval.go.nobin.known.df= data.frame(
  x= rowCounts(eval.go.nobin.known),
  type= "known"
)
eval.go.nobin.unknown.df= data.frame(
  x= rowCounts(eval.go.nobin.unknown),
  type= "unknown"
)

evals.go.nobin.all= rbind(
  eval.go.nobin.train.df,
  eval.go.nobin.known.df,
  eval.go.nobin.unknown.df
)
evals.go.nobin= rbind(
  eval.go.nobin.train.df,
  eval.go.nobin.unknown.df
)

ggplot(
  evals.go.nobin,
  aes(x= x, fill= type)
) +
  geom_histogram(
    binwidth= 20,
    alpha= 0.5
  ) +
  labs(
    x= "Number of ratings per row",
    y= "Count",
    title= "Data used for the runs"
  ) +
  theme_bw()


# confusion matrices
cm.go.ibcf.nobin= getConfusionMatrix(results.go.nobin$ibcf)
cm.go.ubcf.nobin= getConfusionMatrix(results.go.nobin$ubcf)
cm.go.pop.nobin=  getConfusionMatrix(results.go.nobin$pop)

# condense results of all folds
columns.to.sum.go= c(
  "TP", "FP", "FN", "TN", "precision", "recall", "TPR", "FPR"
)
cm.go.sum.ibcf.nobin= Reduce(
  "+",
  getConfusionMatrix(results.go.nobin$ibcf)
)[, columns.to.sum.go]
cm.go.sum.ubcf.nobin= Reduce(
  "+",
  getConfusionMatrix(results.go.nobin$ubcf)
)[, columns.to.sum.go]
cm.go.sum.pop.nobin= Reduce(
  "+",
  getConfusionMatrix(results.go.nobin$pop)
)[, columns.to.sum.go]


plot(
  results.go.nobin,
  lwd= 3,
  annotate= T,
  legend= "topleft",
  main= "ROC curve"
)

plot(
  results.go.nobin$pop,
  lwd= 3,
  col= "green",
  annotate= T,
  "prec/rec",
  #legend= "topright",
  main= "Precision-Recall"
)

eval.go.rec.nobin= Recommender(
  data= eval.go.nobin.train,
  method= "IBCF",
  parameter= NULL
)

ITEMS.REC= 10

eval.go.pred.nobin= predict(
  object= eval.go.rec.nobin,
  newdata= eval.go.nobin.known,
  n= ITEMS.REC,
  type= "ratings"
)

# distribution of ratings per user in the matrix of predictions
ggplot(data.frame(x= rowCounts(eval.go.pred.nobin))) +
  geom_histogram(
    aes(x),
    bins= 20
  ) +
  labs(
    x= "Number of ratings per user",
    y= "Count",
    title= "Distribution of ratings per user"
  )

### THIS YIELDS ONLY NaNs
eval.go.accu= calcPredictionAccuracy(
  x= eval.go.pred.nobin,
  data= eval.go.nobin.unknown,
  byUser= F
)
### THIS YIELDS ONLY NaNs

save(
  eval.go.nobin,
  results.go.nobin,
  eval.go.nobin.train, eval.go.nobin.known, eval.go.nobin.unknown,
  eval.go.rec.nobin, eval.go.pred.nobin,
  eval.go.accu,
  file= "../data/evals.go.non-binary-tiny.rdata"
)



# apriori -----------------------------------------------------------------
## We are now going to build a recommendation system based on an association-
## rule mining technique, by applying the apriori algorithm.
## Given the data set, the goal is to find the films that are associated with
## each other. This will give us the opportunity to understand associations
## between the movies.

## Before applying the Apriori algorithm, we need to transform the data set to
## binary values, where 1 represents a positive ratings, and 0 represents a
## negative rating or no rating. We do that by means of the `binarize()`
## function from the *recommenderlab* package.

# binarize into a binaryRatingMatrix with all 3+ rating as 1
rating.apr= binarize(ratingmat.go.nobin, minRating= 3)
class(rating.apr)

## The Apriori algorithm expects a matrix as input rather than an object of
## class "binaryRatingMatrix".

# convert rating.apriori to matrix format
rating.mat.apr= as(rating.apr, "matrix")
str(rating.mat.apr)

# convert cells of rating.mat.apr to 1 and 0
rating.mat.apr.num= 1*rating.mat.apr
str(rating.mat.apr.num)
save(rating.mat.apr.num, file= "../data/rating.mat.apr.num.rdata")

## We need to pass the parameters support and confidence to the algorithm.
## The support tells us the fraction of transactions that contain an itemset.
## It measures how often a rule should occur in the data set.
## The confidence measures the strength of the rule.
## We used a support of 0.5 and a confidence of 0.8.

params.apr= list(
  supp= 0.3,
  conf= 0.7
)

# create rules
rules= apriori(data= rating.mat.apr.num, parameter= params.apr)

## The `rules` object has all the film associations that were extracted and
## mined from the data set. Thirteen movies association rules were extracted, in
## total.

arules::inspect(rules)

# example: {30936} => {45119}
a= flixster.go %>%
  filter(movieid == 30936) %>%
  select(moviename) %>%
  unique()
b= flixster.go %>%
  filter(movieid == 45119) %>%
  select(moviename) %>%
  unique()
cat(a$moviename, '=>', b$moviename)


rules.df= as(rules, "data.frame")
rules.df[order(-rules.df$lift, -rules.df$confidence), ]

TOPN= 10 # number of recommendations to each user

ratingmat.go.nobin= as.matrix(ratingmat.go.nobin)

# convert rating matrix to recommenderlab sparse matrix
ratingmat.go.nobin= as(ratingmat.go.nobin, "realRatingMatrix")

# create binary rating matrix
ratingmat.go.bin= binarize(ratingmat.go.nobin, minRating= 0.5)


# EXP 01 ===
flixster.apr= flixster.go %>% select(userid, movieid)

rating.mat.apr= dcast(
  flixster.go,
  userid ~ movieid,
  value.var= "rating"
  )

rating.mat= as(flixster.go, "matrix")
rating.mat= as(rating.mat, "realRatingMatrix")
rating.mat= binarize(rating.mat, minRating= 3)
model.apr= Recommender(
  data= rating.mat,
  method= "AR",
  param= list(supp= 0.5, conf= 0.8)
)
getModel(model.apr)
rules= getModel(model.apr)$rule_base
arules::inspect(rules)
#===

# EXP 02 ===
flixster.apr= flixster.go %>% select(userid, movieid, rating)

fap= flixster.go %>%
  mutate(rating= cut(
    rating, 4,
    labels= c("very-bad", "bad", "good", "very-good")
  )
)

fap= fap %>% mutate_if(is.numeric, as.factor)
fap= fap %>% mutate_if(is.character, as.factor)
fapt= as(fap, "transactions")
item.fapt= itemInfo(fapt)
head(item.fapt)
rules= apriori(fapt)

items= subset(item.fapt, variables %in% c("userid"))$labels
head(items)
rules= apriori(fapt,
               parameter= list(conf= 0.6, minlen= 2), # 44 rules
               appearance= list(both= items,
                                default= "none")
              )

lhs.apr= subset(
  item.fapt,
  variables %in% c("movieid", "rating"))$labels
rhs.apr= subset(item.fapt, variables == "moviename")$labels
rules1= apriori(
  fapt,
  parameter= list(
    confidence= 0.15,
    minlen= 1,
    support= 0.05
  ),
  appearance= list(
    lhs= lhs.apr,
    rhs= rhs.apr,
    default= "none"
  )
)
arules::inspect(rules1)





rating.mat.apr= dcast(
  flixster.apr,
  userid ~ movieid,
  value.var= "rating"
  )

rating.mat= as(rating.mat.apr, "matrix")
rating.mat= binarize(rating.mat.apr, minRating= 3)
rating.mat= as(rating.mat.apr, "realRatingMatrix")
model.apr= Recommender(
  data= rating.mat,
  method= "AR",
  param= list(supp= 0.5, conf= 0.8)
)
getModel(model.apr)
rules= getModel(model.apr)$rule_base
arules::inspect(rules)
#===

fap1= flixster.go %>% mutate_if(is.numeric, as.factor)
fap1= fap1 %>% mutate_if(is.character, as.factor)
fapt1= binarize(as.matrix(fap1), minRating= 3)
rules= apriori(
  as.matrix(
    fap1,
    parameter= list(support= 0.2, confidence= 0.5, minlen=5)
  )
)




#===
user.item.matrix= as(
  as.data.frame(split(
    flixster.go[, "movieid"],
    flixster.go[, "userid"]
  )),
  "transactions"
)

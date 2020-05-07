######################################################
### Recommender Lab
### Nikita Goswami
### Created: 5/25/2020
###
######################################################
rm(list = ls())
setwd("~/R workingDir/Recommender Systems/")

install.packages("recommenderlab")
install.packages("ggplot2")
library(recommenderlab)
library(ggplot2)

data(MovieLense)


dim(getRatingMatrix(MovieLense)) #943 1664
getRatingMatrix(MovieLense)[1:10, 1:10]

# Normalization of the MovieLense matrix
MovieLense_Normalize <- normalize(MovieLense)
head(MovieLense_Normalize)


vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings)
table_ratings <- table(vector_ratings)
table_ratings
X11()
barplot(table_ratings, main="Distribution of Ratings")
vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings)
table_ratings <- table(vector_ratings)
table_ratings
X11()
barplot(table_ratings, main="Distribution of Ratings")



#################################################
## Visualize raw ratings and normalized ratings
#################################################
X11()
image(MovieLense_Normalize[1:100,1:100], 
main = "Normalized ratings")


X11()
image(MovieLense[1:100, 1:100], main = "Raw Ratings")

getRatingMatrix(MovieLense_Normalize)[1:10, 1:10]


 #de-normalize
R_denormalize <- denormalize(MovieLense_Normalize)

# Create a Binary Matrix
MovieLense_binarize <- binarize(R_denormalize, minRating = 4)
getRatingMatrix(MovieLense_binarize)

X11()
image(MovieLense_binarize[1:100,1:100], main = "Binarized ratings")


# Visualize the ratings in the form of a histogram
X11()
hist(getRatings(MovieLense_Normalize), breaks = 100, main = "Histogram of normalized ratings")

X11()
hist(rowCounts(MovieLense_Normalize), breaks = 100, main = "ratings given by users")

 
######################################
## Create a recommender system
######################################
?recommenderRegistry
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
lapply(recommender_models, "[[", "description")
 # We will use UBCF
recommender_models$UBCF_realRatingMatrix$parameters
# The parameters for UBCF are : method = cosine, nn=25, sample=false and normalize= center

# Exploring Similarity Data
similarity_users <- similarity(MovieLense[1:4, ], 
                               method = "cosine", 
                               which = "users",min_matching=5)
as.matrix(similarity_users)
X11()
image(as.matrix(similarity_users), main = "User similarity")

######################################################
# User based collaborative Filtering
######################################################
# Building the recommendation Engine

# Divide dataset into train and test
which_train <- sample(x = c(TRUE, FALSE), 
                      size = nrow(ratings_movies),
                      replace = TRUE, 
                      prob = c(0.8, 0.2))
#head(which_train)
train <- MovieLense[which_train, ]
test <- MovieLense[!which_train, ]

recommender_models <- recommenderRegistry$get_entries(dataType ="realRatingMatrix")
recommender_models$UBCF_realRatingMatrix$parameters
############## Experiment 1
# The default parameters for UBCF are 
# similarity method = cosine
# nn = 25
# sample = FALSE
# normalize = center
UBCFrecc_model1 <- Recommender(data = train, method = "UBCF")
UBCFrecc_model1
UBCFmodel_details1 <- getModel(UBCFrecc_model1)
names(UBCFmodel_details1)
UBCFmodel_details1$data

# Applying the recommender model to predict ratings for users
# pred_model1 <- predict(object = UBCFrecc_model1, MovieLense[1:2], type="ratings")
pred_model1 <- predict(object = UBCFrecc_model1, test, type="ratings")
pred_model1
pred = as(pred_model1, "matrix")[1:5,1:5]
as( MovieLense[1:2], "matrix")[,1:5]


##########################################################
## Use different parameters for UBCF
###########################################################
recommenderRegistry$get_entry("UBCF", dataType="realRatingMatrix")
recom <- Recommender(train, method = "UBCF",
                      parameter = list(method = "cosine", nn = 10, normalize = "center"))
recom
as(recom, "matrix")[,1:5]
as(recom, "list")



recom <- Recommender(train, method = "UBCF",
                     parameter = list(method = "k-nearest_neighbors", nn = 10, normalize = "center"))
recom


#####################################################
## Testing the performance of recommender system
#####################################################

# Define Test and Train set
eval <- evaluationScheme(MovieLense,method = "split", given = 15, train=0.5, goodRating=4)
eval

##########################################################
### Building a recommender model using user based collaborative filtering
#######################################################
userbased_model<- Recommender(getData(eval,"train"), "UBCF")
userbased_model


P1<- predict(userbased_model, getData(eval, "known"), type="ratings")


################################################################################
### calculating the error between prediction and the unknown part of test set
################################################################################
?calcPredictionAccuracy
ERROR<- rbind(UBCF = calcPredictionAccuracy(P1, getData(eval,"unknown")))
ERROR
# RMSE      MSE       MAE
# UBCF 1.060867 1.125439 0.8420556


#################################################################################
### evaluation of top-N recommender algorithm using the Given-3 protocol
###i.e, for the test users all but 3 are withheld for evaluation.
#################################################################################
scheme<- evaluationScheme(MovieLense, method="cross",k=4, given=3, goodRating=4) ##?
scheme

results<- evaluate(scheme, method = "POPULAR", type="topNList", n=c(1,3,5,10,15,20))
results
getConfusionMatrix(results)[[1]]

##################################################################
### Plotting the ROC curve
#####################################################################
x11()
plot(results, annotate=TRUE)

##################################################################
###precision and recall plot
#####################################################################
x11()
plot(results, "prec/rec", annotate=TRUE)
graphics.off()



######################################################
## Experimentation with Model parameters
######################################################

set.seed(42) # What other seed is there!
movie <- evaluationScheme(MovieLense, method = "split", train = .8, given = 5, goodRating = 3)
movie

# 1. Neighbourhood Size
user_nn <- list(
  "5 NN" = list(name="UBCF", param=list(normalize = "Z-score",
                                         method="Cosine",
                                         nn=5)),
  "10 NN" = list(name="UBCF", param=list(normalize = "Z-score",
                                         method="Cosine",
                                         nn=10)),
  "20 NN" = list(name="UBCF", param=list(normalize = "Z-score",
                                         method="Cosine",
                                         nn=20)),
  "30 NN" = list(name="UBCF", param=list(normalize = "Z-score",
                                         method="Cosine",
                                         nn=30)),
  "40 NN" = list(name="UBCF", param=list(normalize = "Z-score",
                                         method="Cosine",
                                         nn=40)),
  "50 NN" = list(name="UBCF", param=list(normalize = "Z-score",
                                         method="Cosine",
                                         nn=50))
)

# Running the recommendation system and predicting n movies for evaluation
recs <- c(1,5, 10, 15, 20, 25)
user_nn_results <- evaluate(movie, user_nn, n = recs, progress = FALSE)

# Drawing the ROC plot
X11()
plot(x = user_nn_results, y = "ROC", annotate = 4, legend="topleft", main = "Z-score normalizarion and cosine distance")


# Draw the precision / recall curve
X11()
plot(x = user_nn_results, y = "prec/rec", annotate = 5)

#calculating RMSE with neighbor = 40
model <- Recommender(getData(movie, "train"), method = "UBCF", 
                     param=list(normalize = "Z-Score", method="Cosine", nn=40))
prediction <- predict(model, getData(movie, "known"), type="ratings")
rmse_ubcf <- calcPredictionAccuracy(prediction, getData(movie, "unknown"))[1]
rmse_ubcf


# 2. Normalization - Mean or Z-score

norm <- list(
  "Center" = list(name="UBCF", param=list(normalize = "center",
                                          method="Cosine",
                                          nn=40)),
  "Z-score" = list(name="UBCF", param=list(normalize = "Z-score",
                                           method="Cosine",
                                           nn=40))
)

norm_results <- evaluate(movie, norm, n = recs, progress = FALSE)

X11()
plot(x = norm_results, y = "ROC", legend="topleft")
X11()
plot(x = norm_results, y = "prec/rec", annotate = 1)


# 3. Distance Methods - Pearson, Cosine and Jaccard
dist <- list(
  "Pearsons" = list(name="UBCF", param=list(normalize = "z-score",
                                            method="pearson",
                                            nn=40)),
  "Cosine" = list(name="UBCF", param=list(normalize = "Z-score",
                                          method="Cosine",
                                          nn=40)),
  "Jaccard" = list(name="UBCF", param=list(normalize = "Z-score",
                                           method="jaccard",
                                           nn=40))
)

distresults <- evaluate(movie, dist, n = recs, progress = FALSE)

X11()
plot(x=distresults, y = "ROC", annotate = 3, legend="topleft")
X11()
plot(x =distresults, y = "prec/rec", annotate = c(1,3))

# Calculating RMSe with pearson distance measure
model <- Recommender(getData(movie, "train"), method = "UBCF", 
                     param=list(normalize = "Z-Score", method="Pearson", nn=40))
prediction <- predict(model, getData(movie, "known"), type="ratings")
rmse_ubcf <- calcPredictionAccuracy(prediction, getData(movie, "unknown"))[1]
rmse_ubcf



####################################################
## Test the performance of the Recommender System using hold-out or cross-validation approach
####################################################

# Setting evaluation scheme
eval_sets <- evaluationScheme(data = MovieLense, 
                              method = "cross-validation",
                              k = 5, 
                              given = 15, 
                              goodRating = 4)


model_to_evaluate <- "UBCF"
# Setting model parameters
model_parameters <- list(normalize = "Z-Score", method="Pearson", nn=40)
eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate, 
                                parameter = model_parameters)

items_to_recommend <- 10
eval_prediction <- predict(object = eval_recommender, 
                           newdata = getData(eval_sets, "known"), 
                           n = items_to_recommend, 
                           type = "ratings")
qplot(rowCounts(eval_prediction)) + 
  geom_histogram(binwidth = 10) +
  ggtitle("Distribution of movies per user")


eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, 
                                        data = getData(eval_sets, "unknown"), 
                                        byUser = TRUE)


X11()

qplot(eval_accuracy[, "RMSE"]) + 
  geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")



# Evaluating the Recommendations

results <- evaluate(x = eval_sets, 
                    method = model_to_evaluate, 
                    n = seq(10, 100, 10))
head(getConfusionMatrix(results)[[1]])

columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed)

X11()
plot(results, annotate = TRUE, main = "ROC curve")
X11()
plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")
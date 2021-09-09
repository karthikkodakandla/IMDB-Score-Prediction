# Include all libraries
library(ggplot2)

movie.df<-read.csv("movie_metadata.csv",header=TRUE)

dim(movie.df)
options(scipen=999)
#There are in total 5043 records in the dataset  with 28 different variables.
str(movie.df)
t(t(colnames(movie.df)))
movie.num <- movie.df[,c(3:6,8,9,13,14,16,19,23,25,26,27,28)]
movie.cat <-movie.df[,-c(3:6,8,9,13,14,16,19,23,25,26,27,28)]
summary(movie.cat)
data.frame(mean=round(sapply(movie.num, mean, na.rm = TRUE),0), 
           min=round(sapply(movie.num, min,na.rm = TRUE),0), 
           max=round(sapply(movie.num, max,na.rm = TRUE),0), 
           median=round(sapply(movie.num, median,na.rm = TRUE),0), 
           miss.val=sapply(movie.num, function(x) 
             sum(length(which(is.na(x))))))

#checking for duplicate rows if any
sum(duplicated(movie.df))
#there are 45 rows which are duplicate we are omitting the duplicate rows.
movie.df <- movie.df[!duplicated(movie.df), ]
dim(movie.df)

#Analysis of variables
plot(movie.df$language,xlab = "Language", ylab="Movies", main ="By Language", ylim =c(0,5000))
#Only English is most viewed language among all languages. We can ignore this variable as it is not useful for our anlaysis
plot(movie.df$color, xlab ="Type of Color", ylab ="Movies", main = "By Color",ylim =c(0,5000))
plot(movie.df$content_rating,xlab ="Content Rating", ylab ="Movies", main = "By Content Rating",ylim =c(0,5000))
plot(movie.df$country, xlab ="Country", ylab ="Movies", main = "By Country",ylim =c(0,5000))

data.for.plot <- aggregate(movie.df$imdb_score, by = list(movie.df$content_rating), FUN = sum)
data.for.plot
summary(movie.df$content_rating)
summary(movie.df$country)
levels(movie.df$content_rating)[levels(movie.df$content_rating)%in%c('Approved','Passed','G','TV-G')] <- 'G'
levels(movie.df$content_rating)[levels(movie.df$content_rating)%in%c('PG','PG-13','TV-PG','GP','TV-Y','TV-Y7','TV-14')] <- 'PG'
levels(movie.df$content_rating)[levels(movie.df$content_rating)%in%c('NC-17','R','X','M','TV-MA')] <- 'NC-17'
levels(movie.df$content_rating)[levels(movie.df$content_rating)%in%c('Unrated','Not Rated','')] <- 'Not_Rated'
######################
t((t(colnames(movie.df))))
movie1.df<-movie.df[,c(3,4,5,6,8,9,13,14,19,21,22,23,25,26,27,28)]
movie1.df[movie1.df==""]<-NA
data.frame(miss.val=sapply(movie.df,function(x) 
  sum(length(which(is.na(x))))))
heatmap(1 * is.na(movie1.df), Rowv = NA, Colv = NA)


movie1.df<-movie1.df[!is.na(movie1.df$gross),]
movie1.df<-movie1.df[!is.na(movie1.df$budget),]
data.for.plot <- aggregate(movie.df$aspect_ratio, by = list(movie.df$title_year), FUN = mean,na.rm=TRUE)
movie1.df$aspect_ratio[is.na(movie1.df$aspect_ratio)] <- median(movie1.df$aspect_ratio, na.rm =TRUE)
movie1.df<-movie1.df[!is.na(movie1.df$actor_3_facebook_likes),]
movie1.df<-movie1.df[!is.na(movie1.df$content_rating),]
movie1.df<-movie1.df[!is.na(movie1.df$num_critic_for_reviews),]
data.frame(miss.val=sapply(movie1.df,function(x) 
  sum(length(which(is.na(x))))))
dim(movie1.df)# final dimension of the datset
summary(movie1.df$country)
movie1.df$rating <- .bincode(movie1.df$imdb_score, c(0,5,7,10))
movie1.df$rating <-as.factor(movie1.df$rating)
levels(movie1.df$rating)<-list("low"=1,"medium"=2,"high"=3)
summary(movie1.df$rating)
levels(movie1.df$country)[-c(5,12,21,23,63,65)] <-NA
droplevels(movie1.df$country)
movie1.df<-movie1.df[!is.na(movie1.df$country),]
############################################################
summary(movie1.df)

library(reshape) 

# use melt() to stack a set of columns into a single column of data.
# stack Price values for each combination of (binned) Age and Fuel Type
mlt <- melt(movie1.df, id=c("content_rating", "rating"), measure=c("gross"))
head(mlt, 5)

library(ggplot2)
p <- ggplot(data = mlt, aes(x = content_rating, y = value, fill = rating))
p <- p + geom_bar(stat = "identity", width = 0.5, position = "dodge")
p <- p + theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 90))
p

# use cast() to reshape data and generate pivot table
cast(mlt, content_rating~ rating, subset=variable=="gross", 
     margins=c("grand_row", "grand_col"), mean)


summary(movie1.df[,c(1,2,6,7,9,12,14,15)])

## simple panel of scatterplots
library(GGally)
t(t(colnames(movie1.df)))
ggpairs(movie1.df[,c(1,2,6,7,9,12,14)])
names(movie1.df)<-make.names(names(movie1.df),unique = TRUE)
str(movie1.df)

#
#Correlation
library(ggcorrplot)
round(cor(movie1.df[,c(1:9,12:16)]),2)
#observations:
#Actor 1 Face book likes and Cast total facebook likes are highly correlated
#Num of user reviews and Num of voted users are highly correlated
# gross and budget also collinear with each other. As the currencies in budget differ we are removing budget
# we can use either one of the variable and delete the other variable
# we are removing cast total fb likes and Num of user for reviews
movie1.df <- movie1.df[,-c(8,9)]
movie1.df <-movie1.df[,-10]
str(movie1.df)
library(corrplot)
correlations <- round(cor(movie1.df[,c(1:7,10:13)]),2)
corrplot(correlations, method="circle")
ggcorrplot(corr,  type = "upper", outline.col = "white", lab = TRUE)

###########################################################
set.seed(211)
train.rows<-sample(rownames(movie1.df), dim(movie1.df)[1]*0.5)
valid.rows<-sample(setdiff(rownames(movie1.df), train.rows), dim(movie1.df)[1]*0.3)
test.rows<-setdiff(rownames(movie1.df), union(train.rows, valid.rows))
train.data<-movie1.df[train.rows,];valid.data<-movie1.df[valid.rows,];test.data<-movie1.df[test.rows,]
dim(train.data)
dim(valid.data)
dim(test.data)
str(train.data)

###############################Linear Regression##########################################
# for Linear Regression Response variable will be IMDB_SCORE so we are removing rating
mvr <- lm(imdb_score ~.,data=train.data[,-14])
options(scipen=999) # avoid scientific notation
summary(mvr)
#Adj R square is 0.3445
accuracy(mvr$fitted.values,train.data$imdb_score)
summary(mvr$fitted.values)
#                               ME      RMSE       MAE       MPE     MAPE
#Test set -0.00000000000000001008085 0.8412661 0.6415981 -2.475292 11.41797

#######################################################
library(forecast)
pred.valid.mvr <- predict(mvr, newdata = valid.data[,-14])
valid.res <- data.frame(valid.data$imdb_score, pred.valid.mvr, residuals = 
                          valid.data$imdb_score - pred.valid.mvr)
head(valid.res)
hist(valid.res$residuals)
plot(valid.res$residuals)
sd(valid.res$residuals)
mean(valid.res$residuals)
summary(valid.res)
# compute accuracy on validation data
accuracy(pred.valid.mvr, valid.data$imdb_score)

#                 ME      RMSE       MAE       MPE     MAPE
#Test set 0.04968232 0.8546566 0.6549334 -1.803692 11.58426


#on test data
pred.test.mvr <- predict(mvr, newdata = test.data[,-14])
test.res <- data.frame(test.data$imdb_score, pred.test.mvr, residuals = 
                         test.data$imdb_score - pred.test.mvr)
# compute accuracy on test data
accuracy(pred.test.mvr, test.data$imdb_score)
#                 ME      RMSE       MAE       MPE     MAPE
#Test set 0.04191114 0.8586888 0.660845 -1.912273 11.73256
###########################################To pick best model########################################
# use regsubsets() in package leaps to run an exhaustive search. 
# unlike with lm, categorical predictors must be turned into dummies manually. May not be needed in newer versions
library(leaps)
str(train.data)
search <- regsubsets(imdb_score ~ ., data = train.data[,-14], nbest = 1, nvmax = dim(train.data[,-14])[2],
                     method = "exhaustive")
sum <- summary(search)
# show models
sum$which
# show metrics
sum$rsq #Bad criteria to use. R-square always increases with number of variables
sum$adjr2
sum$cp
########################################################################
#Removed movie fb likes,aspect ratio, actor 3, actor 1 fb likes
mvrmod2 <- lm(imdb_score ~ num_critic_for_reviews + duration +
                num_voted_users +country + content_rating + gross , data = train.data[,-14])
options(scipen=999) # avoid scientific notation
summary(mvrmod2)
#Adj R squared : 0.3388
str(valid.data)
pred.valid.mvrmod2 <- predict(mvrmod2, newdata = valid.data[,-14])
valid.res.mod2 <- data.frame(valid.data$imdb_score, pred.valid.mvrmod2, residuals = 
                               valid.data$imdb_score - pred.valid.mvrmod2)
hist(valid.res.mod2$residuals)
plot(valid.res.mod2$residuals)
accuracy(pred.valid.mvrmod2, valid.data$imdb_score)

# Some change in values here

#                 ME      RMSE       MAE       MPE     MAPE
#Test set 0.04920471 0.8611151 0.6608178 -1.844395 11.68123

################################## New movie Prediction ######################################3
mvrmod3 <- lm(imdb_score ~ duration  +  actor_1_facebook_likes +
                director_facebook_likes + 
                country + num_voted_users +
                content_rating , data = train.data[,-14])
options(scipen=999) # avoid scientific notation
summary(mvrmod3)
#Adj R squared : 0.3485
pred.valid.mvrmod3 <- predict(mvrmod3, newdata = valid.data[,-14])
valid.res.mod3 <- data.frame(valid.data$imdb_score, pred.valid.mvrmod3, residuals = 
                               valid.data$imdb_score - pred.valid.mvrmod3)
accuracy(pred.valid.mvrmod3, valid.data$imdb_score)
hist(valid.res.mod3$residuals)
plot(valid.res.mod3$residuals)
#                 ME     RMSE       MAE       MPE     MAPE
#Test set 0.05124116 0.8610606 0.6564395 -1.821529 11.61765
#Based on Parsimonus model selected from exhaustive search

############################Neural networks ############################
#scaling to 0 and 1 scale for train,vaid and test data
train1.data <- dummy.data.frame(train.data[,-14], sep =".")
train_nn.data<-as.data.frame(apply(train1.data,2,rescale))

valid1.data <- dummy.data.frame(valid.data[,-14], sep =".")
valid_nn.data<-as.data.frame(apply(valid1.data,2,rescale))

test1.data <- dummy.data.frame(test.data[,-14], sep =".")
test_nn.data<-as.data.frame(apply(test1.data,2,rescale))

str(train.data)
str(train_nn.data)
################################################################################
names(train_nn.data)<-make.names(names(train_nn.data),unique = TRUE)
names(valid_nn.data)<-make.names(names(valid_nn.data),unique = TRUE)
names(test_nn.data)<-make.names(names(test_nn.data),unique = TRUE)

library(neuralnet)

#----------------------------------------------------------------------------------

# Creating network using 1 hidden layer with 2 nodes

movie_nn<- neuralnet(imdb_score ~., data = train_nn.data,linear.output=TRUE, hidden = 2)
movie_nn$weights

# plot network
plot(movie_nn, rep="best")
# display predictions
pred.train<-compute(movie_nn,train_nn.data)
##residuals of training data##########
train.res<-data.frame(train_nn.data$imdb_score,pred.train$net.result,
                      residuals=train_nn.data$imdb_score-pred.train$net.result)
head(train.res)

##residuals of validation data#################
pred.valid <- compute(movie_nn,  valid_nn.data)
valid.res <- data.frame(valid_nn.data$imdb_score, pred.valid$net.result,
                        residuals = valid_nn.data$imdb_score - pred.valid$net.result)
head(valid.res)
mean(valid.res$residuals)
hist(valid.res$residuals)

##residuals of test data#################
pred.test <- compute(movie_nn,  test_nn.data)
test.res <- data.frame(test_nn.data$imdb_score, pred.test$net.result,
                       residuals = test_nn.data$imdb_score - pred.test$net.result)
head(test.res)
mean(test.res$residuals)
hist(test.res$residuals)

#### Comparing the accuracy

library(forecast)
library(caret)

## Accuracy on the training data
pred.train.score <- pred.train$net.result*(max(train.data$imdb_score)-min(train.data$imdb_score))+min(train.data$imdb_score)
train.score <- (train_nn.data$imdb_score)*(max(train.data$imdb_score)-min(train.data$imdb_score))+min(train.data$imdb_score)
#head(data.frame(train_nn.data$imdb_score,train.score))
### RMSE on training data
library(ModelMetrics)
rmse(train.score,pred.train.score)

#head(train.data$imdb_score)
#head(train.score)

## Accuracy on validation data
pred.valid.score <- pred.valid$net.result*(max(valid.data$imdb_score)-min(valid.data$imdb_score))+min(valid.data$imdb_score)
valid.score <- (valid_nn.data$imdb_score)*(max(valid.data$imdb_score)-min(valid.data$imdb_score))+min(valid.data$imdb_score)
head(data.frame(pred.valid.score,valid.score))
### RMSE on validation data
rmse(valid.score,pred.valid.score)
#0.7937869

### MAE on validation data
mae(valid.score,pred.valid.score)
# 0.5861806


# Old
# RMSE: 0.7442035
# MAE: 0.5596211

## Accuracy on test data
pred.test.score <- pred.test$net.result*(max(test.data$imdb_score)-min(test.data$imdb_score))+min(test.data$imdb_score)
test.score <- (test_nn.data$imdb_score)*(max(test.data$imdb_score)-min(test.data$imdb_score))+min(test.data$imdb_score)
head(data.frame(pred.test.score,test.score))
### RMSE on test data
rmse(test.score,pred.test.score)
# RMSE: 0.7801108

### MAE on test data
mae(test.score,pred.test.score)
# MAE: 0.5866157

#old
# RMSE: 0.7932276
# MAE: 0.5958997

#---------------------------------------------------------------------------------

# Creating network using 1 hidden layer with 4 nodes

movienn_1_4<- neuralnet(imdb_score ~ ., data = train_nn.data, linear.output = T, hidden = 4)
movienn_1_4$weights

# plot network
plot(movienn_1_4, rep="best")

pred1_4.train<-compute(movienn_1_4,train_nn.data)
pred1_4.valid<-compute(movienn_1_4,valid_nn.data)
pred1_4.test<-compute(movienn_1_4,test_nn.data)

## Accuracy on the training data
pred1_4.train.score <- pred1_4.train$net.result*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
train1_4.score <- (train_nn.data$imdb_score)*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
###RMSE on train data
#rmse(train1_4.score,pred1_4.train.score)
# RMSE: 0.6988877

## Accuracy on validation data

pred1_4.valid.score <- pred1_4.valid$net.result*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
valid1_4.score <- (valid_nn.data$imdb_score)*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)

head(data.frame(valid_nn.data$imdb_score,valid1_4.score))

##RMSE on validation data 
rmse(valid1_4.score,pred1_4.valid.score)
mae(valid1_4.score,pred1_4.valid.score)

# RMSE: 0.786133
# MAE: 0.5748493



#Old
# RMSE: 0.7618439
# MAE:  0.5716382

## Accuracy on test data
pred1_4.test.score <- pred1_4.test$net.result*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
test1_4.score <- (test_nn.data$imdb_score)*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)

head(data.frame(test_nn.data$imdb_score,test1_4.score))

##RMSE on test data 
rmse(test1_4.score,pred1_4.test.score)
mae(test1_4.score,pred1_4.test.score)

# RMSE: 0.7688729
# MAE: 0.5737517


#OLd
# RMSE: 0.8347657
# MAE:  0.6092861

#----------------------------------------------------------------------------------------------------------

# Creating network using 2 hidden layers with 4 nodes each

movienn_2_4<- neuralnet(imdb_score ~ ., data = train_nn.data, linear.output = T, hidden = c(4,4))
movienn_2_4$weights

# plot network
plot(movienn_2_4, rep="best")

pred2_4.train<-compute(movienn_2_4,train_nn.data)
pred2_4.valid<-compute(movienn_2_4,valid_nn.data)
pred2_4.test<-compute(movienn_2_4,test_nn.data)
## Accuracy on the training data
pred2_4.train.score <- pred2_4.train$net.result*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
train2_4.score <- (train_nn.data$imdb_score)*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
###RMSE on train data
rmse(train2_4.score,pred2_4.train.score)

## Accuracy on validation data
pred2_4.valid.score <- pred2_4.valid$net.result*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
valid2_4.score <- (valid_nn.data$imdb_score)*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
##RMSE on validation data 
rmse(valid2_4.score,pred2_4.valid.score)
mae(valid2_4.score,pred2_4.valid.score)

# Accuracy from network with 2 hidden layers and 4 nodes each.
# RMSE: 0.7820409
# MAE: 0.5655248


#Old:
# RMSE: 0.7243578
# MAE:  0.5488857

## Accuracy on test data
pred2_4.test.score <- pred2_4.test$net.result*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
test2_4.score <- (test_nn.data$imdb_score)*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
##RMSE on test data 
rmse(test2_4.score,pred2_4.test.score)
mae(test2_4.score,pred2_4.test.score)

# Accuracy on test data from network with 2 hidden layers and 4 nodes each.
# RMSE: 0.7729614
# MAE:  0.567897

#OLd
# RMSE: 0.7925559
# MAE:  0.5898617


#---------------------------------------------------------------------------------

# Creating network using 3 hidden layers with 4 nodes each

movienn_4_4_4<- neuralnet(imdb_score ~ ., data = train_nn.data, linear.output = T, hidden = c(4,4,4))
movienn_4_4_4$weights

# plot network
plot(movienn_4_4_4, rep="best")

pred4_4_4.train<-compute(movienn_4_4_4,train_nn.data)
pred4_4_4.valid<-compute(movienn_4_4_4,valid_nn.data)
pred4_4_4.test<- compute(movienn_4_4_4,test_nn.data)
## Accuracy on the training data
pred4_4_4.train.score <- pred4_4_4.train$net.result*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
train4_4_4.score <- (train_nn.data$imdb_score)*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
#RMSE on train data
rmse(train4_4_4.score,pred4_4_4.train.score)

## Accuracy on validation data
pred4_4_4.valid.score <- pred4_4_4.valid$net.result*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
valid4_4_4.score <- (valid_nn.data$imdb_score)*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
##RMSE on validation data 
rmse(valid4_4_4.score,pred4_4_4.valid.score)
mae(valid4_4_4.score,pred4_4_4.valid.score)

# Accuracy from network with 3 hidden layers and 4 nodes each.
# RMSE: 0.9754159
# MAE:  0.6311466


## Accuracy on test data
pred4_4_4.test.score <- pred4_4_4.test$net.result*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)
test4_4_4.score <- (test_nn.data$imdb_score)*(max(movie3.df$imdb_score)-min(movie3.df$imdb_score))+min(movie3.df$imdb_score)

head(data.frame(test_nn.data$imdb_score,test4_4_4.score))

##RMSE on test data 
rmse(test4_4_4.score,pred4_4_4.test.score)
mae(test4_4_4.score,pred4_4_4.test.score)

# RMSE: 0.8105099
# MAE:  0.6066616

###########################################################################

# scaling
#library(caret)
#norm.values <- preProcess(train.data[, ], method=c("center", "scale"))
#head(norm.values)
#train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
#head(train.norm.df)
##Similarly scale valid data, mower data and the new data
#valid.norm.df <- predict(norm.values, valid.df[, 1:2])
#mower.norm.df <- predict(norm.values, mower.df[, 1:2])
#new.norm.df <- predict(norm.values, new.df[, 1:2])
#head(mower.norm.df)

############################### Common data###############################
###########################################################
#classification using Logit model
#install.packages("nnet")
library(nnet)
str(movie1.df)
train1.data <-train.data[,-c(12,14,17)]
valid1.data <-valid.data[,-c(12,14,17)]
test1.data <-test.data[,-c(12,14,17)]

train.data$rating<- relevel(train.data$rating, ref = "low")
#train1.data$rating<- relevel(train1.data$rating, ref = 3)
multinom.fit <- multinom(rating ~ ., data = train1.data) 
summary(multinom.fit)
exp(coef(multinom.fit))
head(probability.table <- fitted(multinom.fit))
# Predicting the values for valid dataset
pred1 <- predict(multinom.fit, newdata = valid1.data[,-c(15)])
# first 5 actual and predicted records from valid data
#data.frame(actual.rating = valid.data$rating[1:5], predicted.rating = valid.data$pred[1:5])
#valid.data$rating <-as.numeric(valid.data$rating)
#valid.data$pred <-as.numeric( valid.data$pred)
#bankgain <- gains(valid.data$rating, valid.data$pred, groups=10)
#bankgain
#str(valid.data)

library(caret)
confusionMatrix(pred1,valid1.data$rating)
testpred <- predict(multinom.fit, newdata = test1.data)
confusionMatrix(testpred,test1.data$rating)


############################################

# finding best model using glmulti

library(glmulti)
library(caret)
movie.glmulti<-glmulti(multinom.fit,name = "glmulti.analysis", intercept = TRUE, 
                      marginality = FALSE, bunch=5, level = 1,method = "h", crit = "aic")

library(glmulti)
#str(train1.data)
#prednames <- c("num_critic_for_reviews","duration","director_facebook_likes",
 #              "actor_1_facebook_likes","gross","num_voted_users","num_user_for_reviews",
  #             "content_rating","movie_facebook_likes")
#g1 <- glmulti("multinom.fit",xr= prednames,data=train1.data,level=1, fitfunction = "multinom")

#View(train1.data)

summary(valid1.data$rating)
#After 1050 models:
#Best model: rating~1+content_rating+duration+director_facebook_likes+
#  actor_1_facebook_likes+gross+num_voted_users+num_user_for_reviews+movie_facebook_likes

multinom1.fit <- multinom(rating ~ duration + director_facebook_likes + actor_1_facebook_likes + 
                            gross + num_voted_users + cast_total_facebook_likes + num_user_for_reviews + 
                            country + content_rating + actor_2_facebook_likes + aspect_ratio + 
                            movie_facebook_likes, data = train1.data)
summary(multinom1.fit)
pred2 <- predict(multinom1.fit, newdata = valid1.data[,-c(15)])
confusionMatrix(pred2,valid1.data$rating)

####################################################################
library(caret)
# use step() to run stepwise regression.
movie.lm.step <- step(multinom.fit, direction = "both")
summary(movie.lm.step)  # Which variables were dropped/added?
movie.lm.step.pred <- predict(movie.lm.step, valid1.data)
confusionMatrix(movie.lm.step.pred, valid1.data$rating)

######################################################################

#################Logit for predictors that are available before movie release
multinom1.fit <- multinom(rating ~ duration + director_facebook_likes + actor_1_facebook_likes + 
                             num_voted_users + cast_total_facebook_likes +  
                            country + content_rating +  
                            movie_facebook_likes, data = train1.data)
summary(multinom1.fit)
pred2 <- predict(multinom1.fit, newdata = valid1.data[,-c(15)])
confusionMatrix(pred2,valid1.data$rating)


#for test data
pred3 <- predict(multinom1.fit, newdata = test1.data[,-c(15)])
confusionMatrix(pred3,test1.data$rating)
#knn for all predictors

str(train1.data)
#10,11 and 15 are factors
knntrain.norm <- train1.data
knnvalid.norm <-valid1.data
knntest.norm<-test1.data

# for variables 

library(caret)
knnnorm.values <- preProcess(train1.data[,c(1:9,12:14)], method=c("center", "scale"))
head(knnnorm.values)
knntrain.norm[,c(1:9,12:14)] <- predict(knnnorm.values, train1.data[,c(1:9,12:14)])
head(knntrain.norm)
##Similarly scale valid data, mower data and the new data
knnvalid.norm[,c(1:9,12:14)] <- predict(knnnorm.values, valid1.data[,c(1:9,12:14)])
knntest.norm[,c(1:9,12:14)] <- predict(knnnorm.values, test1.data[,c(1:9,12:14)])
#new.norm.df <- predict(norm.values, new.df[, 1:2])
#head(mower.norm.df)
str(knntrain.norm)
knntrain.norm<-dummy.data.frame(knntrain.norm[,c(1:14)])
knnvalid.norm<-dummy.data.frame(knnvalid.norm[,c(1:14)])
#2,3,5,7,8,10,11
# knn() is available in library FNN (provides a list of the nearest neighbors) 
library(FNN)
movieknn <- knn(train = knntrain.norm[,c(1:14)], test = knnvalid.norm[,1:14], 
                 cl = train1.data[,15], k = 3) #we are predicting the thrid column (the last input in cl) using k=3

#row.names(train1.data)[attr(Mowersknn, "nn.index")]##Display the row names
#train.df[attr(Mowersknn, "nn.index"),]

summary(movieknn) #The final predicted class

confusionMatrix(movieknn, valid1.data[, 15])

# initialize a data frame with two columns: k, and accuracy.
movieaccuracy.df <- data.frame(k = seq(1, 50, 1), accuracy = rep(0, 50)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
movieaccuracy.df

# compute knn for different k on validation.
for(i in 1:50) {
  #Use knn function with k=i and predict for valid dataset
  movieknn.pred <- knn(train = knntrain.norm[,c(1:14)], test = knnvalid.norm[,1:14], 
                       cl = train1.data[,15], k = i)
  movieaccuracy.df[i, 2] <- confusionMatrix(movieknn.pred, valid1.data[, 15])$overall[1] 
}

movieaccuracy.df
# k -10 and accuracy is 66% for valid data



#####################################KNN with variables available for new data


knntrain1.norm <- train1.data[,c(2,3,5,7,8,10,11,14,15)]
str(knntrain1.norm)
knnvalid1.norm <-valid1.data[,c(2,3,5,7,8,10,11,14,15)]
knntest1.norm<-test1.data[,c(2,3,5,7,8,10,11,14,15)]
names(new1.norm)<-make.names(names(new1.norm),unique = TRUE)


# for variables 

library(caret)
knnnorm1.values <- preProcess(knntrain1.norm[,c(1:5,8)], method=c("center", "scale"))
knntrain1.norm[,c(1:5,8)] <- predict(knnnorm1.values, knntrain1.norm[,c(1:5,8)])
str(knntrain1.norm)
##Similarly scale valid data, mower data and the new data
knnvalid1.norm[,c(1:5,8)] <- predict(knnnorm1.values, knnvalid1.norm[,c(1:5,8)])
knntest1.norm[,c(1:5,8)] <- predict(knnnorm1.values, knntest1.norm[,c(1:5,8)])

#new1.norm[,c(1:5,8)]  <- predict(knnnorm1.values, new.data[,c(1:5,8)])
#new1.norm
#dim(knnnorm1.values)
#new1.norm[,c(1:5,8)]
#new.norm.df <- predict(norm.values, new.df[, 1:2])
#head(mower.norm.df)

knntrain2.norm<-dummy.data.frame(knntrain1.norm[,c(1:8)])
knnvalid2.norm<-dummy.data.frame(knnvalid1.norm[,c(1:8)])
knntest2.norm<-dummy.data.frame(knntest1.norm[,c(1:8)])
#2,3,5,7,8,10,11
# knn() is available in library FNN (provides a list of the nearest neighbors) 
library(FNN)
movieknn1 <- knn(train = knntrain2.norm, test = knnvalid2.norm, 
                cl = knntrain1.norm[,9], k = 3) #we are predicting the thrid column (the last input in cl) using k=3

#row.names(train1.data)[attr(Mowersknn, "nn.index")]##Display the row names
#train.df[attr(Mowersknn, "nn.index"),]

summary(movieknn1) #The final predicted class

confusionMatrix(movieknn1, knnvalid1.norm[,9])

# initialize a data frame with two columns: k, and accuracy.
movieaccuracy1.df <- data.frame(k = seq(1, 50, 1), accuracy = rep(0, 50)) #rep just repeats a value (0 in this case) 14 times. We are just initiating accuracy
movieaccuracy1.df

# compute knn for different k on validation.
for(i in 1:50) {
  #Use knn function with k=i and predict for valid dataset
  movieknn1.pred <- knn(train = knntrain2.norm, test = knnvalid2.norm, 
                        cl = knntrain1.norm[,9], k = i)
  movieaccuracy1.df[i, 2] <- confusionMatrix(movieknn1.pred, knnvalid1.norm[, 9])$overall[1] 
}

movieaccuracy1.df

movieknn3.pred <- knn(train = knntrain2.norm, test = knntest2.norm, 
                      cl = knntrain1.norm[,9], k = 20)
confusionMatrix(movieknn3.pred, knntest1.norm[,9])

str(knntrain1.norm)

#k =20 for new movie available variables accuracy is 63%
movieknn3.pred <- knn(train = train.norm.df[, 1:2], test = valid.norm.df[, 1:2], 
                      cl = train.norm.df[, 3], k = 3)
confusionMatrix(Mowerknn3.pred, valid.df[, 3])

#using neural networks

# predicting based on continuous variable
trainnew.data <- train1.data[,c(2,3,5,7,8,10,11,14,15)]
validnew.data <- valid1.data[,c(2,3,5,7,8,10,11,14,15)]
scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
trainnew.data[, c(1:5,8)] <- data.frame(lapply(trainnew.data[, c(1:5,8)], scl))
validnew.data[, c(1:5,8)] <- data.frame(lapply(validnew.data[, c(1:5,8)], scl))
summary(validnew.data)
train1nn <- dummy.data.frame(trainnew.data, sep=".")
valid1nn <-dummy.data.frame(validnew.data, sep=".")

library(neuralnet)
names(train1nn)<-make.names(names(train1nn),unique = TRUE)


#trainnn.data <- dummy.data.frame(trainnn.data, sep = ".")
movienewnn <- neuralnet(rating.low + rating.medium ~ .,data = train1nn, hidden =5)

# display predictions
prediction(movienewnn)

# plot network
plot(movienewnn, rep="best")
movienewnn$net.result
str(train1nn)
###################3
training.prediction=compute(movienewnn, train1nn[,-c(17,18,19)]) 
data.frame(training.prediction$net.result,train1nn[,c(17,18,19)])
round(training.prediction$net.result,2)
training.class=apply(training.prediction$net.result,2,which.max)-1 

training.class<-as.factor(training.class)
training.class
levels(training.class)<-c('low','medium','high')
confusionMatrix((as.factor(training.class)), movie1.df[train.rows,]$rating)
validation.prediction=compute(movienn, validnn.data[,-c(23,24,25)]) 
validation.class=apply(validation.prediction$net.result,1,which.max)-1
validation.class<-as.factor(validation.class)

levels(validation.class)<-c('low','medium','high')
confusionMatrix((as.factor(validation.class)), valid$rating)


#################



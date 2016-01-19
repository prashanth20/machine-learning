library(xgboost)
library(Matrix)
#library(reshape2)

cat("Setting the path and reading data \n")
setwd("~/Mutual/")
train <- read.csv("train.csv", stringsAsFactors = T, header = T)
test <- read.csv("test.csv", stringsAsFactors = T, header = T)

cat("doing some feature engineering \n")
# temp = train$T1_V1 + train$T2_V1
# train$T3_V1 = temp
# temp = train$T1_V2 + train$T2_V2
# train$T3_V2 = temp
# temp = train$T1_V10 + train$T2_V10
# train$T3_V10 = temp
# temp = train$T1_V14 + train$T2_V14
# train$T3_V14 = temp

# temp1 = test$T1_V1 + test$T2_V1
# test$T3_V1 = temp1
# temp1 = test$T1_V2 + test$T2_V2
# test$T3_V2 = temp1
# temp1 = test$T1_V10 + test$T2_V10
# test$T3_V10 = temp1
# temp1 = test$T1_V14 + test$T2_V14
# test$T3_V14 = temp1

train$T2_V6 = NULL
test$T2_V6 = NULL
train$T2_V4 = NULL
test$T2_V4 = NULL

#rf <- randomForest(train[,3:38], train$Hazard, ntree = 1000, imp = TRUE, sampsize = 10000, do.trace = 10)

cat("preparing for xgboost \n")
id_test <- test$Id
test$Id = NULL
id_train <- train$Id
train$Id = NULL
y_train = train$Hazard
train$Hazard = NULL

x_train = sparse.model.matrix(~., data = train)
x_test = sparse.model.matrix(~., data = test)

cat("Loading settings \n")
parameter = data.frame(eta = c(0.05, 0.04, 0.03, 0.05, 0.04, 0.03, 0.05, 0.04, 0.03, 0.05),
					   rounds = c(187, 187, 187, 187, 187, 187, 187, 187, 187, 187),
					   depth = c(8, 9, 9, 10, 10, 9, 8, 8, 9, 10),
					   colsample.bytree = c(0.7, 0.6, 0.65, 0.6, 0.85, 0.85, 0.6, 0.65, 0.6, 0.7),
					   subsample = c(1, 0.9, 0.95, 1, 0.6, 0.8, 0.9, 0.7, 1, 0.6));

# cv_parmeter = list("objective" = "reg:linear",
#                    "eta" = 0.05,
#                    "depth" = 9,
#                    "colsample.bytree" = 0.8) 

# xgboost.cv = xgb.cv(param = cv_parmeter, data = x_train, label = y_train, nfold = 10, nrounds = 500)

cat("Gonna build model now \n")
models <- 10
repeats <- 10
yhat.test  <- rep(0,nrow(x_test))
for (j in 1:repeats) {
  for (i in 1:models){
    set.seed(j * 3000 + i * 300)
    xgboost.mod <- xgboost(data = x_train, label = y_train, max.depth = parameter$depth[i], eta = parameter$eta[i],
                           nround = parameter$rounds[i], nthread = 5, objective = "reg:linear",
                           subsample = parameter$subsample[i], colsample.bytree = parameter$colsample.bytree[i], min.child.weight = 5, silent = 1)
    yhat.test  <- yhat.test + predict(xgboost.mod, x_test)
    gc()  
  }
}


yhat.test <-  yhat.test/(models*repeats)

cat("writing result \n")
write.csv(data.frame(Id = id_test, Hazard = round(yhat.test)), "R_xgboostCV2.csv", row.names = F, quote = FALSE)

#Adding T3_V1 scored 0.374599
#Adding T3_V2 scored 0.374707
# M <- cor(train[sapply(train, function(x) is.numeric(x))])
# corrplot(M, method = "number",order = "hclust",type='lower', diag=F, addCoefasPercent=T)

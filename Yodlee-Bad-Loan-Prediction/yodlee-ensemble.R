#Author : Prashant Hiremath

#load the data into file
cat("Reading the data")
train = read.csv("~/yodlee/Yodlee_Training.csv", header = T, stringsAsFactors = T, check.names = F)
test = read.csv("~/yodlee/Yodlee_Evaluation.csv", header = T, stringsAsFactors = T, check.names = F)

#check summary of the train and test data
summary(train)
summary(test)


# Make Loan status as factor
train$`Loan Status` = as.factor(train$`Loan Status`)

# Replace the interest rates with numeric value
train$`Interest Rate` = as.numeric(sub("%", "", train$`Interest Rate`))
test$`Interest Rate` = as.numeric(sub("%","", test$`Interest Rate`))

#hold the loan duration in numeric form for future calculation
train_loan_duration = as.numeric(sub("months", "", train$`Loan Duration`))
test_loan_duration = as.numeric(sub("months", "", test$`Loan Duration`))

#Get the loan issue year and create new variable with Loan_Issue_Year for both train and test.
issue_date_train = strptime(train$`Loan Issue Date`, "%d-%m-%Y")
train$Loan_Issue_Year = format(issue_date_train, "%Y")
train$Loan_Issue_Year = as.factor(train$Loan_Issue_Year)

issue_date_test = strptime(test$`Loan Issue Date`, "%d-%m-%Y")
test$Loan_Issue_Year = format(issue_date_test, "%Y")
test$Loan_Issue_Year = as.factor(test$Loan_Issue_Year)

# Get the Last Payment year and create new variable Last_Payment_Year for both train and test.
last_date_train = strptime(train$`Last Month Payment was Received`, "%d-%m-%Y")
train$Last_Payment_Year = format(last_date_train, "%Y")
train$Last_Payment_Year = as.factor(train$Last_Payment_Year)

last_date_test = strptime(test$`Last Month Payment was Received`, "%d-%m-%Y")
test$Last_Payment_Year = format(last_date_test, "%Y")
test$Last_Payment_Year = as.factor(test$Last_Payment_Year)

#Get all the missing columns
misssing_col_train = which(apply(train, 2, function(x) any(is.na(x))))
misssing_col_train = unname(misssing_col_train)

#initially just replace the missing columns with median and some dummy categorical variable
for (i in 1:length(misssing_col_train)) {
  if (is.numeric(train[,misssing_col_train[i]])) {
    median_val = median(train[,misssing_col_train[i]], na.rm = T)
    train[,misssing_col_train[i]] = ifelse(is.na(train[,misssing_col_train[i]]), median_val, train[,misssing_col_train[i]])
  }

  if (is.factor(train[,misssing_col_train[i]])) {
    train[,misssing_col_train[i]] = ifelse(is.na(train[,misssing_col_train[i]]), "Null", train[,misssing_col_train[i]])
  }
}

#Get all the missing columns
misssing_col_test = which(apply(test, 2, function(x) any(is.na(x))))
misssing_col_test = unname(misssing_col_test)

#initially just replace the missing columns with median and some dummy categorical variable
for (i in 1:length(misssing_col_test)) {
  if (is.numeric(test[,misssing_col_test[i]])) {
    median_val = median(test[,misssing_col_test[i]], na.rm = T)
    test[,misssing_col_test[i]] = ifelse(is.na(test[,misssing_col_test[i]]), median_val, test[,misssing_col_test[i]])
  }

  if (is.factor(test[,misssing_col_test[i]])) {
    test[,misssing_col_test[i]] = ifelse(is.na(test[,misssing_col_test[i]]), "Null", test[,misssing_col_test[i]])
  }
}

#Introduce simple interest as new attribute
train$Simple_Interest = (train$`Loan Amount` * train$`Interest Rate` * train_loan_duration) / (100 * 12)
test$Simple_Interest = (test$`Loan Amount` * test$`Interest Rate` * test_loan_duration) / (100 * 12)

#Introduce new attribute Total_Amount_To_Pay.
train$Total_Amount_To_Pay = train$EMI * train_loan_duration
test$Total_Amount_To_Pay = test$EMI * test_loan_duration

#Introduce new attribute Ratio of Total Amount to Loan Amount
train$Ratio_TP_LA = train$Total_Amount_To_Pay / train$`Loan Amount`
test$Ratio_TP_LA = test$Total_Amount_To_Pay / test$`Loan Amount`

# Introduce new attribute remaining months calculated from the Loan issue year and Loan last payment year,
# subtracting it with total loan duration.
train_last_payment_year = as.numeric(as.character(train$Last_Payment_Year))
train_issue_year = as.numeric(as.character(train$Loan_Issue_Year))
train_total_year = train_last_payment_year - train_issue_year
train$Remaining_Months = train_loan_duration - train_total_year * 12
train$Remaining_Months = as.factor(train$Remaining_Months)

test_last_payment_year = as.numeric(as.character(test$Last_Payment_Year))
test_issue_year = as.numeric(as.character(test$Loan_Issue_Year))
test_total_year = test_last_payment_year - test_issue_year
test$Remaining_Months = test_loan_duration - test_total_year * 12
test$Remaining_Months = as.factor(test$Remaining_Months)

# Introduce the attribute possible Loan completion Year.
train_loan_completion_year = as.numeric(as.character(train$Loan_Issue_Year)) + floor(train_loan_duration / 12)
train$Loan_Completion_Year = as.factor(train_loan_completion_year)

test_loan_completion_year = as.numeric(as.character(test$Loan_Issue_Year)) + floor(test_loan_duration / 12)
test$Loan_Completion_Year = as.factor(test_loan_completion_year)

# EMI and Loan Amount are highly correlated, lets keep both the variables but use randomForest and GBM's col_sample_rate
# to avoid multicollinearity.
# train$EMI = NULL
# train$EMI = NULL

# Remove Loan Status column in test with ???
test$`Loan Status` = NULL

train = train[c(1:25, 26, 28, 29, 30, 31, 32, 33, 34, 27)]
test = test[c(1:25, 26, 28, 29, 30, 31, 32, 33, 26)]

# Below code is used for internal validation to calculate out-of-bag error.
# train = train[sample(nrow(train)),]
# split = floor(nrow(train) * (3/4))
# training = train[1:split,]
# testing = train[(split + 1):nrow(train),]

training = train
testing = test

library(h2o)
library(h2oEnsemble)

local = h2o.init(nthreads = -1)

train.hex = as.h2o(training)
test.hex = as.h2o(testing)

yVars = colnames(training)[ncol(training)]
xVars = colnames(training)[2:(ncol(training) - 1)]

# Build the ensemble of the models using below 6 models.
h2o.randomForest.1 <- function(..., ntrees = 500, max_depth = 15) h2o.randomForest.wrapper(..., ntrees = ntrees, max_depth = max_depth)
h2o.randomForest.2 <- function(..., ntrees = 300, max_depth = 20) h2o.randomForest.wrapper(..., ntrees = ntrees, max_depth = max_depth)
h2o.gbm.1 <- function(..., ntrees = 500, max_depth = 10, min_rows = 5, learn_rate = 0.01, sample_rate = 0.6, col_sample_rate = 0.6) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, min_rows = min_rows, learn_rate = learn_rate, sample_rate = sample_rate, col_sample_rate = col_sample_rate)
h2o.gbm.2 <- function(..., ntrees = 300, max_depth = 15, min_rows = 7, learn_rate = 0.03, sample_rate = 0.7, col_sample_rate = 0.6) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, min_rows = min_rows, learn_rate = learn_rate, sample_rate = sample_rate, col_sample_rate = col_sample_rate)
h2o.deeplearning.1 <- function(..., activation = "Tanh", hidden = c(40, 40, 40), l1 = 1e-05, l2 = 1e-05, input_dropout_ratio = 0.7) h2o.deeplearning.wrapper(..., activation = activation, hidden = hidden, l1 = l1, l2 = l2, input_dropout_ratio = input_dropout_ratio)
h2o.deeplearning.2 <- function(..., activation = "Rectifier", hidden = c(50, 50, 50), l1 = 1e-05, l2 = 1e-05, input_dropout_ratio = 0.8) h2o.deeplearning.wrapper(..., activation = activation, hidden = hidden)


learner = c("h2o.randomForest.1", "h2o.randomForest.2", "h2o.gbm.1", "h2o.gbm.2", "h2o.deeplearning.1", "h2o.deeplearning.2", "h2o.glm.wrapper")
metalearner = "h2o.gbm.wrapper"
family <- "binomial"

fit <- h2o.ensemble(x = xVars, y = yVars,
                    training_frame = train.hex,
                    learner = learner,
                    family = family,
                    metalearner = metalearner,
                    cvControl = list(V = 5, shuffle = TRUE))

pred = predict(fit, newdata = test.hex)

pred_res = as.data.frame(pred$pred)[,1]
pred_res = as.array(pred_res)

# used for internal validation on test set for mean F1 score
# label = testing$`Loan Status`
# label = as.array(label)

# library(rminer)
# mmetric(label, pred_res, metric = "F1")

# library(caret)
# confusionMatrix(label, pred_res)

# write the predicted value on to the submission file.
write.csv(data.frame("Loan Application Number" = test$`Loan Application Number`, "Loan Status" = pred_res), "~/yodlee/submission2.csv", row.names = FALSE, quote = FALSE)
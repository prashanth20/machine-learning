#### Author         : Prashant Hiremath
#### Challenge Name : Mobileum

library(rworldmap)

#helper function to convert longitude and latitude to country
coords2country = function(points) {
  countriesSP <- getMap(resolution='low')

  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))

  gc()

  # use 'over' to get indices of the Polygons object containing each point
  indices = over(pointsSP, countriesSP)

  gc()

  as.character(indices$NAME)
}

coords2continent = function(points) {
  countriesSP <- getMap(resolution='low')

  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))

  gc()

  # use 'over' to get indices of the Polygons object containing each point
  indices = over(pointsSP, countriesSP)

  gc()

  as.character(indices$REGION)
}

distanceBetweenTwoPoints <- function(long1, lat1, long2, lat2) {
  rad <- pi / 180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad

  dlon <- b2 - a2
  dlat <- b1 - a1

  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# # Load the data from csv file.
AttendanceData = read.csv("~/Mobiel/AttendanceData.csv", header = T, check.names = F, stringsAsFactors = T)
AttendeeData = read.csv("~/Mobiel/AttendeeData.csv", header = T, check.names = F, stringsAsFactors = T)
AttendeeRatingsTrainig = read.csv("~/Mobiel/AttendeeRatings(for training).csv", header = T, check.names = F, stringsAsFactors = T)
AttendeeRatingsEvaluation = read.csv("~/Mobiel/AttendeeRatings(for evaluation).csv", header = T, check.names = F, stringsAsFactors = T)
Eventdata = read.csv("~/Mobiel/Eventdata.csv", header = T, check.names = F, stringsAsFactors = T)

# Remove all the redundant data from the loaded data.
AttendanceData = unique(AttendanceData)
AttendeeData = unique(AttendeeData)
Eventdata = unique(Eventdata)

# merge the data for training and evaluation set from other data frame.
temp = merge(AttendeeRatingsTrainig, Eventdata, all.x = T)
temp = unique(temp)
train = merge(temp, AttendeeData, all.x = T)
#train = read.csv("~/R/Mobiel/train_country.csv", header = T, check.names = F, stringsAsFactors = T)

# while merging for evaluation set lets keep it repeated things as its is, as we need to
# predict for them.
AttendeeRatingsEvaluation$ID = 1:nrow(AttendeeRatingsEvaluation)
temp = merge(AttendeeRatingsEvaluation, Eventdata, all.x = T, sort = F)
test = merge(temp, AttendeeData, all.x = T, sort = F)
test = test[order(test$ID),]
test$ID = NULL

#test = read.csv("~/R/Mobiel/test.csv", header = T, check.names = F, stringsAsFactors = T)

EventPointsTrain = data.frame(EventLongitude = train$`Event Venue Longitude`, EventLatitude = train$`Event Venue Latitude`)
result = character(nrow(EventPointsTrain))

split = floor(nrow(EventPointsTrain) / 25)

for (i in 1:25) {
  Points = EventPointsTrain[((split * (i-1)) + 1): (split * i),]
  bad = with(Points, is.na(EventLongitude) | is.na(EventLatitude))
  tempResult <- character(length(bad))
  tempResult[!bad] = coords2country(Points[!bad,])
  tempResult[bad] = "Null"
  result[((split * (i-1)) + 1): (split * i)] = tempResult
  gc()
}

result = as.factor(result)
train$Event_Country = result

HomePointsTrain = data.frame(HomeLongitude = train$`Home Longitude`, HomeLatitude = train$`Home Latitude`)
result = character(nrow(HomePointsTrain))

split = floor(nrow(HomePointsTrain) / 25)

for (i in 1:25) {
  Points = HomePointsTrain[((split * (i-1)) + 1): (split * i),]
  bad = with(Points, is.na(HomeLongitude) | is.na(HomeLatitude))
  tempResult <- character(length(bad))
  tempResult[!bad] = coords2country(Points[!bad,])
  tempResult[bad] = "Null"
  result[((split * (i-1)) + 1): (split * i)] = tempResult
  gc()
}

result = as.factor(result)
train$Home_Country = result

EventPointsTest = data.frame(EventLongitude = test$`Event Venue Longitude`, EventLatitude = test$`Event Venue Latitude`)
result = character(nrow(EventPointsTest))

split = floor(nrow(EventPointsTest) / 25)

for (i in 1:25) {
  Points = EventPointsTest[((split * (i-1)) + 1): (split * i),]
  bad = with(Points, is.na(EventLongitude) | is.na(EventLatitude))
  tempResult <- character(length(bad))
  tempResult[!bad] = coords2country(Points[!bad,])
  tempResult[bad] = "Null"
  result[((split * (i-1)) + 1): (split * i)] = tempResult
  gc()
}

result = as.factor(result)
test$Event_Country = result

HomePointsTest = data.frame(HomeLongitude = test$`Home Longitude`, HomeLatitude = test$`Home Latitude`)
result = character(nrow(HomePointsTest))

split = floor(nrow(HomePointsTest) / 25)

for (i in 1:25) {
  Points = HomePointsTest[((split * (i-1)) + 1): (split * i),]
  bad = with(Points, is.na(HomeLongitude) | is.na(HomeLatitude))
  tempResult <- character(length(bad))
  tempResult[!bad] = coords2country(Points[!bad,])
  tempResult[bad] = "Null"
  result[((split * (i-1)) + 1): (split * i)] = tempResult
  gc()
}

result = as.factor(result)
test$Home_Country = result

EventPointsTrain = data.frame(EventLongitude = train$`Event Venue Longitude`, EventLatitude = train$`Event Venue Latitude`)
result = character(nrow(EventPointsTrain))

split = floor(nrow(EventPointsTrain) / 25)

for (i in 1:25) {
  Points = EventPointsTrain[((split * (i-1)) + 1): (split * i),]
  bad = with(Points, is.na(EventLongitude) | is.na(EventLatitude))
  tempResult <- character(length(bad))
  tempResult[!bad] = coords2continent(Points[!bad,])
  tempResult[bad] = "Null"
  result[((split * (i-1)) + 1): (split * i)] = tempResult
  gc()
}

result = as.factor(result)
train$Event_Continent = result

HomePointsTrain = data.frame(HomeLongitude = train$`Home Longitude`, HomeLatitude = train$`Home Latitude`)
result = character(nrow(HomePointsTrain))

split = floor(nrow(HomePointsTrain) / 25)

for (i in 1:25) {
  Points = HomePointsTrain[((split * (i-1)) + 1): (split * i),]
  bad = with(Points, is.na(HomeLongitude) | is.na(HomeLatitude))
  tempResult <- character(length(bad))
  tempResult[!bad] = coords2continent(Points[!bad,])
  tempResult[bad] = "Null"
  result[((split * (i-1)) + 1): (split * i)] = tempResult
  gc()
}

result = as.factor(result)
train$Home_Continent = result

EventPointsTest = data.frame(EventLongitude = test$`Event Venue Longitude`, EventLatitude = test$`Event Venue Latitude`)
result = character(nrow(EventPointsTest))

split = floor(nrow(EventPointsTest) / 25)

for (i in 1:25) {
  Points = EventPointsTest[((split * (i-1)) + 1): (split * i),]
  bad = with(Points, is.na(EventLongitude) | is.na(EventLatitude))
  tempResult <- character(length(bad))
  tempResult[!bad] = coords2continent(Points[!bad,])
  tempResult[bad] = "Null"
  result[((split * (i-1)) + 1): (split * i)] = tempResult
  gc()
}

result = as.factor(result)
test$Event_Continent = result

HomePointsTest = data.frame(HomeLongitude = test$`Home Longitude`, HomeLatitude = test$`Home Latitude`)
result = character(nrow(HomePointsTest))

split = floor(nrow(HomePointsTest) / 25)

for (i in 1:25) {
  Points = HomePointsTest[((split * (i-1)) + 1): (split * i),]
  bad = with(Points, is.na(HomeLongitude) | is.na(HomeLatitude))
  tempResult <- character(length(bad))
  tempResult[!bad] = coords2continent(Points[!bad,])
  tempResult[bad] = "Null"
  result[((split * (i-1)) + 1): (split * i)] = tempResult
  gc()
}

result = as.factor(result)
test$Home_Continent = result

train$Event_Country = as.factor(ifelse(is.na(train$Event_Country), "New Country", as.character(train$Event_Country)))
train$Home_Country = as.factor(ifelse(is.na(train$Home_Country), "New Country", as.character(train$Home_Country)))
train$Event_Continent = as.factor(ifelse(is.na(train$Event_Continent), "New Continent", as.character(train$Event_Continent)))
train$Home_Continent = as.factor(ifelse(is.na(train$Home_Continent), "New Continent", as.character(train$Home_Continent)))

test$Event_Country = as.factor(ifelse(is.na(test$Event_Country), "New Country", as.character(test$Event_Country)))
test$Home_Country = as.factor(ifelse(is.na(test$Home_Country), "New Country", as.character(test$Home_Country)))
test$Event_Continent = as.factor(ifelse(is.na(test$Event_Continent), "New Continent", as.character(test$Event_Continent)))
test$Home_Continent = as.factor(ifelse(is.na(test$Home_Continent), "New Continent", as.character(test$Home_Continent)))

train$Home_Event_Distance <- distanceBetweenTwoPoints(train$`Home Longitude`, train$`Home Latitude`, train$`Event Venue Longitude`, train$`Event Venue Latitude`) 

test$Home_Event_Distance <- distanceBetweenTwoPoints(test$`Home Longitude`, test$`Home Latitude`, test$`Event Venue Longitude`, test$`Event Venue Latitude`) 

mean_train = mean(train$Home_Event_Distance, na.rm = T)
mean_test = mean(test$Home_Event_Distance, na.rm = T)

train$Home_Event_Distance[which(is.na(train$Home_Event_Distance))] = mean_train
test$Home_Event_Distance[which(is.na(test$Home_Event_Distance))] = mean_test

library(h2o)
local = h2o.init(nthreads = -1, max_mem_size = "4G")

train = train[c(1:6, 8, 9, 10, 11, 12, 7)]

# train = train[sample(nrow(train)),]
# split = floor(nrow(train) * (3/4))
# training = train[1:split,]
# testing = train[(split + 1):nrow(train),]

test$Rating = NULL
training = train
testing = test

# train.hex = as.h2o(training)
# test.hex = as.h2o(testing)

# yVars = colnames(training)[ncol(training)]
# xVars = colnames(training)[1:(ncol(training) - 1)]

# model = h2o.gbm(xVars, yVars, training_frame = train.hex, ntrees = 300, learn_rate = 0.01, sample_rate = 0.7, col_sample_rate = 0.7, nfolds = 5)

# pred = predict(model, newdata = test.hex)

# pred_res = as.data.frame(pred$predict)[,1]
# pred_res = as.array(pred_res)
# pred_res = round(pred_res, 1)

# write.csv(data.frame("Attendee ID" = test$`Attendee ID`, "Event ID" = test$`Event ID`, "Rating" = pred_res), "~/Mobiel/submission.csv", row.names = FALSE, quote = FALSE)

library(h2oEnsemble)

y <- "Rating"
x <- setdiff(names(training), c(y))

h2o.gbm.1 <- function(..., ntrees = 350, learn_rate = 0.02, sample_rate = 0.8, col_sample_rate = 0.7, seed = 10) h2o.gbm.wrapper(..., ntrees = ntrees, learn_rate = learn_rate, sample_rate = sample_rate, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.2 <- function(..., ntrees = 400, learn_rate = 0.01, sample_rate = 0.7, col_sample_rate = 0.7, seed = 10) h2o.gbm.wrapper(..., ntrees = ntrees, learn_rate = learn_rate, sample_rate = sample_rate, col_sample_rate = col_sample_rate, seed = seed)
h2o.randomForest.1 <- function(..., ntrees = 300, sample_rate = 0.75, seed = 10) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.2 <- function(..., ntrees = 350, sample_rate = 0.60, seed = 10) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)

learner <- c("h2o.gbm.1", "h2o.gbm.2", "h2o.randomForest.1", "h2o.randomForest.2")
metalearner <- "h2o.randomForest.wrapper"
family <- "gaussian"

train.hex = as.h2o(training)
test.hex = as.h2o(testing)

fit <- h2o.ensemble(x = x, y = y, 
                    training_frame = train.hex,
                    validation_frame = NULL,
                    family = family, 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 5, shuffle = TRUE))

pred = predict(fit, newdata = test.hex)

pred_res = as.data.frame(pred$pred)[,1]

pred_res = as.array(pred_res)

pred_res = round(pred_res, 1)

# library(Metrics)
# rmse(testing$Rating, pred_res)

write.csv(data.frame("Attendee ID" = test$`Attendee ID`, "Event ID" = test$`Event ID`, "Rating" = pred_res), "~/R/Mobiel/submission12Jan.csv", row.names = FALSE, quote = FALSE)

train = read.csv("train.csv", header = T, check.names = F)
test = read.csv("test.csv", header = T, check.names = F)

date = strptime(as.character(train$Dates), "%Y-%m-%d %H:%M:%S")

train$Year = format(date, "%Y")
train$Year = factor(train$Year)

date_test = strptime(as.character(test$Dates), "%Y-%m-%d %H:%M:%S")

test$Year = format(date_test, "%Y")
test$Year = factor(test$Year)

hour = format(date, "%H")
hour = as.numeric(hour)

x = hour <= 6
train$TimeOfDay[x] = "Early Morning"

x = hour > 6 & hour <= 12
train$TimeOfDay[x] = "Morning"

x = hour > 12 & hour <= 18
train$TimeOfDay[x] = "Evening"

x = hour > 18 & hour <= 24
train$TimeOfDay[x] = "Night"

train$TimeOfDay = factor(train$TimeOfDay)

hour = format(date_test, "%H")
hour = as.numeric(hour)

x = hour <= 6
test$TimeOfDay[x] = "Early Morning"

x = hour > 6 & hour <= 12
test$TimeOfDay[x] = "Morning"

x = hour > 12 & hour <= 18
test$TimeOfDay[x] = "Evening"

x = hour > 18 & hour <= 24
test$TimeOfDay[x] = "Night"

test$TimeOfDay = factor(test$TimeOfDay)

library(randomForest)

rf = randomForest(Category ~ PdDistrict + Year + TimeOfDay, data = train, ntree = 500, do.trace = T)

response = predict(rf, newdata = test, type = "prob")

sampleSubmission = read.csv("sampleSubmission.csv", header = T, check.names = F)

for (i in 1:39) {
  sampleSubmission[,i+1] = response[, i]
}

write.csv(sampleSubmission, "submission.csv", row.names = F)


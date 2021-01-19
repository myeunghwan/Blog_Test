rm(list = ls())
gc()

library(ggplot2)
library(dplyr)
data(diamonds)
head(diamonds)

Original_Dataset = diamonds %>% select(x, y, z, depth, table, carat, price)

#x가 5이상이면 True
Original_Dataset = Original_Dataset %>% mutate(x = ifelse(x > 5, 'True', 'False'))

#y가 30이상이면 True
Original_Dataset = Original_Dataset %>% mutate(y = ifelse(y > 30, 'True', 'False'))

#z가 10이상이면 True
Original_Dataset = Original_Dataset %>% mutate(z = ifelse(z > 10, 'True', 'False'))

#depth가 50이상이면 True
Original_Dataset = Original_Dataset %>% mutate(depth = ifelse(depth > 50, 'True', 'False'))

#table가 50이상이면 True
Original_Dataset = Original_Dataset %>% mutate(table = ifelse(table > 50, 'True', 'False'))

#carat가 2이상이면 True
Original_Dataset = Original_Dataset %>% mutate(carat = ifelse(carat > 2, 'True', 'False'))

#price가 3000이상이면 True
Original_Dataset = Original_Dataset %>% mutate(price = ifelse(price > 3000, 'True', 'False'))

a = Original_Dataset %>% filter(price == 'True')
b = Original_Dataset %>% filter(price == 'False')

### Random Forest
install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)

##TRAIN DATA로 a, b 각각 15,000개씩 사용하자 
TRAIN = rbind(a[c(0:8000),], b[c(0:15000),])
TRAIN = as.matrix(TRAIN)
TEST = rbind(a[c(8000:nrow(a) + 1),], b[c(15000:nrow(b) + 1),])
TEST$price = TEST$price %>% as.factor()

rf.fit = randomForest(as.factor(price) ~ ., data = TRAIN, mtry = floor(sqrt(6)), ntree = 200, importance = T)

y_pred = predict(rf.fit, TEST)

confusionMatrix(y_pred, TEST$price)


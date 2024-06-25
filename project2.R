install.packages("tidyverse")
library(data.table)
library(tidyverse)
library(dplyr)
install.packages('caret')
library(caret)

DF <- fread("Stroke Prediction Dataset.csv") %>% as_tibble()
DF %>% str()

DF[DF == 'N/A'] <- NA                #bmi 값이 N/A인 결측값 제거
DF <- na.omit(DF)
as.numeric(DF$bmi)

DF1 <- DF[DF$stroke == 0,]           #stroke 값이 0인 데이터
DF1 <- DF[sample(nrow(DF), 209), ]   #랜덤으로 209개 추출
DF2 <- DF[DF$stroke == 1,]           #stroke 값이 1인 데이터
DF3 <- rbind(DF1, DF2)               #DF1 데이터와 DF2 데이터 병합

DF4 <- createDataPartition(DF3$stroke, p=0.8, list=F)   #8:2로 분리
train <- DF3[DF4,]      #train/test으로 분리
test <- DF3[-DF4,]

m <- glm(DF3$stroke~., data=DF3, family = "binomial")   #반응변수, 설명변수 생성
summary(m)

m2 <- step(m, direction = "backward")     #새로운 모델 생성
summary(m2)

test_set <- fread("Stroke Prediction Dataset.csv") %>% as_tibble()  #모델 예측
str(test_set)
summary(test_set)

predict(m2, newdata = test_set, type = "response") %>%
  tibble(predict_survived = .) %>% bind_cols(test_set, .)

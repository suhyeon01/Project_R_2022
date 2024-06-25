install.packages("tidyverse")
install.packages("dplyr")
library(data.table)
library(tidyverse)
library(dplyr)
install.packages('caret')
library(caret)

DF <- fread("Regular_Season_Batter.csv") %>% as_tibble()
DF %>% str()

input <- DF %>% na.omit() %>%         #결측값 제거
  select(-'height/weight', -year_born, -position, -career, -starting_salary)
input <- input %>% mutate(H = H - `2B` - `3B`- HR)
head(input)


DF1 <- subset(input, team == 'LG')    #team LG 분석 
DF1

DF2 <- createDataPartition(DF1$team, p=0.8, list=F)   #8:2로 분리
train <- DF1[DF2,]                  #train/test으로 분리
test <- DF1[-DF2,]
train
test

m1 <- lm(formula=G ~ H, data=DF1)      #반응변수 G 출장수로 제한, 설명변수 3개
m2 <- lm(formula=G ~ `2B`, data=DF1)
m3 <- lm(formula=G ~ `3B`, data=DF1)

m1
m2
m3

summary(m1)
summary(m2)
summary(m3)
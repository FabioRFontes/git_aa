# ---------------------------------------------------------------------------- #
                                                                               
#   Script com métodos utilizados na resolução do projeto

# ---------------------------------------------------------------------------- #
# Libraries

library(data.table)
library(AlgDesign)

# ---------------------------------------------------------------------------- #

setwd("/home/fabio/Desktop/UMinho_4_Ano/gits/git_aa/datasets/")

training <- fread("train_V2.csv", header = T)
trest <- fread("test_V2.csv", header = T)

attach(training)
names(training)

# Formúla para obter o winPlacePerc sem a utilização das variáveis categorias
ff <- winPlacePerc ~ . - Id - matchId - groupId - matchType

model <- lm(ff, data=training)
summary(model)

rm(model)
gc()

# Formúla para obter o winPlacePerc sem a utilização das variáveis categorias
ff2 <- winPlacePerc ~ . - Id - matchId - groupId + (killPoints * rankPoints)

model2 <- lm(ff2, data=training)
summary(model2)

rm(model2)
gc()

# Formúla para obter o winPlacePerc sem a utilização das variáveis categorias
ff3 <- winPlacePerc ~ killPoints * rankPoints

model3 <- lm(ff3, data=training)
summary(model3)

rm(model3)
gc()



# ---------------------------------------------------------------------------- #
#### Primeira submissão no kagglelibrary(data.table)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
train<-fread("../input/train_V2.csv", header = T)
train$Id = NULL
train$groupId = NULL
train$matchId = NULL
train$longestKill = NULL
ff3 <- winPlacePerc ~ . + winPoints * rankPoints
model <- lm(ff3,data=train)

test<-fread("../input/test_V2.csv", header = T)
test$Id = NULL
test$groupId = NULL
test$matchId = NULL
test$longestKill = NULL
p <- predict(model,test) 

test<- fread("../input/test_V2.csv", header = T)
submission = cbind(Id=test$Id,winPlacePerc=p)
write.csv(submission,'sample_submission.csv',row.names = FALSE)



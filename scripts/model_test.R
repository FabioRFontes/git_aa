# ---------------------------------------------------------------------------- #
                                                                               
#   Script com métodos utilizados na resolução do projeto

# ---------------------------------------------------------------------------- #
# Libraries used

library(data.table)
library(biglm) # Biblioteca que permite a execução em parcelas da função lm.

# ---------------------------------------------------------------------------- #

setwd("~/Downloads/train")

train <- fread("train_V2.csv", header = T)

chunk.crashfpp <- train[matchType=="crashfpp", ]
chunk.crashtpp <- train[matchType=="crashtpp", ]
chunk.duo <- train[matchType=="duo", ]
chunk.duo_fpp <- train[matchType=="duo-fpp", ]
chunk.flarefpp <- train[matchType=="flarefpp", ]
chunk.flaretpp <- train[matchType=="flaretpp", ]
chunk.normal_duo <- train[matchType=="normal-duo", ]
chunk.normal_duo_fpp <- train[matchType=="normal-duo-fpp", ]
chunk.normal_solo <- train[matchType=="normal-solo", ]
chunk.normal_solo_fpp <- train[matchType=="normal-solo-fpp", ]
chunk.normal_squad <- train[matchType=="normal-squad", ]
chunk.normal_squad_fpp <- train[matchType=="normal-squad-fpp", ]
chunk.solo <- train[matchType=="solo", ]
chunk.solo_fpp <- train[matchType=="solo-fpp", ]
chunk.squad <- train[matchType=="squad", ]
chunk.squad_fpp <- train[matchType=="squad-fpp", ]

attach(train)
train[matchType=="squad-fpp" & groupId=="4d4b580de459be",]
matchType=="squad-fpp" & groupId=="4d4b580de459be"
summary(as.factor(matchType))
ff <- winPlacePerc ~ . - Id - matchId - groupId - matchType
ff <- assists ~ kills + killStreaks + longestKill + maxPlace + rankPoints + revives + roadKills
model <- biglm(ff, data=train)
model <- lm(ff, data=chunck16.squad_fpp)
summary(model)

library(leaps)
regfit.fwd=regsubsets(ff,data=train,nvmax=39,method="forward")
summary(regfit.fwd)

class(regfit.fwd)

# ---------------------------------------------------------------------------- #

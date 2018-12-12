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


#####################################################################
# For big tables
library(bigmemory)
library(biganalytics)
library(bigtabulate)
#####################################################################


#####################################################################
# svm
install.packages("e1071")
library(e1071)
#####################################################################

getwd()
location <- getwd()
setwd(location)

library(ggplot2)

# dataset <- read.csv("tempo-2019.csv", header=TRUE, sep=",")
# dataset <- read.csv("acidente-sp.csv", header=TRUE, sep=";")
dataset <- read.csv("enem-2014.csv", nrows=10, colClasses=c("NU_ANO"), header=TRUE, sep=",")
dataset

str(dataset)

#####################################################################
# Previsão de tempo
#####################################################################

smp_size <- floor(0.8 * nrow(dataset)) # 80% pra treino
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size) # Separa linha de treino

train <- dataset[train_ind,] # Pega as linhas separadas e monta um data.frame
test <- dataset[-train_ind,]

testClass <- test[,3] # Coluna de classificação
test <- test [,-3] # Coluna de classificação

classifier = svm(
  formula = Condicao_do_Tempo_Prevista ~ .,
  data = train,
  type = 'C-classification',
  kernel = 'linear'
)

y_pred = predict(classifier, newdata = test)

acertos <- 0

for(i in 1:length(testClass)) {
  if(testClass[i] == y_pred[i]) {
    acertos <- acertos + 1
  }
}

acuracia <- acertos / length(testClass) * 100



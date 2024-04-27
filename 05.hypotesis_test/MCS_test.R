library(readr)
library(MCS)
library(tidyverse)
library(Metrics)
library(modelconf)
data(Loss)

forecast_results <- read_csv("F:/Educacao/Pós Graduações e Graduações/Mestrado UNB/dissertacao/inflation_forecast_integration/04.non_linear_models/forecast_results.csv")%>%
  rename()

y_test <- as.matrix(forecast_results[,1])
y_pred <- structure((forecast_results[,c(-1)]), class = "data.frame", row.names = c(NA, -33L), check.names = FALSE)
# Split the data frame into a list of data frames
#y_pred <- lapply(y_pred, function(x) data.frame(x))

# semente para reprodutibilidade
set.seed(123)
# MCS para conjunto de teste
# MSE
MCS::MCSprocedure(Loss = (y_pred - y_test)^2, statistic = "Tmax", alpha = 0.05, B = 5000)
# MCS para conjunto de teste
# MAE
MCS::MCSprocedure(Loss = abs(y_pred - y_test), statistic = "Tmax", alpha = 0.05, B = 5000)

############# modelconf ############
loss = (y_pred - y_test)^2

#estMCS(loss, test = "t.range", B = 5000, l = 2)
(my_MCS <- estMCS(loss, test = "t.min", B = 25000, l = 12))
# estimated 95% model confidence set
my_MCS[my_MCS[, "MCS p-val"] > 0.05, ]

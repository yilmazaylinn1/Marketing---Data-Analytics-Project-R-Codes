#First of all, we mixed the data to ensure that the result we obtained on the data was
#homogeneous. For this purpose, the mixing index operation was done. Then we updated the
#name of the data we mixed as Biletal�m. The code we wrote below was written for this
#operation.

shuffle_index <- sample(1:nrow(Kart_Bilet_Alim_MEF))
head(Kart_Bilet_Alim_MEF)
Biletal�m <- Kart_Bilet_Alim_MEF[shuffle_index, ]
library(dplyr)

#At this stage, we are eliminating the data.We create a new data frame by removing the column
#we don't want to use.


clean_biletal�m <- Biletal�m %>%
  select(-c(Bilet_Adedi, Birim_Bilet_Fiyat�, Toplam_Bilet_Fiyat�, �ndirimli_Birim_Fiyat,
            �ndirim_Kayb�,Deg_Kart_No))
glimpse(clean_biletal�m)

#To learn the algorithm , we decided to train the data with 0.8 rate training and 0.2 rate testing
#on the Kart Bilet Al�m Data. We create a fixed function for the data training & testing.

create_traintest <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

#In this section we determined the probability of test and train data

data_train <- create_traintest(clean_biletal�m, 0.8, train = TRUE)
data_test <- create_traintest(clean_biletal�m, 0.8, train = FALSE)

#In this section the outputs show the distribution.


prop.table(table(data_train$Sat��_Kanal�))
prop.table(table(data_test$Kart_Grubu))

#Decision tree 1

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(Kart_Grubu~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$Kart_Grubu, predict_unseen)
Table_mat

#This section shows the accuracy of the test

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$Kart_Grubu, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(Kart_Grubu~., data = data_train, method = 'class', control = control)
accuracy_tune(tune_fit)

#Decision Tree 2

prop.table(table(data_train$Etkinlik_Ad�))
prop.table(table(data_test$Net_Sat��_Geliri))
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

fit <- rpart(Kart_Grubu~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$Kart_Grubu, predict_unseen)
Table_mat

#Decision tree 3

prop.table(table(data_train$Sat��_D�nemi))
prop.table(table(data_test$�deme_Tipi))


install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
fit <- rpart(Kart_Grubu~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$Kart_Grubu, predict_unseen)
Table_mat










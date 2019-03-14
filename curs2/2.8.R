#1
test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
test_data  <- transform(test_data, x = factor(x), y = factor(y)) 
dataset <- test_data

model <- glm(y ~ x, dataset, family = "binomial")
exp(coef(model))

get_coefficients <- function(dataset) {
  model <- glm(y ~ x, dataset, family = "binomial")
  
  return(exp(coef(model)))
}


#2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cen_data.csv")

centered1 <- function(test_data, var_names){
  for (v in var_names) {
    m = mean(test_data[[v]])
    test_data[v] = test_data[v] - m
  }
  
  return(test_data)
}

centered2 <- function(test_data, var_names){    
  test_data[var_names] <- sapply(test_data[var_names], function(x) x - mean(x))    
  return(test_data)    
}


var_names = c("X4")
centered2(test_data, var_names)
centered1(test_data, var_names)

#3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")

formula <- as.formula("is_prohibited ~ .")
model <- glm(formula, test_data, family = "binomial")
summary(model)

result <- anova(model, test = 'Chisq')
cols <- result$`Pr(>Chi)` < 0.05
cols <- is.na(cols)
cols[is.na(cols)] <- FALSE 

length(colnames(test_data)[cols])

get_features <- function(dataset) {
  formula <- as.formula("is_prohibited ~ .")
  model <- glm(formula, test_data, family = "binomial")
  result <- anova(model, test = 'Chisq')
  cols <- result$`Pr(>Chi)` < 0.05
  cols[is.na(cols)] <- FALSE 
  
  if(length(colnames(test_data)[cols]) == 0) {
    return("Prediction makes no sense")
  } else {
    return(colnames(test_data)[cols])  
  }
  
}

get_features(test_data)

test_data <- data.frame(is_prohibited = factor( rep(1:2, each = 15)),weight = c( 83,75,95,80,77,78,86,77,73,71,80,84,94,83,74,83,88,85,91,88,85,82,73,73,80,79,80,87,83,79,83,75,95,80,77,78,86,77,73,71,80,84,94,83,74,83,88,85,91,88,85,82,73,73,80,79,80,87,83,79 ),length = c( 44,40,41,58,48,47,55,54,42,51,54,41,49,56,56,46,53,48,59,54,57,52,59,53,52,54,35,51,56,49,44,40,41,58,48,47,55,54,42,51,54,41,49,56,56,46,53,48,59,54,57,52,59,53,52,54,35,51,56,49 ),width = c( 28,25,18,8,17,19,3,11,23,11,26,11,21,11,26,20,19,34,24,19,12,30,7,16,40,16,14,28,30,31,28,25,18,8,17,19,3,11,23,11,26,11,21,11,26,20,19,34,24,19,12,30,7,16,40,16,14,28,30,31 ),type = factor(c( 1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2 )))
get_features(test_data)



#4
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

most_suspicious <- function(test_data, data_for_predict){
  model <- glm(is_prohibited ~ ., test_data, family = "binomial")
  predictions <-  predict(model, newdata = data_for_predict, type = "response")
  max_prob <- max(predictions)
  data_for_predict['is_prohibited'] <- predictions
  
  rez <- data_for_predict$passangers[which(data_for_predict$is_prohibited == max_prob)]
  
  return (rez)
}

rez <- most_suspicious(test_data, data_for_predict)
rez


#5

str(iris)
nums <- unlist(lapply(iris, is.numeric)) 
numerical_iris <- iris[ , nums]

rez <- lapply(numerical_iris, shapiro.test)
for(i in rez){print(i$p.value)}

normality_test <- function(dataset) {
  nums <- unlist(lapply(dataset, is.numeric)) 
  numerical_dataset <- dataset[ , nums]
  
  rezult_p_values <- c()
  for(i in lapply(numerical_dataset, shapiro.test)) {
    rezult_p_values <- c(rezult_p_values, i$p.value)
  }
  
  return(rezult_p_values)
}

normality_test(iris)

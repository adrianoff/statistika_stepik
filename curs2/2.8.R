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





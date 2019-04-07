# 1
# Напишите функцию smart_hclust, которая получает на вход dataframe  с произвольным числом количественных переменных и число кластеров, которое необходимо выделить при помощи иерархической кластеризации.
# Функция должна в исходный набор данных добавлять новую переменную фактор - cluster  -- номер кластера, к которому отнесено каждое из наблюдений.

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
dist_matrix <- dist(test_data)
fit <- hclust(dist_matrix)
cluster <- cutree(fit, 3)
test_data['cluster'] <- as.factor(cluster)

smart_hclust<-  function(test_data, cluster_number) {
  dist_matrix <- dist(test_data)
  fit <- hclust(dist_matrix)
  cluster <- cutree(fit, cluster_number)
  test_data['cluster'] <- as.factor(cluster)
  
  return(test_data)
}



# 2
get_difference <- function(test_data, n_cluster){    
  dist_matrix <- dist(test_data)    
  fit <- hclust(dist_matrix)    
  test_data$cluster <- as.factor(cutree(fit, n_cluster))    
  p_val <- sapply(test_data[,-ncol(test_data)],    
                  function(x) {summary(aov(x~cluster, test_data))[[1]][1,'Pr(>F)']})    
  return(names(p_val)[p_val < 0.05])    
}


# 3
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
str(prcomp(test_data))
prcomp(test_data)$x[, 'PC1']

get_pc <- function(d) {
  rez <- prcomp(test_data)
  d['PC1'] <- rez$x[, 'PC1']
  d['PC2'] <- rez$x[, 'PC2']
  
  return(d)
}

get_pc(test_data)


# 4
get_pca2 <- function(data){
  rez <- prcomp(data)
  
  n_imp <- min(
    which(
      summary(rez)$importance[3, ] > 0.90
      )
    )
  
  return(cbind(data, rez$x[, 1:n_imp]))
}
get_pca2(swiss)


# 5
#test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
#test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
is_multicol <- function(d) {
  cor <- cor(d)
  
  cor_columns <- c()
  for(j in 1:nrow(cor))
  {
    for(k in 1:ncol(cor))
    {
      if (round(abs(cor[j,k]), 2) == 1.00 && k!=j)
      {
        cor_columns <- c(cor_columns, rownames(cor)[j])
      }
    }
  }
  
  if(length(cor_columns) == 0) {
    return("There is no collinearity in the data")
  } else {
    return(cor_columns)  
  }
}
is_multicol(test_data)


is_multicol_best <- function(d) {
  d <- abs(cor(d))     
  d[lower.tri(d)] <- 0    
  diag(d) <- 0    
  index <- which((1-d) < 1e-10, arr.ind = T)    
  if (length(index) == 0){      
    return('There is no collinearity in the data')    
  } else {      
    return(rownames(d)[index])      
  }      
}









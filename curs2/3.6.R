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


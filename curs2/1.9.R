

#1 ##################################################

long_df <- mtcars[,c("am", "vs")]
short_df <- mtcars[1:20,c("am", "vs")]

smart_test <- function(test_data){
  test_table <- table(test_data) 
  if (min(test_table) < 5){        
    fit  <- fisher.test(test_table)        
    result  <- fit$p.value      
  } else {        
    fit  <- chisq.test(test_table)        
    result  <- c(fit$statistic,
                 fit$parameter,
                 fit$p.value)        
  }        
  return(result)        
}

smart_test(long_df)
smart_test(short_df)



#6 ##################################################

x <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")
t = table(x)  
r = chisq.test(t)
idx <- which(r$stdres == max(r$stdres), arr.ind = T)
c(rownames(t)[idx[1]], colnames(t)[idx[2]])

?chisq.test

max_resid <- function(x){
  t <- table(x)  
  r <- chisq.test(t)
  idx <- which(r$stdres == max(r$stdres), arr.ind = T)
  rez <- c(rownames(t)[idx[1]], colnames(t)[idx[2]])
  
  return(rez)
}

#7 ############################################################

library("ggplot2")
str(diamonds)

obj <- ggplot(diamonds, aes(color))+
  geom_bar(aes(fill = cut), position=position_dodge())

obj



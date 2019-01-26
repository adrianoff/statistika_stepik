

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


#2 ##################################################



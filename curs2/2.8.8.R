#install.packages("ggplot2")
library("ggplot2")

df <- iris
obj <- ggplot(df, aes(x = Sepal.Length, colour=Species, fill=Species)) + geom_density(alpha = 0.2)
obj                                                                   

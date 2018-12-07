df <- read.csv('data/atherosclerosis.csv')
str(df)

df$age <- factor(df$age, labels = c('Young', 'Old'))

mean(df$expr[df$age == 'Young' & df$dose == 'D1'])
mean(df$expr[df$age == 'Young' & df$dose == 'D2'])

mean(df$expr[df$age == 'Old' & df$dose == 'D1'])
mean(df$expr[df$age == 'Old' & df$dose == 'D2'])


mean_age1 <- aggregate(df$expr, by = list(df$age, df$dose), FUN = mean)
colnames(mean_age1) <- c('Age', 'Dose', 'Mean expr')
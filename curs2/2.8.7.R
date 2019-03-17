#install.packages("dplyr")
library("dplyr")


normality_by <- function (test_data) {
  names(test_data)[1] <- c("p_value")
  test_data %>%
    group_by_at(names(test_data)[2:3]) %>%
    summarise(p_value = shapiro.test(p_value)$p.value)
}

df_rez <- normality_by(mtcars[, c("mpg", "am", "vs")])
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")

df_rez <- normality_by(test_data)

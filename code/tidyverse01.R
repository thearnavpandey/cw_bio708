#install.packages("tidyverse")
library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

filter(iris_sub, Species == "virginica")

filter(iris_sub, Species %in% c("virginica", "versicolor"))

filter(iris_sub, Species != "virginica")

filter(iris_sub, !(Species %in% c("virginica", "versicolor")))

filter(iris_sub, Sepal.Length > 5)

filter(iris_sub, Sepal.Length >= 5)

filter(iris_sub, Sepal.Length < 5)



arrange(iris_sub, Sepal.Length)

arrange(iris_sub, desc(Sepal.Length))

select(iris_sub, Sepal.Length)

select(iris_sub, c(Sepal.Length, Sepal.Width))

select(iris_sub, -Sepal.Length)

select(iris_sub, -c(Sepal.Length, Sepal.Width))

select(iris_sub, starts_with("Sepal"))

# remove columns starting with "Sepal"
select(iris_sub, -starts_with("Sepal"))

# select columns ending with "Width"
select(iris_sub, ends_with("Width"))

# remove columns ending with "Width"
select(iris_sub, -ends_with("Width"))

# nrow() returns the number of rows of the dataframe
(x_max <- nrow(iris_sub))

# create a vector from 1 to x_max
(x <- 1:x_max)

# add as a new column
# named `x` as `row_id` when added
mutate(iris_sub, row_id = x)

#piping



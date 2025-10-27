pacman::p_load(tidyverse, patchwork, here)

head(iris)

# Question 1 --------------------------------------------------------------

#  setosa
df_setosa <- iris %>%
  filter(Species == "setosa")
(df_setosa)

m_setosa <- lm(Sepal.Width ~ Petal.Width,
               data = df_setosa)

summary(m_setosa)

# versicolor
df_versicolor <- iris %>%
  filter(Species == "versicolor")

(df_versicolor)

m_versicolor <- lm(Sepal.Width ~ Petal.Width,
               data = df_versicolor)
summary(m_versicolor)

# virginica
df_virginica <- iris %>%
  filter(Species == "virginica")

(df_virginica)

m_virginica <- lm(Sepal.Width ~ Petal.Width,
                   data = df_virginica)
summary(m_virginica)


# Question 2 --------------------------------------------------------------

m_setosa_2 <- lm( Sepal.Width ~ Petal.Width + Petal.Length,
               data = df_setosa)

summary(m_setosa_2)


# Question 3 --------------------------------------------------------------

## if you are done with 1 & 2
# create a new random variable x
v_x <- rnorm(nrow(iris), mean = 0, sd = 1)
iris <- iris %>% 
  mutate (x = v_x)



df_setosa3 <-iris %>%
  filter(Species == "setosa")
(df_setosa3)

m_setosa3 <- lm(Sepal.Width ~ Petal.Width + x,
                  data = df_setosa3)
summary(m_setosa)
summary(m_setosa3)

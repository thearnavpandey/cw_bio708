pacman::p_load(tidyverse, 
               patchwork,
               here)


# normality assumption ----------------------------------------------------
# Question 1
m_tooth <- lm(len ~ supp * dose, 
            data = ToothGrowth)
summary (m_tooth)


# Question 2
eps <- resid(m_tooth)
shapiro.test(eps)

# higher p-value than 0.05, normal assumption holds


# model interpretation ----------------------------------------------------
df_pred <- ToothGrowth %>% 
  group_by (supp) %>% 
  reframe (dose = seq(min(dose),
                      max(dose),
                      length = 100))
           
# get predicted values
y_pred <- predict(m_tooth, 
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

view(df_pred)

# get visual output
ggplot(ToothGrowth, aes ( x = dose,
                   y = len,
                   color = supp))+
  geom_point(alpha = 0.25)+
  geom_line (data = df_pred,
             aes(y = y_pred))


# multicollinearity -------------------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

df_y

#visualization
scat1 <- ggplot (df_y, 
                 aes (x = x1, 
                      y = y)) +
  geom_point (alpha = 0.5)

scat2 <- ggplot (df_y, aes (x = x2, 
                                    y = y)) +
  geom_point (alpha = 0.5)

scat1/scat2

# create lm () model with x1 and x2 as predictors
m_multi <- lm(y ~ x1 + x2, 
              data = df_y)
summary (m_multi)
  
# check relationship between x1 and x2

scat3 <- ggplot (df_y, 
                 aes (x = x1, 
                      y = x2)) +
  geom_point (alpha = 0.5)

scat3

# check correlation between x1 and x2
with(df_y,
     cor(x1, x2))


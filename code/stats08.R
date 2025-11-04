pacman::p_load(tidyverse, 
               patchwork,
               here)


# count data --------------------------------------------------------------

df_count <- read_csv(here("data_raw/data_garden_count.csv"))
print(df_count)
nrow(df_count)

m_normal <- lm(count ~ nitrate,
               data = df_count)
summary(m_normal)


alpha <- coef(m_normal)[1]
beta <- coef(m_normal)[2]

ggplot (df_count)+
  geom_point(aes(x= nitrate,
                 y = count))+
  geom_abline(intercept = alpha,
              slope = beta)
  
## random numbers from Poisson

(y <- rpois(n = 10, lambda = 2))

# apply Poisson using GLM
mpois <- glm(count ~ nitrate,
    data = df_count, 
    family = "poisson")

summary(mpois)

alpha1 <- coef(mpois)[1]
beta2 <- coef(mpois)[2]


ggplot(df_count)+
  geom_point(aes (x = nitrate, 
                  y = count))+
  geom_abline(intercept = alpha1,
              slope = beta2 )

summary(mpois)

## visualization of poisson regression
df_pred <- tibble(nitrate = seq (min(df_count$nitrate),
                      max(df_count$nitrate),
                      length = 100))

y_pred <- predict (mpois,
         newdata = df_pred) %>% 
  exp()

df_pred <- df_pred %>% 
  mutate(y = y_pred)

ggplot(df_count,
       aes(x = nitrate,
           y = count))+
  geom_point()+
  geom_line(data = df_pred , aes (y= y))

## compare summary output from m_normal and mpois
summary (m_normal)
summary (mpois)


# binomial distribution/ proportional data --------------------------------



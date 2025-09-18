library(tidyverse)
library (patchwork)

df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))

# Question 1 ---------------------------------------------------------------


mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2)/nrow(df_h0)

## 50 samples
mu_i50 <- var_i50 <- NULL 

for (i in 1:100) {

df_i50 <- df_h0 %>% 
  sample_n(50)

mu_i50[i] <- mean(df_i50$height)
var_i50[i] <- var(df_i50$height)

}

df_sample50 <- tibble(mu_hat = mu_i50,
                    var_hat = var_i50)

g_mu50 <- df_sample50 %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram()+
  geom_vline(xintercept=mu)+
  scale_x_continuous(limits = c(18,22))

g_var50 <- df_sample50 %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram()+
  geom_vline(xintercept = sigma2)+
  scale_x_continuous(limits = c(17.5,37))

#100 measures
mu_i100 <- var_i100 <- NULL 

for (i in 1:100) {
  
  df_i100 <- df_h0 %>% 
    sample_n(100)
  
  mu_i100[i] <- mean(df_i100$height)
  var_i100[i] <- var(df_i100$height)
  
}

df_sample100 <- tibble(mu_hat = mu_i100,
                      var_hat = var_i100)

g_mu100 <- df_sample100 %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram()+
  geom_vline(xintercept=mu)+
  scale_x_continuous(limits = c(18,22))

g_var100 <- df_sample100 %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram()+
  geom_vline(xintercept=sigma2)+
  scale_x_continuous(limits = c(17.5,37))

g_mu50/g_mu100
g_var50/g_var100

# Question 2 --------------------------------------------------------------

df_h10 <- df_h0 %>% 
  filter(height >= 10)

mu_10 <- mean(df_h10$height)
sigma2_10 <- sum((df_h10$height - mu)^2)/nrow(df_h10)

# 50 measures
mu_h10_i50 <- var_h10_i50 <- NULL 

for (i in 1:100) {
  
  df_h10_i50 <- df_h10 %>% 
    sample_n(50)
  
  mu_h10_i50[i] <- mean(df_h10_i50$height)
  var_h10_i50[i] <- var(df_h10_i50$height)
  
}

df_h10_sample50 <- tibble(mu_hat = mu_h10_i50,
                      var_hat = var_h10_i50)

g_h10_mu50 <- df_h10_sample50 %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram()+
  geom_vline(xintercept=mu_10)+
  scale_x_continuous(limits = c(18.5,22))

g_h10_var50 <- df_h10_sample50 %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram()+
  geom_vline(xintercept = sigma2_10)+
  scale_x_continuous(limits = c(11,35))

#100 measures
mu_h10_i100 <- var_h10_i100 <- NULL 

for (i in 1:100) {
  
  df_h10_i100 <- df_h10 %>% 
    sample_n(100)
  
  mu_h10_i100[i] <- mean(df_h10_i100$height)
  var_h10_i100[i] <- var(df_h10_i100$height)
  
}

df_h10_sample100 <- tibble(mu_hat = mu_h10_i100,
                       var_hat = var_h10_i100)

g_h10_mu100 <- df_h10_sample100 %>% 
  ggplot(aes(x = mu_hat)) +
  geom_histogram()+
  geom_vline(xintercept=mu_10)+
  scale_x_continuous(limits = c(18.5,22))

g_h10_var100 <- df_h10_sample100 %>% 
  ggplot(aes(x = var_hat)) +
  geom_histogram()+
  geom_vline(xintercept=sigma2_10)+
  scale_x_continuous(limits = c(11,35))

g_h10_mu50/g_h10_mu100
g_h10_var50/g_h10_var100



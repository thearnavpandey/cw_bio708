library (tidyverse)
library (patchwork)


# Normal distribution -----------------------------------------------------
z <- rnorm(50, mean = 100, sd = 5)

# create bin
bin <- seq(floor(min(z)), max(ceiling(z)), by = 1)

#create loop
p <- NULL 
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mean(z), sd = sd(z)) - pnorm(bin[i], mean = mean(z), sd = sd(z))
}

# create tibble
df_prob <- tibble (bin = bin[-length(bin)] + 0.5,
        prob = p) %>% 
  mutate(freq = length(z) * prob)

# figure
 df_z <- tibble (z =z)
 
 df_z %>% 
   ggplot(aes(x =z))+
   geom_histogram()+
   geom_point(data = df_prob,
              aes(x = bin, 
                  y = freq),
              color ="skyblue")+
   geom_line(data = df_prob,
             aes(x = bin,
                 y = freq),
             color= "skyblue" )
 

# Poisson Distribution ---------------------------------------------------- 
 x <- rpois(n=1000, lambda = 10)
 
bin <- seq (0, max(x) + 5,
             by = 1)
 
 pm <- dpois(bin, lambda = mean(x))
 
df_prob <- tibble(bin = bin, 
                   prob = pm) %>% 
   mutate(freq = pm *length(x)) 
 
 
 df_x <- tibble(x =x)
 
 df_x %>% 
   ggplot(aes(x = x)) +
   geom_histogram(binwidth = 0.5, 
                  center = 0) +
   geom_line(data = df_prob,
             aes(x = bin,
                 y = freq),
             linetype = "dashed") +
   geom_point(data = df_prob,
              aes(x =bin,
                  y = freq))


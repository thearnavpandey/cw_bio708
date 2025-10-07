pacman::p_load(tidyverse, patchwork, here)

# Question 1
xs <- rnorm(10, mean = 10, sd = 5)
ys <- rnorm(10, mean = 12, sd = 5)
xl <- rnorm(100, mean = 10, sd = 5)
yl <- rnorm(100, mean = 12, sd = 5)

t.test(xs, ys, var.equal = TRUE)

t.test (xl, yl, var.equal = TRUE)

# Question 2
a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

df_ab <- tibble(a1, a2, b1, b2)

df_ab_w <- df_ab %>% 
  pivot_longer(cols = c("a1",
                        "a2",
                        "b1",
                        "b2"),
               names_to = "group",
               values_to = "value")
        
df_ab_sum <- df_ab_w %>%
  group_by(group) %>% 
  summarize(mu = mean(value),
            sigma = sd(value))


df_ab_w %>% 
  filter (group %in% c("a1", "a2")) %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width = 0.1, 
              height = 0, 
              alpha = 0.25) +
  geom_segment(data = df_ab_sum %>% 
                 filter (group %in% c("a1", "a2")),
               aes(x = group,
                   xend = group,
                   y = mu - sigma,
                   yend = mu + sigma)) +
  geom_point(data = df_ab_sum %>% 
             filter (group %in% c("a1", "a2")), 
             aes(x = group,
                 y = mu),
             size = 3) +
  labs(x = "group", 
       y = "value") 

# welch's test
t.test(a1, a2)
t.test(b1, b2)

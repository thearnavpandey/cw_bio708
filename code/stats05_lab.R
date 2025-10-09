pacman::p_load(tidyverse, patchwork, here)

PlantGrowth

df_pg <- as_tibble(PlantGrowth)

df_pg %>% 
  ggplot(aes(x = group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5, 
              alpha = 0.2) + 
  geom_jitter(alpha = 0.2) 

m <- aov(weight ~ group,
         data = df_pg)

summary (m)

## df, F-value and P-value values are reported in a scientific article.

#install.packages("pwr")

pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

# change k, f, and power one at a time 
# see how these changes affect the number of samples you may need
pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

pwr::pwr.anova.test(k = 2,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

pwr::pwr.anova.test(k = 5,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.8)

pwr::pwr.anova.test(k = 3,
                    f = 0.2,
                    sig.level = 0.05,
                    power = 0.8)


pwr::pwr.anova.test(k = 3,
                    f = 0.8,
                    sig.level = 0.05,
                    power = 0.8)


pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.5)

pwr::pwr.anova.test(k = 3,
                    f = 0.5,
                    sig.level = 0.05,
                    power = 0.999999)

#  leave power blank
pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)

# try different levels of k, n, f
pwr::pwr.anova.test(k = 8,
                    n = 5,
                    f = 0.5,
                    sig.level = 0.05)

pwr::pwr.anova.test(k = 3,
                    n = 10,
                    f = 0.5,
                    sig.level = 0.05)

pwr::pwr.anova.test(k = 3,
                    n = 2,
                    f = 0.5,
                    sig.level = 0.05)

pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.2,
                    sig.level = 0.05)

pwr::pwr.anova.test(k = 3,
                    n = 5,
                    f = 0.9,
                    sig.level = 0.05)


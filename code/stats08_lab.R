pacman::p_load(tidyverse, 
               patchwork,
               here)

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)


# GLM exercise ------------------------------------------------------------

# Question 1
mpois1 <- glm(n_sp ~ distance + cat_area + hull_area  ,
             data = df_fish, 
             family = "poisson")

summary(mpois1)


# Question 2
m_binom <- glm(cbind(am, 1 - am) ~ mpg + hp + wt,
               data = mtcars,
               family = "binomial")
summary (m_binom)


m_am_gaussian <- glm(am ~ mpg + hp + wt,
               data = mtcars,
               family = "gaussian")

summary (m_am_gaussian)


# Effect size -------------------------------------------------------------
std_dist <- scale(df_fish$distance)
std_cat <- scale(df_fish$cat_area)
std_hull <- scale(df_fish$hull_area)

df_fish <- df_fish %>% 
  mutate(std_dist, std_cat, std_hull)

m_fish_std <- glm(n_sp ~ std_dist + std_cat +  std_hull,
              data = df_fish, 
              family = "poisson")

### compare coefs
coef(mpois1)
coef(m_fish_std)


# Offset term -------------------------------------------------------------
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- read_csv(url)

df_offset 

g1 <- df_offset %>% 
  ggplot (aes(x = nitrate ,
              y = count))+
  geom_point()


g2 <- df_offset %>% 
  ggplot (aes(x = area ,
              y = count))+
  geom_point()

g3 <- df_offset %>% 
  mutate (density = count/area) %>% 
  ggplot (aes(x = nitrate ,
              y = density))+
  geom_point()

g1+g2+g3

## gaussian trial
df_offset <- df_offset %>% 
  mutate (density = count/ area)

glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")

# Offset
m_count_wo_offset<- glm( count ~ nitrate,
                  data = df_offset, 
                  family = "poisson")

summary(m_count_wo_offset)

m_count_w_offset <- glm(count ~ nitrate + offset(log(area)),
                data = df_offset, 
                family = "poisson")


summary (m_count_w_offset)

# Overdispersion ----------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- read_csv(url)

g_v <- df_tadpole %>% 
  ggplot (aes(x = aqveg,
              y = tadpole))+
  geom_point()

g_p <- df_tadpole %>% 
  ggplot (aes(x = permanence,
              y = tadpole))+
  geom_point()

g_v/g_p

m_tad <- glm(tadpole ~ aqveg + permanence,
                        data = df_tadpole, 
                        family = "poisson")
summary (m_tad)

mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)

## negative binomial regression
m_nb <- MASS::glm.nb (tadpole ~ aqveg + permanence,
              data = df_tadpole)

summary (m_nb)
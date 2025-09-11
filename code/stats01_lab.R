library(tidyverse)


# Comparing Central Tendency Measures -------------------------------------
z <- exp(rnorm(n = 1000, mean = 0, sd = 0.1))
#Q1
# arithmetic mean
z_am <- mean(z)

# geometric mean
z_gm <- prod(z)^(1/length(z))
# z_gm <- exp(mean(log(z)))

# median
z_md<- median(z)

#Q2
df_z <- tibble(z = z) 
df_z %>% 
ggplot(aes(x = z))+
  geom_histogram()

#Q3
g1 <- df_z %>% 
  ggplot(aes(x = z))+
  geom_histogram()+
  geom_vline(xintercept = z_am)+
  geom_vline (xintercept = z_gm, color = "salmon")+
  geom_vline (xintercept = z_md, color = "pink")

#Q5 
z_rev <- -z + max(z) + 0.1
z_rev_am <- mean(z_rev)
z_rev_gm <-exp(mean(log(z_rev)))
z_rev_md <- median(z_rev)

df_z_rev<-tibble(z_rev = z_rev)
df_z_rev %>% 
  ggplot(aes(x=z_rev))+
  geom_histogram()

g2 <- df_z_rev %>% 
  ggplot(aes(x=z_rev))+
  geom_histogram()+
  geom_vline(xintercept = z_rev_am)+
  geom_vline(xintercept = z_rev_gm, color = "salmon")+
  geom_vline(xintercept = z_rev_md, color = "pink")

# Loading library(patchwork) to overlay two ggplot
library(patchwork)
g1/g2


# Comparing Variation Measures --------------------------------------------
w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

#Q1 gram to milligram
m<-w*1000

#Q2 Standard deviation and MAD (both have units so they are not good for comparison with different units)
w_sd <- sd(w)
m_sd<- sd(m)
w_mad <- median(abs(w - median(w)))
m_mad <- median(abs(m -median(m)))

#Q3 Coefficient of Variation and MAD/Median
w_cv <- sd(w)/mean(w) #both numerator and denominator have unit of [g] which cancels out
m_cv <- sd(m)/mean(m) #both numerator and denominator have unit of [mg] which cancels out

w_madmed <- w_mad/median(w) #both numerator and denominator have unit of [g] which cancels out
m_madmed <- m_mad/median(m) #both numerator and denominator have unit of [mg] which cancels out






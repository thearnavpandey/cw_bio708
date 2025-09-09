# cmd + shift + N is hotkey to open new file
library(tidyverse)


# central tendency --------------------------------------------------------
# ctrl + shift + R to create section

# arithmetic mean
# calculate the arithmetic mean of v_x using length() and sum()
v_x <- rnorm(10)
mu_x <- sum(v_x)/length(v_x)

(mean(v_x))

# geometric mean
v_y <- runif(10, min = 10, max = 20)
ge_y <- prod(v_y)^(1/length(v_y))
exp(mean(log(v_y)))

#median
v_z <- runif(9, min = 10, max = 20)
v_z <- sort(v_z)
index <- (length (v_z) +1 )/2
v_z[index]
median(v_z)


library(tidyverse)
library(ggplot2)
theme_set(theme_minimal())

wr <- read.table("day4/WR2011.txt", header = T)

class(wr$time)
class(wr$distance)
class(wr$bend)
class(wr$sex)

wr$time <- as.numeric(wr$time)
wr$distance <- as.numeric(wr$distance)
wr$bend <- as.numeric(wr$bend)
wr$sex <- as.factor(wr$sex)

wr <- wr %>%
  mutate (
    across(c('time', 'distance', 'bend'), as.numeric),
    across('sex', as.factor),
  )



## Plot of time over distance
ggplot (
  data = wr,
  aes (
    x = distance,
    y = time,
    col=sex
  )
) +
  geom_point()

## log time over log distance
ggplot (
  data = wr,
  aes (
    x = log(distance),
    y = log(time),
    col=sex
  )
) +
  geom_point()



## Fit a linear model:
m1 <- lm (
  log(time) ~ sex + log(distance) + sex:log(distance), data = wr
)

ggplot() + 
  geom_point (
    aes (
      x = fitted(m1),
      y = residuals(m1)
    )
  ) +
  geom_hline (
    yintercept = 0
  )

wr[c(6, 22), 2]

## Fit new model
m2 <- lm (
  log(time) ~ sex + log(distance) + sex:log(distance) + sex:log(bend), 
  data = wr
)

ggplot() + 
  geom_point (
    aes (
      x = fitted(m2),
      y = residuals(m2)
    )
  ) +
  geom_hline (
    yintercept = 0
  )


m3 <- lm (
  log(time) ~ sex + log(distance) + sex:log(distance) + sex:log(bend), 
  data = wr[-c(1,11,12,14,15,17,27,28,30,31), ]
)


ggplot() + 
  geom_point (
    aes (
      x = fitted(m3),
      y = residuals(m3)
    )
  ) +
  geom_hline (
    yintercept = 0
  )

## AIC = -Likelihood + 2*no. of parameters
step(m3)

m4 <- lm (
  log(time) ~ sex + log(distance) + log(bend),
  data = wr
)
confint(m4)['sexwomen', ]
exp(confint(m4)['sexwomen', ])









# 4.1:
wage <- read.table("day4/wage.txt", header = T)
s1 <- var(wage$age)
s2 <- 0.7235^2
1 - s2/ s1



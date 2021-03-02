library(tidyverse)
library(psych)
library(lm.beta)
library(car)

data(mtcars)

names(mtcars)

str(mtcars)

ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
cor.test(mtcars$mpg, mtcars$hp)
lm.shape <- lm(mpg ~ hp, data = mtcars)
lm.shape.beta <- lm.beta(lm.shape)
summary(lm.shape.beta)
gencoupe <- data.frame(hp = 300)
predict(lm.shape, gencoupe)

mtcars %>% select(hp) %>% range()

sort(mtcars$hp, decreasing = TRUE) %>% head()

mtcars <- mtcars %>% mutate(vs2 = factor(vs, levels = c(0, 1), labels = c("V-shaped", "Straight")))
ggplot(data = mtcars, aes(x = vs2, y = mpg)) + geom_bar(stat = "summary", fun.y = "mean") + xlab("Engine Shape") + ylab("Avg MPG")

ggplot(data = mtcars, aes(x = mpg)) + geom_histogram(binwidth = 7)
describe(mtcars$mpg)

mtcars %>% group_by(vs2) %>% summarise(mean.mpg = mean(mpg))
t.test1 <- t.test(mpg ~ vs, data = mtcars)
t.test1

lm.shape.cyl <- lm(mpg ~ vs + cyl, data = mtcars)
lm.shape.cyl.beta <- lm.beta(lm.shape.cyl)
summary(lm.shape.cyl.beta)

vif(lm.shape.cyl.beta)
cor.test(mtcars$cyl, mtcars$vs)

mtcars <- mtcars %>% mutate(am2 = factor(am, levels = c(0, 1), labels = c("Automatic Trans", "Manual Trans")))
mtcars %>% group_by(am2) %>% summarise(mean.mpg = mean(mpg))

t.test2 <- t.test(mpg ~ am, data = mtcars)
t.test2
ggplot(data = mtcars, aes(x = disp, y = mpg)) + geom_jitter() + geom_smooth(method = 'lm', se = FALSE)

ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_jitter() + geom_smooth(method = 'lm', se = FALSE)

ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_jitter() + geom_smooth(method = 'lm', se = FALSE)

cor.test(mtcars$wt, mtcars$mpg)

lm1 <- lm(mpg ~ am + cyl + hp + wt, data = mtcars)
lm1.beta <- lm.beta(lm1)
summary(lm1.beta)

vif(lm1.beta)

ggplot(mtcars, aes(cyl, hp)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + xlab("Number of Cylinders") + ylab("Horsepower")

cor.test(mtcars$hp, mtcars$cyl)

lm2 <- lm(mpg ~ hp + wt + am, data = mtcars)
lm2.beta <- lm.beta(lm2)
summary(lm2.beta)

vif(lm2.beta)

lm3 <- lm(mpg ~ hp + wt, mtcars)
lm3.beta <- lm.beta(lm3)
summary(lm3.beta)

vif(lm3.beta)

GenesisCoupe <- data.frame(hp = 300, wt = 3.5)
predict(lm3, GenesisCoupe)

fiat124 <- data.frame(hp = 103, wt = 2.1)
predict(lm3, fiat124)

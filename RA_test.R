rm(list=ls(all=TRUE))
graphics.off()
close.screen(all = TRUE)
erase.screen()
windows.options(record=TRUE)

setwd("C:/Users/jpjor/Desktop/cd2")

library(tidyverse)
library(stargazer)
library(foreign)
library(readxl)

library(collapse)
library(reshape)

RAtest <- read_csv("ra_test_data.csv")

stargazer(as.data.frame(RAtest), type = "text",title = "Summary Statistics",out="Summary Statistics.html")

# Question 1
didreg = lm(y ~ trt + post + trt:post, data = RAtest)
summary(didreg)
stargazer(didreg,type = "text",title = "Question 1: main regression",out="Question 1 Main Regression.html")
# The effect of the treatment on y is -0.5455 (not significant)

# Question 2
Graph <- ggplot(RAtest, aes(x = post, y = y, color = trt)) +
  geom_line(aes(group = trt))+geom_point(aes(group=trt))+labs(title="Effect of treatment on y")
png("Graph.png")
print(Graph)
dev.off()


# Question 3
# To implement Bootstrap Standard Errors
install.packages("boot")
library(boot)
funcdidreg <- function(RAtest, indices) 
didreg2 <- lm(trt + post + trt:post, data = RAtest)
return(coefficients(didreg2))
boot_results <- boot(RAtest, funcdidreg, R = 200)
plot(boot_results)
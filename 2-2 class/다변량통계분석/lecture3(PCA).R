
data <- read.csv("open_closed.csv")
summary(data)

library(psych)
pairs.panels(data)

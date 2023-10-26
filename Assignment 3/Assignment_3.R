df <-read.table("/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/sample-1.dat")
library(mirt)
View(df)

data <- read.delim("/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/sample-1.dat", header = TRUE)
head(data)

data <- read.table("/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/sample-1.dat", sep = ";", header = TRUE)
head(data)

data <- read.fwf("/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/sample-1.dat",
                 header = F,widths = c(4,rep(1,40)))

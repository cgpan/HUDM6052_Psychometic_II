# this file is used to test the code in hw 4

# read the csv file
df <- read.csv("/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/HUDM6052_Psychometic_II/assignment 4/math36.csv")
# remove the pilot items to get the final dataset
df <- df[,-which(names(df) %in% c("mathc10","mathc20","mathc30"))]
# write.csv(df, file = "/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/HUDM6052_Psychometic_II/assignment 4/responses.csv",
#           row.names = FALSE)

# load the keys
keys <- read.csv("/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/HUDM6052_Psychometic_II/assignment 4/keys.csv")

# compare the responses with the keys
names(df)
names(keys)
dim(df)
dim(keys)

keys[1,]
# convert the responses to binart coding
binary_responses <- df
for (i in 1:nrow(df)) {
  binary_responses[i,] <- as.integer(df[i,] == keys[1,])
}

dim(binary_responses)
# save the binary reponses as csv
write.csv(binary_responses, file = "/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/HUDM6052_Psychometic_II/assignment 4/binary_reponses.csv")


df <- read.csv("/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/HUDM6052_Psychometic_II/assignment 4/binary_reponses.csv")
df <- df[,-1]


# estimated the model
# since I constrained all slopes to be equal, here the argument "2PL" is safe
library(mirt)
spec <- 'F = 1-33
CONSTRAIN = (1-33, a1)'
irt_1pl <- mirt(df, 1, SE=T)
irt_1pl
coef(irt_1pl, IRTpars=T, simplify=T)

spec <- 'F = 1-33
PRIOR = (1-33, g, norm, -1.1, 2)'
irt_3pl <- mirt(df, model = spec, itemtype = "3PL", SE=T)
irt_3pl
parameters <- coef(irt_3pl, IRTpars=T, simplify = T)
as.data.frame(parameters$items)  

D = -2*(-20260.22+20078.62)

pchisq(D, df = 32, lower.tail = FALSE)


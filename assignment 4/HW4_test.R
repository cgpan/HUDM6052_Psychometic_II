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

irt_2pl <- mirt(df, model = 1, itemtype = "2PL", SE=T)
irt_2pl

spec <- 'F = 1-33
PRIOR = (1-33, g, norm, -1.1, 2)'
irt_3pl <- mirt(df, model = spec, itemtype = "3PL", SE=T)
irt_3pl
parameters <- coef(irt_3pl, IRTpars=T, simplify = T)
as.data.frame(parameters$items)  

D = -2*(-20078.62+20064.48)

pchisq(D, df = 33, lower.tail = FALSE)

item_fit_1pl <- itemfit(irt_1pl, na.rm = T)
item_fit_2pl <- itemfit(irt_2pl, na.rm = T)
item_fit_3pl <- itemfit(irt_3pl, na.rm = T)
names(item_fit_1pl)
item_fit_all <- cbind(item_fit_1pl[,c("item")],
                      round(item_fit_1pl[,c("S_X2", "p.S_X2")],4),
                      round(item_fit_2pl[,c("S_X2", "p.S_X2")],4),
                      round(item_fit_3pl[,c("S_X2", "p.S_X2")],4))
names(item_fit_all)[1] <- "item"
write.csv(item_fit_all, file = "/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/HUDM6052_Psychometic_II/assignment 4/item_fit_all.csv",
          row.names = F)

# load the cleaned dataset
df <- read.csv("/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/HUDM6052_Psychometic_II/assignment 4/binary_reponses.csv")
df <- df[,-1]
names(df)
dim(df)

# get a total score for each student
total_score <- apply(df, 1, function(x) sum(x, na.rm=T))
hist(total_score)
summary(total_score)


# run rasch model 
library(eRm)
mod.R <-RM(df)
round(cbind(-mod.R$betapar, mod.R$se.beta),3)
summary(mod.R)

p.R <- person.parameter(mod.R)
p.R

# run 1PL model
library(ltm)
ltm_1pl <- rasch(df)
summary(ltm_1pl)
# get the item beta
coef(ltm_1pl)
# get the person latent trait
ltm_trait_out <- factor.scores(ltm_1pl, method='EAP')
ltm_trait <- ltm_trait_out$score.dat
sum_score <- apply(ltm_trait[,c(1:33)], 1, function(x) sum(x, na.rm = T))
ltm_trait$sum_score <- sum_score
View(ltm_trait[which(sum_score==20 & complete.cases(ltm_trait[,c(1:33)])),])

# run 1PL model using mirt

library(mirt)
spec <- 'F = 1-33
CONSTRAIN = (1-33, a1)'
irt_1pl <- mirt(df, model = spec, itemtype = "2PL", SE=T)
irt_1pl
# get the estimated person trait
mirt_trait_out <- fscores(irt_1pl, method = 'EAP', full.scores = F)
mirt_trait <- as.data.frame(mirt_trait_out)
sum_score <- apply(mirt_trait[,c(1:33)], 1, function(x) sum(x, na.rm = T))
summary(sum_score)
mirt_trait$sum_score <- sum_score
View(mirt_trait[which(sum_score==20 & complete.cases(mirt_trait[,c(1:33)])),])
dim(mirt_trait)


# ------------- shorter test -----------------
item_fit_1pl <- itemfit(irt_1pl, na.rm = T)
info_set <- c()
# using a for loop to get all info value at this given trait
for (i in 1:33) {
  info_tempt <- iteminfo(extract.item(irt_1pl, i), .4018117)
  info_set[i] <- info_tempt
}
# make a new df
info_matrix <- data.frame(
  item = item_fit_1pl[,c("item")],
  info_value = info_set
)
# sort this df in decreasing order
info_matrix <- info_matrix[order(-info_matrix$info_value),]
# get the first 10 items
info_matrix[c(1:10),]
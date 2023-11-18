library(mirt)
# load the cleaned dataset
df <- read.csv("/Users/panpeter/Desktop/PhD_Learning/HUDM6052 Psychometric II/HUDM6052_Psychometic_II/assignment 4/binary_reponses.csv")
df <- df[,-1]
names(df)
dim(df)

# run the 2PL model
irt_2pl <- mirt(df, model = 1, itemtype = "2PL", SE=T)
irt_2pl

# find the trait level who have the true score of 20
theta <- fscores(irt_2pl, method = 'EAP')
tscore <- expected.test(irt_2pl, theta)
t_t <- data.frame(
  theta= theta,
  tscore = tscore
)

theta <- fscores(irt_2pl, method = 'EAP')
tscore <- expected.test(irt_2pl, theta)
t_t <- data.frame(
  theta= theta,
  tscore = tscore
)
t_t <- round(t_t,3)
theta_20 <- t_t[t_t$tscore == 20,]$F1
t_t[t_t$tscore == 20,]

info_set <- c()
# using a for loop to get all info value at this given trait
for (i in 1:33) {
  info_tempt <- iteminfo(extract.item(irt_2pl, i), theta_20)
  info_set[i] <- info_tempt
}

item_fit_2pl <- itemfit(irt_2pl, na.rm = T)
# make a new df
info_matrix <- data.frame(
  item = item_fit_2pl[,c("item")],
  info_value = info_set
)
# sort this df in decreasing order
info_matrix <- info_matrix[order(-info_matrix$info_value),]
# get the first 10 items
info_matrix[c(1:10),]


df_short <- df[,which(names(df) %in% info_matrix$item[1:10])]
dim(df_short)
# fit 2PL on this short test
irt_2pl_short <- mirt(df_short, model = 1, itemtype = "2PL", SE=T)
irt_2pl_short
plot(irt_2pl_short)
plot(irt_2pl)

# find the trait level who have the true score of 20
theta <- fscores(irt_2pl_short, method = 'EAP')

theta_tempt <- as.matrix(seq(-6,6,0.01))
tscore <- expected.test(irt_2pl, theta_tempt)
theta_tempt[tscore >19.9 & tscore <20.1]
theta_20 <- theta_tempt[tscore== 20]
theta_20

plot(theta_tempt, tscore)
t_t <- data.frame(
  theta= theta,
  tscore = tscore
)
t_t <- round(t_t,3)


un <- fscores(irt_2pl_short, method = 'EAP', full.scores = T, full.scores.SE = T)
un <- as.data.frame(un)
summary(un$F1)

test_info <- testinfo(irt_2pl_short, theta_tempt)
test_info[theta_tempt == theta_8]

e1 <- exp(1.5)
e2 <- exp(-0.5)
e3 <- exp(-2)
e1
e2
e3

sum <- e1e2+e2e3+e1e3

p1 <- (e1e2)/sum
p2 <- (e1e3)/sum
p3 <- (e2e3)/sum
p1
p2
p3
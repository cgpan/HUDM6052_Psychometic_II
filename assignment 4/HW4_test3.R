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
t_t <- round(t_t,3)
View(t_t)
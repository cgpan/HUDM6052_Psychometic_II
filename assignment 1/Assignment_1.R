# define the theta
theta <- seq(-3,3, by=0.1)
a <- 1
b <- 1
Z <- a*(theta-b)

# define the normal ogive model
nom_2pl <- function(theta, a, b){
  Z <- a*(theta-b)
  func_ <- function(probit){(1/sqrt(2*pi))*exp(-0.5*probit^2)}
  p_list <- c()
  for (i in 1:length(Z)) {
    p_i <- integrate(func_, -Z[i], Inf)
    p_list[i] <- p_i
  }
  return(p_list)
}

P <- nom_2pl(theta = theta, a=1, b=1)

plot(theta, P)


logit_2pl <- function(theta, a, b){
  # get the logit
  Z <- a*(theta-b)
  # get the probability
  P <- 1/(1 + exp(-Z))
  return(P)
}

(tt <- logit_2pl(theta = theta,a=1,b=1))

# define the beta vector as input
B <- c(-2, -1, -0.5, 0.5, 1)

# define the beta vector as input
B <- c(-2, -1, -0.5, 0.5, 1)

# using a for loop to get all results
Ps <- list()
for (i in 1:length(B)){
  # note a is a constant with value of 1.702 in 1PL model.
  P_item1_logit <- logit_2pl(theta=theta,a=1.702,b=i)
  # store the result into a list
  Ps[[i]] <- P_item1_logit
}

Ps[[1]]

# plot all five ICCs on same plot
# replot the two ICCs
plot(theta, Ps[[1]], type = "l", 
     col="red", xlim = c(xlim_min,xlim_max), ylim = c(0,1),
     main = "5 ICCs using 1pl logistic model", ylab = "P(theta)")
# using a for loop to plot the rest 4 ICCs
for (i in c(2:5)) {
  lines(theta, Ps[i], col="red")
}
abline(h=0.5, col="blue", lty=2)
grid()


# read the csv file
df <- read.csv("~/Desktop/PhD_Learning/HUDM6052 Psychometric II/assignment 1/hw1_4.csv")
result_12 <- chisq.test(table(df[,c(2,3)]))
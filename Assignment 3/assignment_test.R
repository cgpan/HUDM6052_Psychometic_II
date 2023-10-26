# write the corresponding information function
iif <- function(theta, a, b){
  # get the logit
  Z <- a*(theta-b)
  # get the probability
  out <- 1/(1 + exp(-Z))
  info <- out*(1-out)*(a^2)
  return(info)
}

irt_2pl <- function(theta, a, b){
  # get the logit
  Z <- a*(theta-b)
  # get the probability
  p <- 1/(1 + exp(-Z))
  return(p)
}

iif_test <- function(P, a){
  info_test <- (a^2)*P*(1-P)
  return(info_test)
}

# set the ability range
theta <- seq(-3,3, by=0.1)
# set the item parameters
a <- c(2, 1.5, 1.5, 1.5, 2)
b <- c(-1, -0.5, 0, 0.5, 1)

P <- irt_2pl(theta,a[1],b[1])

iif_out <- iif_test(P, a = a[2])
iif_out

# plot the item information function
info_out <- iif(theta, a[2], b[2])
info_out

SE_out <- 1/sqrt(info_out)

SE_out

plot(theta, info_out,type = "l", col="red",
     main = "Information and SE for Item 1")
lines(theta, SE_out, type ="o", col="blue")

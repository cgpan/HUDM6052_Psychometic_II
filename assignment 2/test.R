vec_1 <- c(1,2,3,4)
vec_2 <- c(1,0,2,1)

vec_1*vec_2

vec_2_m <- diag(vec_2)

t(vec_1) %*% vec_2_m

diag(vec_1)%*%t(t(vec_2))

vec_1^2

# Sample vectors
a <- c(1, 2, 3)
b <- c(4, 5, 6)
c <- c(7, 8, 9)

# Element-wise multiplication and sum
result <- sum(a * b * c)

# Print the result
print(result)

# load the given values
## the assumed theta_j
theta_set <- seq(-3,3,by =0.5)
## number of correct responses for each theta_j
r_set <- c(0,0,1,2,3,4,5,6,4,4,4,2,1)
## number of respondents for each theta_j
f_set <- c(1,2,4,7,8,9,10,8,6,5,5,2,1)

# get the proportion of correct responses for each theta_j
p_set <- r_set/f_set

# to count the iteration times
iter_time <- 0
deltas <- t(t(c(1,-1)))
zeta <- 0
lambda <- 1.0
abs(deltas[2,1])
abs(deltas[1,1])

# set the initial value
iter_time <- iter_time+1

# define the 2PL model:
# note, both the input and output are vectors rather than scalars
P <- 1/(1+exp(-(zeta+lambda*theta_set)))
Q <- 1-P
print(P)
print(Q)
# define the weight in case of using normal ogive in the future
W <- P*Q
print(W)
diag(round(W,3))

# Define the L1, L11, L2, L22, L12
# Note, to make computation efficient, I use matrix operation
# vec_tempt_1 <- (p_set -P)/(P*Q)
# vec_tempt_2 <- (p_set -P)*theta_set/(P*Q)
vec_tempt_1 <- (p_set -P)
print(vec_tempt_1)
vec_tempt_2 <- (p_set -P)*theta_set
print(vec_tempt_2)

theta_sq_vec <- theta_set^2
print(theta_sq_vec)

L1 <- t(f_set)%*%t(t(vec_tempt_1))
print(L1)

L2 <- t(f_set)%*%t(t(vec_tempt_2))
print(L2)

L11 <- -t(f_set)%*%t(t(W))
print(L11)

L22 <- -t(f_set)%*%diag(W)%*%t(t(theta_sq_vec))
L22_ <- sum(f_set*W*theta_sq_vec)
print(L22)


L12 <- -t(f_set)%*%diag(W)%*%t(t(theta_set))

# make them into matrix form
matrix_L <- matrix(c(L11,L12,L12,L22),2,2)
vector_L <- t(t(c(L1, L2)))
print(paste0("Matrix_L: ",matrix_L))
# get the delta zeta and delta lambda
deltas <- -solve(matrix_L)%*%vector_L
print(paste0("Deltas: ",deltas))
# update the zeta and lambda
updated_parameters <- t(t(c(zeta, lambda))) - deltas
zeta <-updated_parameters[1,1]
lambda <- updated_parameters[2,1]

# ------------------------------------------------------------------

while((abs(deltas[1,1])>0.005) & (abs(deltas[2,1])>0.005)) {
  # set the initial value
  iter_time <- iter_time+1
  print("--------------------test 1------------------")
  # define the 2PL model:
  # note, both the input and output are vectors rather than scalars
  P <- 1/(1+exp(-(zeta+lambda*theta_set)))
  Q <- 1-P
  print("------------P------------")
  print(P)
  # define the weight in case of using normal ogive in the future
  W <- P*Q
  
  # Define the L1, L11, L2, L22, L12
  # Note, to make computation efficient, I use matrix operation
  # vec_tempt_1 <- (p_set -P)/(P*Q)
  # vec_tempt_2 <- (p_set -P)*theta_set/(P*Q)
  vec_tempt_1 <- (p_set -P)
  vec_tempt_2 <- (p_set -P)*theta_set
  theta_sq_vec <- theta_set^2
  L1 <- sum(f_set*vec_tempt_1)
  L2 <- t(f_set)%*%t(t(vec_tempt_2))
  L11 <- -t(f_set)%*%t(t(W))
  L22 <- -t(f_set)%*%diag(W)%*%t(t(theta_sq_vec))
  L12 <- -t(f_set)%*%diag(W)%*%t(t(theta_set))
  
  # make them into matrix form
  matrix_L <- matrix(c(L11,L12,L12,L22),2,2)
  vector_L <- t(t(c(L1, L2)))
  print(paste0("Matrix_L: ",matrix_L))
  # get the delta zeta and delta lambda
  deltas <- -solve(matrix_L)%*%vector_L
  print(paste0("Deltas: ",deltas))
  # update the zeta and lambda
  updated_parameters <- t(t(c(zeta, lambda))) + deltas
  zeta <-updated_parameters[1,1]
  lambda <- updated_parameters[2,1]
}
print(deltas[1])
print(deltas[2])
print(paste0("The iteration time is: ", iter_time," ."))
print(paste0("The estimated zeta is: ", zeta," ."))
print(paste0("The estimated lambda is: ", lambda," ."))

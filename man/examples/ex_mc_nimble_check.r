library(nimble)

n <- 100
x <- 1:n
beta0 <- 12
beta1 <- 3.7
linear_predictor <- beta0 + beta1 * x
sigma <- 42
y <- rnorm(n, linear_predictor, sd = sigma)

data <- list(y = y)
constants <- list(n = n, x = x)
inits <- list(beta0 = 0, beta1 = 0, sigma = 10)

code <- nimbleCode({
   beta0 ~ dnorm(0, sd = 100)
   beta1 ~ dnorm(0, sd = 100)
   sigma ~ dunif(0, 100)
   for (i in 1:n) y[i] ~ dnorm(beta0 + beta1 * x[i], sd = sigma)
})

model <- nimbleModel(
   code = code,
   constants = constants,
   data = data,
   inits = inits,
   check = TRUE
)
model$calculate()
mc_nimble_check(model)

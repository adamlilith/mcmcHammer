library(nimble)

set.seed(1)

n <- 100
x <- 1:n
beta0 <- 12
beta1 <- 3.7
linear_predictor <- beta0 + beta1 * x
sigma <- 42
y <- rnorm(n, linear_predictor, sd = sigma)

data <- list(y = y)
constants <- list(n = n, x = x)
inits <- list(beta0 = 0, beta1 = 0, sigma = 1)

# nimble model
code <- nimbleCode({
   beta0 ~ dnorm(0, sd = 100)
   beta1 ~ dnorm(0, sd = 100)
   sigma ~ dunif(0, 100)
   for (i in 1:n) y[i] ~ dnorm(beta0 + beta1 * x[i], sd = sigma)
})

# build model
model <- nimbleModel(
   code = code,
   constants = constants,
   data = data,
   inits = inits,
   check = TRUE
)
model$calculate()
mc_nimble_check(model)

monitors <- c('beta0', 'beta1', 'sigma')
conf <- configureMCMC(
   model,
   monitors = monitors,
   print = FALSE,
   enableWAIC = FALSE
)
build <- buildMCMC(conf)
compiled <- compileNimble(model, build, showCompilerOutput = FALSE)

# MCMC samples to be saved here
model_dir <- tempdir()

# (using small number of iterations to show continuity between sets)
mc_nimble_rolling(
	model_dir = model_dir,
	code = code,
	config = config,
	compiled = compiled,
	niter_per_set = 20,
	max_sets = 3,
   inits = inits,
   verbose = TRUE
)

mcmc_files <- list.files(model_dir, pattern = 'chains_set_')
mcmc_files

# combine samples into a chain
chain <- mc_combine_rolling_sets(model_dir)

# trace plots
plot(chain[ , 'beta0'])
plot(chain[ , 'beta1'])
plot(chain[ , 'sigma'])

# run 2 more sets
mc_nimble_rolling(
	model_dir = model_dir,
	code = code,
	config = config,
	compiled = compiled,
	niter_per_set = 20,
	max_sets = 5,
   inits = inits,
   verbose = TRUE
)

# combine samples into a chain
chain <- mc_combine_rolling_sets(model_dir)

# trace plots
plot(chain[ , 'beta0'])
plot(chain[ , 'beta1'])
plot(chain[ , 'sigma'])


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
model_dir_1 <- paste0(tempdir(), '/chain_1')
model_dir_2 <- paste0(tempdir(), '/chain_2')

model_dirs <- c(model_dir_1, model_dir_2)

# (using small number of iterations to show continuity between sets)
mc_nimble_rolling(
	model_dir = model_dir_1,
	code = code,
	config = config,
	compiled = compiled,
	niter_per_set = 20,
	max_sets = 3,
   inits = inits,
   verbose = TRUE
)

mc_nimble_rolling(
	model_dir = model_dir_2,
	code = code,
	config = config,
	compiled = compiled,
	niter_per_set = 20,
	max_sets = 3,
   inits = inits,
   verbose = TRUE
)

mcmc_files <- list.files(model_dir_1, pattern = 'chain_set_')
mcmc_files

# combine samples into a chain
chains <- mc_compile_chains(model_dirs)

# trace plots
plot(chains$samples[ , 'beta0'])
plot(chains$samples[ , 'beta1'])
plot(chains$samples[ , 'sigma'])

# run 2 more sets for each chain
mc_nimble_rolling(
	model_dir = model_dir_1,
	code = code,
	config = config,
	compiled = compiled,
	niter_per_set = 20,
	max_sets = 5,
   inits = inits,
   verbose = TRUE
)

mc_nimble_rolling(
	model_dir = model_dir_2,
	code = code,
	config = config,
	compiled = compiled,
	niter_per_set = 20,
	max_sets = 5,
   inits = inits,
   verbose = TRUE
)

# combine samples into a chain
chains <- mc_compile_chains(model_dirs)

# trace plots
plot(chains$samples[ , 'beta0'])
plot(chains$samples[ , 'beta1'])
plot(chains$samples[ , 'sigma'])


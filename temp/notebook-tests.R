library(devtools)
load_all()

####################################################################
### Put code to test notebooks here ################################
### use a subset of the observations to make it faster #############
####################################################################

data <- as.matrix(macrodata[1:100, ])

############################
### SVBVAR #################
############################

svbvar_test <- nb_svbvar(data, lags = 2)

############################
### BART ###################
############################
# about 7 mins on the airbook with 100 obs (7 dims)
# about xx mins on the airbook with all observations (7 dims)
aa <- Sys.time()
bart_test <- nb_bart(macrodata) 
Sys.time() - aa

############################
### TVPSVBVAR ##############
############################

aa <- Sys.time()
tvpsvbvar_test <- nb_tvpsvbvar(data) # about xx min on the airbook with 100 obs
Sys.time() - aa


# Debugging tvpsvbvar
nrep <- 1000
nburn <- 500
tau <- 20

testdata <- as.matrix(macrodata[1:60, 1:3])
bv <- bvarsv::bvar.sv.tvp(testdata, nf = 1, nrep = nrep, nburn = nburn, tau = tau)
summary(bv)
bv$H.postmean

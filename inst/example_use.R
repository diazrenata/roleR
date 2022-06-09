library(roleR)

# make params
p <- roleParams()

# make arguments
argz <- roleArguments(niter, ..., params = p)

# pass arguments to roleExperiment
e <- roleExperiment(argz)



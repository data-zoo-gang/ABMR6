library(R6)
library(profvis)

rm(list = ls())
example <- FALSE
source("World.R")
source("Butterfly.R")
source("Population.R")
source("Simulation.R")

simu <- Simulation$new(years = 200, N = 500, mutation_rate = 1e-2, period = 50)
simu$output
simu$run()
simu$plot()
simu$profile()

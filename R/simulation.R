#' R6 class representing a simulation of the peppered moth system
#' 
#' @description 
#' R6 class representing a simulation of the peppered moth system.
#' 
#' @details
#' Used to generate a world with N peppered moth individuals that can be either black
#' or white. Method `run()` can be used to simulate the evolution of the moth colour over time
#' as the colour of the world changes.
#' 
#' @export
#' @import R6
#' @examples
#' #Normal example where moths change colour with world
#' set.seed(123)
#' simu <- simulation$new(years = 200, N = 100, mutation_rate = 1e-2, period = 10)
#' simu$run()
#' simu$plot()
#' 
#' #Example with low mutation rate that leads to fixation
#' set.seed(332041868)
#' simu <- simulation$new(years = 200, N = 100, mutation_rate = 1e-4, period = 10)
#' simu$run()
#' simu$plot()
simulation <- R6Class(classname = "simulation",
  public = list(
    
    ## Attributes #######################

    #' @field world R6 object of class world. World to simulate.
    world = NA,
    
    #' @field population R6 object of class population. Population of moths within the world.
    population = NA,
    
    #' @field years Integer. Number of years over which to simulate.
    years = NA,
    
    #' @field output Data frame. Output of simulation.
    output = NULL,
    
    ## Methods #######################
    
    #Always need a constructor (called 'initializer' in R6)
    
    #' @description
    #' Initialize new R6 object of class 'simulation'. 
    #'
    #' @param years Integer. Number of years over which to simulate.
    #' @param N Integer. Size of population within the world.
    #' @param mutation_rate Probability. Probability that moths can change colour in a timestep.
    #' @param period period Integer. Number of time steps after which colour of world changes.
    #'
    #' @return An R6 object of class 'simulation'
    initialize = function(years = 10000, N = 100, mutation_rate = 1e-3, period = 100) {
      self$world <- world$new(period)
      self$population <- population$new(N, mutation_rate, self$world)
      self$output <- data.frame(year = 1:years,
                                black_frequency = rep(NA, years),
                                avg_fitness = rep(NA, years),
                                world_dark = rep(NA, years))
      self$years <- years
    },
    
    #' @description
    #' Run simulation.
    #' 
    #' @details
    #' Simulation will run for a number of time steps defined by `years` attribute.
    #' 
    #' @param year_to_browse Integer. The year at which the simulation should be paused to explore its content (or NULL, the default, not to interrupt the simulation).
    #'
    run = function(year_to_browse = NULL){
      time <- system.time({
        pb <- txtProgressBar(min = 1, max = self$years, style = 3)
        for (t in 1:self$years) {
          if (!is.null(year_to_browse) && t == year_to_browse) browser()
          self$output$black_frequency[t] <- self$population$fetch_black_proportion()
          self$output$avg_fitness[t] <- self$population$fetch_avg_fitness()
          self$output$world_dark[t] <- self$world$colour
          self$world$moveforward()
          self$population$generation()
          setTxtProgressBar(pb, t)
        }
      })
      cat("\n")
      print(paste("time elapsed = ", round(time[[3]]), "sec"))
    },
    
    #' @description
    #' Plot simulation.
    plot = function(){
      plot(self$output$world_dark, type = "l", ylim = c(0, 1),
           lwd = 2, col = "grey", las = 1, xlab = "Time (years)", ylab = "Frequency of black")
      points(self$output$black_frequency, type = "l", lwd = 2, col = "blue")
    }
  )
)

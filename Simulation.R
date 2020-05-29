Simulation <- R6Class(
  public = list(
    world = NA,
    population = NA,
    years = NA,
    output = NULL,
    
    initialize = function(years = 10000, N = 100, mutation_rate = 1e-3, period = 100) {
      self$world <- World$new(period)
      self$population <- Population$new(N, mutation_rate, self$world)
      self$output <- data.frame(year = 1:years,
                                black_frequency = rep(NA, years),
                                world_dark = rep(NA, years))
      self$years <- years
    },
    
    run = function(){
      time <- system.time({
        pb <- txtProgressBar(min = 1, max = self$years, style = 3)
        for (t in 1:self$years) {
          black <- unlist(lapply(self$population$individuals, function(ind) ind$colour))
          self$output$black_frequency[t] <- mean(black)
          self$output$world_dark[t] <- self$world$colour
          self$world$moveforward()
          self$population$generation()
          setTxtProgressBar(pb, t)
        }
      })
      cat("\n")
      print(paste("time elapsed = ", round(time[[3]]), "sec"))
    },
    
    plot = function(){
      plot(self$output$world_dark, type = "l", ylim = c(0, 1),
           lwd = 2, col = "grey", las = 1, xlab = "Time (years)", ylab = "Frequency of black")
      points(self$output$black_frequency, type = "l", lwd = 2, col = "blue")
    },
    
    profile = function(){
      if (require("profvis")) {
        profvis(self$run())
      } else {
        message("Profiling could only be performed if the package profvis is installed.")
      }
    }
  )
)

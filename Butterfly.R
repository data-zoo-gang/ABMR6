Butterfly <- R6Class(
  class = FALSE,
  public = list(
    ## variables
    colour = NA,
    mutation_rate = NA,
    world = NA,  ## we can pass the address of an R6 object!
    fitness = NA,
    
    ## constructor
    initialize = function(mutation_rate = 1e-6, world) {
      self$mutation_rate <- mutation_rate
      self$world <- world
      self$colour <- round(runif(1))
    },
    
    ## function
    mutation = function() {
      if (runif(n = 1) < self$mutation_rate) {
        self$colour <- abs(self$colour - 1)
      }
    },
    
    computefitness = function() {
      self$fitness <- abs(self$world$colour + self$colour - 1)*0.5 + 0.5
    }
  )
)

if (example) {
  my_world <- World$new()
  my_butterfly <- Butterfly$new(mutation_rate = 1e-1, world = my_world)
  my_world$colour
  my_butterfly$colour
  my_butterfly$computefitness()
  my_butterfly$fitness
  my_world$colour <- 1
  my_butterfly$computefitness()
  my_butterfly$fitness
  my_butterfly$colour <- 1
  my_butterfly$computefitness()
  my_butterfly$fitness
  
  my_world <- World$new(period = 10)
  my_butterfly <- Butterfly$new(mutation_rate = 1e-1, world = my_world)
  plot(NULL, ylim = c(0, 1), xlim = c(1, 100), xlab = "time", ylab = "fitness", las = 1)
  for (t in 1:100) {
    my_world$moveforward()
    abline(v = t, col = my_world$colour, lwd = 5)
    my_butterfly$mutation()
    my_butterfly$computefitness()
    points(t, my_butterfly$fitness, col = 1 - my_butterfly$colour, bg = my_butterfly$colour, pch = 21)
  }
}


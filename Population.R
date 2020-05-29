Population <- R6Class(
  class = FALSE,
  public = list(
    individuals = NA,

    initialize = function(N = 100, mutation_rate = 1e-6, world) {
      self$individuals <- list()
      for (i in 1:N) {
        self$individuals[[i]] <- Butterfly$new(mutation_rate, world)
      }
    },
    
    computefitness = function() {
      for (i in 1:length(self$individuals)) {
        self$individuals[[i]]$computefitness()
      }
    },
    
    mutation = function() {
      for (i in 1:length(self$individuals)) {
        self$individuals[[i]]$mutation()
      }
    },
    
    reproduction = function() {
      N <- length(self$individuals)
      fitnesses <- unlist(lapply(self$individuals, function(ind) ind$fitness))
      id_reproductor <- sample(1:N, size = N, prob = fitnesses, replace = TRUE)
      
      method <- 2
      if (method == 1) {
        ## Easy
        parents <- self$individuals
        for (i in 1:N) {
          self$individuals[[i]] <- parents[[id_reproductor[i]]]$clone(deep = FALSE)
        }
      }
      
      if (method == 2) {
        ## Optimised: we do not touch the one that make one offspring
        whodies <- setdiff(1:N, id_reproductor)
        howmanyoffspring <- table(id_reproductor) 
        whomultiparent <- as.numeric(names(howmanyoffspring)[howmanyoffspring > 1])
        whereoffspring <- rep(whomultiparent, howmanyoffspring[howmanyoffspring > 1] - 1)
        for (i in 1:length(whodies)) {
          self$individuals[[whodies[i]]] <- self$individuals[[whereoffspring[i]]]$clone(deep = FALSE)
        }
      }
    },
    
    generation = function(){
      self$computefitness()
      self$mutation()
      self$reproduction()
    }
  )
)

if (example) {
  set.seed(2L)
  my_world <- World$new()
  my_population <- Population$new(N = 4, world = my_world)
  my_population$individuals[[1]]$colour <- 1
  my_population$computefitness()
  print(unlist(lapply(my_population$individuals, function(i) i$colour)))
  my_population$reproduction()
  print(unlist(lapply(my_population$individuals, function(i) i$colour)))
}
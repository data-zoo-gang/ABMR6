#' R6 class representing population of multiple moths
#' 
#' @description 
#' R6 class representing population of multiple moths.
#' 
#' @export
#' @import R6
#' @examples
#'  set.seed(2L)
#'  my_world <- world$new()
#'  my_population <- population$new(N = 4, world = my_world)
#'  my_population$individuals[[1]]$colour <- 1
#'  my_population$computefitness()
#'  print(unlist(lapply(my_population$individuals, function(i) i$colour)))
#'  my_population$reproduction()
#'  print(unlist(lapply(my_population$individuals, function(i) i$colour)))

population <- R6Class(classname = "population",
  public = list(
    
    ## Attributes #######################
    
    #' @field individuals List. List of R6 objects of class 'moth'.
    individuals = NA,

    ## Methods #######################
    
    #Always need a constructor (called 'initializer' in R6)
    
    #' @description
    #' Initialize new R6 object of class 'population'.
    #'
    #' @param N Integer. Number of individual moths in the population.
    #' @param mutation_rate Probability. Probability that moths can change colour in a timestep.
    #' @param world R6 object of class world. World in which moths live.
    #'
    #' @return An R6 object of class 'population'
    
    initialize = function(N = 100, mutation_rate = 1e-6, world) {
      self$individuals <- list()
      for (i in 1:N) {
        self$individuals[[i]] <- moth$new(mutation_rate, world)
      }
    },
    
    #' @description
    #' Compute fitness of all moths in the population.
    #' 
    #' @details
    #' Run the method `computefitness` of each moth object in the population.
    #' 
    #' @examples
    #' set.seed(123L)
    #' my_world <- world$new()
    #' my_pop   <- population$new(N = 50, world = my_world)
    #' 
    #' #Fitness of all moths is NA on initialisation
    #' my_pop$individuals[[1]]$fitness
    #' 
    #' #Compute fitness of all moths
    #' my_pop$computefitness()
    #' 
    #' #Plot fitness of moths in the world
    #' all_fitness <- sapply(my_pop$individuals, function(x){x$fitness}) 
    #' hist(all_fitness)
    computefitness = function() {
      for (i in 1:length(self$individuals)) {
        self$individuals[[i]]$computefitness()
      }
    },
    
    #' @description
    #' Change colour of all moths.
    #' 
    #' @details
    #' Run the method `mutation` of each moth object in the population.
    #' 
    #' @examples
    #' set.seed(123L)
    #' my_world <- world$new()
    #' my_pop   <- population$new(N = 50, mutation_rate = 0.75, world = my_world)
    #' 
    #' #Colour of moths should be ~equal at start
    #' all_colour <- sapply(my_pop$individuals, function(x){x$colour})
    #' hist(all_colour)
    #' 
    #' #Change colour of all moths with probability defined by `mutation_rate`
    #' #Colours have changed
    #' my_pop$mutation()
    #' all_colour <- sapply(my_pop$individuals, function(x){x$colour}) 
    #' hist(all_colour)
    mutation = function() {
      for (i in 1:length(self$individuals)) {
        self$individuals[[i]]$mutation()
      }
    },
    
    #' @description
    #' Moths reproduce with a probability defined by their fitness in the environment.
    #' 
    #' @details
    #' Offspring are clones of their parents (i.e. they have the same colour!)
    #' 
    #' @examples 
    #' set.seed(123L)
    #' my_world <- world$new()
    #' my_pop   <- population$new(N = 50, world = my_world)
    #' 
    #' #At initialization colour of moths is ~even
    #' all_colour <- sapply(my_pop$individuals, function(x){x$colour})
    #' hist(all_colour)
    #'
    #' #The world is black (i.e. colour = 1)
    #' #So black moths will have higher fitness than white moths
    #' my_world$colour
    #' my_pop$computefitness()
    #' 
    #' #Black moths have higher fitness and more more likely to reproduce
    #' #After reproduction, black individuals are much more common
    #' my_pop$reproduction()
    #' all_colour <- sapply(my_pop$individuals, function(x){x$colour})
    #' hist(all_colour)
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
    
    #' @description
    #' Run all demographic methods.
    #' 
    #' @details
    #' Compute fitness, mutation and reproduction for all moths.
    #' 
    #' @examples
    #' set.seed(123L)
    #' my_world <- world$new()
    #' my_pop   <- population$new(N = 50, world = my_world)
    #' 
    #' #At initialization colour of moths is ~even
    #' all_colour <- sapply(my_pop$individuals, function(x){x$colour})
    #' hist(all_colour)
    #' 
    #' #The world is black (i.e. colour = 1)
    #' #So black moths will have higher fitness than white moths
    #' my_world$colour
    #' my_pop$generation()
    #' 
    #' #There will be more black moths in the next generation
    #' all_colour <- sapply(my_pop$individuals, function(x){x$colour})
    #' hist(all_colour)
    generation = function(){
      self$computefitness()
      self$mutation()
      self$reproduction()
    }
  )
)
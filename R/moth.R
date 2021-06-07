#' R6 class representing an individual moth
#' 
#' @description 
#' R6 class representing an individual moth.
#' 
#' @export
#' @import R6
#' @examples
#' #Create a world in which the moth lives
#' example_world <- world$new()
#' 
#' #Create new moth object
#' example_moth <- moth$new(world = example_world)
#' 
#' #Moth has empty fitness initially
#' example_moth$fitness
#' 
#' #Compute fitness and then print it
#' example_moth$computefitness()
#' example_moth$fitness

moth <- R6Class(classname = "moth",
  public = list(
    
    ## Attributes #######################
    
    #' @field colour Integer. Colour of the moth. Can be either 0 (white) or 1 (black)
    colour = NA,
    
    #' @field mutation_rate Probability. Probability of mutation in genes that control colour.
    mutation_rate = NA,
    
    #' @field world R6 object of class world. World in which moth lives.
    world = NA,
    
    #' @field fitness Numeric. Fitness of individual in current world. Ranges between 0 and 1.
    fitness = NA,
    
    ## Methods #######################
    
    #Always need a constructor (called 'initializer' in R6)
    
    #' @description
    #' Initialize new R6 object of class 'moth'. 
    #'
    #' @param mutation_rate Probability. Probability of mutation in genes that control colour.
    #' @param world R6 object of class world. World in which moth lives.
    #'
    #' @return An R6 object of class 'moth'

    initialize = function(mutation_rate = 1e-6, world) {
      
      self$mutation_rate <- mutation_rate
      self$world         <- world
      #Colour of moth is assigned randomly on initialization
      self$colour        <- round(runif(1, min = 0, max = 1))
      
    },
    
    #' @description
    #' Change moth colour
    #' 
    #' @details 
    #' Moth colour will change with a probability defined by attribute
    #' `mutation_rate`
    #' 
    #' @examples
    #' #Moth with low mutation rate is very unlikely to change colour
    #' set.seed(123L)
    #' my_world <- world$new()
    #' low_mutation_moth  <- moth$new(mutation_rate = 1e-10, world = my_world)
    #' low_mutation_moth$colour
    #' low_mutation_moth$mutation()
    #' low_mutation_moth$colour
    #' 
    #' #Moth with mutation rate 1 is almost guaranteed to change colour
    #' high_mutation_moth <- moth$new(mutation_rate = 1, world = my_world)
    #' high_mutation_moth$colour
    #' high_mutation_moth$mutation()
    #' high_mutation_moth$colour
    mutation = function() {
      if (runif(n = 1) < self$mutation_rate) {
        self$colour <- abs(self$colour - 1)
      }
    },
    
    #' @description
    #' Compute fitness of individual in current environment.
    #' 
    #' @details
    #' Compare colour of moth to the colour of the world and update attribute
    #' `fitness` accordingly. Fitness can be either 0.5 or 1.
    #' 
    #' @examples 
    #' set.seed(123L)
    #' my_world <- world$new()
    #' my_moth  <- moth$new(world = my_world)
    #' 
    #' #World is black (i.e. colour = 1) and the moth is white (i.e. colour = 0)
    #' #Therefore, fitness is lower (0.5)
    #' my_world$colour
    #' my_moth$colour
    #' my_moth$computefitness()
    #' my_moth$fitness
    #' 
    #' #If the world were also white, moth fitness is 1.
    #' my_world$colour <- 0
    #' my_moth$computefitness()
    #' my_moth$fitness
    computefitness = function() {
      self$fitness <- abs(self$world$colour + self$colour - 1)*0.5 + 0.5
    }
  )
)


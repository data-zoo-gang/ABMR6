#' R6 class representing the world in which the moths live
#' 
#' @description
#' R6 class representing the world in which the moths live
#' 
#' @details
#' Just like moths, the world has a colour either 0 (white) or 1 (black). During a
#' simulation, the colour of the world will change with a frequency defined by the
#' attribute `period`. 
#' 
#' @export
#' @import R6
#' @examples
#' my_world <- world$new(period = 50)
#' 
#' 
#' plot(NULL, ylim = c(0, 1), xlim = c(1, 1000), xlab = "time", ylab = "", axes = FALSE)
#' axis(1)
#' 
#' #Move the world forward 1000 time steps
#' #World will change colour every 50
#' for (t in 1:1000) {
#'   my_world$moveforward()
#'   abline(v = t, col = my_world$colour, lwd = 5)
#' }

world <- R6Class(classname = "world",
                 public = list(
                   ## Attributes #######################
                   
                   #' @field time Integer. Number of time steps that have passed in the world.
                   time = 1,
                   
                   #' @field colour Integer. Colour of the world. Can be either 0 (white) or 1 (black).
                   colour = 1,
                   
                   #' @field period Integer. Number of time steps after which colour of world changes.
                   period = NA,
                   
                   ## Methods #######################
                   
                   #Always need a constructor (called 'initializer' in R6)
                   
                   #' @description
                   #' Initialize new R6 object of class 'world'. 
                   #'
                   #' @param period Integer. Number of time steps after which colour of world changes.
                   #'
                   #' @return An R6 object of class 'world'
                   initialize = function(period = 100){
                     self$period <- period
                   },
                   
                   #' @description
                   #' Change colour of the world.
                   #' 
                   #' @details 
                   #' World will change colour at a given frequency specified by the
                   #' attribute `period`.
                   #' 
                   #' @examples 
                   #' #Create a world that should change colour every 50 timesteps
                   #' set.seed(123L)
                   #' my_world <- world$new(period = 50)
                   #' 
                   #' #Colour of world starts as black (i.e. 1)
                   #' my_world$colour
                   #' 
                   #' #Once time reaches 50, colour will change to white (i.e. 0)
                   #' my_world$time <- 50
                   #' my_world$computecolour()
                   #' my_world$colour
                   computecolour = function() {
                     relative_time <- self$time %% (2*self$period) ## check that, it seems to lead to small initial period sometimes
                     if (self$period == Inf) relative_time <- self$time
                     self$colour <- as.numeric(relative_time < self$period)
                   },
                   
                   #' @description
                   #' Advance time by 1.
                   #'
                   #' @details
                   #' Time of the world will move forward by 1, after which we check to see
                   #' if the colour of the world should change given the attribute `period`.
                   #' 
                   #' @examples
                   #' my_world <- world$new(period = 20)
                   #' #Starts with time 1
                   #' my_world$time
                   #' 
                   #' #Moves to time 2 after moveforward
                   #' my_world$moveforward()
                   #' my_world$time
                   moveforward = function(){ 
                     self$time <- self$time + 1
                     self$computecolour()  ## one function can call another
                   }
                 )
)

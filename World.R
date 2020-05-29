World <- R6Class(
  class = FALSE,
  public = list(
                ## variables
                time = 1,  ## you can define variable directly
                colour = 0,
                period = NA,  ## you can also have variables defined by the constructor
                
                ## constructor
                initialize = function(period = 100){  ## constructors can have default settings
                  self$period <- period  ## pass the argument from the function call to the object
                },
                
                ## functions
                computecolour = function() { ## function are  working directly in the environment of the object
                  relative_time <- self$time %% (2*self$period) ## check that, it seems to lead to small initial period sometimes
                  if (self$period == Inf) relative_time <- self$time
                  self$colour <- as.numeric(relative_time < self$period)
                },
                
                moveforward = function(){ 
                  self$time <- self$time + 1
                  self$computecolour()  ## one function can call another
                  }
                )
)

if (example) {
  my_world <- World$new(period = 50)
  print(my_world)
  
  plot(NULL, ylim = c(0, 1), xlim = c(1, 1000), xlab = "time", ylab = "", axes = FALSE)
  axis(1)
  for (t in 1:1000) {
    my_world$moveforward()
    abline(v = t, col = my_world$colour, lwd = 5)
  }
  print(my_world)
}

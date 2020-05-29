globals [env]

turtles-own 
[
  trait
  fitness
  adult
]

to setup
  ca
  reset-ticks
  
  crt population-size
  [
    set trait 0
    set fitness 0
    set adult true
  ]
  
  set env 0
  
end


to go
  tick
  
  env-dyn
  
  pop-dyn
  
end

to env-dyn
  ifelse ticks mod (2 * period) < period 
  [set env 0]
  [set env 1]
  
end

to pop-dyn
  ask turtles
  [
    set fitness 0.5 + 0.5 * (1 - abs(env - trait))
    
    hatch 2 * fitness
    [
      set adult false
      if random-float 1 < mutation-rate [set trait abs(trait - 1)] 
    ]  
  ]
  
  ask turtles with [adult = true] [die]
  ask n-of population-size turtles [set adult true]
  ask turtles with [adult = false] [die]
  
end

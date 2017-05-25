# This code can also be found in the `examples` folder under this name:
# FallingParticleODE.R
#

setClass("FallingParticleODE", slots = c(
    g = "numeric"
),
prototype = prototype(
    g = 9.8
),
contains = c("ODE")
)


setMethod("initialize", "FallingParticleODE", function(.Object, ...) {
    .Object@state <- vector("numeric", 5)
    return(.Object)
})

setMethod("getState", "FallingParticleODE", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

setMethod("getRate", "FallingParticleODE", function(object, state, ...) {
    # Gets the rate of change using the argument's state variables.
    object@rate[1] <- state[2] * state[3]
    object@rate[2] <- - state[1] * state[3]
    object@rate[3] <- -0.51 * state[1] * state[2]
    object@rate[4] <- 1
    
    object@rate
})


# constructor
FallingParticleODE <- function(y1, y2, y3) {
    .FallingParticleODE <- new("FallingParticleODE")
    .FallingParticleODE@state[1] <- y1
    .FallingParticleODE@state[2] <- y2
    .FallingParticleODE@state[3] <- y3
    .FallingParticleODE@state[4] <- 0
    .FallingParticleODE
}


# This code can also be found in the `examples` folder under this name:
# 
# FallingParticleODEApp.R
#
#
FallingParticleODEApp <- function(verbose = FALSE) {
    library(ggplot2)
    
    # load the R class that sets up the solver for this application
    
    y1 <- 0   # initial y position
    y2 <- 1    # initial x position
    y3 <- 1    # initial x position
    dt        <- 0.01 # delta time for step
    
    
    ball <- FallingParticleODE(y1, y2, y3)
    
    solver <- Euler(ball)
    solver <- setStepSize(solver, dt)
    
    rowVector <- vector("list")
    i <- 1
    # stop loop when the ball hits the ground
    while (ball@state[4] <= 12) {
        rowVector[[i]] <- list(t = ball@state[4],
                               y1 = ball@state[1], 
                               y2 = ball@state[2], 
                               y3 = ball@state[3])
        solver <- step(solver)
        ball <- solver@ode
        if (verbose) {
            cat(sprintf("%12f %12f %12f %12f \n",  
                        ball@state[4], ball@rate[1],
            ball@state[2],
            ball@state[3]))
        }
        i <- i + 1
    }
    
    DT <- data.table::rbindlist(rowVector)
    return(DT)
}


DT <- FallingParticleODEApp(TRUE)
plot(DT)
plot(y1~t, data = DT, type ="l", col = "blue", pch = 1)
lines(y2~t, data = DT, col = "green", pch = 2)
lines(y3~t, data = DT, col = "red", pch = 3)
legend("topright", legend=c("y1","y2", "y3"), pch=c(1,2,3), 
       col = c("blue", "green", "red"))

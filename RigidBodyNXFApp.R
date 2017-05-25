#
# RigidBodyNXFApp.R
#
# example of a nonstiff system is the system of equations describing
# the motion of a rigid body without external forces.

setClass("RigidBodyNXF", slots = c(
    g = "numeric"
),
prototype = prototype(
    g = 9.8
),
contains = c("ODE")
)


setMethod("initialize", "RigidBodyNXF", function(.Object, ...) {
    .Object@state <- vector("numeric", 5)
    return(.Object)
})

setMethod("getState", "RigidBodyNXF", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

setMethod("getRate", "RigidBodyNXF", function(object, state, ...) {
    # Gets the rate of change using the argument's state variables.
    object@rate[1] <- state[2] * state[3]
    object@rate[2] <- - state[1] * state[3]
    object@rate[3] <- -0.51 * state[1] * state[2]
    object@rate[4] <- 1
    
    object@rate
})


# constructor
RigidBodyNXF <- function(y1, y2, y3) {
    .RigidBodyNXF <- new("RigidBodyNXF")
    .RigidBodyNXF@state[1] <- y1
    .RigidBodyNXF@state[2] <- y2
    .RigidBodyNXF@state[3] <- y3
    .RigidBodyNXF@state[4] <- 0
    .RigidBodyNXF
}


# This code can also be found in the `examples` folder under this name:
# 
# RigidBodyNXFApp.R
#
#
RigidBodyNXFApp <- function(verbose = FALSE) {
    library(ggplot2)
    
    # load the R class that sets up the solver for this application
    
    y1 <- 0   # initial y1 value
    y2 <- 1    # initial y2 value
    y3 <- 1    # initial y3 value
    dt        <- 0.01 # delta time for step
    
    
    body <- RigidBodyNXF(y1, y2, y3)
    
    solver <- Euler(body)
    solver <- setStepSize(solver, dt)
    
    rowVector <- vector("list")
    i <- 1
    # stop loop when the body hits the ground
    while (body@state[4] <= 12) {
        rowVector[[i]] <- list(t = body@state[4],
                               y1 = body@state[1], 
                               y2 = body@state[2], 
                               y3 = body@state[3])
        solver <- step(solver)
        body <- solver@ode
        if (verbose) {
            cat(sprintf("%12f %12f %12f %12f \n",  
                        body@state[4], body@rate[1],
            body@state[2],
            body@state[3]))
        }
        i <- i + 1
    }
    
    DT <- data.table::rbindlist(rowVector)
    return(DT)
}

# get the data table from the app
DT <- RigidBodyNXFApp(TRUE)

# multiplot
plot(DT)

# plot lines for time, y1, y2, and y3
plot(y1~t, data = DT, type ="l", col = "blue", pch = 1)
lines(y2~t, data = DT, col = "green", pch = 2)
lines(y3~t, data = DT, col = "red", pch = 3)
legend("topright", legend=c("y1","y2", "y3"), pch=c(1,2,3), 
       col = c("blue", "green", "red"))

# ggplot
library(ggplot2)
library(dplyr)
library(tidyr)

DTplot <- DT %>% gather(key, value, -t)
g <- ggplot(DTplot, mapping = aes(x = t, y = value, color = key)) 
g <-  g + geom_line()
print(g)

# .scatter plot matrix. pair plot
pairs(~t+y1+y2+y3,data=DT)


library(car)
scatterplotMatrix(~t+y2+y2+y3, data=DT,
                   main="Rigid Body - No external forces")

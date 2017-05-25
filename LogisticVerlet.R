# Logistic.R
#
library(rODE)


setClass("Logistic", slots = c(
    K = "numeric",
    r = "numeric",
    odeSolver = "Verlet",
    counter = "numeric"
),
contains = c("ODE")
)

setMethod("initialize", "Logistic", function(.Object, ...) {
    .Object@K <- 10
    .Object@r <- 1.0
    .Object@state <- vector("numeric", 3)  # x, vx
    .Object@odeSolver <- Verlet(.Object)
    .Object@counter <- 0
    return(.Object)
})

setMethod("doStep", "Logistic", function(object, ...) {
    # cat("state@doStep=", object@state, "\n")
    object@odeSolver <- step(object@odeSolver)
    
    object@state <- object@odeSolver@ode@state
    
    object
})

setMethod("getTime", "Logistic", function(object, ...) {
    return(object@state[3])
})


setMethod("init", "Logistic", function(object, initState, r, K, ...) {
    object@r <- r
    object@K <- K
    object@state <- initState
    object@odeSolver <- init(object@odeSolver, getStepSize(object@odeSolver))
    
    object@counter <- 0
    object
})


setMethod("getRate", "Logistic", function(object, state, ...) {
    # Computes the rate using the given state.
    object@rate[1] <- state[2]
    object@rate[2] <- object@r * state[1] * (1 - state[1] / object@K)
    object@rate[3] <- 1   # time derivative
    
    object@counter <- object@counter + 1
    object@rate
    
})


setMethod("getState", "Logistic", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})

# constructor
Logistic <- function() {
    logistic <- new("Logistic")
    return(logistic)
}




#
# Demostration of the use of ODE solver
#

LogisticVerletApp <- function(verbose = FALSE) {
    
    x  <- 0.1
    vx <- 0
    r  <- 2        # Malthusian parameter (rate of maximum population growth)
    K  <- 10.0     # carrying capacity of the environment
    
    dt   <- 0.01
    tol  <- 1e-3
    tmax <- 10
    
    population <- Logistic()
    population <- init(population, c(x, vx, 0), r, K)
    
    odeSolver <- Verlet(population)
    odeSolver <- init(odeSolver, dt)
    population@odeSolver <- odeSolver
    
    rowVector <- vector("list")
    i <- 1
    while (getTime(population) <= tmax) {
        rowVector[[i]] <- list(t = getTime(population),
                               s1 = population@state[1],
                               s2 = population@state[2]
                               )
        population <- doStep(population)
        if (verbose)
            cat(sprintf("time=%12f state[1]=%11f state[2]=%11f \n",
                        getTime(population),
                        population@state[1], population@state[2]))
        i <- i + 1
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)
}


DT <- LogisticVerletApp(verbose = TRUE)



# multiplot
plot(DT)

# plot lines for time, y1, y2
plot(s1~t, data = DT, type ="l", col = "blue", pch = 1)
lines(s2~t, data = DT, col = "green", pch = 2)
legend("topright", legend=c("y1","y2"), pch=c(1,2,3),
       col = c("blue", "green", "red"))

# ggplot
library(ggplot2)
library(dplyr)
library(tidyr)

DTplot <- DT %>% gather(key, value, -t)
g <- ggplot(DTplot, mapping = aes(x = t, y = value, color = key))
g <-  g + geom_line()
print(g)


library(car)
scatterplotMatrix(~t+s1+s2, data=DT,
                  main="Logistic equation")

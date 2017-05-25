# This code can also be found in the `examples` folder under this name:
#
# Kepler.R
#


setClass("Kepler", slots = c(
    GM = "numeric"
),
contains = c("ODE")
)

setMethod("initialize", "Kepler", function(.Object, ...) {
    .Object@GM <- 1.0                 # gravitation constant times combined mass
    .Object@state <- vector("numeric", 5)  # x, vx, y, vy, t
    return(.Object)
})


setMethod("getState", "Kepler", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})


setMethod("getRate", "Kepler", function(object, state, ...) {
    # Computes the rate using the given state.
    r2 <- state[1] * state[1] + state[3] * state[3]  # distance squared
    r3 <- r2 * sqrt(r2)   # distance cubed
    object@rate[1] <- state[2]
    object@rate[2] <- (- object@GM * state[1]) / r3
    object@rate[3] <- state[4]
    object@rate[4] <- (- object@GM * state[3]) / r3
    object@rate[5] <- 1   # time derivative
    
    object@rate
})

# constructor
Kepler <- function(r, v) {
    kepler <- new("Kepler")
    kepler@state[1] = r[1]
    kepler@state[2] = v[1]
    kepler@state[3] = r[2]
    kepler@state[4] = v[2]
    kepler@state[5] = 0
    return(kepler)
}

# This code can also be found in the `examples` folder under this name:
#
# KeplerApp.R
#
KeplerApp <- function(verbose = FALSE) {
    
    # set the orbit into a predefined state.
    r <- c(2, 0)
    v <- c(0, 0.25)
    dt <- 0.1
    
    planet <- Kepler(r, v)
    solver <- RK45(planet)
    
    rowVector <- vector("list")
    i <- 1    
    while (planet@state[5] <= 10) {
        rowVector[[i]] <- list(t =  planet@state[5],
                               r1 = planet@state[1],
                               v1 = planet@state[2],
                               r2 = planet@state[3],
                               v2 = planet@state[4]
                               )
        solver <- step(solver)
        planet <- solver@ode
        i <-  i + 1
        
        if (verbose)
            cat(sprintf("state[1]=%10f, state[2]= %10f, state[3]=%10f, state[5]=%10f\n",
                        planet@state[1],
                        planet@state[2], planet@state[3], planet@state[5]))
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)

}

DT <- KeplerApp(verbose = TRUE)
plot(DT)

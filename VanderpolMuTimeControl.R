# *************************
#
# VanderpolMuTimeControl.R
#
# This is a modification of the original Vanderpol.R script
# In this version, we will add tha ability of setting mu and time lapse
#


setClass("VanderPol", slots = c(
    mu = "numeric"
),
contains = c("ODE")
)

setMethod("initialize", "VanderPol", function(.Object, ...) {
    .Object@mu <- 1.0                 # gravitation constant times combined mass
    .Object@state <- vector("numeric", 3)  # y1, y2, t
    return(.Object)
})


setMethod("getState", "VanderPol", function(object, ...) {
    # Gets the state variables.
    return(object@state)
})


setMethod("getRate", "VanderPol", function(object, state, ...) {
    # Computes the rate using the given state.
    object@rate[1] <- state[2]
    object@rate[2] <-  object@mu * (1 - state[1]^2) * state[2] - state[1]
    object@rate[3] <- 1
    object@rate
})

# constructor
VanderPol <- function(y1, y2, mu = 1.0) {
    VanderPol <- new("VanderPol")
    VanderPol@state[1] = y1
    VanderPol@state[2] = y2
    VanderPol@state[3] = 0
    VanderPol@mu       = mu
    return(VanderPol)
}


# Main
#
VanderpolMuTimeControl <- function(verbose = FALSE) {
    
    # set the orbit into a predefined state.
    y1 <- 2
    y2 <- 0
    mu <- 10
    tmax <- mu * 3
    dt <- 0.1
    
    rigid_body <- VanderPol(y1, y2, mu)
    solver <- RK45(rigid_body)
    
    rowVector <- vector("list")
    i <- 1    
    while (rigid_body@state[3] <= tmax) {
        rowVector[[i]] <- list(t =  rigid_body@state[3],
                               y1 = rigid_body@state[1],
                               y2 = rigid_body@state[2]
                               )
        solver <- step(solver)
        rigid_body <- solver@ode
        i <-  i + 1
        
        if (verbose)
            cat(sprintf("state[3]=%10f, state[1]= %10f, state[2]=%10f \n",
                        rigid_body@state[3],
                        rigid_body@state[1], rigid_body@state[2]))
    }
    DT <- data.table::rbindlist(rowVector)
    return(DT)

}

# main

DT <- VanderpolMuTimeControl(verbose = TRUE)
plot(DT)

# ggplot
library(ggplot2)
library(dplyr)
library(tidyr)

DTplot <- DT %>% gather(key, value, -t)
g <- ggplot(DTplot, mapping = aes(x = t, y = value, color = key)) 
g <-  g + geom_line(size = 1)
print(g)

h <- ggplot(DT, mapping = aes(x = t, y = y1)) 
h <-  h + geom_line(size = 1)
print(h)

j <- ggplot(DT, mapping = aes(x = t, y = y2)) 
j <-  j + geom_line(size = 1, col = "blue")
print(j)

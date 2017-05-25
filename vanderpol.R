# This code can also be found in the `examples` folder under this name:
#
# VanderPol.R
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
    object@rate[2] <-  object@mu* (1 - state[1]^2) * state[2] - state[1]
    object@rate[3] <- 1
    
    object@rate
})

# constructor
VanderPol <- function(y1, y2) {
    VanderPol <- new("VanderPol")
    VanderPol@state[1] = y1
    VanderPol@state[2] = y2
    VanderPol@state[3] = 0
    return(VanderPol)
}

# This code can also be found in the `examples` folder under this name:
#
# VanderPolApp.R
#
VanderPolApp <- function(verbose = FALSE) {
    
    # set the orbit into a predefined state.
    y1 <- 2
    y2 <- 0
    dt <- 0.1
    
    rigid_body <- VanderPol(y1, y2)
    solver <- RK45(rigid_body)
    
    rowVector <- vector("list")
    i <- 1    
    while (rigid_body@state[3] <= 20) {
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

DT <- VanderPolApp(verbose = TRUE)
plot(DT)

# ggplot
library(ggplot2)
library(dplyr)
library(tidyr)

DTplot <- DT %>% gather(key, value, -t)
g <- ggplot(DTplot, mapping = aes(x = t, y = value, color = key)) 
g <-  g + geom_line(size = 1)
print(g)

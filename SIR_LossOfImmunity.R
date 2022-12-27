library(readr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(plotly)
library(RColorBrewer)
library(deSolve)

## Create an SIR function
sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I + alpha * R 
    dI <- beta * S * I - gamma * I 
    dR <- gamma * I - alpha * R
    return(list(c(dS, dI, dR)))
  })
}
### Set parameters
## Proportion in each compartment: Susceptible 0.199999, Infected 0.000001, Recovered 0.8
init<- c(S = 1 - 1e-6 - 0.8, I = 1e-6, R = 0.8)
## beta: infection parameter; gamma: recovery parameter
## alpha = rate at which removed become susceptible again
parameters <- c(beta = 0.293335, gamma = 0.066667, alpha = 0.001) #scenario 1
#parameters <- c(beta = 0.293335, gamma = 0.066667, alpha = 0.01) #scenario 2

## Time frame
times <- seq(0, 3650, by = 0.1) # for alpha = 0.001 (scenario 1)
#times <- seq(0, 600, by = 0.1) # for alpha = 0.01 (scenario 2)
## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
head(out)


names(out) = c("Time","Susceptible","Infected","Removed")


dat.SIR = melt(out,id="Time",measure = c("Susceptible","Infected","Removed"))

head(dat.SIR)
names(dat.SIR) = c("Time","Compartment","Value")


pp1 = ggplot(dat.SIR) +
  geom_line(aes(x = Time,y = Value,color=Compartment),size=1.2) +
  theme_minimal()+
  xlab ("Time [days]")  +
  ylab("Proportion of Population")+
  theme_classic() + 
  theme(text = element_text(size = 20)) +
  ylim(0,1)+ 
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "red2")) 

show(pp1)

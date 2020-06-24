# description: Simulation script for R Carpentry - P-values
#
# author: Anne Lyngholm Soerensen
# email: lynganne@gmail.com
# date: 2020-05-18

#### introduction 
# simulate height from australia
nobs <- 100
meanAUS <- 170
sdAUS <- 7
meanNZ <- 170
sdNZ <- 7

set.seed(24062020)
heightAUS <- rnorm(nobs, mean = meanAUS, sd = sdAUS)
heightNZ <- rnorm(nobs, mean = meanNZ, sd = sdNZ)
hist(heightAUS)
hist(heightNZ)

# statistical inference
heightTest <- t.test(heightAUS, heightNZ, var.equal = TRUE)

str(heightTest)
heightTest$p.value

#### p-values
# (uniform distribution under the null) simulate from the null
zval <- rnorm(n=100000, mean = 0, sd = 1)
# proportion of extremes
quantile(zval, p = c(0.025, 0.975))
pnorm(1.96)
qnorm(0.975)

# simulate from the null
nsim <- 1000
simmean <- numeric(length = 1000)
simpval <- numeric(length = 1000)

for (i in 1:nsim){
    heightAUS <- rnorm(nobs, mean = meanAUS, sd = sdAUS)
    heightNZ <- rnorm(nobs, mean = meanNZ, sd = sdNZ)
    simtest <- t.test(heightAUS, heightNZ, var.equal = TRUE)
    simmean[i] <- simtest$estimate[2]-simtest$estimate[1]
    simpval[i] <- simtest$p.value
}

# look at the distribution
hist(simpval, breaks = 30)
abline(v = 0.05, col = "red")
table(simpval < 0.05)/nsim
    
# Try simulating not under the null
meanAUS <- 169
meanAUS <- 165
table(simpval < 0.05)/nsim
hist(simpval, breaks = 30, xlim = c(0,0.1)) # try running 165 a couple of times

# show the dependency on sample size
nobs <- 25

#### Point estimate
diff <- unname(heightTest$estimate[1]-heightTest$estimate[2])

simout <- lazySim(nsim = 1000, nobs = 100, meanAUS = 170, meanNZ = 170,
                  sdAUS = 7, sd = 7)
hist(simout$simmean)
abline(v = diff, col = "red")

#### Confidence interval
# confidence intervals under the null hypothesis
simout <- lazySim(nsim = 1000, nobs = 100, meanAUS = 170, meanNZ = 170,
                  sdAUS = 7, sd = 7)
head(simout)
table(simout$lower < 0 & simout$upper > 0)/nsim

# show a selection of point estimates and confidence intervals
plot(y = 1:5, x = simout$simmean[1:5], pch = 16, cex = 2, xlim = c(-5,5))
points(y = 1:5, x = simout$lower[1:5], col = "blue", cex = 2, pch = 16)
points(y = 1:5, x = simout$upper[1:5], col = "blue", cex = 2, pch = 16)

#### Simulation function

lazySim <- function(nsim, nobs, meanAUS, meanNZ, sdAUS, sdNZ){

    # create the vectors, where we will store the means and p-values
    simmean <- numeric(length = nsim)
    simpval <- numeric(length = nsim)
    simconfint <- matrix(0, ncol = 2, nrow = nsim)

    # run the loop - simulating new data for each i from 1 to nsim 
    for (i in 1:nsim){
        heightAUS <- rnorm(nobs, mean = meanAUS, sd = sdAUS)
        heightNZ <- rnorm(nobs, mean = meanNZ, sd = sdNZ)
        simtest <- t.test(heightAUS, heightNZ, var.equal = TRUE)
        simmean[i] <- simtest$estimate[1]-simtest$estimate[2]
        simpval[i] <- simtest$p.value
        simconfint[i,] <- simtest$conf.int
    }

    # return a data frame
    return(data.frame(simmean, simpval, "lower" = simconfint[,1], "upper" = simconfint[,2]))
}

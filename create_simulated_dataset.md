Create simulated example dataset
================
Marije Sluiskes

Simulate dataset for example analysis using penalized survRRR function.

# Define parameters

``` r
set.seed(18)

p <- 10 # number of predictors
r <- 2 # number of ranks
k <- 6 # number of outcomes
n <- 1000 # number of rows
```

# Create true Beta matrix

Alpha and Gamma matrix elements are drawn from a uniform\[0,1\]
distribution.

``` r
# Alpha matrix
A_matrix <- matrix(NA, nrow = p, ncol = r)
for (i in 1:r) {
  A_matrix [, i] <- runif(p, 0,1)
}

# Gamma matrix
Gamma_matrix <- matrix(NA, nrow = k, ncol = r)
for (i in 1:r) {
  Gamma_matrix [, i] <- runif(k, 0,1)
}

# Beta matrix
Beta_matrix = A_matrix %*% t(Gamma_matrix)
Beta_matrix
```

    ##             [,1]       [,2]      [,3]      [,4]       [,5]      [,6]
    ##  [1,] 0.35669664 0.39303477 0.8128694 0.9444806 0.64923901 0.8896342
    ##  [2,] 0.41015636 0.42697318 0.7443343 0.8787573 0.73596351 0.8655121
    ##  [3,] 0.07638921 0.16769725 0.8109222 0.8956896 0.17443306 0.7172719
    ##  [4,] 0.16114800 0.14655416 0.1307979 0.1692499 0.28017184 0.2063517
    ##  [5,] 0.51428204 0.44684353 0.2583451 0.3669257 0.88528939 0.5269645
    ##  [6,] 0.07858303 0.12823364 0.4966023 0.5538083 0.16067928 0.4586246
    ##  [7,] 0.28306760 0.28384570 0.4311418 0.5165782 0.50333367 0.5290437
    ##  [8,] 0.03650185 0.08530245 0.4269093 0.4709168 0.08554189 0.3753453
    ##  [9,] 0.49608593 0.46429223 0.5027843 0.6300532 0.86805969 0.7180634
    ## [10,] 0.04119117 0.05838841 0.1929948 0.2170003 0.08048292 0.1847239

# Create dataset

``` r
# Make covariate matrix X (i.i.d., normally distributed)
X <- matrix(NA, nrow = n, ncol = p)
for (i in 1:p) {
  X[, i] <- rnorm(n, mean = 0, sd = 1)
}

# Generate the failure times T (exponentially distributed)
# Linear predictors
eta <- X %*% Beta_matrix
# Outcome times T from exp distribution
T <- matrix(NA, nrow = n, ncol = k)
for (i in 1:k) {
   T[, i] <- rexp(n, rate =  exp(eta)[,i])
}

# Generate censoring time C
#Random censoring time from uniform distribution
C <- runif(n, min = 0, max = 1)

# Determine the observed time and create censoring indicator
T_obs <- pmin(T, C)
delta <- as.integer(T <= C)  # 1 if the event occurred, 0 if censored
# Check the proportion of events
prop_event <- sum(delta == 1)/n/k
prop_event
```

    ## [1] 0.419

``` r
# Create matrix with observed times and status indicators 
observed_times = T_obs
event = matrix(delta,n,k)

# Make matrix into dataframe, add proper column names
data <- data.frame(id = 1:n)
for (i in 1:k) {
  data[[paste0("t", i)]] <- observed_times[, i]
  data[[paste0("d", i)]] <- event[, i]
}
# Add predictors 
for (i in 1:p) {
  data[[paste0("x", i)]] <- X[, i]
}
```

This results in a dataset with $n$ individuals, where each individual
has a time and status indicator for the $k$ different events under
consideration, and $p$ predictor variables The true coefficient matrix
Beta is of rank $r$.

``` r
simdat <- data 
save(simdat, file = "simdat.RData")
```

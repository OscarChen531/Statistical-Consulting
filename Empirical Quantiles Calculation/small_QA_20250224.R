# Task: We want to generate a random sample of size n from a Normal distribution 
# with fix mean mu and fix standard deviation sigma. We then explore R’s built-in cut() 
# function (without using any additional packages) to categorize the data. 
# Next, we compute and compare empirical (sample) quantiles and the corresponding 
# theoretical (population) quantiles, plot them against each other in a scatterplot, 
# and finally create a normal QQ plot using only base R functions to assess how 
# closely the sampled data follow the specified Normal distribution.



set.seed(2023) # Sets random generator seed for consistent output

# -------------------------------------------------------
# Set parameters and generate sample data
# -------------------------------------------------------
n <-  sample(1:3000, 1)    # Number of observations
mu <- 5       # Fix Mean
sigma <- 2    # Fix Standard deviation


# -------------------------------------------------------
# Generate sample data
# -------------------------------------------------------
sample_data <- rnorm(n, mean = mu, sd = sigma)

#-------------------------------------------------------
# Explore the 'cut' function
#-------------------------------------------------------
# Define breakpoints: break the data into 7 categories: from mu +/- 3*sigma
# (-Inf, mu-3*sigma]    (mu-3*sigma, mu-2*sigma]     (mu-2*sigma, mu-sigma]   
# (mu-sigma,mu]     (mu,mu+sigma]     (mu+sigma,mu+2*sigma]    
# (mu+2*sigma,mu+3*sigma]  (mu+3*sigma, Inf] 

break_points <- c(-Inf, mu - 3*sigma, mu - 2*sigma, mu - sigma, mu, 
                  mu + sigma, mu + 2 *sigma, mu + 3*sigma, Inf)

categorical_factor  <- cut(sample_data, breaks = break_points)

# See how many data points fall into each category
table(categorical_factor)

# ---------------------------------------------------------
# Empirical CDF F_hat(t) and its inverse F_hat_inv(p)
# ---------------------------------------------------------
# Sort the data
sample_data_sorted <- sort(sample_data)

# Empirical CDF function: F_hat(t)
F_hat <- function(sample_data, t) {
  return(sum(sample_data <= t) / length(sample_data))
}

# Empirical CDF inverse: 
#    Given p in [0,1], return the smallest x_i s.t. F_hat(x_i) >= p.
#    (One simple way is to choose x_sorted[ceiling(p * n)] if p>0;
#     for p=0, we can return the minimum, etc.)
F_hat_inv <- function(sample_data_sorted, p) {
  n <- length(sample_data_sorted)
  if (p <= 0) return(sample_data_sorted[1])
  if (p >= 1) return(sample_data_sorted[n])
  idx <- ceiling(p * n)
  return(sample_data_sorted[idx])
}

# -----------------------------------------------
# Compare empirical vs. theoretical quantiles
# -----------------------------------------------
# Let's pick a set of probabilities:
probs <- seq(0, 1, 0.01) 

# a) Empirical quantiles using F_hat_inv() "by hand"
sample_quantiles <- sapply(probs, function(p) F_hat_inv(sample_data_sorted, p))

# b) Theoretical quantiles from N(mu, sigma)
population_quantiles <- qnorm(probs, mean = mu, sd = sigma)

cat("\nEmpirical quantiles (by hand):\n")
print(sample_quantiles)
cat("\nTheoretical quantiles:\n")
print(population_quantiles)


################ Sol 2 #################################
#-------------------------------------------------------
# Compute sample quantiles
#-------------------------------------------------------
# Generates the quantiles for a normal distribution from 0 to 1 by increments of 0.01
# probs <- seq(0, 1, 0.01)   
# sample_quantiles <- quantile(sample_data, probs = probs)
#-------------------------------------------------------
# Compute theoretical (population) quantiles
#-------------------------------------------------------
# For a Normal(mu, sigma), the quantiles are qnorm(probs, mean = mu, sd = sigma)
# population_quantiles <- qnorm(probs, mean = mu, sd = sigma)


#-------------------------------------------------------
# Create a scatterplot of sample vs. theoretical quantiles
#-------------------------------------------------------
plot(population_quantiles, sample_quantiles,
     xlab = "Theoretical Quantiles (Normal)",
     ylab = "Sample Quantiles",
     main = "Scatterplot of Sample vs. Theoretical Quantiles")
abline(0, 1, col = "blue")  # Add reference line

#-------------------------------------------------------
# Create a normal QQ plot (base R)
#-------------------------------------------------------
# Using qqnorm (centers and scales for standard normal)
qqnorm(sample_data, main = "QQ Plot of Sample Data vs. Standard Normal")
qqline(sample_data, col = "red")

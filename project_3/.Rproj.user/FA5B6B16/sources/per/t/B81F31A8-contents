library(latex2exp)
### Problem 2
importance_weights <- function(x) {
  dnorm(x = x, mean = 0, sd = 1) /
    dnorm(x = x, mean = 1, sd = sqrt(2))
}

sample_size <- 1e3
set.seed(664)
target_draw <- rnorm(sample_size, # draw from target distribution
                     mean = 0, sd = 1)
instrumental_draw <- rnorm(sample_size, # draw from instrumental distribution
                           mean = 1, sd = sqrt(2))
weighted_sample <- instrumental_draw * # create weighted sample
  importance_weights(instrumental_draw)

mean(weighted_sample); var(weighted_sample) # 0.02561563, 1.226633

par(mfrow = c(1,2)) # plot
hist(weighted_sample, main = 'Histogram of Weighted Sample',
     xlab = 'Weighted Sample Values')
hist(target_draw, main = 'Histogram of Target Sample',
     xlab = 'Target Sample Values')
dev.off()

### Problem 4
target_distribution <- function(x, delta = 0.7) { # target distribution
  delta * dnorm(x = x, mean = 7, sd = 0.5) +
    (1 - delta) * dnorm(x = x, mean = 10, sd = 0.5)
}

proposal_distribution <- function(input, given) { # proposal distribution
  dnorm(x = input, mean = given, sd = 1e-2)
}

proposal_sample <- function(x_t) { # sample from proposal
  rnorm(n = 1, mean = x_t, sd = 1e-2)
}

mh_ratio <- function(x_t, x_star) { # M-H ratio
  (target_distribution(x = x_star) * # numerator
     proposal_distribution(input = x_t, given = x_star)) /
    (target_distribution(x = x_t) * # denominator
       proposal_distribution(input = x_star, given = x_t))
}

x_0s <- c(0, 7, 15, 20) # possible x^(0)'s

mh <- function(x_init = x_0s[1], num_iterations = 1e4) {
  x_t <- x_init # Initialize variables
  sampling_matrix <- matrix(NA, nrow = num_iterations)
  
  for (index in 1:num_iterations) {
    x_star <- proposal_sample(x_t = x_t)
    ratio_r <- mh_ratio(x_t = x_t, x_star = x_star)
    
    if (ratio_r >= 1) { # first check
      sampling_matrix[index] <- x_star
      x_t <- x_star
    } else {
      u <- runif(n = 1)
      if (u < ratio_r) { # second check
        sampling_matrix[index] <- x_star
        x_t <- x_star
      } else {
        sampling_matrix[index] <- x_t
      }
    }
  }
  return(sampling_matrix)
}

sample_x0_1 <- mh(x_init = x_0s[1]) # generate samples
sample_x0_7 <- mh(x_init = x_0s[2])
sample_x0_15 <- mh(x_init = x_0s[3])

par(mfrow = c(2,2)) # plot sample paths and histograms
plot(1:1e4, sample_x0_1, type = 'l', main = TeX('Sample Path for $x^{(0)}=0$'),
     ylab = TeX('$x^{(t)}$'), xlab = TeX('$t$'))
plot(1:1e4, sample_x0_7, type = 'l', main = TeX('Sample Path for $x^{(0)}=7$'),
     ylab = TeX('$x^{(t)}$'), xlab = TeX('$t$'))
plot(1:1e4, sample_x0_15, type = 'l', main = TeX('Sample Path for $x^{(0)}=15$'),
     ylab = TeX('$x^{(t)}$'), xlab = TeX('$t$'))
dev.off()

par(mfrow = c(2,2))
hist(sample_x0_1, main = TeX('Histogram of Samples from $x^{(0)}=1$'),
     xlab = TeX('Values of x^{(t)}'))
hist(sample_x0_7, main = TeX('Histogram of Samples from $x^{(0)}=7$'),
     xlab = TeX('Values of x^{(t)}'))
hist(sample_x0_15, main = TeX('Histogram of Samples from $x^{(0)}=15$'),
     xlab = TeX('Values of x^{(t)}'))
dev.off()


# part (b)
proposal_distribution2 <- function(input) {
  dunif(x = input, min = 0, max = 20)
}

proposal_sample2 <- function() {
  runif(n = 1, min = 0, max = 20)
}

mh_ratio2 <- function(x_t, x_star) {
  (target_distribution(x = x_star) * # numerator
     proposal_distribution2(input = x_t)) /
    (target_distribution(x = x_t) * # denominator
       proposal_distribution2(input = x_star))
}

mh2 <- function(x_init = x_0s[4], num_iterations = 1e4) {
  sampling_matrix <- matrix(NA, nrow = num_iterations)
  x_t <- x_init
  
  for (index in 1:num_iterations) {
    x_star <- proposal_sample2()
    ratio_r <- mh_ratio2(x_t = x_t, x_star = x_star)
    
    if (ratio_r >= 1) {
      sampling_matrix[index] <- x_star
      x_t <- x_star
    } else {
      u <- runif(n = 1)
      if (u < ratio_r) {
        sampling_matrix[index] <- x_star
        x_t <- x_star
      } else {
        sampling_matrix[index] <- x_t
      }
    }
  }
  return(sampling_matrix)
}

set.seed(664)
sample_x0_20 <- mh2(x_init = x_0s[4])
par(mfrow = c(1,2))
hist(sample_x0_20, main = TeX('Histogram of Samples from $x^{(0)}=20$'),
     xlab = TeX('Values of x^{(t)}'))
plot(1:1e4, sample_x0_20, type = 'l', main = TeX('Sample Path for $x^{(0)}=20$'),
     ylab = TeX('$x^{(t)}$'), xlab = TeX('$t$'))
dev.off()

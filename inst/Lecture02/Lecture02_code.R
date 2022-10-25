#### Numerical maximum likelihood estimation ####

# Single simulation study ####
# - S: Simulate; known true model parameters theta_true, simulate data
# - E: Estimate; pretend theta_true are not known, estimate them using the data
# - A: Assess; compare the theta estimates with theta_true

library(ggplot2)
theme_set(theme_bw())

# Simulate data ####
# True model is y_i ~ Gamma(2, 1), i=1,...,100
simulation <- data.frame(values = rgamma(100, shape = 2, scale = 1))
# Simple data plot:
ggplot(simulation) +
  geom_point(aes(
    x = seq_along(values),
    y = values
  )) +
  xlab("Index")

# Empirical cumulative distribution function:
ggplot(simulation) +
  stat_ecdf(aes(x = values))
ggplot(simulation) +
  stat_ecdf(aes(x = values, col = "ECDF")) +
  geom_function(
    fun = pgamma,
    args = list(shape = 2, scale = 1),
    mapping = aes(col = "Theoretical")
  ) +
  ylab("P(Y <= y)") +
  xlab("y") +
  labs(colour = "Type")


# Optimisation target ####

neg_log_like <- function(theta, data) {
  shape <- exp(theta[1])
  scale <- exp(theta[2])
  -sum(dgamma(data$values, shape = shape, scale = scale, log = TRUE))
}

# Storing a plot for later display (and/or modification)
pl <-
  ggplot() +
  xlim(log(c(0.1, 4))) +
  xlab("log(shape)") +
  geom_function(
    fun = ~ vapply(.x,
                   function(x) {
                     neg_log_like(theta = c(x, log(1)),
                                  data = simulation)
                   },
                   0.0),
    mapping = aes(colour = "Target (known scale)")
  ) +
  labs(colour = "Quantity")
# Show
pl
# Add a marker for the true parameter value
pl <- pl + geom_vline(aes(xintercept = log(2), col = "True value"))
pl

# Estimate ####

opt <- optim(
  c(0, 0),
  neg_log_like,
  data = simulation,
  method = "Nelder-Mead",
  control = list(trace = 1)
)

# Inspect the result ####

shape_est <- exp(opt$par[1])
scale_est <- exp(opt$par[2])
shape_est
scale_est

pl + geom_vline(aes(xintercept = log(shape_est), col = "Estimated value"))



# Tidy data ####

data <- data.frame(
  x = 1:4,
  y = 11:14,
  z = 21:24
)
data

# Need to explicitly plot each variable:
ggplot(data) +
  geom_line(aes(x, y), color = "red") +
  geom_line(aes(x, z), color = "blue")

library(tidyverse)
# Collect all variables except "x" into long format:
data %>%
  pivot_longer(-x)

# The "%>%" operator from 'magrittr' (tidyverse provides access to it as well)
# is called the "pipe operator", as it "sends the data through a pipe" from
# one function to the next.
# Essentially, "A %>% f(...)" is equivalent to "f(A, ...)", so that
# g(f(A)) can be written as A %>% f %>% g
# "Take A, first apply f, and then apply g"

# Can automatically plot all the variables:
ggplot(data %>% pivot_longer(-x)) +
  geom_line(aes(x, value, color = name))

# Can automatically split the plot for each variable:
ggplot(data %>% pivot_longer(-x)) +
  geom_line(aes(x, value)) +
  facet_wrap(vars(name))





# Multiple simulation study ####
#
# The single simulation above doesn't provide much information about how
# the estimation method behaves over multiple realisations of the model.
# To get more information we can simulate the whole procedure multiple times.

#' @param N_repeat The number of simulation repetitions
#' @param n The number of observations in each sample
#' @param shape The true model shape parameter
#' @param scale The true model scale parameter
do_simulation <- function(N_repeat, n, shape, scale) {
  results <- list()
  for (loop in seq_len(N_repeat)) {
    # Simulate model observations
    data <- data.frame(values = rgamma(n, shape = shape, scale = scale))
    # Estimate the parameters
    opt <- optim(
      c(0, 0),
      neg_log_like,
      data = data,
      method = "Nelder-Mead"
    )
    # Store the results, including some diagnostic information
    results[[loop]] <-
      data.frame(
        Parameter = c("shape", "scale"),
        True.value = c(shape, scale),
        Estimated.value = exp(opt$par),
        log.likelihood = -opt$value,
        Converged = (opt$convergence == 0)
      )
  }
  # Combine the results
  do.call(rbind, results)
}

# Run the simulation study ####
results <- do_simulation(10000, n = 100, shape = 2, scale = 1)

# Plot the results ####
pl_base <-
  ggplot(results) +
  geom_vline(aes(xintercept = True.value, colour = "True")) +
  facet_wrap(vars(Parameter),
             nrow = 1,
             scales = "free") +
  labs(colour = "Type") +
  xlab("Value")

pl_cdf <-
  pl_base +
  stat_ecdf(aes(Estimated.value, colour = "Estimated")) +
  ylab("CDF")

pl_pdf <-
  pl_base +
  geom_density(aes(Estimated.value, colour = "Estimated")) +
  ylab("PDF")

# Can store a geom to use with multiple plots
pl_quantiles <-
  geom_vline(aes(xintercept = Quantiles,
                 colour = "Quantiles"),
             data = results %>%
               group_by(Parameter) %>%
               summarise(data.frame(
                 Quantiles = quantile(Estimated.value,
                                      probs = c(0.025, 0.5, 0.975)),
                 Probabilities = c(0.025, 0.5, 0.975)),
                 .groups = "drop")
  )

pl_cdf + pl_quantiles
pl_pdf + pl_quantiles

# Note: the outer quantiles give _prediction_ intervals for the
# parameter estimates, given the true values. They do _not_ give
# confidence intervals.

# Let's combine them all:
library(patchwork) # Use |,+,/ to combine plots
(
  (pl_cdf + pl_quantiles) / (pl_pdf + pl_quantiles)
) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Note the special "&" before "theme" that makes it apply to both the
# subplots and the combined plot




# Plotting function ####

make_plot <- function(results) {
  pl_base <-
    ggplot(results) +
    geom_vline(aes(xintercept = True.value, colour = "True")) +
    facet_wrap(vars(Parameter),
               nrow = 1,
               scales = "free") +
    labs(colour = "Type") +
    xlab("Value")

  pl_cdf <-
    pl_base +
    stat_ecdf(aes(Estimated.value, colour = "Estimated")) +
    ylab("CDF")

  pl_pdf <-
    pl_base +
    geom_density(aes(Estimated.value, colour = "Estimated")) +
    ylab("PDF")

  # Can store a geom to use with multiple plots
  pl_quantiles <-
    geom_vline(aes(xintercept = Quantiles,
                   colour = "Quantiles"),
               data = results %>%
                 group_by(Parameter) %>%
                 summarise(data.frame(
                   Quantiles = quantile(Estimated.value,
                                        probs = c(0.025, 0.5, 0.975)),
                   Probabilities = c(0.025, 0.5, 0.975)),
                   .groups = "drop")
    )

  pl_cdf + pl_quantiles
  pl_pdf + pl_quantiles

  # Note: the outer quantiles give _prediction_ intervals for the
  # parameter estimates, given the true values. They do _not_ give
  # confidence intervals.

  # Let's combine them all:
  (
    (pl_cdf + pl_quantiles) / (pl_pdf + pl_quantiles)
  ) + plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
}


# Experimentation ####

results <- do_simulation(10000, n = 100, shape = 2, scale = 1)
make_plot(results)

results <- do_simulation(10000, n = 10, shape = 2, scale = 1)
make_plot(results)

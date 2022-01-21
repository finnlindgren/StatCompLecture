#### Numerical maximum likelihood estimation ####

# Simulation study:
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
  xlab("y")


# Optimisation target ####

neg_log_like <- function(theta, data) {
  shape <- exp(theta[1])
  scale <- exp(theta[2])
  -sum(dgamma(data$values, shape = shape, scale = scale, log = TRUE))
}

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
    mapping = aes(col = "Target (known scale)")
  ) +
  geom_vline(aes(xintercept = log(2), col = "Theoretical"))
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

pl +
  geom_vline(aes(xintercept = log(shape_est), col = "Estimate"))


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

library(tidyr)
# Collect all variables except "x" into long format:
data %>%
  pivot_longer(-x)

# The "%>%" operator is called the "pipe operator", as
# it "sends the data through a pipe from one function to the next.
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


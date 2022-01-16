# Example function
# @param lambda A numeric vector
# @param y A numeric vector of observations from Po(lambda)
# @return A negated log-likelihood function,
#   not including the normalisation constant
my_function <- function(lambda, y) {
  sum(lambda - log(lambda) * y)
}

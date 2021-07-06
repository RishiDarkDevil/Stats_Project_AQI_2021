ev_finder <- function(transform = identity) {
  function(f, ..., from = -Inf, to = Inf) {
    integrate(function(x) transform(x) * f(x, ...), from, to)
  }
}

moment_finder <- function(n, c = 0) {
  ev_finder(function(x) (x - c) ^ n)
}

find_mean <- moment_finder(1)
find_variance <- function(f, ...) {
  mu <- find_mean(f, ...)$value
  moment_finder(2, mu)(f, ...)
}


dsinu <- function(x, alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  sineCurve <- function(z) {
    z.s <- flip * (1 - z) + (!flip) * z
    (sin(pi * z.s^omega))^chi
  }
  
  sineArea <- integrate(sineCurve, lower = 0, upper = 1)$value
  z <- (x - alpha) / delta
  pdf <- numeric(length(x))
  within_support <- (z >= 0) & (z <= 1)
  pdf[within_support] <- sineCurve(z[within_support]) / (sineArea * delta)
  
  return(pdf)
}

psinu <- Vectorize(function(q, alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  if (q <= alpha) {
    cdf <- 0
  } else if (q <= alpha + delta) {
    q.std <- (q - alpha) / delta
    cdf <- integrate(dsinu, lower = 0, upper = q.std, omega = omega, chi = chi, flip = flip)$value
  } else {
    cdf <- 1
  }
  return(cdf)
}, vectorize.args = 'q')

qsinu <- Vectorize(function(p, alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  if (p < 0 || p > 1) {
    return(NA)
  }
  z <- uniroot(function(t) psinu(t, 0, 1, omega, chi, flip) - p, interval = c(0, 1))$root
  return(z * delta + alpha)
}, vectorize.args = 'p')

rsinu <- function(n, alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  qsinu(runif(n), alpha, delta, omega, chi, flip)
}

sinu.rmom <- function(r, alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  rmom.integrand <- function(x) {
    x^r * dsinu(x, alpha = alpha, delta = delta, omega = omega, chi = chi, flip = flip)
  }
  rmomVal <- integrate(rmom.integrand, lower = alpha, upper = alpha + delta)$value
  return(rmomVal)
}

sinu.cmom <- function(r, alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  meanVal <- sinu.rmom(1, alpha, delta, omega, chi, flip)
  cmom.integrand <- function(x) {
    (x - meanVal)^r * dsinu(x, alpha = alpha, delta = delta, omega = omega, chi = chi, flip = flip)
  }
  cmomVal <- integrate(cmom.integrand, lower = alpha, upper = alpha + delta)$value
  return(cmomVal)
}

sinu.mean <- function(alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  sinu.rmom(1, alpha, delta, omega, chi, flip)
}

sinu.var <- function(alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  sinu.cmom(2, alpha, delta, omega, chi, flip)
}

sinu.skew <- function(alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  cmom2 <- sinu.cmom(2, alpha, delta, omega, chi, flip)
  cmom3 <- sinu.cmom(3, alpha, delta, omega, chi, flip)
  (cmom3 / cmom2^(3/2))
}

sinu.kurt <- function(alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  cmom2 <- sinu.cmom(2, alpha, delta, omega, chi, flip)
  cmom4 <- sinu.cmom(4, alpha, delta, omega, chi, flip)
  (cmom4 / cmom2^2) - 3
}

sinu.msm <- function(alpha = 0, delta = 1, omega = 1, chi = 1, flip = FALSE) {
  measures <- c(
    sinu.mean(alpha, delta, omega, chi, flip),
    sinu.var(alpha, delta, omega, chi, flip),
    sinu.skew(alpha, delta, omega, chi, flip),
    sinu.kurt(alpha, delta, omega, chi, flip)
  )
  return(measures)
}
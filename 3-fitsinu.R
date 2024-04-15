fitsinu.msm <- function(msm.target, flip = FALSE, init.par = c(1, 1), lower.par = c(0.001, 0.001), upper.par = c(1000, 1000)) {
  mean.t <- msm.target[1]; var.t <- msm.target[2]; gamma.t <- msm.target[3]; kappa.t <- msm.target[4]

  costFn <- function(pars) {
    omega <- pars[1]; chi <- pars[2]
    skLoss <- sinu.skew(0, 1, omega, chi, flip) - gamma.t
    kuLoss <- sinu.kurt(0, 1, omega, chi, flip) - kappa.t
    sum(abs(skLoss), abs(kuLoss))
  }
  
  optim1 <- optim(init.par, fn = costFn, lower = lower.par, upper = upper.par, method = "L-BFGS-B")
  
  omega_optim <- optim1$par[1]
  chi_optim <- optim1$par[2]
  delta_cor <- sqrt(var.t / sinu.var(0, 1, omega_optim, chi_optim, flip))
  alpha_cor <- mean.t - sinu.mean(0, delta_cor, omega_optim, chi_optim, flip)
  
  optim1$par <- c(alpha_cor, delta_cor, omega_optim, chi_optim)
  
  return(optim1)
}

###########################################################

fitsinu.pdf <- function(PDF.str, flip = FALSE, init.par = c(0, 1, 1, 1), lower.par = c(-1000, 0.001, 0.001, 0.001), upper.par = c(1000, 1000, 1000, 1000)) {
  
  parsed_expr <- parse(text = PDF.str)
  targetPDF <- function(x) eval(parsed_expr)
  
  helDist <- function(pars) {
    aIn <- pars[1]; dIn <- pars[2]; oIn <- pars[3]; cIn <- pars[4]
    ff <- function(x) {
      sqrt(dsinu(x, aIn, dIn, oIn, cIn, flip) * targetPDF(x))
    }
    1 - integrate(ff, lower = aIn, upper = aIn + dIn)$value
  }
  
  optim1 <- optim(init.par, helDist, lower = lower.par, upper = upper.par, method = 'L-BFGS-B')
  optim1
}

#############################################################

fitsinu.ecdf <- function(data, flip = FALSE, init.par = c(1, 1), lower.par = c(0.001, 0.001), upper.par = c(500, 500), pgtol = 0) {
  a.out <- min(data); d.out <- max(data) - a.out
  data.std <- (data - a.out) / d.out; ecdfAdj <- ecdf(data.std)
  
  ks.score <- function(pars) {
    omega <- pars[1]; chi <- pars[2]
    devs <- ecdfAdj(data.std) - psinu(data.std, 0, 1, omega, chi, flip = FALSE)
    return(mean(devs^2))
  }
  
  optim1 <- optim(init.par, ks.score, method = 'L-BFGS-B', lower = lower.par, upper = upper.par, control = list(pgtol = pgtol))
  optim1$par <- c(a.out, d.out, optim1$par)
  
  return(optim1)
}

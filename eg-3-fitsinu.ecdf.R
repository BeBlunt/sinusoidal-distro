fitsinu.ecdf.curve = function (data, str='', flip=F) {
  fit1 = fitsinu.ecdf(data)
  par1 = fit1$par
  
  ecdf1 = ecdf(data)
  curve(ecdf1(x), xlim=c(min(data), max(data)))
  curve(psinu(x, par1[1], par1[2], par1[3], par1[4], flip), add=T, col='red', lwd=2)
  
  pars = paste(round(par1, 1), collapse=', ')
  
  if (str=='')
    str = deparse(substitute(data))
  sinu.str = paste0('Sinu(',pars,')')
  legend('topright', legend=c(str, sinu.str), fill=c('green','red'))
}

######################### Binomial ###########################################
fitsinu.ecdf.curve(rbinom(1000, 50, 0.1))

################################### Poisson ##########################################################
fitsinu.ecdf.curve(rpois(1000, 3.4))



### Normal
data1=rnorm(1000, 50, 30)
ecdf1 = ecdf(data1)
curve(ecdf1(x), xlim=c(min(data), max(data)))
fit1 = fitsinu.cdf(data1)
pars = fit1$par
fit1
curve(psinu(x, pars[1], pars[2], pars[3], pars[4]), add=T, col='red')




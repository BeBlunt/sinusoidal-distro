### fitsinu.pdf.curve for PDFs
fitsinu.pdf.curve = function(PDF.str, str='', flip=F, xlim=c(-3,3)) {
  fit1 = fitsinu.pdf(PDF.str, flip=flip)
  par1 = fit1$par
  
  parsed_expr <- parse(text = PDF.str)
  targetPDF <- function(x) eval(parsed_expr)
  
  curve(targetPDF(x), xlim=xlim, ylab='Density', col='green', lwd=5)
  curve(dsinu(x, par1[1], par1[2], par1[3], par1[4], flip), add=T, col='red', lwd=2)
  
  pars = paste(round(par1, 1), collapse=', ')
  if (str=='') str = PDF.str
  sinu.str = paste0('Sinu(',pars,')')
  legend('topright', legend=c(str, sinu.str), fill=c('green','red'))
}

par(mfrow=c(2,3))

### Normal
fitsinu.pdf.curve('dnorm(x,4,59)', 'N(4,59)', xlim=c(-180, 180))

### Chisq
fitsinu.pdf.curve('dchisq(x, 14)', expression({chi^2} (14)), xlim=c(1,33))

### t
fitsinu.pdf.curve('dt(x,7)', 't(7)')

# Exponential
fitsinu.pdf.curve('dexp(x, 3.2)', 'Exp(3.2)', xlim=c(0,1.2))

### GAMMA
fitsinu.pdf.curve('dgamma(x,3,6)', 'Gamma(3,6)', xlim=c(0, 1.8))

### BETA
fitsinu.pdf.curve('dbeta(x,3,6)', 'Beta(3,6)', xlim=c(0, 0.8))
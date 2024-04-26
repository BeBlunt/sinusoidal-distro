deltaCor.chi = Vectorize(function(chi) {
  1/sqrt(sinu.var(chi=chi))}, vectorize.args='chi')
alphaCor.chi = Vectorize(function(chi) {
  -deltaCor.chi(chi)/2}, vectorize.args='chi')

par(mar=c(4,4.5,0,0), cex=1.5)

curve(deltaCor.chi(x), xlim=c(0,100), lwd=2, xlab=expression(chi), ylab=expression(delta~ 'cor' ~ (chi)))


curve(deltaCor.chi(x)^2, xlim=c(0,100), lwd=2, xlab=expression(chi), ylab=expression(delta^2~ 'cor' ~ (chi))) #delta^2 is linear WRT chi


curve(dnorm(x), xlim=c(-3,3))
chiIn=20
curve(dsinu(x, alphaCor.chi(chiIn), deltaCor.chi(chiIn), 1, chiIn), col='red', add=T)

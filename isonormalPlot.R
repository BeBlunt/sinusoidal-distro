deltaCor.chi = Vectorize(function(chi) {
  1/sqrt(sinu.var(chi=chi))}, vectorize.args='chi')
alphaCor.chi = Vectorize(function(chi) {
  -deltaCor.chi(chi)/2}, vectorize.args='chi')

curve(deltaCor.chi(x), xlim=c(0,100))

curve(deltaCor.chi(x)^2, xlim=c(0,100)) #delta^2 is linear WRT chi

curve(dnorm(x), xlim=c(-3,3))

chiIn=20
curve(dsinu(x, alphaCor.chi(chiIn), deltaCor.chi(chiIn), 1, chiIn), col='red', add=T)


###

sinu.kurt.omegachi = Vectorize(function(omega, chi) sinu.kurt(omega=omega, chi=chi), vectorize.args=c('chi','omega'))
sinu.skew.omegachi = Vectorize(function(omega, chi) sinu.skew(omega=omega, chi=chi), vectorize.args=c('chi','omega'))

curve(1/sinu.kurt.omegachi(omega=1, chi=x), xlim=c(0,100)) #linear in the short run
curve(1/sinu.kurt.omegachi(omega=1, chi=x), xlim=c(0,2000)) #jagged but maybe due to computational error
curve(sinu.kurt.omegachi(omega=1, chi=x), xlim=c(0,100))


###
omega.vec=c(0.5,1,2,3,4,5,6,7,8,9)
curve(0*x, xlim=c(0,10), ylim=c(-1.3,1.9))

i=0
for (omega in omega.vec){
  i=i+1; print(i)
  curve(sinu.kurt.omegachi(omega=omega, chi=x), col=rgb((10-i)/10,i/10,0), add=T)
}

chi.vec=c(0.5,1,2,3,4,5,6,7,8,9)
curve(0*x, xlim=c(0,10), ylim=c(-1.3,2.5))

i=0
for (chi in chi.vec){
  i=i+1; print(i)
  curve(sinu.skew.omegachi(chi=chi, omega=x), col=rgb((10-i)/10,i/10,0), add=T)
}



### POSSIBILITY REGION: Monte Carlo Style simulation


omega.vec = chi.vec = seq(1,10, by=0.01)
skew.vec = kurt.vec = c()

red.vec = green.vec = c()
j=0
for (omega in omega.vec) {
  for (chi in chi.vec) {
    skew.vec = c(skew.vec, sinu.skew(omega=omega, chi=chi))
    kurt.vec = c(kurt.vec, sinu.kurt(omega=omega, chi=chi))
    print(c(omega,chi))
  }
}

omegaCol.vec = rep(omega.vec/10, 10)
chiCol.vec = rep(chi.vec/10, each=10)


plot(skew.vec, kurt.vec, col=rgb(omegaCol.vec, chiCol.vec, 0), type='p', pch=15)


###
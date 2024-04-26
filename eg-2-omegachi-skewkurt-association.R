
sinu.kurt.omegachi = Vectorize(function(omega, chi) sinu.kurt(omega=omega, chi=chi), vectorize.args=c('chi','omega'))
sinu.skew.omegachi = Vectorize(function(omega, chi) sinu.skew(omega=omega, chi=chi), vectorize.args=c('chi','omega'))


omega.vec=c(1:4/4, 1:20/2)
n=length(omega.vec)
par(mar = c(4,4,0,0))

curve(0*x, xlim=c(0,20), ylim=c(-1.3,1.9), xlab=expression(chi), ylab='kurtosis')
i=0
for (omega in omega.vec){
  i=i+1; print(i)
  curve(sinu.kurt.omegachi(omega=omega, chi=x), col=rgb((n-i)/n, i/n, 0), add=T, lwd=5)
}
i.vec=1:n
legend('topright', legend=paste0(omega.vec), fill=rgb((n-i.vec)/n, i.vec/n, 0), title='ω value')



chi.vec=c(1:4/4, 1:20/2)
n=length(chi.vec)
curve(0*x, xlim=c(0,10), ylim=c(-1.3,2.5), xlab=expression(omega), ylab='skewness')
i=0
for (chi in chi.vec){
  i=i+1; print(i)
  curve(sinu.skew.omegachi(chi=chi, omega=x), col=rgb((n-i)/n, i/n, 0), add=T,lwd=3)
}
i.vec=1:n
legend('topright', legend=paste0(omega.vec), fill=rgb((n-i.vec)/n, i.vec/n, 0), title=expression(chi))
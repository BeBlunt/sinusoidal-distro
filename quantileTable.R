qsinu.omegaChi = function(q, omega, chi, switch=F) {
  qsinu(q, 0,1, omega, chi, switch)
}

qsinu.delta = Vectorize(function(delta) {
  qsinu(0.95, alpha=0, delta=delta, omega=1, chi=10)
}, vectorize.args='delta')

curve(qsinu.delta(x), xlim=c(1,1000000), add=T)


omega.vec = 1:20
chi.vec = 1:20
q.vec=c()

for (chi in chi.vec) {
  for (omega in omega.vec) {
      q.vec = c(qsinu.omegaChi(0.95, omega, chi), q.vec)
  }
}

q.mat = matrix(q.vec, byrow=T, ncol=length(omega.vec))

matplot(q.mat, type='o')


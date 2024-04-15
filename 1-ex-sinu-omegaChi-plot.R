par(mfrow=c(6,6), cex.axis=0.8)

omega.vec = c(0.2, 0.6, 1,2,3,4)
chi.vec = c(0.2, 0.6, 1,2,3,4)

for (omega in omega.vec) {
  for (chi in chi.vec) {
    par(mar=c(0,0,0,0))
    curve(dsinu(x, 0,1, omega, chi), ylim=c(0,6), xaxt='n')
  }
}
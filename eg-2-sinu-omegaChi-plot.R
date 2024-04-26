par(mfrow=c(6,6), cex=0.8, cex.axis=0.5)

omega.vec = c(0.2, 0.6, 1,2,3,4)
chi.vec = c(0.2, 0.6, 1,2,3,4)

for (omega.val in omega.vec) {
  for (chi.val in chi.vec) {
    par(mar=c(0,0,1,0))
    mainlab = paste(omega.val,chi.val, sep=', ')
    curve(dsinu(x, 0,1, omega.val, chi.val), ylim=c(0,6), xaxt='n',
          main= mainlab, lwd=2
          )
  }
}
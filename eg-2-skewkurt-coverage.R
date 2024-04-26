omega.vec = chi.vec = seq(1,20, by=0.1)
n = length(omega.vec)
skew.mat = kurt.mat = matrix(0, ncol=n,nrow=n)

red.vec = green.vec = c()
j=0
for (j in 1:n) {
  for (i in 1:n) {
    skew.mat[i,j] = sinu.skew(omega=omega.vec[i], chi=chi.vec[j])
    kurt.mat[i,j] = sinu.kurt(omega=omega.vec[i], chi=chi.vec[j])
    print(c(i,j))
  }
}

#omegaCol.vec = rep(omega.vec/10, 10)
#chiCol.vec = rep(chi.vec/10, each=10)


matplot(skew.mat, kurt.mat, type='p', pch=15)
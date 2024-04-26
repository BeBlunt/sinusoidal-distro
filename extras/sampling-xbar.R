xbar.vec=c()
n = 1000
for (i in 1:n) {
  xbar.vec[i] = mean(rsinu(1000))
}

hist(xbar.vec)
### extremely time consuming
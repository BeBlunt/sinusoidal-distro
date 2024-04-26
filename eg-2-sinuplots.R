par(mar=c(2,2,0,0), cex=1.2, cex.axis=0.8)

curve(dsinu(x), xlim=c(0,2), ylim=c(0,2.7), ylab='Density', lwd=5)
curve(dsinu(x, alpha=1), col='grey', lwd=5, add=T)
curve(dsinu(x, delta=2), col='green', lwd=5, add=T)
curve(dsinu(x, omega=3), col='red', lwd=5, add=T)
curve(dsinu(x, chi=4), col='blue', lwd=5, add=T)

text1 = 'Sinu(0,1,1,1)'
text2 = expression(alpha==1)
text3 = expression(delta==2)
text4 = expression(omega==3)
text5 = expression(chi==4)
text = c(text1,text2,text3,text4,text5)
legend('topright', legend=text, fill=c('black', 'grey','green','red','blue'))


curve(dsinu(x), xlim=c(0,2), ylim=c(0,2.7), main='PDFs', ylab='Density')
curve(dsinu(x, alpha=1), col='grey', add=T)
curve(dsinu(x, delta=2), col='green', add=T)
curve(dsinu(x, omega=3), col='red', add=T)
curve(dsinu(x, chi=4), col='blue', add=T)

text1 = 'Sinu(0,1,1,1)'
text2 = expression(alpha==1)
text3 = expression(delta==2)
text4 = expression(omega==3)
text5 = expression(chi==4)
text = c(text1,text2,text3,text4,text5)
legend('topright', legend=text, fill=c('black', 'grey','green','red','blue'))
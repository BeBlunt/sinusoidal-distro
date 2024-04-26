par(mar=c(2,2,0,0), mfrow=c(1,2))

par(cex=0.8)
curve(dsinu(x, omega=3), col='green', lwd=5)
curve(dsinu(x, omega=3, flip=T), col='red', lwd=5, add=T)
text1 = 'Sinu(0,1,3,1)'
text2 = 'Sinu\' (0,1,3,1)'
text = c(text1,text2)
legend('topright', legend=text, fill=c('green','red'))

curve(dsinu(x, omega=0.1), col='green', lwd=5)
curve(dsinu(x, omega=0.1, flip=T), col='red', lwd=5, add=T)

text1 = 'Sinu(0, 1, 0.1, 1)'
text2 = 'Sinu\' (0, 1, 0.1, 1)'
text = c(text1,text2)
legend('topright', legend=text, fill=c('green','red'))
# Obtain graphic app presets
source('90_presets/graphic_presets.R')

# Obrain random data and plot it
x <- cumsum(rnorm(100))
range(x)
par(pty = 's')
plot(x,
     type = 'l',
     yaxt = 'n',
     ylim = c(
       floor(min(x)),
       ceiling(max(x))
     ),
     xlab = 'Index',
     ylab = 'Cumulative sum of x'
     )
axis(side = 2,
     at = seq(
       floor(min(x)),
       ceiling(max(x)),
       1
     ),
     las = 2)





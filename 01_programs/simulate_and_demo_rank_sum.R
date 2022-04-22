library(data.table)
# Make simulated example about rank sum test

# Simulate x and y
k <- 20
x <- rnorm(k, mean = 10.5)
y <- rnorm(k, mean = 10)
z <- c(x,y)

z_size_m <- data.table(I = rep(1:20,2),RAW = z, TEAM = c(rep('deepskyblue3',k),rep('grey70',k)))

# Size factor for plotting
fun.normalize <- function(x) {
  
  (x - min(x)) / (max(x) - min(x))

  }

z_size_m[, D:= fun.normalize(RAW) + 1]





# Visualize: 'people randomly in the field'
par(pty = 's')
plot(
  z_size_m[TEAM == 'deepskyblue3',D],z_size_m[TEAM == 'grey70',D],
  cex = c( z_size_m[TEAM == 'deepskyblue3',D],z_size_m[TEAM == 'grey70',D]),
  pch = 16,
  col = c(rep('deepskyblue3',10),rep('grey70',10)),
  axes = FALSE, xlab = '', ylab = ''
  )
box()

# Visualize: 'people in two lines, blue and grey'
plot(y = z_size_m[TEAM == 'deepskyblue3',I], x = rep(1.25,20), cex = z_size_m[TEAM == 'deepskyblue3',D],  pch = 16, col = z_size_m[TEAM == 'deepskyblue3',TEAM], axes = FALSE, xlab = '', ylab = '', xlim = c(1,2))
points(y = z_size_m[TEAM == 'grey70',I], x = rep(1.75,20), cex = z_size_m[TEAM == 'grey70',D],  pch = 16, col = z_size_m[TEAM == 'grey70',TEAM])
box()

# Visualize: 'people ordered'
plot(y = z_size_m[TEAM == 'deepskyblue3',I], x = rep(1.25,20), cex = z_size_m[order(D)][TEAM == 'deepskyblue3',D],  pch = 16, col = z_size_m[TEAM == 'deepskyblue3',TEAM], axes = FALSE, xlab = '', ylab = '', xlim = c(1,2))
points(y = z_size_m[TEAM == 'grey70',I], x = rep(1.75,20), cex = z_size_m[order(D)][TEAM == 'grey70',D],  pch = 16, col = z_size_m[TEAM == 'grey70',TEAM])
box()

# Visulize: 'people in single ordered line'
plot(y = 1:nrow(z_size_m), x = rep(1.5,40), cex = z_size_m[order(D),D],  pch = 16, col = z_size_m[order(D),TEAM], axes = FALSE, xlab = '', ylab = '', xlim = c(1,2))

# Make wilcox.test
wilcox.test(z_size_m$D ~ z_size_m$TEAM)

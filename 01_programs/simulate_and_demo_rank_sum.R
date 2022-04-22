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

#



fun.w.simu <- function(x,y) {
  n1 <- length(x)
  n2 <- length(y)
  
  z <- c(x,y)
  
  Z <- data.table(XY = z, S = c(rep('X',n1),rep('Y',n2)))
  Z[, Z_RANK := rank(XY)]
  
  
  R1 <- Z[S == 'X', sum(Z_RANK)]
  R2 <- Z[S == 'Y', sum(Z_RANK)]
  
  
  U1 <- R1 - (n1*(n1+1) / 2)
  U2 <- R2 - (n2*(n2+1) / 2)
  
  p1 <- pwilcox(U1 - 1, n1, n2, lower.tail = FALSE)
  p2 <- pwilcox(U2 - 1, n1, n2, lower.tail = FALSE)
  
  p1 <- min(2 * p1, 1)
  p2 <- min(2 * p2, 1)
  
  print(paste0('The p-value for test statistic U1 = ', U1, ' is ', p1))
  print(paste0('The p-value for test statistic U2 = ', U2, ' is ', p2))
  
  wilcox.test(Z$XY ~ Z$S)
}


fun.w.simu(x = rnorm(12), y = rnorm(13))

# hmm...it is almost true, i.e., the Mann-Whitney U test test statistic U and Wilcoxon rank sum test test statistic W
# are almost the same, but it is in the MW test that the U is selectect as the min(U1, U2), but it is not
# so in the wilcox test; in the R wilcox.test implementation it is actually THE largest, i.e., max(U1, U2)
# that is used!

# However, this is noted in the wilcox.test documentation, altough, somewhat 
# implicitly.

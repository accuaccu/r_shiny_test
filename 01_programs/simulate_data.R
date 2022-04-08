library(data.table)

# Simulate some random time-series data from normal distribution
p <- 10
n <- 100
matrix_empty <- matrix(NA, ncol = p, nrow = n)

for(i in 1:p) {
  
  matrix_empty[,i] <- cumsum(rnorm(n))
  
}

dt_random_data <- data.table(matrix_empty)

write.csv(dt_random_data, file = '02_data/simulated_data.csv', row.names = FALSE)
<- 
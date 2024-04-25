library(truncnorm)
source("TestForEqualVariance/bootstrap_code.R")
# =================
# CASE ONE
# =================
# Simulate parameters
n1 <- 100
mu1    <- 0
sigma1 <- 1
trunc1 <- c(-1, 1)

n2 <- 150
mu2    <- 0
sigma2 <- 0.6
trunc2 <- c(-1.5, 1.5)

# Get true variances
var1 <- vtruncnorm(trunc1[1], trunc1[2], mu1, sigma1)
var2 <- vtruncnorm(trunc2[1], trunc2[2], mu2, sigma2)

# Start Simulation Study
num_reps <- 100
p1 <- p2 <- p3 <- num_reps
for(i in 1:num_reps){
  # Generate data
  X1 <- rtruncnorm(n1, trunc1[1], trunc1[2], mu1, sigma1)
  X2 <- rtruncnorm(n2, trunc2[1], trunc2[2], mu2, sigma2)

  # Bootstrap
  p1[i] <- bootstrap_equal_var(X1, X2, B=10000, "two-sided")

  # F-Test
  Fstat <- var(X1)/var(X2)
  p2[i] <- 2*min(pf(Fstat, n1-1, n2-1), 1 - pf(Fstat, n1-1, n2-1))

  # Modified F-Test
  z1 <- (X1-mean(X1))/sd(X1)
  kappa1 <- mean(z1^4)
  nu1 <- 3*(n1-1)/kappa1
  z2 <- (X2-mean(X2))/sd(X2)
  kappa2 <- mean(z2^4)
  nu2 <- 3*(n2-1)/kappa2
  p3[i] <- 2*min(pf(Fstat, nu1, nu2), 1 - pf(Fstat, n1-1, n2-1))
}

ord <- order(p1)
png("TestForEqualVariance/figs/case1.png", height=5, width=5, units="in", res=300)
plot(p1[ord], type='l', lty=1, col='dodgerblue',
     main="Case 1", ylab="p-value", xlab="Simulation #")
lines(p2[ord], lty=2, col='firebrick')
lines(p3[ord], lty=3, col='orange')
abline(0, .011, col='grey')
legend("topleft",
       c("Reference Line", "Bootstrap", "F-Test", "Modified F-Test"),
       lty=1:4, bty="n", cex=0.75,
       col=c("grey", "dodgerblue", "firebrick", "orange"))
dev.off()

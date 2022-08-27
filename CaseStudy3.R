#QM Case Study 3

set.seed(272727)

#Task 1

SIM <- function(M,mean,standard_deviation){
  x1 <- rnorm(M, mean = mean, sd = standard_deviation)
  x2 <- rnorm(M, mean = mean, sd = standard_deviation)
  
  s0 <- 100
  s1 <- s0 * exp(x1)
  s2 <- s1 * exp(x2)
  
  n <- cbind(s0,s1, s2)
  k <- matrix(data = n, nrow = M, ncol = 3)
  colnames(k) = c("s0","s1","s2")
  #print(k)
  return(k)
}

s <- SIM(M = 1000,mean = 0.04,standard_deviation = 0.2)



#Task 2

s <- SIM(M = 1000,mean = 0.04,standard_deviation = 0.2)
f <- function(s){4*pmin(20, pmax(0,s-100))}
s2 <-f(S[,3])

p <- mean(s2)
SE <- sd(s2)/sqrt(length(s2))
errorregion = c(p-3*SE, p+3*SE)

#print(s)
#Task 3

G <- function(S, r, b0, H0){
  h <- function(ss){5*(pnorm((ss-95)/25) - pnorm((ss-115)/25))}
  H1 <- h(S[,2])
  print(H1[1:25])
  b1 <- (b0-H0*S[,1])*exp(r)
  b2 <- (b1 - (H1-H0)*S[,2])*exp(r)
  g <- cbind(H1*S[,3]+b2-s2)
  return(g)
}

x <- G(S = s, r = 0.02, b0 = p, H0 = 1.25 )


#Task 4

#plot the possible gains or losses for the seller.
plot(G(S = s, r = 0.02, b0 = p, H0 = 1.25), ylab = "Gain/Loss", xlab = "Iterations", main = "Gain/Loss of the seller")
#Plot of the p value in blue
abline(h = p, col ='red')



hist(G(S = s, r = 0.02, b0 = p, H0 = 1.25), breaks = 50, freq = TRUE, xlab = "Gain/Loss", main = "Gain/Loss")
abline(v = p, col ='red')


boxplot(G(S = s, r = 0.02, b0 = p, H0 = 1.25), ylab = "gains and losses", main = "Possible gains for the seller in a Boxplot")
abline(h = p, col ='red')
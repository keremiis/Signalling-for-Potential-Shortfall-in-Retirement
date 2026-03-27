initial_wealth         <- 500000
annual_withdrawal_rate <- 0.05
annual_withdrawal      <- initial_wealth * annual_withdrawal_rate
years                  <- 30

returns_A    <- rep(0.04, years)
returns_A[5] <- -0.25

returns_B     <- rep(0.04, years)
returns_B[20] <- -0.25

wealth_A <- numeric(years)
wealth_B <- numeric(years)

current_A <- initial_wealth
current_B <- initial_wealth

for(t in 1:years) {
  current_A <- current_A * (1 + returns_A[t]) - annual_withdrawal
  current_B <- current_B * (1 + returns_B[t]) - annual_withdrawal
  
  wealth_A[t] <- max(current_A, 0)
  wealth_B[t] <- max(current_B, 0)
}

plot(1:years, wealth_B, 
     type = "l", 
     col  = "blue", 
     lwd  = 2,
     ylim = c(0, max(wealth_B)), 
     xlab = "Years in Retirement",
     ylab = "Portfolio Wealth", 
     main = "Sequence of Return Risk: Early vs. Late Crash")

lines(1:years, wealth_A, col = "red", lwd = 2)

legend("topright", 
       legend = c("Late Crash (Year 20)", "Early Crash (Year 5)"),
       col    = c("blue", "red"), 
       lwd    = 2)
initial_wealth <- 500000
annual_withdrawal_rate <- 0.05
annual_withdrawal <- initial_wealth * annual_withdrawal_rate
years <- 30

returns_early_crash <- rep(0.04, years)
returns_late_crash <- rep(0.04, years)

returns_early_crash[5] <- -0.2
returns_early_crash[20] <- 0.2

returns_late_crash[5] <- 0.2
returns_late_crash[20] <- -0.2

wealth_early <- numeric(years)
wealth_late <- numeric(years)

current_early <- initial_wealth
current_late <- initial_wealth

for(t in 1:years) {
  current_early <- current_early * (1 + returns_early_crash[t]) - annual_withdrawal
  current_late  <- current_late * (1 + returns_late_crash[t]) - annual_withdrawal
  
  wealth_early[t] <- max(current_early, 0)
  wealth_late[t]  <- max(current_late, 0)
}

plot(1:years, wealth_late, type="l", col="blue", lwd=2,
     ylim=c(0, max(wealth_late) * 1.1), 
     xlab="Years in Retirement",
     ylab="Portfolio Wealth (€)", 
     main="Sequence of Return Risk: Early vs. Late Volatility")

lines(1:years, wealth_early, col="red", lwd=2)

abline(h=0, lty=2, col="gray")

legend("bottomleft", 
       legend=c("Late Crash (Boom Yr 5, Crash Yr 20)", 
                "Early Crash (Crash Yr 5, Boom Yr 20)"),
       col=c("blue", "red"), lwd=2)
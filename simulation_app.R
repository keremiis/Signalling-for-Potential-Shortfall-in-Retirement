library(shiny)
library(stats)

ui <- fluidPage(
  titlePanel("Retirement Solvency Simulator (Early Warning System)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Client Parameters"),
      numericInput("W0", "Initial Wealth (€)", value = 700000, step = 10000),
      numericInput("needs_0_mo", "Essential Needs (€/month)", value = 1250, step = 50),
      numericInput("wishes_0_mo", "Discretionary Wishes (€/month)", value = 750, step = 50),
      numericInput("life_exp", "Life Expectancy (Years)", value = 20.6, step = 0.1),
      
      h4("Market & Economic Assumptions"),
      sliderInput("mu_ret", "Expected Return", min = 0, max = 0.10, value = 0.05, step = 0.005),
      sliderInput("sig_ret", "Return Volatility", min = 0, max = 0.20, value = 0.12, step = 0.01),
      sliderInput("pi_needs", "Needs Inflation", min = 0, max = 0.10, value = 0.0274, step = 0.001),
      sliderInput("pi_wishes", "Wishes Inflation", min = 0, max = 0.10, value = 0.0303, step = 0.001),
      sliderInput("sig_inf", "Inflation Volatility", min = 0, max = 0.05, value = 0.01, step = 0.005)
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("trajectoryPlot", height = "400px"))
      ),
      hr(),
      
      fluidRow(
        column(12, verbatimTextOutput("advisoryText"))
      ),
      hr(),
      
      fluidRow(
        column(12, align = "center", plotOutput("dialPlot", height = "300px"))
      )
    )
  )
)

server <- function(input, output) {
  
  sim_results <- reactive({
    set.seed(2026)
    N <- 2000 
    horizon <- 35
    rf_rate <- 0.02
    
    needs_0_ann <- input$needs_0_mo * 12
    wishes_0_ann <- input$wishes_0_mo * 12
    
    drift <- input$mu_ret - (input$sig_ret^2 / 2)
    R_mat <- matrix(exp(rnorm(N * horizon, drift, input$sig_ret)) - 1, nrow = N)
    Inf_n_mat <- matrix(rnorm(N * horizon, input$pi_needs, input$sig_inf), nrow = N)
    Inf_w_mat <- matrix(rnorm(N * horizon, input$pi_wishes, input$sig_inf), nrow = N)
    
    eval_sim <- function(W, wishes_ann, return_paths = FALSE, target = "buffer") {
      paths <- matrix(0, nrow = N, ncol = horizon + 1)
      paths[, 1] <- W
      curr_n <- rep(needs_0_ann, N)
      curr_w <- rep(wishes_ann, N)
      
      for (t in 1:horizon) {
        curr_n <- curr_n * (1 + Inf_n_mat[, t])
        curr_w <- curr_w * (1 + Inf_w_mat[, t])
        
        growth <- ifelse(paths[, t] > 0, paths[, t] * R_mat[, t], 0)
        paths[, t + 1] <- paths[, t] + growth - (curr_n + curr_w)
      }
      
      if (return_paths) {
        ruin_indices <- apply(paths, 1, function(p) {
          idx <- which(p <= 0)
          if (length(idx) == 0) return(horizon + 1)
          t_ruin_idx <- idx[1]
          if (t_ruin_idx == 1) return(0)
          w_prev <- p[t_ruin_idx - 1]
          w_curr <- p[t_ruin_idx]
          frac <- w_prev / (w_prev - w_curr)
          return((t_ruin_idx - 2) + frac)
        })
        return(list(paths = paths, ruin = ruin_indices))
      } 
      
      if (target == "var") {
        t_x_idx <- min(round(input$life_exp), horizon)
        return(quantile(paths[, t_x_idx + 1], 0.05))
      } else {
        ruin_indices <- apply(paths, 1, function(p) {
          idx <- which(p <= 0)
          if (length(idx) == 0) return(horizon + 1)
          t_ruin_idx <- idx[1]
          if (t_ruin_idx == 1) return(0)
          w_prev <- p[t_ruin_idx - 1]
          w_curr <- p[t_ruin_idx]
          frac <- w_prev / (w_prev - w_curr)
          return((t_ruin_idx - 2) + frac)
        })
        return(quantile(ruin_indices, 0.05) - input$life_exp) 
      }
    }
    
    base_sim <- eval_sim(input$W0, wishes_0_ann, return_paths = TRUE)
    paths <- base_sim$paths
    ruin_indices <- base_sim$ruin
    
    X10 <- quantile(ruin_indices, 0.10)
    X5 <- quantile(ruin_indices, 0.05)
    X1 <- quantile(ruin_indices, 0.01)
    
    buffer_10 <- X10 - input$life_exp
    buffer_5 <- X5 - input$life_exp
    buffer_1 <- X1 - input$life_exp
    
    t_x_idx <- min(round(input$life_exp), horizon)
    wealth_at_Tx <- paths[, t_x_idx + 1]
    
    VaR_05 <- quantile(wealth_at_Tx, 0.05)
    ES_05 <- mean(wealth_at_Tx[wealth_at_Tx <= VaR_05])
    
    discount_factors <- (1 + rf_rate)^-(1:t_x_idx)
    S_needs_det <- needs_0_ann * (1 + input$pi_needs)^(1:t_x_idx)
    PV_needs <- sum(S_needs_det * discount_factors)
    
    df_Tx <- (1 + rf_rate)^-t_x_idx 
    RSR_05 <- (PV_needs + (VaR_05 * df_Tx)) / PV_needs
    
    Y <- 0
    W_inj <- 0
    
    if (VaR_05 < 0) {
      solve_y <- function(test_w) eval_sim(input$W0, test_w, target = "var")
      opt_w_res <- tryCatch(uniroot(solve_y, interval = c(0, wishes_0_ann), extendInt = "yes")$root, error=function(e) NA)
      if (!is.na(opt_w_res)) {
        Y <- (wishes_0_ann - opt_w_res) / 12
      } else {
        Y <- wishes_0_ann / 12 
      }
      
      solve_w <- function(test_cap) eval_sim(test_cap, wishes_0_ann, target = "var")
      opt_W_res <- tryCatch(uniroot(solve_w, interval = c(input$W0, input$W0 * 5), extendInt = "yes")$root, error=function(e) NA)
      if (!is.na(opt_W_res)) W_inj <- opt_W_res - input$W0
    }
    
    list(
      paths = paths, RSR = RSR_05, VaR = VaR_05, ES = ES_05, 
      X10 = X10, X5 = X5, X1 = X1, 
      buffer_10 = buffer_10, buffer_5 = buffer_5, buffer_1 = buffer_1, 
      Y = Y, W_inj = W_inj
    )
  })
  
  output$trajectoryPlot <- renderPlot({
    res <- sim_results()
    
    path_base <- apply(res$paths, 2, quantile, probs = 0.50)
    path_10   <- apply(res$paths, 2, quantile, probs = 0.10)
    path_05   <- apply(res$paths, 2, quantile, probs = 0.05)
    path_01   <- apply(res$paths, 2, quantile, probs = 0.01)
    
    age_axis <- 65:(65 + 35)
    y_max <- max(path_base[1], input$W0) * 1.1
    y_min <- min(path_01) * 1.05 
    
    plot(age_axis, path_base, type = "l", col = "darkgray", lwd = 2, lty = 2, 
         ylim = c(y_min, y_max), xlab = "Retiree Age", ylab = "Portfolio Wealth (€)",
         main = "Wealth Trajectories Across Market Scenarios")
    
    lines(age_axis, path_10, col = "orange", lwd = 2)
    lines(age_axis, path_05, col = "tomato", lwd = 3)
    lines(age_axis, path_01, col = "purple", lwd = 2)
    
    abline(h = 0, col = "black", lwd = 2)
    
    plot_life_exp <- min(input$life_exp, 35)
    abline(v = 65 + plot_life_exp, col = "blue", lwd = 2, lty = 3)
    
    polygon(c(age_axis, rev(age_axis)), 
            c(rep(0, length(age_axis)), rep(y_min, length(age_axis))), 
            col = rgb(1, 0, 0, 0.05), border = NA)
    
    text(65 + plot_life_exp + 0.5, y_max * 0.9, 
         paste("Life Expectancy (", 65 + input$life_exp, ")", sep=""), 
         col = "blue", adj = 0, font = 2)
    
    legend("bottomleft", 
           legend = c("Base Case", "10% Worst-Case", "5% Worst-Case", "1% Worst-Case"),
           col = c("darkgray", "orange", "tomato", "purple"), 
           lwd = c(2, 2, 3, 2), lty = c(2, 1, 1, 1), bg = "white")
  })
  
  output$advisoryText <- renderText({
    res <- sim_results()
    
    health_score <- res$RSR * 100 
    
    future_status <- ifelse(res$VaR < 0, 
                            sprintf("Projected Shortfall of €%s", format(round(-res$VaR), big.mark=",")), 
                            sprintf("Projected Surplus of €%s", format(round(res$VaR), big.mark=",")))
    
    tail_risk_status <- ifelse(res$ES < 0, 
                               sprintf("In severe tail events (worst 5%%), this shortfall averages €%s.", format(round(-res$ES), big.mark=",")), 
                               "Even in severe worst-case events, your baseline needs remain covered.")
    
    margin_text <- sprintf(
      "Safety Margins (Years vs. Life Expectancy):\n  * 10%% Worst-Case (1-in-10): %+.1f years\n  * 5%% Worst-Case (1-in-20): %+.1f years\n  * 1%% Worst-Case (1-in-100): %+.1f years",
      res$buffer_10, res$buffer_5, res$buffer_1
    )
    
    client_summary <- sprintf(
      "RETIREMENT HEALTH SCORE: %.2f%%\n(Target: 100%% or higher)\n\n--- YOUR FINANCIAL RUNWAY ---\nBased on 2,000 simulated market environments, we stress-tested your retirement plan.\n\nAt your life expectancy (Age %.1f):\n* %s\n* %s\n\n%s\n",
      health_score, (65 + input$life_exp), future_status, tail_risk_status, margin_text
    )
    
    if (res$VaR >= 0) {
      action_text <- "\n--- ADVISOR RECOMMENDATION ---\nSTATUS: ON TRACK.\nYour current spending level is highly sustainable. No adjustments to your lifestyle or portfolio are required at this time."
      
    } else if (res$buffer_5 >= 0 && res$VaR < 0) {
      action_text <- sprintf(
        "\n--- ADVISOR RECOMMENDATION ---\nSTATUS: CAUTION.\nYou have enough capital to survive standard market drops, but you are vulnerable to extreme financial shocks.\n\nTo reach a 100%% Health Score, consider:\n(Option A) Reducing discretionary spending by €%s/month.\n(Option B) Keeping an additional cash reserve of €%s.",
        format(round(res$Y, 2), nsmall = 2, big.mark = ","), 
        format(round(res$W_inj, 2), nsmall = 2, big.mark = ",")
      )
      
    } else {
      action_text <- sprintf(
        "\n--- ADVISOR RECOMMENDATION ---\nSTATUS: ACTION REQUIRED.\nIf a severe market downturn occurs, your current withdrawal rate will deplete your portfolio before your life expectancy.\n\nTo secure your retirement today, you have two mathematically sound options:\n(Option A) Reduce your flexible spending by €%s per month.\n(Option B) Inject €%s into your investment portfolio today (e.g., downsizing or equity release).",
        format(round(res$Y, 2), nsmall = 2, big.mark = ","), 
        format(round(res$W_inj, 2), nsmall = 2, big.mark = ",")
      )
    }
    
    paste0(client_summary, action_text)
  })
  
  output$dialPlot <- renderPlot({
    res <- sim_results()
    buffer_5 <- res$buffer_5
    
    par(mar = c(2, 2, 4, 2), bg = "#FAFAFA")
    plot(0, 0, type = "n", xlim = c(-1.3, 1.3), ylim = c(-0.2, 1.2), 
         axes = FALSE, xlab = "", ylab = "", asp = 1,
         main = "Retirement Runway Monitor\n(Time-to-Ruin Buffer)", 
         col.main = "#2C3E50", cex.main = 1.3)
    
    col_red <- "#E74C3C"; col_yel <- "#F1C40F"; col_grn <- "#2ECC71"; col_needle <- "#34495E"
    
    draw_sector <- function(start_angle, end_angle, col) {
      theta <- seq(start_angle, end_angle, length.out = 100)
      polygon(c(cos(theta), 0.55 * cos(rev(theta))), c(sin(theta), 0.55 * sin(rev(theta))), col = col, border = NA)
    }
    
    val_to_angle <- function(v) { pi - ((max(-10, min(10, v)) + 10) / 20) * pi }
    
    draw_sector(pi, val_to_angle(-3), col_red)
    draw_sector(val_to_angle(-3), val_to_angle(0), col_yel)
    draw_sector(val_to_angle(0), 0, col_grn)
    
    for (v in c(-10, -5, 0, 5, 10)) {
      a <- val_to_angle(v)
      lines(c(0.96 * cos(a), 1.04 * cos(a)), c(0.96 * sin(a), 1.04 * sin(a)), col = "#7F8C8D", lwd = 2)
    }
    
    text(-1.15, -0.1, "-10 Yrs", font = 2, col = "#C0392B", cex = 0.9)
    text(0, -0.15, "0 Yrs\n(Target)", font = 2, col = "#7F8C8D", cex = 0.9)
    text(1.15, -0.1, "+10 Yrs", font = 2, col = "#27AE60", cex = 0.9)
    
    needle_angle <- val_to_angle(buffer_5)
    lines(c(0, 0.95 * cos(needle_angle)), c(0, 0.95 * sin(needle_angle)), lwd = 5, col = col_needle)
    points(0, 0, pch = 19, cex = 5, col = "white")
    points(0, 0, pch = 19, cex = 1.5, col = col_needle)
    
    text(0, 0.45, sprintf("%+.1f Years", buffer_5), cex = 2, font = 2, col = "#2C3E50")
    
    if (res$VaR >= 0) { 
      status_text <- "SAFE"; status_col <- col_grn 
    } 
    else if (buffer_5 >= 0 && res$VaR < 0) { 
      status_text <- "CAUTION"; status_col <- "#D4AC0D" 
    } 
    else { 
      status_text <- "CRITICAL"; status_col <- col_red 
    }
    
    text(0, 0.28, status_text, cex = 1.2, font = 2, col = status_col)
  })
}

shinyApp(ui = ui, server = server)
plotDistStat <- function(dist = "Chi-Squared",
                         df = 10,
                         df1 = 1,
                         df2 = 1,
                         from = NULL,
                         to = NULL,
                         alpha_level = 0.05,
                         statistic_point,
                         p_value
                         ) {
  
  # Chi-Squared Distribution:
  
  if (dist == "Chi-Squared") {
    
    # Create Density Curve
    curve(expr = dchisq(x = x, df = df),
          from = from,
          to = to,
          xlab = paste0(dist, " Statistic"),
          ylab = "Probability Density",
          lwd = 3,
          add = FALSE)
    
    # Find Upper and Lower Values for Middle 95% of Distribution
    lower95 <- qchisq(p = (alpha_level / 2), df = df)
    upper95 <- qchisq(p = (1 - (alpha_level / 2)), df = df)
    
    # Create Vector of x Values
    x_lower95 <- seq(from = from, to = lower95, by = 0.001)
    
    # Create Vector of Chi-Square Density Values
    p_lower95 <- dchisq(x = x_lower95, df = df)
    
    # Fill in Portion of the Density Plot to from from Lower 95% Value
    polygon(x = c(x_lower95, rev(x = x_lower95)),
            y = c(p_lower95, rep(x = 0, length(p_lower95))),
            col = adjustcolor(col = 'red', alpha = 0.6),
            border = NA)
    
    # Create Vector of x Values
    x_upper95 <- seq(from = upper95, to = to, by = 0.001)
    
    # Create Vector of Chi-Square Density Values
    p_upper95 <- dchisq(x = x_upper95, df = df)
    
    # Fill in Portion of the Density Plot for Upper 95% Vvalue to End of Plot
    polygon(x = c(x_upper95, rev(x = x_upper95)),
            y = c(p_upper95, rep(0, length(p_upper95))),
            col = adjustcolor(col = 'red', alpha=0.6),
            border = NA)
    
    # Add Statistic Point
    points(x = statistic_point,
           y = dchisq(x = statistic_point,
                      df = df),
           pch = 21,
           cex = 1.5,
           col = "black",
           bg = "deepskyblue")
    
    # Add Text
    mtext(text = paste0("chi-squared = ",
                        round(x = statistic_point, digits = 2),
                        "\n",
                        "df = ",
                        df,
                        ", ",
                        "p-value = ",
                        round(x = p_value, digits = 4)), 
          side = 3)
  }
  
  
  # Normal Distribution:
  
  if (dist == "Normal") {
    
    # Find Upper and Lower Values for Middle 95% of Distribution
    lower95 <- qnorm(p = (alpha_level / 2), mean = 0, sd = 1)
    upper95 <- qnorm(p = (1 - (alpha_level / 2)), mean = 0, sd = 1)
    
    # Create Vector of x Values
    x_lower95 <- seq(from = -to, to = lower95, by = 0.001)
    x_upper95 <- seq(from = upper95, to = to, by = 0.001)
    
    # Create Vector of Chi-Square Density Values
    p_lower95 <- dnorm(x = x_lower95, mean = 0, sd = 1)
    p_upper95 <- dnorm(x = x_upper95, mean = 0, sd = 1)
    
    # Create Density Curve
    curve(expr = dnorm(x = x, mean = 0, sd = 1),
          from = -to,
          to = to,
          xlab = paste0(dist, " Statistic"),
          ylab = "Probability Density",
          lwd = 3,
          add = FALSE)

    # Fill in Portion of the Density Plot to from from Lower 95% Value
    polygon(x = c(x_lower95, rev(x = x_lower95)),
            y = c(p_lower95, rep(x = 0, length(p_lower95))),
            col = adjustcolor(col = 'red', alpha = 0.6),
            border = NA)

    # Fill in Portion of the Density Plot for Upper 95% Vvalue to End of Plot
    polygon(x = c(x_upper95, rev(x = x_upper95)),
            y = c(p_upper95, rep(0, length(p_upper95))),
            col = adjustcolor(col = 'red', alpha=0.6),
            border = NA)
    
    # Add Statistic Point
    points(x = statistic_point,
           y = dnorm(x = statistic_point, mean = 0, sd = 1),
           pch = 21,
           cex = 1.5,
           col = "black",
           bg = "deepskyblue")
    
    # Add Text
    mtext(text = paste0("z = ",
                        round(x = statistic_point, digits = 2),
                        "\n",
                        "p-value = ",
                        round(x = p_value, digits = 4)), 
          side = 3)
  }
  
  # F Distribution:
  
  if (dist == "F") {
    
    # Find Upper and Lower Values for Middle 95% of Distribution
    lower95 <- qf(p = (alpha_level / 2), df1 = df1, df2 = df2)
    upper95 <- qf(p = (1 - (alpha_level / 2)), df1 = df1, df2 = df2)
    
    # Create Vector of x Values
    if (is.null(to)) {
      x_lower95 <- seq(from = 0, to = lower95, by = 0.001)
      x_upper95 <- seq(from = upper95, to = (floor(x = upper95) + 2), by = 0.001)
    } else {
      x_lower95 <- seq(from = 0, to = lower95, by = 0.001)
      x_upper95 <- seq(from = upper95, to = to, by = 0.001)
    }

    # Create Vector of F Density Values
    p_lower95 <- df(x = x_lower95, df1 = df1, df2 = df2)
    p_upper95 <- df(x = x_upper95, df1 = df1, df2 = df2)
    
    
    if (is.null(to)) {
      # Create Density Curve
      curve(expr = df(x = x, df1 = df1, df2 = df2),
            from = 0,
            to = (floor(x = upper95) + 2),
            xlab = paste0(dist, " Statistic"),
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
    } else {
      # Create Density Curve
      curve(expr = df(x = x, df1 = df1, df2 = df2),
            from = 0,
            to = to,
            xlab = paste0(dist, " Statistic"),
            ylab = "Probability Density",
            lwd = 3,
            add = FALSE)
    }
    
    # Fill in Portion of the Density Plot to from from Lower 95% Value
    polygon(x = c(x_lower95, rev(x = x_lower95)),
            y = c(p_lower95, rep(x = 0, length(p_lower95))),
            col = adjustcolor(col = 'red', alpha = 0.6),
            border = NA)
    
    # Fill in Portion of the Density Plot for Upper 95% Vvalue to End of Plot
    polygon(x = c(x_upper95, rev(x = x_upper95)),
            y = c(p_upper95, rep(0, length(p_upper95))),
            col = adjustcolor(col = 'red', alpha=0.6),
            border = NA)
    
    # Add Statistic Point
    points(x = statistic_point,
           y = df(x = statistic_point, df1 = df1, df2 = df2),
           pch = 21,
           cex = 1.5,
           col = "black",
           bg = "deepskyblue")
    
    # Add Text
    mtext(text = paste0("F = ",
                        round(x = statistic_point, digits = 2),
                        "\n",
                        "p-value = ",
                        round(x = p_value, digits = 4)), 
          side = 3)
  }
  
  if (dist == "t") {
    
    # Find Upper and Lower Values for Middle 95% of Distribution
    lower95 <- qt(p = (alpha_level / 2), df = df)
    upper95 <- qt(p = (1 - (alpha_level / 2)), df = df)
    
    # Create Vector of x Values
    x_lower95 <- seq(from = -to, to = lower95, by = 0.001)
    x_upper95 <- seq(from = upper95, to = to, by = 0.001)
    
    # Create Vector of F Density Values
    p_lower95 <- dt(x = x_lower95, df = df)
    p_upper95 <- dt(x = x_upper95, df = df)
    
    # Create Density Curve
    curve(expr = dt(x = x, df = df),
          from = -to,
          to = to,
          xlab = paste0(dist, " Statistic"),
          ylab = "Probability Density",
          lwd = 3,
          add = FALSE)
    
    # Fill in Portion of the Density Plot to from from Lower 95% Value
    polygon(x = c(x_lower95, rev(x = x_lower95)),
            y = c(p_lower95, rep(x = 0, length(p_lower95))),
            col = adjustcolor(col = 'red', alpha = 0.6),
            border = NA)
    
    # Fill in Portion of the Density Plot for Upper 95% Vvalue to End of Plot
    polygon(x = c(x_upper95, rev(x = x_upper95)),
            y = c(p_upper95, rep(0, length(p_upper95))),
            col = adjustcolor(col = 'red', alpha=0.6),
            border = NA)
    
    # Add Statistic Point
    points(x = statistic_point,
           y = dt(x = statistic_point, df = df),
           pch = 21,
           cex = 1.5,
           col = "black",
           bg = "deepskyblue")
    
    # Add Text
    mtext(text = paste0("t = ",
                        round(x = statistic_point, digits = 2),
                        "\n",
                        "p-value = ",
                        round(x = p_value, digits = 4)), 
          side = 3)
  }
  
}







# Function to Extract the F-statistics and p-value from the lm Class.
extract_lm_F_p <- function(modelobject) {
  
  if (class(modelobject) != "lm"){
    stop("Not An Object Of Class 'lm'.")
  }
  
  f <- summary(modelobject)$fstatistic
  
  F_statistics <- f[1]
  
  p_value <- pf(q = f[1], df1 = f[2], df2 = f[3], lower.tail=F)
  attributes(p_value) <- NULL
  
  result <- data.frame(F_statistics = F_statistics,
                       p_value = p_value)
  
  return(result)
}







#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @noRd
app_server <- function(input, output, session) {
  
  # Calculate equilibrium prices and quantities
  equilibrium <- reactive({
    alpha <- input$alpha
    beta <- input$beta
    omega11 <- input$omega11
    omega21 <- input$omega21
    omega12 <- input$omega12
    omega22 <- input$omega22
    
    # Calculate equilibrium price ratio p1*/p2*
    price_ratio <- (alpha * omega21 + beta * omega22) / 
                   ((1 - alpha) * omega11 + (1 - beta) * omega12)
    
    # Calculate prices based on normalization
    if (input$normalize_price == "p2") {
      p1 <- price_ratio
      p2 <- 1
    } else {
      p1 <- 1
      p2 <- 1 / price_ratio
    }
    
    # Calculate offer curves at equilibrium
    total_wealth1 <- p1 * omega11 + p2 * omega21
    total_wealth2 <- p1 * omega12 + p2 * omega22
    
    # Consumer 1 demand at equilibrium
    x11_star <- alpha * total_wealth1 / p1
    x21_star <- (1 - alpha) * total_wealth1 / p2
    
    # Consumer 2 demand at equilibrium  
    x12_star <- beta * total_wealth2 / p1
    x22_star <- (1 - beta) * total_wealth2 / p2
    
    list(
      price_ratio = price_ratio,
      p1 = p1,
      p2 = p2,
      x11_star = x11_star,
      x21_star = x21_star,
      x12_star = x12_star,
      x22_star = x22_star,
      total_omega1 = omega11 + omega12,
      total_omega2 = omega21 + omega22
    )
  })
  
  # Generate contract curve points
  contract_curve <- reactive({
    alpha <- input$alpha
    beta <- input$beta
    eq <- equilibrium()
    
    # Contract curve condition: MRS1 = MRS2
    # alpha/(1-alpha) * x21/x11 = beta/(1-beta) * x22/x12
    # where x12 = total_omega1 - x11 and x22 = total_omega2 - x21
    # Solving for x21: alpha/(1-alpha) * x21/x11 = beta/(1-beta) * (total_omega2 - x21)/(total_omega1 - x11)
    
    x11_seq <- seq(0.01, eq$total_omega1 - 0.01, length.out = 200)
    
    contract_points <- data.frame(
      x11 = x11_seq,
      x21 = sapply(x11_seq, function(x11) {
        # Cross multiply and solve for x21
        # alpha/(1-alpha) * x21/x11 = beta/(1-beta) * (Omega2 - x21)/(Omega1 - x11)
        # alpha * x21 * (Omega1 - x11) = beta * x11 * (Omega2 - x21) * (1-alpha)/(1-beta)
        # alpha * x21 * (Omega1 - x11) = beta * x11 * (Omega2 - x21) * (1-alpha)/(1-beta)
        
        a_ratio <- alpha / (1 - alpha)
        b_ratio <- beta / (1 - beta)
        
        # a_ratio * x21/x11 = b_ratio * (Omega2 - x21)/(Omega1 - x11)
        # a_ratio * x21 * (Omega1 - x11) = b_ratio * x11 * (Omega2 - x21)
        # a_ratio * x21 * (Omega1 - x11) = b_ratio * x11 * Omega2 - b_ratio * x11 * x21
        # x21 * (a_ratio * (Omega1 - x11) + b_ratio * x11) = b_ratio * x11 * Omega2
        
        numerator <- b_ratio * x11 * eq$total_omega2
        denominator <- a_ratio * (eq$total_omega1 - x11) + b_ratio * x11
        
        x21 <- numerator / denominator
        return(x21)
      })
    )
    
    # Filter valid points (within the box)
    contract_points <- contract_points[
      contract_points$x21 >= 0 & 
      contract_points$x21 <= eq$total_omega2 &
      contract_points$x11 >= 0 & 
      contract_points$x11 <= eq$total_omega1, 
    ]
    
    return(contract_points)
  })
  
  # Generate indifference curves
  indifference_curves <- reactive({
    alpha <- input$alpha
    beta <- input$beta
    eq <- equilibrium()
    
    # Create indifference curves for both consumers
    curves_data <- data.frame()
    
    # Utility levels to plot (around equilibrium)
    u1_eq <- (eq$x11_star)^alpha * (eq$x21_star)^(1-alpha)
    u2_eq <- (eq$x12_star)^beta * (eq$x22_star)^(1-beta)
    
    utility_levels1 <- c(0.5, 0.75, 1, 1.25, 1.5) * u1_eq
    utility_levels2 <- c(0.5, 0.75, 1, 1.25, 1.5) * u2_eq
    
    # Consumer 1 indifference curves
    for (u in utility_levels1) {
      x11_seq <- seq(0.1, eq$total_omega1 - 0.1, length.out = 50)
      x21_vals <- (u / (x11_seq^alpha))^(1/(1-alpha))
      
      valid_idx <- x21_vals > 0 & x21_vals < eq$total_omega2
      if (sum(valid_idx) > 2) {
        curves_data <- rbind(curves_data, data.frame(
          x11 = x11_seq[valid_idx],
          x21 = x21_vals[valid_idx],
          consumer = "Consumer 1",
          utility = u
        ))
      }
    }
    
    # Consumer 2 indifference curves (from their perspective, flipped)
    for (u in utility_levels2) {
      x12_seq <- seq(0.1, eq$total_omega1 - 0.1, length.out = 50)
      x22_vals <- (u / (x12_seq^beta))^(1/(1-beta))
      
      # Convert to Consumer 1's coordinate system
      x11_vals <- eq$total_omega1 - x12_seq
      x21_vals <- eq$total_omega2 - x22_vals
      
      valid_idx <- x21_vals > 0 & x21_vals < eq$total_omega2 & 
                   x11_vals > 0 & x11_vals < eq$total_omega1
      
      if (sum(valid_idx) > 2) {
        curves_data <- rbind(curves_data, data.frame(
          x11 = x11_vals[valid_idx],
          x21 = x21_vals[valid_idx],
          consumer = "Consumer 2",
          utility = u
        ))
      }
    }
    
    return(curves_data)
  })
  
  # Main plot
  output$edgeworth_plot <- renderPlot({
    eq <- equilibrium()
    cc <- contract_curve()
    ic <- indifference_curves()
    
    # Create the base plot
    p <- ggplot() +
      # Draw the Edgeworth box
      geom_rect(aes(xmin = 0, xmax = eq$total_omega1, 
                    ymin = 0, ymax = eq$total_omega2), 
                fill = "white", color = "black", size = 1) +
      
      # Draw indifference curves
      geom_line(data = ic[ic$consumer == "Consumer 1", ], 
                aes(x = x11, y = x21, group = utility), 
                color = "blue", alpha = 0.6, linetype = "dashed") +
      geom_line(data = ic[ic$consumer == "Consumer 2", ], 
                aes(x = x11, y = x21, group = utility), 
                color = "red", alpha = 0.6, linetype = "dashed") +
      
      # Draw contract curve
      geom_line(data = cc, aes(x = x11, y = x21), 
                color = "green", size = 2, alpha = 0.8) +
      
      # Mark initial endowments
      geom_point(aes(x = input$omega11, y = input$omega21), 
                 color = "orange", size = 4, shape = 16) +
      
      # Mark equilibrium point
      geom_point(aes(x = eq$x11_star, y = eq$x21_star), 
                 color = "purple", size = 4, shape = 17) +
      
      # Labels and styling
      labs(
        title = "Edgeworth Box with Contract Curve",
        subtitle = paste("α =", input$alpha, ", β =", input$beta),
        x = "Good 1 (Consumer 1's allocation)",
        y = "Good 2 (Consumer 1's allocation)",
        caption = "Orange dot: Initial endowment | Purple triangle: Walras equilibrium | Green line: Contract curve"
      ) +
      
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        legend.position = "bottom"
      ) +
      
      coord_fixed(ratio = 1) +
      xlim(0, eq$total_omega1) +
      ylim(0, eq$total_omega2)
    
    return(p)
  })
  
  # Equilibrium information output
  output$equilibrium_info <- renderText({
    eq <- equilibrium()
    
    paste(
      sprintf("Price ratio (p1/p2): %.3f", eq$price_ratio),
      sprintf("Price 1: %.3f", eq$p1),
      sprintf("Price 2: %.3f", eq$p2),
      "",
      "Equilibrium allocation:",
      sprintf("Consumer 1: (%.2f, %.2f)", eq$x11_star, eq$x21_star),
      sprintf("Consumer 2: (%.2f, %.2f)", eq$x12_star, eq$x22_star),
      "",
      sprintf("Total endowments: (%.0f, %.0f)", eq$total_omega1, eq$total_omega2),
      sep = "\n"
    )
  })
}

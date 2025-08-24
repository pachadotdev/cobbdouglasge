#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel("Edgeworth Box with Contract Curves"),
      
      sidebarLayout(
        sidebarPanel(
          width = 2,
          h4("Utility Function Parameters"),
          sliderInput("alpha", 
                     "Alpha (Consumer 1):", 
                     min = 0.1, max = 0.9, 
                     value = 0.5, step = 0.05),
          
          sliderInput("beta", 
                     "Beta (Consumer 2):", 
                     min = 0.1, max = 0.9, 
                     value = 0.5, step = 0.05),
          
          h4("Initial Endowments"),
          h5("Consumer 1:"),
          numericInput("omega11", "Good 1:", value = 10, min = 1, max = 50),
          numericInput("omega21", "Good 2:", value = 10, min = 1, max = 50),
          
          h5("Consumer 2:"),
          numericInput("omega12", "Good 1:", value = 10, min = 1, max = 50),
          numericInput("omega22", "Good 2:", value = 10, min = 1, max = 50),
          
          h4("Price Normalization"),
          radioButtons("normalize_price", 
                      "Normalize to 1:",
                      choices = list("Price of Good 1" = "p1",
                                   "Price of Good 2" = "p2"),
                      selected = "p2"),
          
          h4("Equilibrium Information"),
          verbatimTextOutput("equilibrium_info")
        ),
        
        mainPanel(
          width = 10,
          plotOutput("edgeworth_plot", height = "650px"),
          br(),
          h4("Utility Functions"),
          p("Consumer 1's utility function:"),
          withMathJax("$$U_{1}(x_{1}^{1}, x_{2}^{1}) = (x_{1}^{1})^{\\alpha} (x_{2}^{1})^{1-\\alpha}$$"),
          p("Consumer 2's utility function:"),
          withMathJax("$$U_{2}(x_{1}^{2}, x_{2}^{2}) = (x_{1}^{2})^{\\beta} (x_{2}^{2})^{1-\\beta}$$"),
          br(),
          h4("Contract Curve Equation"),
          p("The contract curve represents allocations where the marginal rate of substitution 
            is equal for both consumers. For Cobb-Douglas utilities, this occurs when:"),
          withMathJax("$$\\frac{\\alpha x_{2}^{1}}{(1-\\alpha) x_{1}^{1}} = \\frac{\\beta x_{2}^{2}}{(1-\\beta) x_{1}^{2}} \\text{ subject to } x_{1}^{2} = \\omega_{1}^{1} + \\omega_{1}^{2} - x_{1}^{1} \\text{ and } x_{2}^{2} = \\omega_{2}^{1} + \\omega_{2}^{2} - x_{2}^{1}$$")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "cobbdouglasge"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

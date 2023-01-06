#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinyWidgets)

## Enable vector based for income, util, loss
utility <- function(income = c(2500, 25000), util = c(0.05, 0.1),
                    loss = c(20, 20), u_multiplier = c(0.05, 0.05),
                    s_multiplier = c(0.05, 0.05),
                    equal_price = NULL) {
  ## Function to calculate overall utility from vectors of characteristics
  #' @param income Vector of monthly incomes
  #' @param util Vector of utility of product
  #' @param loss Vector of price of product
  #' @param u_multiplier Vector of disutitility proportion for unfairness
  #' @param s_multiplier Vector of disutitility proportion for surveillance
  #' @param type Character indicating type of utility to be calculated
  
  ## Vector indicating whether the individual outperforms the equal price
  if (is.null(equal_price)) {
    equal_price <- mean(loss)
  }
  
  if (all(loss == mean(loss))) {
    loser_vector <- loss
    util_vector <- util
  } else {
    loser_vector <- loss > equal_price
    util_vector <- util * (1 - loser_vector * u_multiplier - s_multiplier)
  }
  
  cost_vector <- log(income / (income - loss))
  capital_vector <- log(income)
  
  total_welfare <- sum(util_vector - cost_vector)
  prior_welfare <- sum(log((capital_vector + (util_vector - cost_vector)) / capital_vector))
  return(list(total_welfare, util_vector - cost_vector, prior_welfare))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The ethicality of personalized pricing, a commentary on Coker and Izaret (2020)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h4("Introduction"),
        p("This tool is a companion to the paper 'Is Price Personalization Ethical? The Disutility of Unfairness Perception and Surveillance Aversion', which assesses an earlier paper by Coker and Izaret (2020) who show that under certain circumstances it can be socially desirable to charge different consumers different prices. In our paper we i) assess the generality of this claim by varying the various parameter assumptions made in the original paper, and ii) by including two behavioral components that can lead to disutility to the consumer as a result of personalized pricing (fairness perception and surveillance aversion). We show that the original claims by Coker and Izaret (2020) only hold under certain circumstances, and that small levels of disutility due to our two behavioral components can already mitigate any welfare gains due to personalized pricing."),
        p("Although we supply a large set of sensitivity analyses of the setup by Coker and Izaret (2020) in the paper, we cannot assess every single parameter combination. Therefore, we developed this tool such that the reader can try parameter combinations not currently showcased in the paper."),
        numericInput("price", "Unit price under equal pricing", value=20, min=0, max=100),
        numericInput("price_A", "Unit price Alice under personalized pricing", value=35, min=0, max=100),
        numericInput("price_B", "Unit price Bob under personalized pricing", value=5, min=0, max=100),
        numericInput("income_A", "Income Alice", value=25000, min=0, max=100000),
        numericInput("income_B", "Income Bob", value=2500, min=0, max=100000),
        numericInput("util_A", "Product utility Alice", value=0.1, min=0, max=1),
        numericInput("util_B", "Product utility Bob", value=0.05, min=0, max=1),
        materialSwitch(inputId = "fairness_bin",
                       label = "Include loss due to fairness perception",
                       status = "primary",
                       right = TRUE
        ),
        conditionalPanel(
          condition = "input.fairness_bin",
          numericInput("fairness_loss", "Loss due to fairness perception",
                       value=0.05, min=0, max=1)
        ),
        materialSwitch(inputId = "surveillance_bin",
                       label = "Include loss due to surveillance aversion",
                       status = "primary",
                       right = TRUE
        ),
        conditionalPanel(
          condition = "input.surveillance_bin",
          numericInput("surveillance_loss", "Loss due to surveillance aversion",
                       value=0.05, min=0, max=1)
        ),
        p("The code underlying this tool can be found at: ")
      ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
      
      fairness_loss <- 0
      surveillance_loss <- 0
      
      if (input$surveillance_bin) {
        surveillance_loss <- input$surveillance_loss
      }
      if (input$fairness_bin) {
        fairness_loss <- input$fairness_loss
      }
        u_pp <- utility(loss = c(input$price_B, input$price_A),
                        income = c(input$income_B, input$income_A),
                        util = c(input$util_B, input$util_A),
                        u_multiplier = c(fairness_loss, fairness_loss),
                        s_multiplier = c(surveillance_loss,
                                         surveillance_loss))
        util_a_pp <- u_pp[[2]][2]
        util_b_pp <- u_pp[[2]][1]
        prior_util_pp <- u_pp[[3]]
        total_util_pp <- u_pp[[1]]
        egal_util_pp <- abs(util_a_pp - util_b_pp)
        lex_util_pp <- min(util_a_pp, util_b_pp)
        
        u_ep <- utility(loss = c(input$price, input$price),
                        income = c(input$income_B, input$income_A),
                        util = c(input$util_B, input$util_A),
                        u_multiplier = c(fairness_loss, fairness_loss),
                        s_multiplier = c(surveillance_loss,
                                         surveillance_loss))
        util_a_ep <- u_ep[[2]][2]
        util_b_ep <- u_ep[[2]][1]
        prior_util_ep <- u_ep[[3]]
        total_util_ep <- u_ep[[1]]
        egal_util_ep <- abs(util_a_ep - util_b_ep)
        lex_util_ep <- min(util_a_ep, util_b_ep)
        
        results_df <- data.frame(
          "utility" = c(total_util_pp, total_util_ep, prior_util_pp,
                        prior_util_ep, egal_util_pp, egal_util_ep,
                        lex_util_pp, lex_util_ep),
          "swf" = c("Utilitarianism", "Utilitarianism",
                    "Prioritarianism", "Prioritarianism",
                    "Egalitarianism", "Egalitarianism",
                    "Leximin", "Leximin"),
          "scheme" = c("Personalized", "Equal",
                       "Personalized", "Equal",
                       "Personalized", "Equal",
                       "Personalized", "Equal")
        )
        
        results_df$utility_char <- round(results_df$utility, 4)
        
        # results_df <- results_df[results_df$swf %in% input$swf_selection, ]
        
        ggplot(results_df, aes(y = utility, x = scheme, fill = swf)) +
          geom_bar(stat = "identity") + facet_wrap(~swf) +
          theme_bw() + guides(fill = guide_legend(title = "Social Welfare Function")) +
          geom_text(vjust = -1, aes(label = utility_char)) +
          theme(text = element_text(size=18)) +
          ylim(0, max(results_df$utility *1.1)) +
          ylab("Welfare") + xlab("Pricing scheme") +
          scale_fill_manual(values = c("#dd5129", "#0f7ba2", "#43b284", "#fab255")) +
          theme(legend.position = "bottom")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

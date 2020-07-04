## app.R ##
library(shinydashboard)
library(ggplot2)

father_son <- readRDS("../data/father_son.Rds")

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Příklad 1", tabName = "ex_1_a"),
      menuItem("Příklad 1 - rezidua", tabName = "ex_1_b"),
      menuItem("Příklad 2", tabName = "ex_2_a"),
      menuItem("Příklad 2 - odhad", tabName = "ex_2_b"),
      menuItem("Příklad 3", tabName = "ex_3")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "ex_1_a",
        fluidRow(
          box(
            withMathJax(),
            div("Zkuste odhadnout parametry regresní přímky"),
            div("$$ y = \\beta_0 + \\beta_1 x + u, $$"),
            div("kde	\\(\\beta_0\\) představuje úrovňovou konstantu, \\(\\beta_1\\) sklon přímky")
          )
        ),
        fluidRow(
          plotOutput("plot1_intro", height = 250),
        ),
        fluidRow(
          box(sliderInput("beta_0_a", "beta_0", 0, 50, value = 0, step = .1)),
          box(sliderInput("beta_1_a", "beta_1", 0, 50, value = 0, step = 1))
        )
      ),
      tabItem(
        tabName = "ex_1_b",
        fluidRow(
          box(
            withMathJax(),
            div("Zkuste odhadnout parametry regresní přímky"),
            div("$$ y = \\beta_0 + \\beta_1 x + u, $$"),
            div("$$ \\hat{y} = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = b_0 + b_1 x, $$"),
            div("kde	\\(\\beta_0\\) představuje úrovňovou konstantu, \\(\\beta_1\\) sklon přímky")
          )
        ),
        fluidRow(
          box(plotOutput("plot1_fit", height = 250)),
          box(plotOutput("plot1_residuals", height = 250))
        ),
        fluidRow(
          h3("Zkuste odhadnout parametry regresní přímky"),
          box(sliderInput("beta_0_b", "beta_0", 0, 50, value = 0, step = .1)),
          box(sliderInput("beta_1_b", "beta_1", 0, 50, value = 0, step = 1))
        )
      ),
      tabItem(
        tabName = "ex_2_a",
        h2("Pearsonův dataset - výška otců a jejich synů"),
        fluidRow(
          box(plotOutput("plot_father_son", height = 250)),
          valueBoxOutput("error_box"),
          valueBoxOutput("error_min_box")
        ),
        fluidRow(
          h3("Zkuste odhadnout parametry regresní přímky"),
          box(sliderInput("fs_beta_0", "fs_beta_0", 0, 250, value = 0, step = .1)),
          box(sliderInput("fs_beta_1", "fs_beta_1", 0, 1.2, value = 0, step = 0.01))
        )
      ),
      tabItem(
        tabName = "ex_2_b",
        box(plotOutput("plot_2_estimated")),
        box(actionButton("estimate", "Odhadnout přímku")),
        box(verbatimTextOutput("model_values"))
      ),
      tabItem(
        tabName = "ex_2_b",
        fluidRow(box(plotOutput("plot_2_estimated"))),
        box(actionButton("estimate", "Odhadnout přímku")),
        box(verbatimTextOutput("model_values"))
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  father_son_sample <- father_son[sample(1:nrow(father_son), 30), ]
  father_son_sample$fit <- NA
  mse <- mean(lm(sheight ~ fheight,
    data = father_son_sample
  )$residuals^2)
  x <- 1:25
  y <- 15 + 8.3 * x + rnorm(length(x), 0, 30)
  sim_data <- data.frame(y, x, fit = 0, res = 0)

  output$plot1_intro <- renderPlot({
    sim_data$fit <- input$beta_0_a + input$beta_1_a * sim_data$x
    ggplot(data = sim_data) +
      geom_point(aes(x, y)) +
      geom_line(aes(x, fit),
        color = "red"
      ) +
      theme_bw() +
      xlab("x") +
      ylab("y")
  })
  output$plot1_fit <- renderPlot({
    sim_data$fit <- input$beta_0_b + input$beta_1_b * sim_data$x
    ggplot(data = sim_data) +
      geom_point(aes(x, y)) +
      geom_line(aes(x, fit),
        color = "red"
      ) +
      theme_bw() +
      xlab("x") +
      ylab("y")
  })

  output$plot1_residuals <- renderPlot({
    sim_data$fit <- input$beta_0_b + input$beta_1_b * sim_data$x
    sim_data$res <- sim_data$y - sim_data$fit
    ggplot(data = sim_data) +
      geom_point(aes(x, res)) +
      theme_bw() +
      xlab("x") +
      ylab("Rezidua")
  })

  output$plot_father_son <- renderPlot({
    father_son_sample$fit <- input$fs_beta_0 + input$fs_beta_1 * father_son_sample$fheight
    ggplot(data = father_son_sample) +
      geom_point(aes(fheight, sheight)) +
      geom_line(aes(fheight, fit),
        color = "red"
      ) +
      theme_bw() +
      xlab("Výška otce") +
      ylab("Výška syna") +
      ylim(100, 250)
  })

  model_1 <- reactive({
    if (input$estimate > 0) {
      model <- lm(sheight ~ fheight, data = father_son_sample)
      return(list(model = model, b0 = coef(model)[1], b1 = coef(model)[2]))
    } else {
      return(list(model = NULL, b0 = 0, b1 = 0))
    }
  })

  output$model_values <- renderPrint({
    if (is.null(model_1()$model)) {
    } else {
      print(summary(model_1()$model))
    }
  })


  output$plot_2_estimated <- renderPlot({
    beta_0 <- model_1()$b0
    beta_1 <- model_1()$b1
    father_son_sample$fit <- beta_0 + beta_1 * father_son_sample$fheight
    ggplot(data = father_son_sample) +
      geom_point(aes(fheight, sheight)) +
      geom_line(aes(fheight, fit),
        color = "red"
      ) +
      theme_bw() +
      xlab("Výška otce") +
      ylab("Výška syna") +
      ylim(100, 250)
  })

  output$error_box <- renderValueBox({
    valueBox(
      "Chyba",
      paste(round(mean(
        (father_son_sample$sheight - (input$fs_beta_0 + input$fs_beta_1 * father_son_sample$fheight))^2
      ), 2)),
      color = "green"
    )
  })

  output$error_min_box <- renderValueBox({
    valueBox(
      "Minimální chyba",
      paste(round(mse, 2)),
      color = "green"
    )
  })
}

shinyApp(ui, server)

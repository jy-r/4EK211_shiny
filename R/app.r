## app.R ##
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(ggExtra)

# Data loading
# --------------------------
father_son <- readRDS("../data/father_son.Rds")
set.seed(122)
father_son_sample <- father_son[sample(1:nrow(father_son), 30), ]
father_son_sample$fit <- NA
mse <- mean(lm(sheight ~ fheight,
  data = father_son_sample
)$residuals^2)
x <- 1:25
y <- 15 + 8.3 * x + rnorm(length(x), 0, 30)
sim_data <- data.frame(y, x, fit = 0, res = 0)

# Data - for bootstrap 
# --------------------------
x <- rep(1:25, 200)
u <- rnorm(length(x), mean = 0, sd = 60)
y <- 10 + 5 * x + u
model <- lm(y ~ x)
simulated_dta <- matrix(c(y, x, u), ncol = 3)
colnames(simulated_dta) <- c("y", "x", "u")
simulated_models <- list()
l <- 0
for (i in 1:(199)) {
  dta_sample <- as.data.frame(simulated_dta[seq(l, l + 25, by = 1), ])
  model <- lm(y ~ x, data = dta_sample)
  dta_sample$fit <- fitted(model)
  simulated_models[[i]] <- list(model = model, dta_sample = dta_sample)
  l <- l + 25
}
simulated_dta <- as.data.frame(simulated_dta)

# UI
# -----------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Příklad 1 - Úvod", tabName = "ex_1_a"),
      menuItem("Příklad 1 - rezidua", tabName = "ex_1_b"),
      menuItem("Příklad 2 - Otcové a synové", tabName = "ex_2_a"),
      menuItem("Příklad 2 - odhad MNČ", tabName = "ex_2_b"),
      menuItem("Příklad 3 - Simulace", tabName = "ex_3_a"),
      menuItem("Příklad 3 - vlastnosti MNČ", tabName = "ex_3_b")
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
            div("kde	\\(\\beta_0\\) představuje úrovňovou konstantu, \\(\\beta_1\\) sklon přímky"),
            sliderInput("beta_0_a", "b_0", 0, 50, value = 0, step = .1),
            sliderInput("beta_1_a", "b_1", 0, 50, value = 0, step = 1)
          ),
          box(
            plotOutput("plot1_intro", height = 250)
          )
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
            div("kde	\\(\\beta_0\\) představuje úrovňovou konstantu, \\(\\beta_1\\) sklon přímky"),
					  div("$$\\hat{u} = y - \\hat{y}$$"),
						div("Zkuste odhadnout parametry regresní přímky"),
          	sliderInput("beta_0_b", "b_0", 0, 50, value = 0, step = .1),
          	sliderInput("beta_1_b", "b_1", 0, 50, value = 0, step = 1)),
          	box(plotOutput("plot1_fit", height = 250),
          	plotOutput("plot1_residuals", height = 250)
					)
        ),
      ),
      tabItem(
        tabName = "ex_2_a",
        h2("Pearsonův dataset - výška otců a jejich synů"),
        fluidRow(
					box(
					div("$$\\hat{y} = b_0 + b_1 x$$"),
					div("$$\\hat{u} = y - \\hat{y}$$"),
					div("Chyba = \\(\\sum{\\hat{u}^2} = \\sum(y - \\hat{y})^2\\)"),
          div("Zkuste zvolit parametry, tak aby chyba byla minimální"),
          sliderInput("fs_beta_0", "b_0", 0, 250, value = 0, step = .1),
          sliderInput("fs_beta_1", "b_1", 0, 1.2, value = 0, step = 0.01)),
          box(plotOutput("plot_father_son", height = 250))
				),
				fluidRow(
          valueBoxOutput("error_box", width= 4),
          valueBoxOutput("error_min_box", width= 4)
        ),
      ),
      tabItem(
        tabName = "ex_2_b",
        box(actionButton("estimate", "Odhadnout přímku metodou nejmenších čtverců"),
        verbatimTextOutput("model_values")),
        box(plotOutput("plot_2_estimated"))
      ),
      tabItem(
        tabName = "ex_3_a",
        fluidRow(
          box(
							div("Zkusíme nasimulovat data na kterých můžeme ověřit zdali MNČ"), 
							div("$$ y = \\beta_0 + \\beta_1 x + u$$ a náhodná složka by pocházela z normálního rozdělení se střední hodnotou \\( \\mu \\) a směrodatnou odchylkou \\( \\sigma \\)"),
							div("$$ u \\sim N(\\mu, \\sigma), $$"),
							div("dále zvolíme populační parametry \\( \\beta_0 = 10 \\) a \\( \\beta_1 = 5 \\)"),
							div("pak populace lze popsat jako"), 
							div("$$ y \\sim N(10 + 5 x, \\sigma) $$"),
							div("z této populace náhodně vybereme vzorek o velikosti \\( n = 25 \\) a odhadneme."),
            numericInput("sim_1_mu", "Střední hodnota", value = 0),
            numericInput("sim_1_sd", "Směrodatná odchylka", value = 60),
            sliderInput("sim_1_beta_0", "$beta_0$", 0, 100, value = 10, step = .1),
            sliderInput("sim_1_beta_1", "$beta_1$", 0, 10, value = 5, step = 0.01),
          ),
          box(plotOutput("plot_3_sim_1"),
            actionButton("sim_1_estimate", "Odhadnout"),
        verbatimTextOutput("sim_1_model_values"))
        ),
      ),
      tabItem(
        tabName = "ex_3_b",
        fluidRow(
          box(
            plotOutput("plot_3_sim_2"),
            actionButton("sim_2_estimate", "Zopakovat výběr a odhadnout")
          ),
          box(
            plotOutput("plot_3_sim_2_bar"),
            verbatimTextOutput("sim_2_model_values")
          )
        ),
      )
    )
  )
)

# Server side
# -----------------------------

server <- function(input, output) {

  # Example 1 a - Introduction
  #-----------------------------

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

  # Example 1 b - Residuals
  #-----------------------------

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

  # Example 2 a - Father and son
  #-----------------------------

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

  # Example 2 b - Father and son - Estimation
  #-----------------------------
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

  # Example 3 a - Simulation
  #-----------------------------

  model_sim_1 <- reactive({
    x <- 1:25
    u <- rnorm(length(x), mean = input$sim_1_mu, sd = input$sim_1_sd)
    y <- input$sim_1_beta_0 + input$sim_1_beta_1 * x + u
    dta <- data.frame(y, x, u, fit = 0)
    print(input$sim_1_estimate)
    if (input$sim_1_estimate > 0) {
      model <- lm(y ~ x, data = dta)
      dta$fit <- fitted(model)
      return(list(model = model, b0 = coef(model)[1], b1 = coef(model)[2], dta = dta))
    } else {
      return(list(model = NULL, b0 = 0, b1 = 0, dta = dta))
    }
  })

  output$plot_3_sim_1 <- renderPlot({
    rec_model <- model_sim_1()
    dta <- rec_model$dta
    ggplot(data = dta) +
      geom_point(aes(x, y)) +
      geom_line(aes(x, fit), color = "red") +
      theme_bw() +
      xlab("x") +
      ylab("y")
  })

  output$sim_1_model_values <- renderPrint({
    if (is.null(model_sim_1()$model)) {
      print("Odhadněte model")
    } else {
      print(summary(model_sim_1()$model)$coefficients)
    }
  })

  # Example 3 b - Simulation - coef distribution
  #-----------------------------


  output$plot_3_sim_2 <- renderPlot({
    if (input$sim_2_estimate == 0) i <- 1 else i <- input$sim_2_estimate + 1
    dta <- simulated_models[[i]]$dta
    ggplot() +
      geom_point(aes(x, y), data = simulated_dta, alpha = 0.01) +
      geom_point(aes(x, y), data = dta) +
      geom_line(aes(x, fit), color = "red", data = dta) +
      theme_bw() +
      xlab("x") +
      ylab("y")
  })

  output$plot_3_sim_2_bar <- renderPlot({
    if (input$sim_2_estimate == 0) i <- 1 else i <- input$sim_2_estimate + 1
    if (input$sim_2_estimate > 5) i <- input$sim_2_estimate * 3
    if (input$sim_2_estimate > 10) i <- input$sim_2_estimate * 10
    if (input$sim_2_estimate > 15) i <- input$sim_2_estimate * 20
    if (i > 199) i <- 199



    simulated_coefs <- simulated_models[1:i]

    simulated_coefs <- data.frame(t(sapply(simulated_coefs, function(x) coef(x$model))))
    colnames(simulated_coefs) <- c("beta0", "beta1")
    simulated_coefs <- tidyr::gather(simulated_coefs, "param", "value")

    # 3.96 sd beta0
    # 0.25 sd beta1
    g1_beta0 <- ggplot(data = simulated_coefs[simulated_coefs$param == "beta0", ]) +
      geom_histogram(aes(x = value, y = ..density..), fill = "lightgrey", colour = "black", bins = 15) +
      stat_function(fun = dnorm, args = list(mean = 10, sd = 24)) +
      geom_vline(xintercept = 10, colour = "red") +
			#ylim(0, 0.05)+
      xlim(-45, 65) +
      theme_bw()
    g2_beta1 <- ggplot(data = simulated_coefs[simulated_coefs$param == "beta1", ]) +
      geom_histogram(aes(x = value, y = ..density..), fill = "lightgrey", colour = "black", bins = 15) +
      stat_function(fun = dnorm, args = list(mean = 5, sd = 1.5)) +
      geom_vline(xintercept = 5, colour = "red") +
			#ylim(0, 0.5)+
      xlim(0, 10) +
      theme_bw()
    gridExtra::grid.arrange(g1_beta0, g2_beta1)
  })

  output$sim_2_model_values <- renderPrint({
    if (input$sim_2_estimate == 0) i <- 1 else i <- input$sim_2_estimate + 1
    print(summary(simulated_models[[i]]$model)$coefficients)
  })
}

# -----------------------------
shinyApp(ui, server)

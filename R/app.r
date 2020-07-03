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
    menuItem("Příklad 2", tabName = "ex_2"),
    menuItem("Příklad 3", tabName = "ex_3")
   )
	),
  dashboardBody(
		tabItems(
			tabItem(tabName = "ex_1_a", 
    	fluidRow(
    	  plotOutput("plot1_intro", height = 250),
			 ),
			fluidRow(
			  h3("Zkuste odhadnout parametry regresní přímky"),
    	  box(sliderInput("beta_0_a", "beta_0", 0, 50, value= 0, step= .1)),
				box(sliderInput("beta_1_a", "beta_1", 0, 50, value = 0, step= 1))
		   )
			),
			tabItem(tabName = "ex_1_b", 
    	fluidRow(
    	  box(plotOutput("plot1_fit", height = 250)),
    	  box(plotOutput("plot1_residuals", height = 250))
			  ),
			fluidRow(
			  h3("Zkuste odhadnout parametry regresní přímky"),
    	  box(sliderInput("beta_0_b", "beta_0", 0, 50, value= 0, step= .1)),
				box(sliderInput("beta_1_b", "beta_1", 0, 50, value = 0, step= 1))
		   )
		  ),
			tabItem(tabName = "ex_2",
				h2("Pearsonův dataset - výška otců a jejich synů"),
    	fluidRow(
    	  plotOutput("plot_father_son", height = 250),
				valueBoxOutput("error_box")
				),
			fluidRow(
				h3("Zkuste odhadnout parametry regresní přímky"),
    	  box(sliderInput("fs_beta_0", "fs_beta_0", 110, 250, value= 0, step= 1)),
				box(sliderInput("fs_beta_1", "fs_beta_1", 0, 1.2, value = 0, step= 0.01))
    	)),
    	tabItem(tabName = "ex_3",
    	  h2("Widgets tab content")
    	)
  )
  )
)

server <- function(input, output) {
  set.seed(122)
	father_son_sample <- father_son[sample(1:nrow(father_son), 30),]
	father_son_sample$fit <- NA
	mse <- mean(lm(sheight ~ fheight,
								 data = father_son_sample)$residuals^2)
	x <- 1:25
	y <- 15 + 8.3 * x + rnorm(length(x), 0,30)
	sim_data <- data.frame(y, x, fit = 0, res = 0)

  output$plot1_intro <- renderPlot({
			sim_data$fit <- input$beta_0_a + input$beta_1_a * sim_data$x
			ggplot(data = sim_data) +
				 	geom_point(aes(x, y)) +
					geom_line(aes(x, fit),
									 	color = "red") +
				 	theme_bw() +
					xlab("x")+
					ylab("y")
  })
  output$plot1_fit <- renderPlot({
			sim_data$fit <- input$beta_0_b + input$beta_1_b * sim_data$x
			ggplot(data = sim_data) +
				 	geom_point(aes(x, y)) +
					geom_line(aes(x, fit),
									 	color = "red") +
				 	theme_bw() +
					xlab("x")+
					ylab("y")
  })

  output$plot1_residuals <- renderPlot({
			sim_data$fit <- input$beta_0_b + input$beta_1_b * sim_data$x
			sim_data$res <- sim_data$y - sim_data$fit 
			ggplot(data = sim_data) +
				 	geom_point(aes(x, res)) +
				 	theme_bw() +
					xlab("x")+
					ylab("Rezidua")
  })

  output$plot_father_son <- renderPlot({
			father_son_sample$fit <- input$fs_beta_0 + input$fs_beta_1 * father_son_sample$fheight
		ggplot(data = father_son_sample) +
				 	geom_point(aes(fheight, sheight)) +
					geom_line(aes(fheight, fit),
									 	color = "red") +
				 	theme_bw() +
					xlab("Výška otce")+
					ylab("Výška syna")+
					ylim(100, 250)
  })

  output$error_box <- renderValueBox({
    valueBox(
      "Chyba", 
			paste("Chyba:", 
						round(mean(
											 (father_son_sample$sheight - (input$fs_beta_0 + input$fs_beta_1 * father_son_sample$fheight ))^2
											 ),2),
			" Minimální chyba:", round(mse,2)), 
      color = "green"
    )
  })
}

shinyApp(ui, server)

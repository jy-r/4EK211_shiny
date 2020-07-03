## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
	 sidebarMenu(
    menuItem("Příklad 1", tabName = "ex_1"),
    menuItem("Příklad 2", tabName = "ex_2")
   )
	),
  dashboardBody(
		tabItems(
			tabItem(tabName = "ex_1",
    	  h2("Příklad 1"),
    	fluidRow(
    	  box(plotOutput("plot1", height = 250)),
#    	  box(textOutput("error"))
				valueBoxOutput("error_box")

				),
			fluidRow(h3("Zkuste odhadnout parametry regresní přímky"),
    	  box(
    	    sliderInput("beta_0", "beta_0", 0, 50, value= 0, step= 0.1)),
				box(
    	    sliderInput("beta_1", "beta_1", 0, 20, value = 0, step= 0.1)
    	  )
    	)
    	),
    	tabItem(tabName = "ex_2",
    	  h2("Widgets tab content")
    	)
  )
    # Boxes need to be put in a row (or column)
  )
)

server <- function(input, output) {
  set.seed(122)
  x <- 1:25
	y <- 10 + 8.7 * x + rnorm(length(x), 0, 20)
	mse <- mean(lm(y~x)$residuals^2)

  output$plot1 <- renderPlot({
			plot(y~x)
			lines(input$beta_0 + input$beta_1 * x ~ x, col= "red")
  })
	output$error <- renderText({
			paste("Chyba:", round(mean((y - (input$beta_0 + input$beta_1 * x ))^2),2),
			" Minimální chyba:", round(mse,2))
	})
  output$error_box <- renderValueBox({
    valueBox(
      "Chyba", 
			paste("", round(mean((y - (input$beta_0 + input$beta_1 * x ))^2),2),
			" (Minimální chyba:", round(mse,2), ")")
, icon = icon("list"),
      color = "green"
    )
  })
}

shinyApp(ui, server)

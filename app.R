## Shiny application for graph data

library(shiny)
library(dplyr)
library(gghighlight)
library(mathjaxr)

dir <- "C:/Users/pamel/OneDrive/Jobs/Seism/"
p_new <- read.csv(paste0(dir,"pruebappn.csv"))

Cr1 <- function(t,H,D,G) {
  return(round((950 * (t^2)) / ((H)^2 * (D) * (G)),3))
}

Cr2 <- function(t,H,D,G) {
  return(round(((t^2)) / ((D) * (G)),3))
}

Sim <- tibble(
			R_t = runif(100, min(p_new$t, na.rm = TRUE), max(p_new$t, na.rm = TRUE)),
			R_H = runif(100, min(p_new$Hft, na.rm = TRUE), max(p_new$Hft, na.rm = TRUE)),
			R_D = runif(100, min(p_new$Dft, na.rm = TRUE), max(p_new$Dft, na.rm = TRUE)),
			R_G = runif(100, min(p_new$G, na.rm = TRUE), max(p_new$G, na.rm = TRUE))) %>% 			
		mutate(cr1 = Cr1(t = R_t, H = R_H, D = R_D, G = R_G),
			   cr2 = Cr2(t = R_t, H = R_H, D = R_D, G = R_G)) 


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
	titlePanel("Estimación del Coeficiente de resistencia (Cr)"),
		  
	# Sidebar with a slider input for number of bins 
	fluidRow(
		column(12,
			sidebarLayout(
				sidebarPanel(
					sliderInput(inputId = "t1_x",
								label = "Ingrese Espesor del manto (cm):",
								min = min(p_new$t, na.rm = TRUE),
								max = max(p_new$t, na.rm = TRUE),
								value = min(p_new$t, na.rm = TRUE)),
					sliderInput(inputId = "H1_x",
								label = "Ingrese Altura del contenido del estanque (m):",
								min = min(p_new$Hft, na.rm = TRUE),
								max = max(p_new$Hft, na.rm = TRUE),
								value = min(p_new$Hft, na.rm = TRUE)),
					sliderInput(inputId = "D1_x",
								label = "Ingrese Diametro del estanque (m):",
								min = min(p_new$Dft, na.rm = TRUE),
								max = max(p_new$Dft, na.rm = TRUE),
								value = min(p_new$Dft, na.rm = TRUE)),
					sliderInput(inputId = "G1_x",
								label = "Ingrese Peso especifico del contenido (ton/cm3):",
								min = min(p_new$G, na.rm = TRUE),
								max = max(p_new$G, na.rm = TRUE),
								value = min(p_new$G, na.rm = TRUE))),
				mainPanel(
					headerPanel("Formula 1: "),
					uiOutput("cr_text1"),
					plotOutput("distPlot1"))
			)	
		),
		column(12,
			sidebarLayout(
				sidebarPanel(
					sliderInput(inputId = "t2_x",
								label = "Ingrese Espesor del manto (cm):",
								min = min(p_new$t, na.rm = TRUE),
								max = max(p_new$t, na.rm = TRUE),
								value = min(p_new$t, na.rm = TRUE)),
					sliderInput(inputId = "H2_x",
								label = "Ingrese Altura del contenido del estanque (m):",
								min = min(p_new$Hft, na.rm = TRUE),
								max = max(p_new$Hft, na.rm = TRUE),
								value = min(p_new$Hft, na.rm = TRUE)),
					sliderInput(inputId = "D2_x",
								label = "Ingrese Diametro del estanque (m):",
								min = min(p_new$Dft, na.rm = TRUE),
								max = max(p_new$Dft, na.rm = TRUE),
								value = min(p_new$Dft, na.rm = TRUE)),
					sliderInput(inputId = "G2_x",
								label = "Ingrese Peso especifico del contenido (ton/cm3):",
								min = min(p_new$G, na.rm = TRUE),
								max = max(p_new$G, na.rm = TRUE),
								value = min(p_new$G, na.rm = TRUE))),
				mainPanel(
					headerPanel("Formula 2: "),
					uiOutput("cr_text2"),
					plotOutput("distPlot2"))
			)
		)
	)
)

# Define server logic required to draw a graph
server <- function(input, output) {
  
	t1 <- reactive({as.numeric(input$t1_x)})
	h1 <- reactive({as.numeric(input$H1_x)})
	d1 <- reactive({as.numeric(input$D1_x)})
	g1 <- reactive({as.numeric(input$G1_x)})
  
  #Formula tab 1
    output$cr_text1 <- renderUI({ 
						withMathJax(paste0("\\(C_{r} = \\dfrac{950 * t^2}{H^2 * D * \\gamma} = \\)", Cr1(t1(), h1(), d1(), g1())))
						})
    
  #Plot random values and highlight selected values 
    output$distPlot1 <- renderPlot({

		Sim  %>% 
			ggplot(aes(x = R_D, y = cr1)) +
			geom_point() +
			geom_point(aes(x = d1(),y = Cr1(t1(), h1(), d1(), g1())), size = 5) +
			theme(legend.position = "none") +
			labs(title = "Resistencia del estanque", x = "Diametro del estanque (m)", y =  "Coeficiente de resistencia - Cr1") +
			geom_label(aes(x = d1(), y = Cr1(t1(), h1(), d1(), g1()), 
					label = paste("t = ", round(t1(),2), "H = ", round(h1(),2), "D = ", round(d1(),2), "G = ", round(g1(),2))),
					hjust = 0, vjust = -1, fill = "orange", colour = "darkblue", alpha= 0.5)
      
    })
    
    ###Change function HERE!!!!
    t2 <- reactive({as.numeric(input$t2_x)})
    h2 <- reactive({as.numeric(input$H2_x)})
    d2 <- reactive({as.numeric(input$D2_x)})
    g2 <- reactive({as.numeric(input$G2_x)})
    
    #Formula tab 2
    output$cr_text2 <- renderUI({ 
						withMathJax(paste0("\\(C_{r} = \\dfrac{t^2}{D * \\gamma} = \\)", Cr2(t2(), h2(), d2(), g2())))
						})
    output$distPlot2 <- renderPlot({
	
      Sim  %>% 
			ggplot(aes(x = R_D, y = cr2)) +
			geom_point() +
			geom_point(aes(x = d2(),y = Cr2(t2(), h2(), d2(), g2())), size = 5) +
			theme(legend.position = "none") +
			labs(title = "Resistencia del estanque", x = "Diametro del estanque (m)", y =  "Coeficiente de resistencia - Cr2") +
			geom_label(aes(x = d2(), y = Cr2(t2(), h2(), d2(), g2()), 
					label = paste("t = ", round(t2(),2), "H = ", round(h2(),2), "D = ", round(d2(),2), "G = ", round(g2(),2))),
					hjust = 0, vjust = -1, fill = "orange", colour = "darkblue", alpha= 0.5)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

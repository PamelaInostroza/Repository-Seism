## Shiny application for graph data

library(shiny)
library(dplyr)
library(gghighlight)
library(mathjaxr)

dir <- "C:/Users/pamel/OneDrive/Jobs/Seism/"
p_new <- read.csv(paste0(dir,"pruebappn.csv"))

Cr <- function(t,H,D,G) {
  return(round((950 * (t^2)) / ((H)^2 * (D) * (G)),3))
}

Sim <- tibble(R_t = runif(100, min(p_new$t, na.rm = TRUE), max(p_new$t, na.rm = TRUE)),
              R_H = runif(100, min(p_new$Hft, na.rm = TRUE), max(p_new$Hft, na.rm = TRUE)),
              R_D = runif(100, min(p_new$Dft, na.rm = TRUE), max(p_new$Dft, na.rm = TRUE)),
              R_G = runif(100, min(p_new$G, na.rm = TRUE), max(p_new$G, na.rm = TRUE))) %>% 
    mutate(cr = Cr(t = R_t, H = R_H, D = R_D, G = R_G)) %>% 
    arrange(cr)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Estimación del Coeficiente de resistencia (Cr)"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "t_x",
                        label = "Ingrese Espesor del manto (cm):",
                        min = min(p_new$t, na.rm = TRUE),
                        max = max(p_new$t, na.rm = TRUE),
                        value = min(p_new$t, na.rm = TRUE)),
            sliderInput(inputId = "H_x",
                         label = "Ingrese Altura del contenido del estanque (m):",
                         min = min(p_new$Hft, na.rm = TRUE),
                         max = max(p_new$Hft, na.rm = TRUE),
                         value = min(p_new$Hft, na.rm = TRUE)),
            sliderInput(inputId = "D_x",
                         label = "Ingrese Diametro del estanque (m):",
                         min = min(p_new$Dft, na.rm = TRUE),
                         max = max(p_new$Dft, na.rm = TRUE),
                         value = min(p_new$Dft, na.rm = TRUE)),
            sliderInput(inputId = "G_x",
                         label = "Ingrese Peso especifico del contenido (ton/cm3):",
                         min = min(p_new$G, na.rm = TRUE),
                         max = max(p_new$G, na.rm = TRUE),
                         value = min(p_new$G, na.rm = TRUE))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          headerPanel("Formula utilizada: "),
          uiOutput("cr_text"),
           plotOutput("distPlot")
           
        )
    )
)

# Define server logic required to draw a graph
server <- function(input, output) {
  
  t <- reactive({as.numeric(input$t_x)})
  h <- reactive({as.numeric(input$H_x)})
  d <- reactive({as.numeric(input$D_x)})
  g <- reactive({as.numeric(input$G_x)})
  
  #Calculate value of Cr using formula
    output$cr_text <- renderUI({ 
      withMathJax(paste0("\\(C_{r} = \\dfrac{950 * t^2}{H^2 * D * \\gamma} = \\)", Cr(t(), h(), d(), g())))
      })
    
  #Plot random values and highlight selected values 
    output$distPlot <- renderPlot({
       Sim  %>% 
        ggplot(aes(x = R_D, y = cr)) +
        geom_point() +
        geom_point(aes(x = d(),y = Cr(t(), h(), d(), g())), size = 5) +
        theme(legend.position = "none") +
            labs(title = "Resistencia del estanque", x = "Diametro del estanque (m)", y =  "Coeficiente de resistencia - Cr") +
        geom_label(aes(x = d(), y = Cr(t(), h(), d(), g()), label = paste("t = ", round(t(),2), "H = ", round(h(),2), "D = ", round(d(),2), "G = ", round(g(),2))),
                   hjust = 0, vjust = -1, fill = "orange", colour = "darkblue", alpha= 0.5)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

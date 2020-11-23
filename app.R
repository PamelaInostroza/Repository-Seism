## Shiny application for graph data

library(shiny)
library(dplyr)
library(gghighlight)
library(mathjaxr)
library(kableExtra)

options(knitr.kable.NA = '')

dir <- "C:/Users/pamel/OneDrive/Jobs/Seism/"
limits <- read.csv(paste0(dir,"pruebappn.csv"), encoding = "UTF-8")

  # c("ï..Item", "Country", "Loc", "Year", "Ep", "Mag", "Seism", "TAG", "Dano", "Cont", "G", "Dft", "Dm",     
  # "Hft", "Hm", "DH", "Hmax", "HLLm", "Llen", "VL", "t", "tf", "Cr", "Anc", "Roof", "Damage")
Cr1 <- function(t,H,D,G) {
  return(round((950 * (t^2)) / ((H)^2 * (D) * (G)),3))
}

Cr2 <- function(t,H,D,G) {
  return(round(((t^2)) / ((D) * (G)),3))
}

model <- function(datosmodel) {
  datosmodel$estado_n <- ifelse(datosmodel$Daño %in% c("sin daño","pandeo"), 0, 1)
  # total_fit <- glm(estado_n ~  G + Dm + t + Hm + DH + G + HLLm + VL + tf + cr1, data = datosmodel, family = "binomial")
  # backwards = step(total_fit)
  mod_fit <- glm(estado_n ~  Dm + cr1, data = datosmodel, family = "binomial")
  #summary(mod_fit)
  return(mod_fit)
}

predict <- function(datos, model){
  datos$cats.prob <- stats::predict(object = model, newdata = datos, type = "response")
  datos$cats.pred <- rep("sin daño", nrow(datos))
  datos$cats.pred[datos$cats.prob > .5] = "colapso"
  return(datos)
}

limits <- limits %>% 
  mutate(cr1 = Cr1(t = t, H = Hm, D = Dm, G = G),
         cr2 = Cr2(t = t, H = Hm, D = Dm, G = G),
         cats.pred = ifelse(Daño == "pandeo", "sin daño", Daño),
         Tipo = "Efectivos")

col_names <- colnames(limits)

model <- model(limits)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
	titlePanel("Prediccion de colapso"),
		  
	# Sidebar with a slider input for number of bins 
	fluidRow(
	  column(12,
	     sidebarPanel(
	         actionButton("dodat", "Simular datos"),
	         fileInput("File1", "Actualizar parámetros con archivo CSV", multiple = FALSE, 
	                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 
	                   width = NULL,
	                   buttonLabel = "Seleccione...", placeholder = "No archivo seleccionado"),
	         actionButton("getdat", "Aplicar"),
	         textOutput(outputId = "out"),
	         tags$head(tags$style("#out{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"
	         )
	         )
	     ),
	     mainPanel(
	       headerPanel("Estimación de parámetros: "),
	       uiOutput("cr_text1"),
	       uiOutput("model_text1")
	     )
	  ),
		column(12,
			sidebarLayout(
				sidebarPanel(
				  sliderInput(inputId = "t1_x",
	              label = "t: Ingrese Espesor del manto (cm):",
	              min = min(limits$t, na.rm = TRUE),
	              max = 8, #max(limits$t, na.rm = TRUE),
	              value = min(limits$t, na.rm = TRUE)),
					sliderInput(inputId = "H1_x",
								label = "H: Ingrese Altura del contenido del estanque (m):",
								min = min(limits$Hm, na.rm = TRUE),
								max = max(limits$Hm, na.rm = TRUE),
								value = min(limits$Hm, na.rm = TRUE)),
					sliderInput(inputId = "D1_x",
								label = "D: Ingrese Diametro del estanque (m):",
								min = min(limits$Dm, na.rm = TRUE),
								max = max(limits$Dm, na.rm = TRUE),
								value = min(limits$Dm, na.rm = TRUE)),
					sliderInput(inputId = "G1_x",
								label = "G: Ingrese Peso especifico del contenido (ton/cm3):",
								min = min(limits$G, na.rm = TRUE),
								max = max(limits$G, na.rm = TRUE),
								value = min(limits$G, na.rm = TRUE))),
				mainPanel(
					plotOutput("distPlot1", height = 500,
					           # Equivalent to: click = clickOpts(id = "plot_click")
					           click = "plot1_click",
					           brush = brushOpts(
					             id = "plot1_brush"
					           )),
					tableOutput("click_info")
					)
			)	
		)
		# ,
		# column(12,
		# 	sidebarLayout(
		# 		sidebarPanel(
		# 			sliderInput(inputId = "t2_x",
		# 						label = "Ingrese Espesor del manto (cm):",
		# 						min = min(limits$t, na.rm = TRUE),
		# 						max = max(limits$t, na.rm = TRUE),
		# 						value = min(limits$t, na.rm = TRUE)),
		# 			sliderInput(inputId = "H2_x",
		# 						label = "Ingrese Altura del contenido del estanque (m):",
		# 						min = min(limits$Hm, na.rm = TRUE),
		# 						max = max(limits$Hm, na.rm = TRUE),
		# 						value = min(limits$Hm, na.rm = TRUE)),
		# 			sliderInput(inputId = "D2_x",
		# 						label = "Ingrese Diametro del estanque (m):",
		# 						min = min(limits$Dm, na.rm = TRUE),
		# 						max = max(limits$Dm, na.rm = TRUE),
		# 						value = min(limits$Dft, na.rm = TRUE)),
		# 			sliderInput(inputId = "G2_x",
		# 						label = "Ingrese Peso especifico del contenido (ton/cm3):",
		# 						min = min(limits$G, na.rm = TRUE),
		# 						max = max(limits$G, na.rm = TRUE),
		# 						value = min(limits$G, na.rm = TRUE))),
		# 		mainPanel(
		# 			headerPanel("Formula 2: "),
		# 			uiOutput("cr_text2"),
		# 			plotOutput("distPlot2")
		# 			)
		# 	)
		# )
	)
)

# Define server logic required to draw a graph
server <- function(input, output, session) {
 
  
  upd <- eventReactive(input$getdat, {
      df <- read.csv(input$File1$datapath, encoding = "UTF-8")
      df <- df %>% mutate(cr1 = Cr1(t = t, H = Hm, D = Dm, G = G),
                          cr2 = Cr2(t = t, H = Hm, D = Dm, G = G),
                          Tipo = "Efectivos")
      predict(df, model)
    })
  
  observeEvent(input$getdat, {
    df <- upd()
    
    updateSliderInput(session, "t1_x", value = min(df$t, na.rm = TRUE), min = min(df$t, na.rm = TRUE), max = 8)#max(df$t, na.rm = TRUE))
    updateSliderInput(session, "H1_x", value = min(df$Hm, na.rm = TRUE), min = min(df$Hm, na.rm = TRUE), max = max(df$Hm, na.rm = TRUE))
    updateSliderInput(session, "D1_x", value = min(df$Dm, na.rm = TRUE), min = min(df$Dm, na.rm = TRUE), max = max(df$Dm, na.rm = TRUE))
    updateSliderInput(session, "G1_x", value = min(df$G, na.rm = TRUE), min = min(df$G, na.rm = TRUE), max = max(df$G, na.rm = TRUE))
    
    # updateSliderInput(session, "t2_x", value = min(df$t, na.rm = TRUE), min = min(df$t, na.rm = TRUE), max = max(df$t, na.rm = TRUE))
    # updateSliderInput(session, "H2_x", value = min(df$Hm, na.rm = TRUE), min = min(df$Hm, na.rm = TRUE), max = max(df$Hm, na.rm = TRUE))
    # updateSliderInput(session, "D2_x", value = min(df$Dm, na.rm = TRUE), min = min(df$Dm, na.rm = TRUE), max = max(df$Dm, na.rm = TRUE))
    # updateSliderInput(session, "G2_x", value = min(df$G, na.rm = TRUE), min = min(df$G, na.rm = TRUE), max = max(df$G, na.rm = TRUE))
  })
  
  output$out <- renderText({
    if (input$getdat == 1){
      dat <- upd()
      if (any(!colnames(dat) %in% c(col_names,"cats.prob", "cats.pred"))) {
        paste("Nombre de columnas no corresponden:", paste(colnames(dat)[!colnames(dat) %in% col_names], collapse = ","),"\n",
        "Nombres deben ser:", paste(col_names, collapse = ","))
      } else paste("Los datos se cargaron correctamente")
    }
    
  })
  
  
  s <- eventReactive(input$dodat | input$getdat, {
    if (input$getdat == 0 | input$dodat == 1) dat <- limits else dat <- upd()
    
    sim <- tibble(
      t = runif(100, min(dat$t, na.rm = TRUE), 8), #max(dat$t, na.rm = TRUE)),
      Hm = runif(100, min(dat$Hm, na.rm = TRUE), max(dat$Hm, na.rm = TRUE)),
      Dm = runif(100, min(dat$Dm, na.rm = TRUE), max(dat$Dm, na.rm = TRUE)),
      G = runif(100, min(dat$G, na.rm = TRUE), max(dat$G, na.rm = TRUE))) %>%
      mutate(cr1 = Cr1(t = t, H = Hm, D = Dm, G = G),
             cr2 = Cr2(t = t, H = Hm, D = Dm, G = G),
             Tipo = "Simulados") 
    predict(sim, model)
    
  })

   
 	t1 <- reactive({as.numeric(input$t1_x)})
	h1 <- reactive({as.numeric(input$H1_x)})
	d1 <- reactive({as.numeric(input$D1_x)})
	g1 <- reactive({as.numeric(input$G1_x)})
   

  #Formula tab 1
    output$cr_text1 <- renderUI({
						withMathJax(paste0("Coeficiente de resistencia: ","\\(C_{r} = \\dfrac{950 * t^2}{H^2 * D * G} = \\)", 
						                   Cr1(t1(), h1(), d1(), g1())))
						})
    
    output$model_text1 <- renderUI({
      withMathJax(paste0("Predicción de daño: ","\\(Colapso = \\dfrac{1}{1 + e^{-( \\beta_{0} + \\beta_{1}* D + \\beta_{2}*C_{r})}} > 0.5 \\)"))
    })
     
    p <- eventReactive(input$t1_x | input$H1_x | input$D1_x | input$G1_x, {
      poindata <- data.frame(t = t1(), Hm = h1(), Dm = d1(), G = g1(), cr1 = Cr1(t(1), h1(), d1(), g1()), Tipo = "Punto seleccionado")
      predict(poindata, model)
      
    })
    
    all <- eventReactive(input$t1_x | input$H1_x | input$D1_x | input$G1_x, {
      sim <- s()
      if (input$getdat == 0) u <- limits else u <- upd() 
      if (input$t1_x == 0) p <- NULL else p <- p() 
      rbind.fill(sim[,c("t", "Hm", "Dm", "G", "cr1",  "Tipo", "cats.prob", "cats.pred")],
                     u[,c("t", "Hm", "Dm", "G", "cr1",  "Tipo", "cats.pred")],
                     p[,c("t", "Hm", "Dm", "G", "cr1",  "Tipo", "cats.prob", "cats.pred")])
    })
  #Plot random values and highlight selected values
    output$distPlot1 <- renderPlot({
      if (input$getdat == 0) dat <- limits else dat <- upd()
      
      
    all() %>% 
      ggplot(aes(x = Dm, y = cr1, shape = Tipo, alpha = ifelse(Tipo == "Punto seleccionado" | cats.pred == "colapso", 1, 0.7), 
                 color = factor(ifelse(Tipo == "Punto seleccionado", "Punto seleccionado", ifelse(cats.pred == "colapso", "colapso","sin daño"))))) +
      geom_point(size = 4) +
      theme(legend.position = "bottom", legend.box = "vertical") +
      guides(alpha = FALSE) +
      labs(title = "Resistencia del estanque", x = "Diametro del estanque (m)", y =  "Coeficiente de resistencia - Cr1", 
           color = "Prediccion", shape = "Datos") +
      scale_color_manual(breaks = c("colapso", "Punto seleccionado", "sin daño"), 
                         values=c("red", "orange", "green")) +
      geom_label(aes(x = d1(), y = Cr1(t1(), h1(), d1(), g1()),
  				label = paste("t = ", round(t1(),2), "H = ", round(h1(),2), "D = ", round(d1(),2), "G = ", round(g1(),2))),
  				hjust = 0, vjust = -1, fill = "orange", colour = "darkblue", alpha= 0.5)

    })
    
   # output$click_info <- renderPrint({
      output$click_info <- function() {
        
      # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
      # were a base graphics plot, we'd need those.
        tabla <- nearPoints(all(), input$plot1_click)
      tabla %>% 
      knitr::kable("html", digits = c(2,2,2,2,2,NA,3), row.names = FALSE) %>%
        kable_styling("striped", full_width = F)
      
    }
    
#      
#   ###Change function HERE!!!!
#   t2 <- reactive({as.numeric(input$t2_x)})
#   h2 <- reactive({as.numeric(input$H2_x)})
#   d2 <- reactive({as.numeric(input$D2_x)})
#   g2 <- reactive({as.numeric(input$G2_x)})
#     
#     #Formula tab 2
#     output$cr_text2 <- renderUI({
# 						withMathJax(paste0("\\(C_{r} = \\dfrac{t^2}{D * \\gamma} = \\)", Cr2(t2(), h2(), d2(), g2())))
# 						})
# 
#     output$distPlot2 <- renderPlot({
#       if (input$getdat == 0) dat <- limits else dat <- upd()
#     s()  %>%
# 			ggplot(aes(x = Dm, y = cr2, color = Tipo, shape = cats.pred)) +
# 			geom_point() +
# 			geom_point(aes(x = d2(),y = Cr2(t2(), h2(), d2(), g2()),  color = Tipo), size = 5) +
#       geom_point(data = dat, aes(x = Dm,y = cr2, shape = Daño, color = Tipo), size = 3) +
# 			#theme(legend.position = "none") +
# 			labs(title = "Resistencia del estanque", x = "Diametro del estanque (m)", y =  "Coeficiente de resistencia - Cr2") +
# 			geom_label(aes(x = d2(), y = Cr2(t2(), h2(), d2(), g2()),
# 					label = paste("t = ", round(t2(),2), "H = ", round(h2(),2), "D = ", round(d2(),2), "G = ", round(g2(),2))),
# 					hjust = 0, vjust = -1, fill = "orange", colour = "darkblue", alpha= 0.5)
#     })
}

# Run the application 
shinyApp(ui = ui, server = server)

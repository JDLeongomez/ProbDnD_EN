#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(thematic)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(scales)
library(fontawesome)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),

                # Application title
                titlePanel(
                  title = tags$link(rel = "icon",
                                    type = "image/gif",
                                    href = "https://image.pngaaa.com/393/402393-middle.png"),
                  "DnD Dice Probality"),
                HTML("<center><img src='dnd.png'' width='200'></center>"),
                p(HTML("<center>Calculation of the probability of obtaining a number with multiple dice,
           calculated from simulations</center>")),
           p(HTML("<center>Code available from
      <a style=color:#ce0000ff;  href='https://github.com/JDLeongomez/ProbDnD_EN'>GitHub</a>
      - Created by
      <a style=color:#ce0000ff;  href='https://jdleongomez.info/es/'>Juan David Leongómez</a>
      · 2021 · <a style=color:#4075de;  href='https://shiny.jdl-svr.lat/ProbDnD/'>Versión en español</a>
      </center>")),
    p(),
    hr(),
    fluidRow(
        column(3,
               tags$h2("Dice", img(src="dice.png", width="80"), img(src="dice.png", width="80")),
               tags$h4("First dice type (mandatory)"),
               numericInput(inputId = "Dado1Max",
                            label = "How many sides does it have?",
                            min = 1,
                            max = 100,
                            value = 20,
                            step = 1,
                            width = '400px'),
               numericInput(inputId = "Dado1Num",
                            label = "How many dice of this type?",
                            min = 1,
                            max = 100,
                            value = 1,
                            step = 1,
                            width = '400px'),
               tags$h4("Second dice type (optional)"),
               numericInput(inputId = "Dado2Max",
                            label = "How many sides does it have?",
                            min = 1,
                            max = 100,
                            value = 6,
                            step = 1,
                            width = '400px'),
               numericInput(inputId = "Dado2Num",
                            label = "How many dice of this type?",
                            min = 0,
                            max = 100,
                            value = 1,
                            step = 1,
                            width = '400px'),
               tags$h4("Third dice type (optional)"),
               numericInput(inputId = "Dado3Max",
                            label = "How many sides does it have?",
                            min = 1,
                            max = 100,
                            value = 4,
                            step = 1,
                            width = '400px'),
               numericInput(inputId = "Dado3Num",
                            label = "How many dice of this type?",
                            min = 0,
                            max = 100,
                            value = 0,
                            step = 1,
                            width = '400px')
               ),
        column(3,
               tags$h2("Modifier"),
               numericInput(inputId = "modifier",
                            label = "Enter the modifier (if any)",
                            min = -100,
                            max = 100,
                            value = 0,
                            step = 1,
                            width = '400px'),
               hr(),
               tags$h2("Minimum target score"),
               numericInput(inputId = "Need",
                            label = "Total minimum expected score?",
                            min = 1,
                            max = 100,
                            value = 10,
                            step = 1,
                            width = '400px'),
               hr(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               tags$h4("Number of simulations"),
               numericInput(inputId = "reps",
                            label = "No need to change it",
                            min = 1,
                            max = 10000000,
                            value = 500000,
                            step = 1,
                            width = '400px')
               ),
        column(5,
               tags$h1("Distribution of scores"),
               plotOutput("probPlot"),
               tags$p(HTML("<b style=color:#ce0000ff;>NOTE: </b>"),
                      " For the histogram of the simulation to look right,
                      the", HTML("<b> minimum target score</b>"), "must be within the
                      range of the sum of the dice rolled and the modifier.
                      For example, when rolling", HTML("<b> 1d20 + 3</b>"), "the result can only
                      be between 4 and 23. If you select a", HTML("<b> minimum target score</b>"),
                      "smaller than 4 or greater than 23, the histogram will look
                      strange, although the", HTML("<b> probability</b>"), "(shown below) will be correct"),
               hr(),
               tags$h1("Probability"),
               htmlOutput("probText"),
               htmlOutput("probPerc")))
  )


# Define server logic required to draw a histogram
server <- function(input, output, session){
    #Simulaciones
    datPre <- reactive({
      dats <- data.frame(
        d1 = rowSums(replicate(input$Dado1Num, sample(1:input$Dado1Max, input$reps, replace=T))),
        d2 = rep(0, times = input$reps),
        d3 = rep(0, times = input$reps))
      return(dats)
      })
    data <- reactive({
      datos <- datPre()
      datos$d2 <- if(input$Dado2Num != 0){
        datos$d2 = rowSums(replicate(input$Dado2Num, sample(1:input$Dado2Max, input$reps, replace=T)))
      }
      datos$d3 <- if(input$Dado3Num != 0){
        datos$d3 = rowSums(replicate(input$Dado3Num, sample(1:input$Dado3Max, input$reps, replace=T)))
      }
      return(datos)
    })
    probPre <- reactive({
      dat <- data()
      conmod <- rowSums(dat) + input$modifier
      return(conmod)
    })
    #Calcular probabilidad por encima del número deseado
    probAbovePre <- reactive({
      prob <- probPre()
      percent(sum(prob >= input$Need)/input$reps)})

    #Crear histograma
    output$probPlot <- renderPlot({
      prob <- probPre()
      probAbove <- probAbovePre()
      #Histograma de valores
      ggplot() +
        annotate("rect", xmin = input$Need, xmax =  Inf, ymin = 0, ymax = Inf,
                 alpha = 0.4, fill = "white") +
        geom_histogram(aes(prob, y = ..density.., fill = ..density..), bins = max(prob)-min(prob)+1, color = "black", alpha = 0.5) +
        #geom_density(aes(prob, y = ..density..), size = 0.5, adjust = 4) +
        scale_fill_gradient(low = "midnightblue", high = "red") +
        scale_x_continuous(breaks = seq(min(prob), max(prob), by = 1)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        geom_vline(xintercept = input$Need, size = 2, alpha = 0.2) +
        annotate(geom = "text",
                 x = input$Need, y = -Inf, vjust = 0.5, hjust = -0.1, angle=90,
                 size = 5,
                 label = paste0("Minimum target score", " = ", input$Need)) +
        annotate(geom = "text",
                 x = Inf, y = Inf, vjust = 2, hjust = 1.1,
                 size = 7,
                 color = "white",
                 label = probAbove) +
        annotate(geom = "text",
                 x = input$Need, y = sum(prob == input$Need)/input$reps, vjust = -0.5, hjust = 0.5,
                 size = 5,
                 color = "white",
                 label = percent(sum(prob == input$Need)/input$reps)) +
        theme(legend.position = "none") +
        labs(x = "Sum of dice and modifier", y = "Probability", fill = "Probability")
        })
    #Crear texto con resultados detallados
    output$probText <- renderText({
      prob <- probPre()
      #Mostrar probabilidad de obtener número igual o mayor a
      paste("When rolling <font color=\'#FF0000\'><b>",input$Dado1Num,"d", input$Dado1Max,
            ifelse(input$Dado2Num == 0, "", paste(" + ", input$Dado2Num,"d",input$Dado2Max)),
            ifelse(input$Dado3Num == 0, "", paste(" + ", input$Dado3Num,"d",input$Dado3Max)),
            ifelse(input$modifier == 0, "", paste(" + ", input$modifier)),
            "</b></font>, you can get total scores between <font color=\'#FF0000\'><b>",
            min(prob), "</b></font> and <font color=\'#FF0000\'><b>", max(prob),
            "</b></font>. The probability of obtaining a total score of exactly <font color=\'#FF0000\'><b>",
            input$Need, "</b></font>is approximately <font color=\'#FF0000\'><b>", percent(sum(prob == input$Need)/input$reps),
            "</b></font>, while the probability of obtaining a total score of <font color=\'#FF0000\'><b>",
            input$Need, "or more</b></font>, is approximately:")
      })
    output$probPerc <- renderText({
      probAbove <- probAbovePre()
      paste("<font size='20' color='red'>",
        probAbove,
        "</font>"
      )
    })
}

thematic_shiny()
# Run the application
shinyApp(ui = ui, server = server)

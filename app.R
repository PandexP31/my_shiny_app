library(shiny)
library(dplyr)
library(ggplot2)
library(glue)
library(DT)
library(bslib)
library(thematic)

thematic_shiny(font="auto")

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "vapor"), 
  titlePanel("My First Shiny App"),
  sidebarLayout(
    sidebarPanel(
      h1("Star Wars Characters"),
      h2("My app from scratch"),
      sliderInput(
        inputId = "taille",
        label = "Height of Characters",
        min = 0,
        max = 250,
        value = 30
      ), 
      selectInput(
        inputId = "gender",
        label = "Choisir le genre des personnages",
        choices = c("masculine", "feminine")),
      actionButton(
        inputId = "boutton",
        label = "Cliquez moi"
      )
    ),

    
    mainPanel(
      textOutput("StarWarsTitle"),
      plotOutput("StarWarsPlot"),
      DTOutput("StarWarsTable")
    )
  )
)

server <- function(input, output) {
  
  rv <- reactiveValues()
  
  observeEvent(c(input$taille, input$gender),{
    rv$starwars_filtrer <- starwars |>
      filter(height > input$taille) |>
      filter(gender == input$gender)
  })
  
  output$StarWarsPlot <- renderPlot({
    rv$starwars_filtrer |>
      ggplot(aes(x = height))+
      geom_histogram(binwidth = 10,
                      fill ="white",
                      color ="black") +
      labs(title = glue("Vous avez sélectionné le genre : {input$gender}"))
  })
  
  output$StarWarsTitle <- renderText({
    nb_lignes <- starwars |>
      filter(height > input$taille) |>
      filter(gender == input$gender) |>
      nrow()
    
    glue("Nb de lignes selectionnés : {nb_lignes}")
  })
  
  output$StarWarsTable <- renderDT({
    starwars |> 
      filter(height > input$taille) |>
      filter(gender == input$gender)
  })
  
  observeEvent(input$boutton, {
    message("vous avez cliqué sur un boutton")
  })
  
  observeEvent(input$taille, {
    showNotification(
      glue("La taille recherchée est maintenant de {input$taille} cm"),
      type = "message"
    )
  })
}

shinyApp(ui = ui, server = server)

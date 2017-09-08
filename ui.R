library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme=shinytheme("darkly"),
  fluidRow(
    column(12,
           titlePanel("Next Word Predictor"),
           h2(" ")),
    fluidRow(
      column(4,
            verbatimTextOutput("instructions"),
            tags$head(tags$style("#instructions{color:grey;
                                                font-size: 12px;
                                                font-style: italic;
                                                font-family: arial
                                                }"))),
      column(8,
            fluidRow(
              column(12,
                     textInput("text", "Insert Text"),
                     submitButton("submit"),
                     h2(" "))),
            fluidRow(
              column(12,
                     h5("Expected Next Word"),
                     verbatimTextOutput("nextwords")))
            )))
))
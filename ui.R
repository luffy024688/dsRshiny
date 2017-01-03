
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("readdata.R")

shinyUI(fluidPage(

  # Application title
  titlePanel("臉書配對系統"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("name",
                  "臉書用戶名稱:",
                  value="請輸入")
    ),

    # Show a plot of the generated distribution
    mainPanel( 
      h3("文字雲:"),
      plotOutput("wordcloud"),
       h3("人格說明:"),
       tableOutput("type"),
       h3("配對說明:"),
       tableOutput("match")
       
    )
  )
))

library(shiny)
shinyUI(
  pageWithSidebar(
    headerPanel("This is the selection of Steins;Gate.")
    ,
    sidebarPanel(
      textInput('x', "Xの値を入力","")
      ,
      textInput('y', "Yの値を入力","")
      ,
      actionButton("submit", "実行")
    )
    ,
    mainPanel(uiOutput('table'))
  ))
library(plotly)
shinyUI(fluidPage(
  titlePanel("title panel"),
  sidebarLayout( position = "right",
    sidebarPanel( "",
      fileInput("datainput", label = "Input your data:"),
      selectInput("filetype", label = "Select file type:", choices  = c("Comma","Tabs"), selected = "CSV"),
      checkboxGroupInput("checkboxes",label = "", choices  = c("Contains Header") ,selected = FALSE),
      uiOutput("data"),
      uiOutput("data2"),
      uiOutput("data3"),
      actionButton("submit" ,"Submit",class = "btn btn-primary")),
  mainPanel(
    plotOutput("plot1", click = "plot_click")
  )
  )
))
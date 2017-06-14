library(plotly)
shinyUI(fluidPage(
  titlePanel("title panel"),
  sidebarLayout( position = "right",
    sidebarPanel( "",
      fileInput("datainput", label = "Input your data:"),
      selectInput("filetype", label = "Select file type:", choices  = c("Comma","Tabs"), selected = "Tabs"),
      checkboxGroupInput("checkboxes",label = "", choices  = c("Contains Header") ,selected = TRUE),
      actionButton("select" ,"Upload File",class = "btn btn-primary"),
      uiOutput("data")
      ),
  mainPanel(
  plotlyOutput("plot1")
  )
  )
))
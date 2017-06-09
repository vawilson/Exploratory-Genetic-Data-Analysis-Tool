library(plotly)
shinyUI(fluidPage(
  titlePanel("title panel"),
  sidebarLayout( position = "right",
      sidebarPanel( "",
          fileInput("datainput", label = "Input your data:"),
          selectInput("filetype", label = "Select file type:", choices  = c("Comma","Tabs"), selected = "CSV"),
          checkboxGroupInput("checkboxes",label = "", choices  = c("Contains Header") ,selected = FALSE),
          selectInput("statusrows", label = "Use rows:", choices  = c("Including","Excluding"), selected = "Including"),
          selectInput("statuscolumns", label = "Use columns:", choices  = c("Including","Excluding"), selected = "Including"),
          actionButton("submit" ,"Submit",class = "btn btn-primary")),
  mainPanel(
    #tableOutput("data"),
    #plotlyOutput("plot")
    plotOutput("plot1", click = "plot_click")
    
  )
)
))
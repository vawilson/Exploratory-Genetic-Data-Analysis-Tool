library(plotly)
shinyUI(fluidPage(
  
  
  sidebarLayout( position = "right",
    sidebarPanel( "",
      fileInput("datainput", label = "Input your data:"),
      selectInput("filetype", label = "Select file type:", choices  = c("Comma","Tabs"), selected = "Tabs"),
      checkboxGroupInput("checkboxes",label = "", choices  = c("Contains Header") ,selected = TRUE),
      actionButton("select" ,"Upload File",class = "btn btn-primary"),
      uiOutput("data")
      ),
    
  mainPanel(
    fluidRow(
      column(1,offset = 3,uiOutput("donorbutton")),
      column(1,uiOutput("stimulusbutton")),
      column(1,uiOutput("timebutton"))
      
      
    ),
   
    tabsetPanel( 
      tabPanel("PCA",plotlyOutput("plot1")),
      tabPanel("Scree Plot",plotlyOutput("plot2"))
    )
  
  )
  )
))
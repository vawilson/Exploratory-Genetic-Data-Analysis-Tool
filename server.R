#Notes
# make it so table is only reparsed when its value changes
library(factoextra)
#changes maximum upload size to 30MB
options(shiny.maxRequestSize=30*1024^2) 
shinyServer(function(input, output,session) {
  # creates a dynamic variable for the excel file's data which is reparsed when submit is pressed, it is then used in following calculations 
  path <<- "./data/Labex_modified.txt"
  filea<<-read.csv(path,header=TRUE,sep="\t")
  observeEvent(input$select, {
  isolate({ 
  path <<- input$datainput
  filea <<- parseFile(input$datainput,input$filetype,headerstate = input$checkboxes)
  })
  })
  
  #parse data file into list of lists depending on deliminator, strings are NOT converted to ints
  parseFile <- function(datafile,delim,headerstate){
    #cannot directly access a file, must first find it's file path
    file<-datafile$datapath
    #parse a tab seperated file
    if(delim=="Tabs") {
      return(read.csv(file,header = !(is.null(headerstate[1])),sep = "\t", stringsAsFactors=FALSE))
    }
    # parse a comma seperated file
    if(delim == "Comma") {
      return(read.csv(file,header = !(is.null(headerstate[1])), stringsAsFactors=FALSE))
    }
    #output error message if delimanator type not accepted
    else{
      textOutput("Error: Deliminator type not supported.")
    }
  }
  #when submit button is pressed and data is loaded, more options appear
   output$data <- renderUI({ 
      input$select
      tagList(
        isolate(
          selectizeInput("donor",label = "Select Donor:", choices = unique(filea[1]), selected = as.character(unique(filea[1])$Donor),
          multiple = TRUE, options = list()
          )
        ),
        isolate(
          selectizeInput("stimulus", label = "Select Stimulus:", choices  = unique(filea[3]), selected = as.character(unique(filea[3])$StimulusName),
          multiple = TRUE, options = list()
          )
        ),
        isolate(
          selectizeInput("timepoint", label = "Select Timepoint:", choices  = unique(filea[5]), selected = "22",
          multiple = TRUE, options = list()
          )
        ),
      isolate(
        selectInput("dimension",label = "Select Dimension to View:", choices = c("2D","3D"))
        ),
      isolate(
        actionButton("submit" ,"Submit",class = "btn btn-primary")
      )
      )
   })
   pcaPlot <- function(don,stim,tim){
     subsetgenes <- subset(filea,Donor %in% don & StimulusName %in% stim & Timepoint %in% tim)[,6:592]
     pca <- prcomp(subsetgenes, center = TRUE, scale = TRUE)
     return(pca$x)
   }
   
  output$plot1 <- renderPlotly({
  input$submit
    isolate({set<- pcaPlot(input$donor, input$stimulus, input$timepoint)
     if(input$dimension == "2D"){
       isolate(plot_ly(x=set[,1],y=set[,2],type = "scatter"))
     }
    else if(input$dimension=="3D"){
       isolate(plot_ly(x=set[,1],y=set[,2],z=set[,3], type = "scatter3d"))
    }
    })
  
  
  
  })
})
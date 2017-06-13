#Notes
# make it so table is only reparsed when its value changes

#changes maximum upload size to 30MB
options(shiny.maxRequestSize=30*1024^2) 
shinyServer(function(input, output,session) {
  # creates a dynamic variable for the excel file's data which is reparsed when submit is pressed, it is then used in following calculations 
  observeEvent(input$submit, {
  path <<- input$datainput
  filea <<- parseFile(input$datainput,input$filetype,headerstate = input$checkboxes)
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
      input$submit
      tagList(
        isolate(
          selectizeInput("donor",label = "Select Donor:", choices = unique(filea[1]), selected = as.character(unique(filea[1])$Donor),
          multiple = TRUE, options = list()
          )
        ),
        isolate(
          selectizeInput("stimulus", label = "Select Stimulus:", choices  = unique(filea[5]), selected = as.character(unique(filea[5])$StimulusName),
          multiple = TRUE, options = list()
          )
        ),
        isolate(
          selectizeInput("timepoint", label = "Select Timepoint:", choices  = unique(filea[7]), selected = "22",
          multiple = TRUE, options = list()
          )
        ),
      # isolate(
      #   selectizeInput("gene", label = "Select Genes:", choices  = unique(names(filea[58:644])),
      #   multiple = TRUE, options = list()
      #   )
      # ),
      isolate(
        selectInput("dimension",label = "Select Dimension to View:", choices = c("2D","3D"))
        )
      )
   })
   pcaPlot <- function(don,stim,tim){
     subsetgenes <- subset(filea,Donor %in% don & StimulusName %in% stim & Timepoint %in% tim)[,58:644]
     pca <- prcomp(t(subsetgenes), center = TRUE, scale. = TRUE)
    
     
     
   }
  output$plot1 <- renderPlotly({
  input$submit
    isolate(set<- pcaPlot(input$donor, input$stimulus, input$timepoint))
    print(set[,1])
    #print(set[,2])
    # if(input$dimension == "2D"){
    #   plot_ly(set[,1],set[,2])
    # }
    # if(dim == "3D"){
    #   plot_ly(set[,1],set[,2],set[,3])
    # }
  
  
  
  })
})
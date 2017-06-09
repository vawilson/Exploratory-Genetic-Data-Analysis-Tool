#Notes
# make it so table is only reparsed when its value changes

#changes maximum upload size to 30MB
options(shiny.maxRequestSize=30*1024^2) 
shinyServer(function(input, output,session) {
  # creates a dynamic variable for the excel file's data which is reparsed when submit is pressed, it is then used in following calculations 
  observeEvent(input$submit, {
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
        isolate(selectInput("donors", label = "Select Donor:", choices  = unique(filea[1]), selected = "Including")),
        isolate(selectInput("stimulus", label = "Select Stimulus:", choices  = unique(filea[5]), selected = "Including")),
        isolate(selectInput("timepoint", label = "Select Timepoint:", choices  = unique(filea[7]), selected = "Including"))
      )
   })
  output$plot1 <- renderPlot({ 
    input$submit
    isolate(
    plot(filea[c(1,50),1],filea[c(1,50),2])
    )
  })
})
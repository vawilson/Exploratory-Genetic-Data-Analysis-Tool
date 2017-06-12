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
          selectizeInput("donor",label = "Select Donor:", choices = unique(filea[1]),
          multiple = TRUE, options = list()
          )
        ),
        isolate(
          selectizeInput("stimulus", label = "Select Stimulus:", choices  = unique(filea[5]),
          multiple = TRUE, options = list()
          )
        ),
        isolate(
          selectizeInput("timepoint", label = "Select Timepoint:", choices  = unique(filea[7]),
          multiple = TRUE, options = list()
          )
        ),
      isolate(
        selectizeInput("genes", label = "Select Genes:", choices  = unique(names(filea[58:644])),
        multiple = TRUE, options = list()
        )
      ),
      isolate(
        selectInput("dimension",label = "Select Dimension to View:", choices = c("2D","3D"))
        )
      )
   })
  output$plot1 <- renderPlot({ 
    input$submit
    #when donor, stimuls, and time point are selected and submit is hit, the data is extracted from the spreadsheet
    isolate(genes <- subset(filea,Donor == input$donors & StimulusName == input$stimulus & Timepoint == input$timepoint))
   # isolate(print((genes[c(58,60)])))
    isolate(plot(t(genes[58:68]),xaxt="n"))
    isolate(axis(1,at=1:11,labels=names(genes[c(58:68)])))
  })
})
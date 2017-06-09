#Notes
# Submit button and on-reactive elements until it is pressed so redundant calculations are not computed
#

#changes maximum upload size to 30MB
options(shiny.maxRequestSize=30*1024^2) 
shinyServer(function(input, output) {
  
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
  
  #observeEvent(input$submit, {
  #output a table to check if data is correctly being parsed
  # output$data <- renderTable({
  #   input$submit
  #   isolate(
  #       paste(parseFile(input$datainput,input$filetype,headerstate = input$checkboxes)[1,])
  #   )
  # })
  
  #plots go here for output
  # output$plot <- renderPlotly({
  #   input$submit
  #   isolate()
  # })
  output$plot1 <- renderPlot({
    filea <- parseFile(input$datainput,input$filetype,headerstate = input$checkboxes)
    isolate(
    plot(filea[c(1,50),1],filea[c(1,50),2])
    )

  })
})
#Notes
# make it so table is only reparsed when its value changes
library(factoextra)
#changes maximum upload size to 30MB
options(shiny.maxRequestSize=30*1024^2) 
shinyServer(
  
  function(input, output,session) {
  #sets a default data file for the app and displays its plot
  defaultPlot <- function(){
    path <<- "./data/Labex_modified.txt"
    filea<<-read.csv(path,header=TRUE,sep="\t")
    subsetfilea2 <- subset(filea,Timepoint ==22)
    subsetgenes2 <- subsetfilea2[,6:592]
    pca2 <- (prcomp(subsetgenes2, center = TRUE, scale = TRUE))$x
    pca4<-cbind(pca2,subsetfilea2[,c(1,3,5)])
    p<-plot_ly(x=pca4[,1],y=pca4[,2],type = "scatter", name = "All Data",text=paste(" D:",pca4[,588]," S:",pca4[,589]," T:",pca4[,590]), hoverinfo="text", width=900,height=800) 
    db <- actionButton("donorbutton" ,"Donors",class = "btn btn-primary")
    sb <- actionButton("stimulusbutton" ,"Stimuli",class = "btn btn-primary")
    tb <- actionButton("timebutton" ,"Time Points",class = "btn btn-primary")
    output$plot1<-renderPlotly({p})
    output$donorbutton <-renderUI({db})
    output$stimulusbutton <-renderUI({sb})
    output$timebutton <-renderUI({tb})
  }
  defaultPlot()
  
  
  # updates dynamic variables for the excel file's data which is reparsed when submit is pressed, it is then used in following calculations 
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
     subsetfilea <<- subset(filea,Donor %in% don & StimulusName %in% stim & Timepoint %in% tim)
     subsetgenes <- subsetfilea[,6:592]
     pca <- (prcomp(subsetgenes, center = TRUE, scale. = TRUE))$x
     pca3<-cbind(pca,subsetfilea[,c(1,3,5)])
     return(pca3)
   }
   #sets up plot with donor traces
   setupPlotDon <- function(don,stim,tim,dim){
     c <- distinctColorPalette(length(don))
     
     if(dim == "2D"){
       p<-plot_ly(x=set[,1],y=set[,2],type = "scatter",name = "All Data", width=900,height=800) 
     for(i in 1:length(don)){
        p<-add_trace(p,x=subset(set,Donor==don[i])[,1],y=subset(set,Donor==don[i])[,2],marker = list(color = c[i]),name = don[i])
        
     }
     }
     else {
       p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d",name = "All Data",width=900,height=800) 
       for(i in 1:length(don)){
         p<-add_trace(p,x=subset(set,Donor==don[i])[,1],y=subset(set,Donor==don[i])[,2],z=subset(set,Donor==don[i])[,3],marker = list(color = c[i]),name = don[i])
       }
     }
     return(p)
   }
   # Sets up plot with stimulus traces
   setupPlotStim <- function(don,stim,tim,dim){
     c <- distinctColorPalette(length(stim))
  
     
     if(dim == "2D"){
       #p<-plot_ly(x=set[,1],y=set[,2],type = "scatter", name = "All Data",text=paste(" D:",set[,588]," S:",set[,589]," T:",set[,590]), hoverinfo="text", width=900,height=800) 
       
       p<-plot_ly(x=set[,1],y=set[,2],type = "scatter", width=900,height=800) 
 
       for(i in 1:length(stim)){
         #validate(need(set.isNull(), paste("set",dim(set),"len",length(set[,588]))))
         
         p<-add_trace(p,x=subset(set,StimulusName==stim[i])[,1],y=subset(set,StimulusName==stim[i])[,2],marker = list(color = c[i]), name = stim[i],evaulate = TRUE)

       }
     }
     else {
       p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d",name = "All Data",width=900,height=800) 
       for(i in 1:length(stim)){
         print(length(stim))
         p<-add_trace(p,x=subset(set,StimulusName==stim[i])[,1],y=subset(set,StimulusName==stim[i])[,2],z=subset(set,StimulusName==stim[i])[,3],marker = list(color = c[i]), name = stim[i])
         
       }
     }
     return(p)
   }
   setupPlotTime <- function(don,stim,tim,dim){
     c <- distinctColorPalette(length(tim))
     if(dim == "2D"){
       print(length(set[,588]))
       print(length(set[,1]))
       p<-plot_ly(x=set[,1],y=set[,2],type = "scatter",name = "All Data", name = "All Data",width=900,height=800) 
       for(i in 1:length(tim)){
         p<-add_trace(p,x=subset(set,Timepoint==tim[i])[,1],y=subset(set,Timepoint==tim[i])[,2],marker = list(color = c[i]), name = tim[i])
       }
     }
     else {
      p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d",name = "All Data",width=900,height=800) 
       for(i in 1:length(tim)){
         p<-add_trace(p,x=subset(set,Timepoint==tim[i])[,1],y=subset(set,Timepoint==tim[i])[,2],z=subset(set,Timepoint==tim[i])[,3],marker = list(color = c[i]), name = tim[i])
       }
     }
     return(p)
   }
   #Sets up default plot w/o traces
   setupPlot <- function(don,stim,tim,dim){
     set<<-pcaPlot(don,stim,tim)
     if(dim == "2D"){
       p<-plot_ly(x=set[,1],y=set[,2],type = "scatter",name = "All Data",text=paste(" D:",set[,588]," S:",set[,589]," T:",set[,590]),hoverinfo="text",width=900,height=800) 
       
     }
     else {
       
       p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d",name = "All Data",text=paste(" D:",set[,588]," S:",set[,589]," T:",set[,590]),hoverinfo="text",width=900,height=800) 
     }
     return(p)
   }
   
   observeEvent(input$donorbutton, {
     k<-setupPlotDon(input$donor, input$stimulus, input$timepoint,input$dimension)
     output$plot1<-renderPlotly({k})
   })
   observeEvent(input$stimulusbutton, {
     k<-setupPlotStim(input$donor, input$stimulus, input$timepoint,input$dimension)
     output$plot1<-renderPlotly({k})
   })
   observeEvent(input$timebutton, {
     k<-setupPlotTime(input$donor, input$stimulus, input$timepoint,input$dimension)
     output$plot1<-renderPlotly({k})
   })
   
   
   observeEvent(input$submit, {
     k<-setupPlot(input$donor, input$stimulus, input$timepoint,input$dimension)
    output$plot1<-renderPlotly({k})
   })
})
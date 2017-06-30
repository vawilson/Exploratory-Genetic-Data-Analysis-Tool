#' @title  Notes
#' 
#' @description need to add hover text describing a point's donor, stimuli, and timepoint, one method which works only for the defualt values is :
#' p<-add_trace(p, opacity = 0,showlegend = F, hoverinfo = "text",text = paste("D:",set[,588],"S:",set[,589],"T:",set[,590]))
#' this however, throws an error if the values are changed
library(factoextra)
library(randomcoloR)
library(roxygen2)
library(Rtsne)
#changes maximum upload size to 30MB
options(shiny.maxRequestSize=30*1024^2,warn = -1) 
#start server
shinyServer(

#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
  function(input, output,session) {
    
    #' @title  Scree Plot Function
    #' 
    #' @param pcaraw the output of prcompt function run on a dataset without modification
    #' 
    #' @description This function uses pca data to generate a scree plot for the variance
    #'  and cumulative variance captured with each dimension.
    #'  
    #' @return s a scree plot generated using the plotly library
    
    screePlot <- function(pcaraw){
      prop <- pcaraw$sdev^2 / sum(pcaraw$sdev^2)
      s <- plot_ly(y = prop[1:100],width = 900, height = 700,
                   type = 'bar',
                   name = "proportion of variance") %>%
        layout(title = "")
      s <-add_trace(s,
                    y = cumsum(prop)[1:100],
                    type = 'scatter', mode = 'lines+markers',
                    marker = list(symbol="circle"),
                    name = "cumulative variance")
      return(s)
    }  
    #' @title Default Plot Function
    #'
    #' @description Sets up the example plot pca and scree for the application along with buttons.
    defaultPlot <- function(){
      path <<- "./data/Labex_modified.txt"
      filea<<-read.csv(path,header=TRUE,sep="\t")
      subsetfilea2 <- subset(filea,Timepoint ==22)
      subsetgenes2 <- subsetfilea2[,6:592]
      pcaraw2 <-prcomp(subsetgenes2, center = TRUE, scale = TRUE)
      pca2 <- pcaraw2$x
      pca4<-cbind(pca2,subsetfilea2[,c(1,3,5)])
      ds <-screePlot(pcaraw2)
      p<-plot_ly(x=pca4[,1],y=pca4[,2],type = "scatter",mode = "markers",marker = list(symbol = "circles"), name = "All Data",text=paste(" D:",pca4[,588]," S:",pca4[,589]," T:",pca4[,590]), hoverinfo="text",width = 900, height = 700) 
      output$plot1<-renderPlotly({p})
      output$plot2 <- renderPlotly({ds})
    }
    defaultPlot()
    
  
    # updates dynamic variables for the excel file's data which is reparsed when submit is pressed, it is then used in following calculations 
    observeEvent(input$upload, {
      isolate({ 
        path <<- input$datainput
        filea <<- parseFile(input$datainput,input$filetype,headerstate = input$checkboxes)
      })
    })
    
    #
#' Parse File Function
#' 
#' @param datafile the name of the file to be parsed
#' @param delim the deliminator the be used when parsing the file
#' @param headerstate boolean whether there is a header or not
#'
#' @description Parses data file into list of lists depending on deliminator, strings are NOT converted to ints
#' 
#' @return the parsed file
#' 
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
      input$upload
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
          actionButton("select" ,"Select",class = "btn btn-primary")
        )
        
       
      )
    })
    output$data2 <- renderUI({ 
      input$select
      tagList(
        isolate(
          checkboxInput("pca", label = "PCA", value = TRUE)
        ),
        isolate(
          checkboxInput("tsne", label = "t-SNE", value = TRUE)
        ),
        isolate(
          selectInput("dimension",label = "Select Dimension to View:", choices = c("2D","3D"))
        ),
        isolate(
          actionButton("submit" ,"Submit",class = "btn btn-primary")
        )
        
      )
    })
    
    #' PCA Plot Function
    #'
    #' @param don the selected donors to have pca done on (vector)
    #' @param stim the selected stimuli to have pca done on
    #' @param tim time selected timepoints to have pca done on
    #'
    #' @return a datatable of the pca dimensions for each set of donor, simulus, and timepoints
    #'
   subsetData <- function(don,stim,tim){
     subsetfilea <<- subset(filea,Donor %in% don & StimulusName %in% stim & Timepoint %in% tim)
     subsetgenes <<- subsetfilea[,6:592]
   }
    pcaPlot <- function(c,s){
      pcaorig <<- (prcomp(subsetgenes, center = c, scale. = s))
      pca <- pcaorig$x
      pca3<-cbind(pca,subsetfilea[,c(1,3,5)])
      return(pca3)
    }
    tsnePlot <- function(pc,pe,it,lr){
      tsneplot <- Rtsne(subsetgenes,verbose = TRUE, max_iter = it,pca = pc,perplexity = pe, dims =3, eta = lr)
      output$error <- renderText(paste("Cost: ",tail(tsneplot$itercosts,n=1)))
     tsne2<-cbind(tsneplot$Y,subsetfilea[,c(1,3,5)])
     
      return(tsne2)
    }
    tsnePlotM <- function(pc,pe,it,c,s,pdim,lr){
      tsneplot <- Rtsne(subsetgenes,verbose = TRUE, max_iter = it,pca = pc,perplexity = pe, dims =3,pca_center = c, pca_scale = s, initial_dims = pdim,eta = lr)
      output$error <- renderText(paste("Cost: ",tail(tsneplot$itercosts,n=1)))
      tsne2<-cbind(tsneplot$Y,subsetfilea[,c(1,3,5)])
      return(tsne2)
    }
    #' Setup Plot Donor Function
    #'
    #' @param don the selected donors to be plotted (vector)
    #' @param stim the selected stimuli to be plotted
    #' @param tim time selected timepoints to be plotted
    #' @param dim the dimension to be plotted in
    #'
    #' @return a plot of the pca with traces for each donor

    setupPlotDon <- function(don,stim,tim,dim){
      c <- distinctColorPalette(length(don))
      
      if(dim == "2D"){
        p<-plot_ly(x=set[,1],y=set[,2],type = "scatter",mode = "markers",marker = list(symbol = "circles"),name = "All Data",width = 900, height = 700) 
        
        for(i in 1:length(don)){
          p<-add_trace(p,type = "scatter",x=subset(set,Donor==don[i])[,1],y=subset(set,Donor==don[i])[,2],marker = list(color = c[i]),name = don[i])
          
        }
      }
      else {
        p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d",mode = "markers",marker = list(symbol = "circles"),name = "All Data",width = 900, height = 700) 
        for(i in 1:length(don)){
          p<-add_trace(p,x=subset(set,Donor==don[i])[,1],y=subset(set,Donor==don[i])[,2],z=subset(set,Donor==don[i])[,3],marker = list(color = c[i]),name = don[i])
        }
        # p<-add_trace(p, opacity = 0,hoverinfo = "text",showlegend = F,text = paste("D:",set[,588],"S:",set[,589],"T:",set[,590]))
      }
      return(p)
    }

    #' Setup Plot Stimulus Function
    #'
    #' @param don the selected donors to be plotted (vector)
    #' @param stim the selected stimuli to be plotted
    #' @param tim time selected timepoints to be plotted
    #' @param dim the dimension to be plotted in
    #'
    #' @return a plot of the pca with traces for each stimulus

    setupPlotStim <- function(don,stim,tim,dim){
      c <- distinctColorPalette(length(stim))
      
      
      if(dim == "2D"){
        p<-plot_ly(x=set[,1],y=set[,2],type = "scatter",mode = "markers",marker = list(symbol = "circles"),width = 900, height = 700) 
        
        for(i in 1:length(stim)){
          
          p<-add_trace(p,x=subset(set,StimulusName==stim[i])[,1],y=subset(set,StimulusName==stim[i])[,2],marker = list(color = c[i]), name = stim[i],evaulate = TRUE)
        }
      }
      else {
        p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d",mode = "markers",marker = list(symbol = "circles"),name = "All Data",width = 900, height = 700) 
        for(i in 1:length(stim)){
          
          p<-add_trace(p,x=subset(set,StimulusName==stim[i])[,1],y=subset(set,StimulusName==stim[i])[,2],z=subset(set,StimulusName==stim[i])[,3],marker = list(color = c[i]), name = stim[i])
          
        }
      }
      return(p)
    }
    #' Setup Plot Timepoint Function
    #'
    #' @param don the selected donors to be plotted (vector)
    #' @param stim the selected stimuli to be plotted
    #' @param tim time selected timepoints to be plotted
    #' @param dim the dimension to be plotted in
    #'
    #' @return a plot of the pca with traces for each timepoint

    setupPlotTime <- function(don,stim,tim,dim){
      c <- distinctColorPalette(length(tim))
      if(dim == "2D"){
        p<-plot_ly(x=set[,1],y=set[,2],type = "scatter",mode = "markers",marker = list(symbol = "circles"),name = "All Data", name = "All Data",width = 900, height = 700) 
        for(i in 1:length(tim)){
          p<-add_trace(p,x=subset(set,Timepoint==tim[i])[,1],y=subset(set,Timepoint==tim[i])[,2],marker = list(color = c[i]), name = tim[i])
        }
      }
      else {
        p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d", mode = "markers",marker = list(symbol = "circles"),text ="",name = "All Data",width = 900, height = 700) 
        
        for(i in 1:length(tim)){
          p<-add_trace(p,x=subset(set,Timepoint==tim[i])[,1],y=subset(set,Timepoint==tim[i])[,2],z=subset(set,Timepoint==tim[i])[,3], marker = list(color = c[i]), name = tim[i])
        }
      }
      return(p)
    }

    #' Setup Plot Function
    #'
    #' @param don the selected donors to be plotted (vector)
    #' @param stim the selected stimuli to be plotted
    #' @param tim time selected timepoints to be plotted
    #' @param dim the dimension to be plotted in
    #'
    #' @return a plot of the pca without any traces

    setupPlot <- function(dim,plot){
      set<<-plot
      
      if(dim == "2D"){
        p<-plot_ly(x=set[,1],y=set[,2],type = "scatter",mode = "markers",marker = list(symbol = "circles"),name = "All Data",width = 900, height = 700) 
      }
      else {
        p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d",mode = "markers",marker = list(symbol = "circles"),name = "All Data",width = 900, height = 700) 
      }
      return(p)
    }
    # reactive code for buttons in the application, when they are submitted each button deploys its own specific functions
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
    
    observeEvent(input$select, {
      subsetData(input$donor, input$stimulus, input$timepoint)
    })
    observeEvent(input$submit, {
      if(input$pca & !input$tsne){
        output$param <- renderUI({
          tagList(
          checkboxInput("scale", label = "Scale", value = TRUE),
          checkboxInput("center", label = "Center", value = TRUE),
          actionButton("go" ,"Go!",class = "btn btn-primary")
          )
        })
      } 
      else if(input$tsne & !input$pca){
        output$param <- renderUI({
          tagList(
            sliderInput("perplexity", label = "Perplexity", min = 5, max = 100, value = 50),
            sliderInput("iterations", label = "Iterations", min = 1, max = 20000, value = 50),
            sliderInput("lr", label = "t-SNE:Learning Rate", min = 100, max = 1000, value = 200), 
            actionButton("go2" ,"Go!",class = "btn btn-primary")
    )
        })
        
      }
      else if(input$tsne & input$pca){
        output$param <- renderUI({
          tagList(
            sliderInput("pcadims", label = "PCA:Dimensions used", min = 1, max = 100, value = 50),
            checkboxInput("scale", label = "PCA:Scale", value = TRUE),
            checkboxInput("center", label = "PCA:Center", value = TRUE),
            sliderInput("perplexity", label = "t-SNE:Perplexity", min = 5, max = 100, value = 50),
            sliderInput("iterations", label = "t-SNE:Iterations", min = 1, max = 20000, value = 50),  
            sliderInput("lr", label = "t-SNE:Learning Rate", min = 100, max = 1000, value = 200), 
          actionButton("go3" ,"Go!",class = "btn btn-primary")
          )
        })
      }
      else{
        textOutput("Error: No algorithm selected.")
      }
      
      
    })
    observeEvent(input$go, {
      tsneout<<-pcaPlot(input$center,input$scale)
      k<-setupPlot(input$dimension,tsneout)
      output$plot2 <-renderPlotly({screePlot(pcaorig)})
      output$plot1<-renderPlotly({k})
    })
    observeEvent(input$go2, {
      tsneout<<- tsnePlot(FALSE,input$perplexity,input$iterations,input$lr)
      k<-setupPlot(input$dimension,tsneout)
      output$plot1<-renderPlotly({k})
      
    })
    observeEvent(input$go3, {
      tsneout <<- tsnePlotM(TRUE,input$perplexity,input$iterations,input$center,input$scale,input$pcadims,input$lr)
      k<-setupPlot(input$dimension,tsneout)
      output$plot1<-renderPlotly({k})
      
    })
    observeEvent(list(input$go,input$go2,input$go3),{
      output$kmeansbutton <- renderUI({
        actionButton("km" ,"K-means",class = "btn btn-primary")
      })
    })
    observeEvent(input$km,{
      original_tsne <<- as.data.frame(tsneout[,1:3])
      fit_cluster_kmeans=kmeans(scale(original_tsne), 35)  
      original_tsne$cl_kmeans = factor(fit_cluster_kmeans$cluster)
      
      plot_k <- plot_ly(data = original_tsne, x = original_tsne[,1], y = original_tsne[,2],z=original_tsne[,3],type = "scatter3d",mode = "markers",marker = list(symbol = "circles"), color = ~cl_kmeans, colors = "Set1")
   
      output$plotk<-renderPlotly({plot_k})
    })
    
  })
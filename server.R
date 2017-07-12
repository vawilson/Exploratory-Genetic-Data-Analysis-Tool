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
              getInteractions <- function(stims){
                #compiles a vector with the interactions of the inputted vector of stims
                thing <- unique(rbind(subset(filea,Stimulus %in% stims),subset(filea,Stimulus2 %in% stims)))
                # print(dim(subset(filea,Stimulus2 %in% stims)))
                return(thing)
              }
              mappedV <- function(vector){
                stimuli <- c("Null","C12IEDAP","aCD3aCD28","aCD3aCD28+IFNa","CPPD","Gardiqiumod","FLA","FSL","HKCandida","HKEcoli",
                             "HKHpylori","HKLactobac","HKStaphaur","IFNb","IFNg","IL1b","IL1b+TNFa","IL23","Influenza","IFNa","LPS+IFNa",
                             "Lipoarabinomannan","LPS","BCG","ODN","LPS+ODN","PolyIC","R848","SEB","Sendai","TNFa","Dectin")
                map <- setNames(stimuli, c(9:40))
                return(map[vector-8])
              }
              observeEvent(input$stimulus,{
                if(input$datatype == "Processed"){
                  v <-unique(getInteractions(input$stimulus))[,3:4]
                  vvalues <- paste(v[,1],v[,2])
                  names(vvalues) <- paste(mappedV(v[,1]),mappedV(v[,2]))
                  
                  updateSelectizeInput(session,"stimulus2",choices = vvalues)
                }
              })
              #when submit button is pressed and data is loaded, more options appear
              loadNames <- function(){
                pathtable <<- "./data/correspondancetable.txt"
                nametable<<-read.csv(pathtable,header=F,sep="\t")
                st <<-unique(nametable[,1])
                names(st) <<- unique(nametable[,2])
                
              }
              observeEvent(input$upload, {
                output$data <- renderUI({ 
                  loadNames()
                  if(input$datatype == "Raw"){
                    tagList(
                      isolate(
                        selectizeInput("donor",label = "Select Donor:", choices = unique(filea[1]), selected = as.character(unique(filea[1])$Donor),
                                       multiple = TRUE, options = list()
                        )
                      ),
                      isolate(
                        selectizeInput("stimulus", label = "Select Stimulus:", choices  = st, selected = st,
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
                  }
                  else{
                    tagList(
                      isolate(
                        selectizeInput("donor",label = "Select Donor:", choices = unique(filea[1]), selected = as.character(unique(filea[1])$Donor),
                                       multiple = TRUE, options = list()
                        )
                      ),
                      isolate(
                        # is all as.character(unique(filea[3])$Stimulus)
                        selectizeInput("stimulus", label = "Select Stimulus:", choices  = st, selected = c(),
                                       multiple = TRUE, options = list()
                        )
                      ),
                      isolate(
                        selectizeInput("stimulus2", label = "Select Second Stimulus:", choices  = c(),
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
                    
                  }
                })
              })
              observeEvent(input$select, {
                output$data2 <- renderUI({ 
                  
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
              })
              
              #' PCA Plot Function
              #'
              #' @param don the selected donors to have pca done on (vector)
              #' @param stim the selected stimuli to have pca done on
              #' @param tim time selected timepoints to have pca done on
              #'
              #' @return a datatable of the pca dimensions for each set of donor, simulus, and timepoints
              #'
              parseStim <- function(stim){
                out <- data.frame( do.call( rbind, strsplit( stim, ' ' ) ) ) 
                names(out) <- paste('Stimulus',1:2,sep="") 
                return(out) 
              }
              subsetData <- function(don,stim,tim){
                if(input$datatype == "Processed"){
                  stimcols <- parseStim(stim)
                  subsetdt <- subset(filea,Donor %in% don & Timepoint %in% tim)
                  subsetfilea <<- data.frame()
                  for( r in  c(1:nrow(stimcols))){
                    rs <- subset(subsetdt,Stimulus == stimcols[r,1] & Stimulus2 == stimcols[r,2])
                    subsetfilea<<-rbind(subsetfilea,rs)
                  }
                  subsetgenes <<- subsetfilea[,6:104]
                }else{
                  subsetfilea <<- subset(filea,Donor %in% don & Stimulus %in% stim & Timepoint %in% tim)
                  subsetgenes <<-subsetfilea[,6:592]
                }
                
                
              }
              pcaPlot <- function(c,s){
                
                pcaorig <<- (prcomp(subsetgenes, center = c, scale. = s))
                pca <- pcaorig$x
                if(input$datatype == "Raw"){
                pca3<-cbind(pca,subsetfilea[,c(1,2,5)])
                }
                else{
                  pca3<-cbind(pca,subsetfilea[,c(1,3,4,5)])
                }
                return(pca3)
              }
              tsnePlot <- function(pc,pe,it,lr){
                tsneplot <- Rtsne(subsetgenes,verbose = TRUE, max_iter = it,pca = pc,perplexity = pe, dims =3, eta = lr)
                output$error <- renderText(paste("Cost: ",tail(tsneplot$itercosts,n=1)))
                
                if(input$datatype == "Raw"){
                  tsne2<-cbind(tsneplot$Y,subsetfilea[,c(1,2,5)])
                }
                else{
                  tsne2<-cbind(tsneplot$Y,subsetfilea[,c(1,3,4,5)])
                }
                
                return(tsne2)
              }
              tsnePlotM <- function(pc,pe,it,c,s,pdim,lr){
                tsneplot <- Rtsne(subsetgenes,verbose = TRUE, max_iter = it,pca = pc,perplexity = pe, dims =3,pca_center = c, pca_scale = s, initial_dims = pdim,eta = lr)
                output$error <- renderText(paste("Cost: ",tail(tsneplot$itercosts,n=1)))
                
                if(input$datatype == "Raw"){
                  tsne2<-cbind(tsneplot$Y,subsetfilea[,c(1,2,5)])
                }
                else{
                  tsne2<-cbind(tsneplot$Y,subsetfilea[,c(1,3,4,5)])
                }
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
                  d <- set[["Donor"]]
                  s <- mappedV(set[["Stimulus"]])
                  s2 <- mappedV(set[["Stimulus2"]])
                  t <- set[["Timepoint"]]
                  text <- paste(d,s,s2,t)
                  p<-plot_ly(x=set[,1],
                             y=set[,2],
                             type = "scatter", mode = "markers",
                             marker = list(symbol = "circles"),
                             text = text, hoverinfo = "text",
                             name = "All Data",
                             width = 900, height = 700) 
                  
                  for(i in 1:length(don)){
                    texti <- text[d==don[i]]
                    p<-add_trace(p,
                                 x=subset(set,Donor==don[i])[,1],
                                 y=subset(set,Donor==don[i])[,2],
                                 type = "scatter", mode = "markers",
                                 marker = list(color = c[i]),
                                 text = texti, hoverinfo = "text",
                                 name = don[i],
                                 inherit = FALSE)
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
                if(input$datatype == "Raw"){
                c <- distinctColorPalette(length(stim))
                
                
                if(dim == "2D"){
                  p<-plot_ly(x=set[,1],y=set[,2],type = "scatter",mode = "markers",marker = list(symbol = "circles"),width = 900, height = 700) 
                  
                  for(i in 1:length(stim)){
                   
                    p<-add_trace(p,x=subset(set,Stimulus==stim[i])[,1],y=subset(set,Stimulus==stim[i])[,2],marker = list(color = c[i]), name = stim[i],evaulate = TRUE)
                  }
                }
                else {
                  p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d",mode = "markers",marker = list(symbol = "circles"),name = "All Data",width = 900, height = 700) 
                  for(i in 1:length(stim)){
                    
                    p<-add_trace(p,x=subset(set,Stimulus==stim[i])[,1],y=subset(set,Stimulus==stim[i])[,2],z=subset(set,Stimulus==stim[i])[,3],marker = list(color = c[i]), name = stim[i])
                    
                  }
                }
                
                }
                else{
                  stims <- parseStim(stim)

                 stim<-unique(c(as.vector(stims[,1]),as.vector(stims[,2])))
                  c <- distinctColorPalette(length(stim))
                
                  if(dim == "2D"){
                    p<-plot_ly(x=set[,1],y=set[,2],type = "scatter",mode = "markers",marker = list(symbol = "circles"),width = 900, height = 700) 
                    
                    for(i in 1:length(stim)){
                      
                     
                      p<-add_trace(p,x=subset(set,Stimulus==stim[i]| Stimulus2 == stim[i])[,1],y=subset(set,Stimulus==stim[i]| Stimulus2 == stim[i])[,2],marker = list(color = c[i]), name = nametable[as.integer(stim[i])-8,2],evaulate = TRUE)
                    }
                  }
                  else {
                    p<-plot_ly(x=set[,1],y=set[,2],z=set[,3],type = "scatter3d",mode = "markers",marker = list(symbol = "circles"),name = "All Data",width = 900, height = 700) 
                    for(i in 1:length(stim)){
                      
                      p<-add_trace(p,x=subset(set,Stimulus==stim[i]| Stimulus2 == stim[i])[,1],y=subset(set,Stimulus==stim[i]| Stimulus2 == stim[i])[,2],z=subset(set,Stimulus==stim[i]| Stimulus2 == stim[i])[,3],marker = list(color = c[i]), name = nametable[as.integer(stim[i])-8,2])
                      
                    }
                  
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
                if(input$datatype == "Raw"){
                k<-setupPlotStim(input$donor, input$stimulus, input$timepoint,input$dimension)
                }
                else{
                  k<-setupPlotStim(input$donor, input$stimulus2, input$timepoint,input$dimension)
                }
                output$plot1<-renderPlotly({k})
              })
              observeEvent(input$timebutton, {
                k<-setupPlotTime(input$donor, input$stimulus, input$timepoint,input$dimension)
                output$plot1<-renderPlotly({k})
              })
              
              observeEvent(input$select, {
                if(input$datatype == "Raw"){
                  subsetData(input$donor, input$stimulus, input$timepoint)
                }
                else{
                  subsetData(input$donor, input$stimulus2, input$timepoint)
                }
              })
              #params for only pca
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
                #params for only tsne
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
                #params for tsne and pca
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
              observeEvent(
                #go go pca only
                input$go, {
                  tsneout<<-pcaPlot(input$center,input$scale)
                  k<-setupPlot(input$dimension,tsneout)
                  output$plot2 <-renderPlotly({screePlot(pcaorig)})
                  output$plot1<-renderPlotly({k})
                })
              #go for tsne only
              observeEvent(input$go2, {
                tsneout<<- tsnePlot(FALSE,input$perplexity,input$iterations,input$lr)
                k<-setupPlot(input$dimension,tsneout)
                output$plot1<-renderPlotly({k})
                
              })
              #go for pca and tsne
              observeEvent(input$go3, {
                tsneout <<- tsnePlotM(TRUE,input$perplexity,input$iterations,input$center,input$scale,input$pcadims,input$lr)
                k<-setupPlot(input$dimension,tsneout)
                output$plot1<-renderPlotly({k})
                
              })
              #buttonfor kmeans
              observeEvent(list(input$go,input$go2,input$go3),{
                output$kmeansbutton <- renderUI({
                  actionButton("km" ,"K-means",class = "btn btn-primary")
                })
              })
              #runs kmeans and creates the plot for kmeans
              observeEvent(input$km,{
                original_tsne <<- as.data.frame(tsneout[,1:3])
                fit_cluster_kmeans=kmeans(scale(original_tsne), 35)  
                original_tsne$cl_kmeans = factor(fit_cluster_kmeans$cluster)
                
                plot_k <- plot_ly(data = original_tsne, x = original_tsne[,1], y = original_tsne[,2],z=original_tsne[,3],type = "scatter3d",mode = "markers",marker = list(symbol = "circles"), color = ~cl_kmeans, colors = "Set1")
                
                output$plotk<-renderPlotly({plot_k})
              })
              
            })
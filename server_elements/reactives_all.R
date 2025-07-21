### defining reactive() and reactiveValues() [values that are stored]
##################################################################### #

# Tag-life data frame object
data <- reactive({ 
  if(input$reset){
    df=NULL
  }
  else{
  req(input$file1) ## ?req #  require that the input is available
  inFile <- input$file1 
  df <- read.csv(inFile$datapath,header = T,sep=",")
  
  # If it is one-dimensional
  if(ncol(df)<2){
    df$taglife=df[,1]
    df$studyID="single"
    df=df[,-c(1)]
    updateSelectInput(session, inputId = 'xcol', label = 'Tag Life Column',
                      choices = names(df), selected = names(df)[1])
  }
  # If it is an actual .csv value with multiple columns
  else{
    updateSelectInput(session, inputId = 'xcol', label = 'Tag Life Column',
                      choices = names(df), selected = names(df)[1])
    updateSelectInput(session, inputId = 'grp_var', label = 'Lot ID',choices = c("[none]",names(df)),selected = "[none]")}
  }
  return(df)
})

# T/F of the dimensionality of the uploaded .csv (1 dimension vs. 2 or more)
TL_data_1dim <- reactive({ 
  req(input$file1) #require that the input is available
  inFile <- input$file1 
  df <- read.csv(inFile$datapath,header = T,sep=",")
  return(ncol(df)<2)
})


# Tag-life data frame reformatted for convenient model fitting
tag_ptDFR <- reactive({
  if(input$reset){
    tag_ptDF=NULL
  }
  else{
  req(input$file1)
  # If one dimensional data set
  if(TL_data_1dim()){
    y <- data()[, 1]
    s_y=sort(y) #sorting taglife values
    y_sfrac=sapply(s_y,function(x){1-length(which(s_y<=x))/length(y)})
    tag_ptDF=data.frame(time=s_y,est=y_sfrac,varname="single")}
  # Multi-dimensional data set
  else{
    y <- data()[, c(input$xcol)]
    if(is.numeric(y)){
      # Removing NAs
      ind=!is.na(y)
      y=y[ind]}
    else{ind=y=1}
    label <- data()[!is.na(y), c(input$grp_var)]
    label=label[ind]
    ord=order(y)
    label=label[ord]
    s_y=sort(y) #sorting taglife values
    y_sfrac=sapply(s_y,function(x){1-length(which(s_y<=x))/length(y)})
    tag_ptDF=data.frame(time=s_y,est=y_sfrac,varname=factor(label))
  }}
    return(tag_ptDF)
})


GOF_tab <- reactive({
  if(input$reset){
  out=list("model_params"=NULL,"GOF_table"=NULL)
  }
  else{
  # req(input$TL_GOF_comp)
  out=fail_mod(time=tag_ptDFR()$time)
  print(out)}
  return(out)
})

observeEvent(input$reset,{
  #   GOF_tab()  # running the data reactiveValue here populates table
  print(data())
  print(GOF_tab())
  #   # print(fail_mod(time=tag_ptDFR()$time))
}, ignoreInit = F)


### CJS module

# Unadjusted CJS object
CJSdata <- reactive({
  req(input$file2) ## ?req #  require that the input is available
  inFile <- input$file2 
  CJSdf <- read.csv(inFile$datapath,header = as.logical(input$headersCH),sep=",")
  return(CJSdf)
})

################################## #
### Reactive Modeling Results
################################## #

# reactiveValues
TL_mod_info <- reactiveValues(
  mod_ch = "No model specified",
  mod_ests = matrix(NA,ncol=3,nrow=4)
)


# Used to populate column names in selectInput and allows "Lock selection" box to be checked
observeEvent(input$file2,{
  CJSdata()  # running the data reactiveValue here populates column names
  enable("next_CJS1")
  }, ignoreInit = F)

## User Interface that appears only after being triggered by uploading a CSV
output$CJS_data_sel <- renderUI({
  if(length(input$file2)==0) {
    return(NULL)
  }
  else{
    # if(as.logical(input$headersCH)){
      return(
      tagList(
        radioButtons("select_CH_col", "Set column labels/order",choices = list("No"=FALSE,"Yes"=TRUE),inline=T),
        br(),
        actionButton("show_CJS_DF", "Show Data Table"),
        # actionButton("select_CH_col", "Set Column labels"),        
        # checkboxInput("select_CH_col", "Set Column labels"),
        bsModal("modal_CJS_DT", "Data Table", "show_CJS_DF", size = "large",
                dataTableOutput('CJS_contents'))
      ))
  }
})

observeEvent(input$file2,{
  CJSdata()  # running the data reactiveValue here populates column names
  radioButtons("select_CH_col", "Set Column labels",choices = list("No"=FALSE,"Yes"=TRUE),selected = "No")
}, ignoreInit = F)
update


## User Interface that appears only after being triggered by uploading a CSV
output$CJS_data_col_sel <- renderUI({
  if(length(input$select_CH_col)==0){
    return(NULL)}
  else{
  if(!as.logical(input$select_CH_col)){
  # CJS_df=isolate({CJSdata()})
  # if(!as.logical(input$select_CH_col)) {
  # if(length(input$select_CH_col)==0) {
    return(NULL)
  }
  else{
    if(as.logical(input$headersCH)){
      return(
        tagList(
          selectInput(inputId='CJS_relname', 'Release name',choices = names(CJSdata()),selected = names(CJSdata())[1]),
          selectInput(inputId='CJS_bin_num', 'Bin number',choices =names(CJSdata()),selected = names(CJSdata())[2]),
          selectInput(inputId = 'CJS_tag_id','Tag ID',choices = names(CJSdata()),selected = names(CJSdata())[3]),
          selectInput(inputId = 'CJS_act_dt_time','Activation date/time',choices = names(CJSdata()),selected = names(CJSdata())[4]),
          selectInput(inputId = 'CJS_rel_dt','Release date',choices = names(CJSdata()),selected = names(CJSdata())[5]),
          selectInput(inputId = 'CJS_site_nm','Site name',choices = names(CJSdata()),selected = names(CJSdata())[6]),
          selectInput(inputId = 'CJS_det','Detection',choices = names(CJSdata()),selected = names(CJSdata())[7]),
          selectInput(inputId = 'CJS_det_dt_time','Detection date/time',choices = names(CJSdata()),selected = names(CJSdata())[8]),
        ))
        }
    if(!(as.logical(input$headersCH))){
      colIds=1:length(names(CJSdata()))
      return(
        tagList(
          selectInput(inputId='CJS_relname', 'Release name',choices = colIds,selected = 1),
          selectInput(inputId='CJS_bin_num', 'Bin number',choices =colIds,selected = 2),
          selectInput(inputId = 'CJS_tag_id','Tag ID',choices = colIds,selected = 3),
          selectInput(inputId = 'CJS_act_dt_time','Activation date/time',choices = colIds,selected = 4),
          selectInput(inputId = 'CJS_rel_dt','Release date',choices = colIds,selected = 5),
          selectInput(inputId = 'CJS_site_nm','Site name',choices = colIds,selected = 6),
          selectInput(inputId = 'CJS_det','Detection',choices = colIds,selected = 7),
          selectInput(inputId = 'CJS_det_dt_time','Detection date/time',choices = colIds,selected = 8),
        ))
        }
  }}
})


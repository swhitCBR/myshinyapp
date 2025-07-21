# EXAMPLE CHANGE

# Used to populate column names in selectInput and allows "Lock selection" box to be checked
observeEvent(input$file1,{
  data()  # running the data reactiveValue here populates column names
  TL_data_1dim() # reactive value that determines whether the data is one-dimensional or not
  enable("next_TL1")
  # print(head(data()))
}, ignoreInit = F)


## User Interface that appears only after being triggered by uploading a CSV
output$in_ops <- renderUI({
  if(length(input$file1)==0) {
    return(NULL)
  }
  else{
    if(TL_data_1dim()){
      # When the data are one dimensional the user cannot select among columns
      return(
        tagList(
          actionButton("show_TL_DF", "Show Data Table"),
          br(),
          br(),
          actionButton("show_TL_PLT", "Plot Raw Tag-Life Data"),
          br(),
          bsModal("modal_TL_DT", "Data Table", "show_TL_DF", size = "large",
                  dataTableOutput('contents')),
          bsModal("modal_TL_PLT", "Data Plotting", "show_TL_PLT", size = "large",
                  uiOutput("box_tl_data_plt"))
        ))  
    }
    else{
    return(
      tagList(
        actionButton("show_TL_DF", "Show Data Table"),
        br(),
        br(),
        selectInput(inputId='xcol', 'Tag Life Column',choices = names(df)),
        selectInput(inputId='grp_var', 'Grouping Variable',choices = c("[none]",names(df)),selected = "[none]"),
        actionButton("show_TL_PLT", "Plot Raw Tag-Life Data"),
        br(),
        # uiOutput(box_tl_mod_plt),
        # If the checkbox is added in a dynamically, then I can't get it to gray-out options after being clipped
        # checkboxInput("next_TL1", "Lock selection"),
        bsModal("modal_TL_DT", "Data Table", "show_TL_DF", size = "large",
                dataTableOutput('contents')),
        bsModal("modal_TL_PLT", "Data Plotting", "show_TL_PLT", size = "large",
                uiOutput("box_tl_data_plt"))
        
        #         uiOutput("TL_mod_plt"))
      ))}
  }
})


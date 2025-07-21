## User Interface that appears only after being triggered by using the "Accept selection" checkbox AND observeEvent disabling previous data-selection options

# Disables the selections when the TL choice is locked in 
observeEvent(input$next_CJS1,{
  # print(c(
  #   input$CJS_relname,
  #   input$CJS_bin_num,
  #   input$CJS_tag_id,
  #   input$CJS_act_dt_time,
  #   input$CJS_rel_dt,
  #   input$CJS_site_nm,
  #   input$CJS_det,
  #   input$CJS_det_dt_time))
  # print(any(duplicated(c(
  #   input$CJS_relname,
  #   input$CJS_bin_num,
  #   input$CJS_tag_id,
  #   input$CJS_act_dt_time,
  #   input$CJS_rel_dt,
  #   input$CJS_site_nm,
  #   input$CJS_det,
  #   input$CJS_det_dt_time))))
  
  
  # disables selections when data set is looked in
  toggleState('CJS_relname' )
  toggleState('CJS_bin_num')
  toggleState('CJS_tag_id' )
  toggleState('CJS_act_dt_time')
  toggleState('CJS_rel_dt' )
  toggleState('CJS_site_nm')
  toggleState('CJS_det' )
  toggleState('CJS_det_dt_time')
})



output$CJS_lock_data <- renderUI({
  if(is.null(input$next_CJS1)){
    return(NULL)}
  if(input$next_CJS1){
    tagList(
      box(inputID="CJS model fitting",title="Survival model fitting",collapsible = T,closable = F,collapsed = F,width = 5,solidHeader=T,sidebar_width = 5,
        # selectInput(inputId="TL_mod_ch",label = "Select Model",choices = list(`Vitality` = list("Vitality (2009)","Vitality (2013)"),
        #                                                                               `Other` = list("Weibull (3)","Weibull (4)")),selected = "Vitality (2009)"),
        #         actionButton(inputId="fig_bt",label = "Fit Model"),
                br(),
                strong("TBD"),
                br()#,
                # uiOutput("post_tl_mod")
        ))}
})
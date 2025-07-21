## User Interface that appears only after being triggered by using the "Accept selection" checkbox AND observeEvent disabling previous data-selection options

# Disables the selections when the TL choice is locked in 
observeEvent(input$next_TL1,{
  toggleState("xcol" )
  toggleState("grp_var")})
c("Weibull(2)","Gompertz","Lognormal", "Log-logistic","Generalized gamma")

output$TL_mod_fit_cmds <- renderUI({
  if(is.null(input$next_TL1)){
    return(NULL)}
  if(input$next_TL1){
    box(inputID="Tag-life model fitting",title="Tag life model fitting",collapsible = T,closable = F,collapsed = F,width = 5,solidHeader=T,sidebar_width = 5,
        tagList(
          selectInput(inputId="TL_mod_ch",label = "Select Model",choices = list(`All models`=list("All"),
                                                                                `Vitality` = list("Vitality (2009)","Vitality (2013)"),
                                                                                      `Weibull` = list("Weibull (2)","Weibull (3)"),
                                                                                      `Gamma` = list("Gamma","Generalized gamma"),
                                                                                      `Other` = list("Gompertz","Lognormal", "Log-logistic")),selected = "All",multiple = F),
          br(),
          actionButton(inputId="TL_GOF_comp",label = "Rank models"),
          br(),
          br(),
          uiOutput("post_GOF_rank"),
          actionButton(inputId="fig_bt",label = "Fit Model"),
          uiOutput("post_mod_fit"),
          strong("Model Fitting Log:"),
          verbatimTextOutput("message"),
          br(),
          br(),
          uiOutput("post_tl_mod")
        ))}
})
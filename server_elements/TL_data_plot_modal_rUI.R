# UI code inside the modal after upload
## User Interface that appears only after being triggered by uploading a CSV

## User interface that fills in the modal for plotting taglife data
output$box_tl_data_plt <- renderUI({
  if(length(input$show_TL_PLT)==0) {  return(NULL)}
  else {
    return(
      tagList(selectInput('tl_plt_op', 'Plot',
                          choices = c(
                            "Survival Curve (within range)"="tl_curv_rng",
                            "Survival Curve (from zero)"="tl_curv_zero",
                            "Histogram"="tl_hist")),
              plotOutput('taglife_data_plt')))
  }
})


output$contents <- renderDT({ data() }, options = list(pageLength = 5,scrollX=TRUE, scrollCollapse=TRUE,searching=FALSE))

output$taglife_data_plt <-  renderPlot({
  # loading the reactiveValue of the taglife data set
  tag_ptDF=tag_ptDFR()
  tl_plts=list(
    "tl_hist"=ggplot(tag_ptDF,aes(x=time,fill=varname)) + geom_histogram(color="black",bins = 20) + mytheme+ labs(fill = input$grp_var),
    "tl_curv_zero"=ggplot() + geom_point(data=tag_ptDF,aes(x=time,y=est,fill=varname),shape=21,size=3,color="black") +
      mytheme + xlab(input$xcol)+ ylab("Survival proportion") + labs(fill = input$grp_var)+ scale_x_continuous(limits=c(0,max(tag_ptDF$time)))+
      theme(legend.position = "top"),
    "tl_curv_rng"=ggplot() + geom_point(data=tag_ptDF,aes(x=time,y=est,fill=varname),shape=21,size=3,color="black") +
      mytheme + xlab(input$xcol)+ ylab("Survival proportion") + labs(fill = input$grp_var)+
      theme(legend.position = "top"))
  
  tl_plts[[input$tl_plt_op]]
})

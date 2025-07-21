# Fitted taglife model ui that appears
output$post_tl_mod <-renderUI({
  if(is.null(input$fig_bt)){
    return(NULL)}
  if(input$fig_bt){
    tagList(
      # actionButton(inputId="show_TL_MOD_PLT","Plot Model"),
      selectInput('tl_plt_mod_op', 'Plot',
                  choices = c(
                    "Parameter"="tl_parm_ests",
                    "Survival Curve (within range)"="tl_curv_rng_fit",
                    "Survival Curve (from zero)"="tl_curv_zero_fit"),selected = "tl_curv_zero_fit"),
      
      plotOutput('TL_mod_plt'))
  }
})

# Model Log Output
output$message=renderText({
  if (input$fig_bt == 0)
    return()
  else{
# 
    # This isolate() is used to prevent this code block from running when the choice of model changes
    # mod_choice=isolate({ 
      mod_choice=input$TL_mod_ch
      # })
# # 
# #     y=tag_ptDFR()$time
# #     s_y=sort(y) #sorting taglife values
# #     y_sfrac=sapply(s_y,function(x){1-length(which(s_y<=x))/length(s_y)})
# # 
# #     vit_alt=vitality.ku(time = s_y,
# #                         sdata =  y_sfrac,se=T,
# # 
# #                         rc.data = F,
# #                         datatype = "CUM",
# #                         ttol = 1e-06,
# #                         pplot = F,
# #                         Iplot = F,
# #                         # Mplot = F,
# #                         tlab = "years",
# #                         silent = T)
# # 
# #     est_mat=data.frame(Parameter=c("r","s","k","u"),Estimate=signif(as.numeric(vit_alt[,1]),5),SE=signif(as.numeric(vit_alt[,2]),5))
# #     TL_mod_info$mod_ch=mod_choice
# #     TL_mod_info$mod_ests=est_mat
#     
    mod_pars=GOF_tab()[["model_params"]]
    mod_pars=subset(mod_pars,Model %in% mod_choice)
#     
#     # CREATING LOG THAT IS REPORTED
    if(any(is.na(mod_pars[,"se"]))){
      p2=  "Standard errors were NOT estimated for all parameters"
    }else{
      p2=  "Standard errors estimated for all parameters"
    }

    paste("Fit the",mod_choice,"model","\n",p2,"\n",paste(mod_pars[1,c("Parameter","est")],collapse=" = "),
          " (",mod_pars[1,c("se")],")")#,p2,"\n\n",signif(mod_pars[,"Parameter"],3))#,"\n",signif(mod_pars[,"est"],3))

    # paste(mod_choice)

  }
})


# Fitted taglife model ui that appears
output$post_mod_fit <-renderUI({
  tagList(
    bsModal("modal_TL_plt", "Observed vs. predicted tag life", "fig_bt", size = "large",
            
            strong("PLOT"),
            plotOutput("TL_mod_plt")
    )
  )
})


output$TL_mod_plt <- renderPlot({

  # mod_choice=isolate({ input$TL_mod_ch })
  # # loading the reactiveValue of the taglife data set
  # tag_ptDF=tag_ptDFR()
  # # list of plots that may be selected
  # 
  # pars=TL_mod_info$mod_ests$Estimate
  # dat_rng=seq(0,max(tag_ptDF$time)*1.1,0.5)
  # pred_mod_DF=data.frame(time=dat_rng,
  #                        est=SurvFn.ku(dat_rng,r = pars[1],s=pars[2],k=pars[3],u=pars[4]),varname=unique(tag_ptDF$varname)
  #                        )
  # print(head(pred_mod_DF))
  # 
  # tl_plts_mod=list(
  #   "tl_parm_ests"=ggplot(TL_mod_info$mod_ests,aes(x=Parameter,y=Estimate,ymin=Estimate-SE*1.96,ymax=Estimate+SE*1.96)) + geom_hline(yintercept = 0,linetype="dotted")+
  #     geom_point() + mytheme + geom_errorbar() + facet_wrap(~Parameter,nrow = 1,scales = "free_x") + ggtitle(mod_choice),
  #   "tl_curv_zero_fit"=ggplot() + geom_point(data=tag_ptDF,aes(x=time,y=est,fill=varname),shape=21,size=3,color="black") +
  #     mytheme + xlab(input$xcol)+ ylab("Survival proportion") + labs(fill = input$grp_var)+ scale_x_continuous(limits=c(0,max(tag_ptDF$time)))+
  #     theme(legend.position = "top") + facet_wrap(~varname) + geom_line(data=pred_mod_DF,aes(x=time,y=est),color="black",size=0.75),
  #   "tl_curv_rng_fit"=ggplot() + geom_point(data=tag_ptDF,aes(x=time,y=est,fill=varname),shape=21,size=3,color="black") + scale_x_continuous(limits=c(min(tag_ptDF$time),max(tag_ptDF$time)*1.1)) +
  #     mytheme + xlab(input$xcol)+ ylab("Survival proportion") + labs(fill = input$grp_var)+ facet_wrap(~varname) + geom_line(data=pred_mod_DF,aes(x=time,y=est),color="black",size=0.75) +
  #     theme(legend.position = "top"))
  # 
  # tl_plts_mod[[input$tl_plt_mod_op]]
  plot(1)

})
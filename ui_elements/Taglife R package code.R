# (THE NEWEST VERSION; STRIPPED DOWN VERSION)
# Code for fitting a particular tag-life model or models
fail_fit=function(time,model){
  # INPUTS
  # time = numeric vector of travel time
  # model= character object of model name or vector of model names
  
  # OUTPUT (named list)
  # "data" = cumulative
  # "mod_choice" = character object of model name or vector of model names,
  # "fit_vals" = data frame with fitted values for each time and 95% confidence interval
  # "mod_objs" = model objects from various packages: kaplan-meiser ("surival"), vitality models ("vitality")
  
  require(flexsurv)  # allows for fitting less common distributions
  require(survival)  # fitting KM model
  require(vitality)  # code for fitting Vitality 2013 model
  
  if(model=="all"){model=c("weibull", "gompertz", "gamma", "lognormal", "llogis", "gengamma","vitality.ku","vitality.4p")}
  
  # WARNINGS AND VALIDATION
  if(any(is.na(time))){
    message(paste(length(which(is.na(time))),"NA times removed"))}
  
  if(any(!model %in% c("weibull", "gompertz", "gamma", "lognormal", "llogis", "gengamma","vitality.ku","vitality.4p"))){
    message("One or more model names are not recognized \n Default model names = {'weibull','gompertz','gamma','lognormal','llogis','gengamma','vitality.ku','vitality.4p'}")}
  
  y=sort(time)  # sorted data necessary for Vitality package functions
  y_sfrac=sapply(y,function(x){1-length(which(y<=x))/length(y)}) # survival fraction calc
  
  fit=list()
  fit_vals=NULL
  for (i in 1:length(model)){
    
    # FITTING DISTRIBUTIONS IN THE FLEXSURV PACKAGE
    if(model[i] %in% c("weibull", "gompertz", "gamma", "lognormal", "llogis", "gengamma")){
      fit[[model[i]]] <- flexsurvreg(Surv(y) ~ 1, dist = model[i])
      fit_vals=rbind(fit_vals,
                     data.frame(model=model[i],time=0,est=1,lcl=1,ucl=1),
                     data.frame(model=model[i],summary(fit[[i]])[[1]]))}
    else{
      if(model[i]=="vitality.ku"){
        # fitting vitality model
        fit[[model[i]]] = vitality.ku(time = sort(y),sdata = y_sfrac,se=T,pplot =F,lplot = F, silent = T)
        pars_tmp=fit[[model[i]]][,"params"]
        fit_vals=rbind(fit_vals,
                       data.frame(model="Vitality (2009)",
                                  time=c(0,y), # survival proportions
                                  est=c(1,SurvFn.ku(y,pars_tmp[1],pars_tmp[2],pars_tmp[3],pars_tmp[4])),
                                  lcl=0,ucl=0))
        # df1=data.frame(parm=c("r","s","k","u"),data.frame(vit_mod));names(df1)[2]="Estimate"
      }
      if(model[i]=="vitality.4p"){
        # fitting vitality model
        fit[[model[i]]] = vitality.4p(time = sort(y),sdata = y_sfrac,se=T,pplot =F,silent = T)
        # pars_tmp=fit[[model[i]]][,"params"]
        # fit_vals=rbind(fit_vals,
        #                data.frame(model="Vitality (2013)",
        #                           time=c(0,y), # survival proportions
        #                           est=c(1,SurvFn.4p(y,pars_tmp[1],pars_tmp[2],pars_tmp[3],pars_tmp[4])),
        #                           lcl=0,ucl=0))
        # df1=data.frame(parm=c("r","s","k","u"),data.frame(vit_mod));names(df1)[2]="Estimate"
      }
      print("ha")}
    
    
  }
  
  # PARAMETER ESTIMATES FROM ALL FITTED MODELS
  # surv_mod_params=data.frame(
  #   Model=rep(model,times=sapply(fit,function(x){length(x$opt$par)})),
  #   Parameter=unlist(sapply(fit,function(x){names(x$opt$par)})),
  #   do.call(rbind,sapply(fit,function(x){x$res})))[,-c(4:5)] ; rownames(surv_mod_params)=NULL
  
  out=structure(list("mod_choice"=model,
           "times"=data.frame(time=y,surv_frac=y_sfrac),
           "fit_vals"=fit_vals,
           "mod_objs"=fit),
           class="failmod_obj")
  
  # DEV NOTES
  # can use a tapply() to execute function for multiple LotIDs
  # consider replacing KM_mod object with simpler function for computing survivial proportions
  # need to add:   KM_mod=survfit(Surv(y)~1) # kaplan meier model used to convert times into survival propotions
  # Consider trying alternative optimization methods
}

# function that limits the amount of output displayed when 
# a failmod_obj is called
print.failmod_obj <- function(x){
  print(x[[1]])
  invisible(x)
}

# (THE OLD COMBINED VERSION FOR MODEL FITTING & COMPARISON)
# Code for fitting and comparing among survival models
fail_mod=function(time,model){
  require(flexsurv)  # allows for fitting less common distributions
  require(survival)  # fitting KM model
  require(vitality)  # code for fitting Vitality 2013 model

  # All non-vitality models compared
  dists <- c("weibull", "gompertz", "gamma", "lognormal", "llogis", "gengamma")
  dists_long <- c("Weibull (2)","Gompertz", "Gamma", "Lognormal", "Log-logistic","Generalized gamma")
  
  # Estimating Kaplacn Meier values for the data
  KM_mod=survfit(Surv(time)~1)
  
  # SURVIVAL CURVE
  surv_predDF=data.frame(matrix(NA,ncol=6,nrow=length(KM_mod$time)+1)) # adding one for the first part of step function
  names(surv_predDF)=c("model","time","est","lcl","ucl","npars")
  surv_predDF$time=c(0,KM_mod$time);
  surv_predDF$model="Kaplan-Meier"
  surv_predDF$est=c(1,KM_mod$surv);surv_predDF$lcl=c(1,KM_mod$lower)
  surv_predDF$ucl=c(1,KM_mod$upper);surv_predDF$npars=NA
  # return(surv_predDF)
  
  y=time
  
  # FITTING DISTRIBUTIONS IN THE FLEXSURV PACKAGE
  fit=list()
  for (i in 1:length(dists)){
    fit[[i]] <- flexsurvreg(Surv(y) ~ 1, dist = dists[i])
    surv_predDF=rbind(surv_predDF,
                      data.frame(model=dists_long[i],time=0,est=1,lcl=1,ucl=1,npars=fit[[i]]$npars),
                      data.frame(model=dists_long[i],summary(fit[[i]])[[1]],npars=fit[[i]]$npars))
  }
  
  surv_mod_params=data.frame(
    Model=rep(dists_long,times=sapply(fit,function(x){length(x$opt$par)})),
    Parameter=unlist(sapply(fit,function(x){names(x$opt$par)})),
    do.call(rbind,sapply(fit,function(x){x$res})))[,-c(4:5)] ; rownames(surv_mod_params)=NULL
  
  #COMPUTING SURVIVAL FRACTION
  s_y=sort(y) #sorting taglife values
  y_sfrac=sapply(s_y,function(x){1-length(which(s_y<=x))/length(y)})
  
  # VITALITY FUNCTION PROVIDED BY RICH
  # vit_modOLD=vitality.sa(time = KM_mod$time,sdata = KM_mod$surv,se=F,pplot =F,lplot = T, silent = T)
  # vit_mod=vitality.sa(time = sort(y),sdata = y_sfrac,se=F,pplot =F,lplot = T, silent = T)
  vit_mod=vitality.ku(time = sort(y),sdata = y_sfrac,se=F,pplot =F,lplot = T, silent = T)
  
  
  df1=data.frame(parm=c("r","s","k","u"),data.frame(vit_mod));names(df1)[2]="Estimate"
  
  ### VITALITY PACKAGE ESTIMATE
  vit_alt=vitality.4p(time = s_y,
                      sdata =  y_sfrac,se=F,
                      #init.params=FALSE,
                      init.params=c(0.012, 0.01, 0.1, 0.1),
                      lower = c(0, 0, 0, 0), upper = c(100,50,1,50),
                      rc.data = F,
                      datatype = "CUM",
                      ttol = 1e-06,
                      pplot = F,
                      Iplot = F,
                      Mplot = F,
                      tlab = "years",
                      silent = T)
  surv_mod_params=rbind(surv_mod_params,data.frame(Model="Vitality (2009)",Parameter=c("r","s","k","u"),est=vit_mod,se=NA),
                    data.frame(Model="Vitality (2013)",Parameter=c("r","s","lambda","beta"),est=vit_alt,se=NA))
  
  #FITTING WEIBULL(3)
  # weib3_res=weibull3(KM_mod$time,plots=F) # OLDE CODE THAT HAS STANDARD ERRORS
  weib3_res=weibull3_NOSE(KM_mod$time,plots=F)
  shp=weib3_res[[1]][1,2]
  scl=weib3_res[[1]][2,2]
  trsh=weib3_res[[1]][3,2]
  
  surv_mod_params=rbind(surv_mod_params,data.frame(Model="Weibull (3)",Parameter=c("shape","scale","thresh"),
                                           est=weib3_res[[1]]$Est,se=NA))
  
  # ADDS PREDICTED VALUES OR THE THREE DATASETS NOT IN THE fitsurv package
  surv_predDFcomb=rbind(surv_predDF,
                        data.frame(model="Weibull (3)",time=c(0,KM_mod$time),
                                   est=c(1,weib3_res[[3]]$S),
                                   lcl=0,ucl=0,npars=3),
                        data.frame(model="Vitality (2009)",time=c(0,KM_mod$time),
                                   est=c(1,SurvFn.ku(KM_mod$time,vit_mod[1],vit_mod[2],vit_mod[3],vit_mod[4])),
                                   lcl=0,ucl=0,npars=4),
                        data.frame(model="Vitality (2013)",time=c(0,KM_mod$time),
                                   est=c(1,SurvFn.4p(KM_mod$time,vit_alt[1],vit_alt[2],vit_alt[3],vit_alt[4])),
                                   lcl=0,ucl=0,npars=4)
  )
  
  
  KM_ests=subset(surv_predDFcomb,model=="Kaplan-Meier")$est
  mod_predsCOMB=surv_predDFcomb
  mod_predsCOMB$sqr_err=(mod_predsCOMB$est-KM_ests)^2

  mod_predsCOMB$resid=mod_predsCOMB$est-KM_ests
  mod_predsCOMB$emp_est=KM_ests
  # 
  # SUMMARIZING THE FIT OF VARIOUS MODELS
  # 
  sumDF=data.frame(
    SSE_KM=tapply(mod_predsCOMB$sqr_err,mod_predsCOMB$model,sum),
    n=length(y),
    npars=tapply(mod_predsCOMB$npars,mod_predsCOMB$model,max))

  sumDF$GOF=sumDF$SSE_KM/(sumDF$n - sumDF$npars -1)
  sumDF=data.frame(model=rownames(sumDF),sumDF); rownames(sumDF)=NULL; sumDF[order(sumDF$GOF,decreasing = F),]
  sumDF$model=factor(sumDF$model,levels = sumDF$model[order(sumDF$GOF,decreasing = T)])
  sumDF=subset(sumDF,model!="Kaplan-Meier")
  sumDF$model=factor(as.character(sumDF$model),levels=unique(as.character(sumDF$model)))
  sumDF=sumDF[order(sumDF$GOF,decreasing = F),]
  sumDF$GOF=round(sumDF$GOF,4)
  
  # Reordering parameters according to GOF rank
  surv_mod_params$Model=factor(surv_mod_params$Model,levels=as.character(sumDF$model))
  surv_mod_params=surv_mod_params[order(surv_mod_params$Model),]
  
  sumDF$message=c("","SEs not estimated")[tapply(surv_mod_params$se,surv_mod_params$Model,function(x){any(is.na(x))})+1]
  
  rownames(surv_mod_params)=rownames(sumDF)=NULL
  out=list("model_params"=surv_mod_params,"GOF_table"=sumDF)#,"preds"=surv_predDFcomb)
  
  return(out)
}

# Fitting the 3-parameter Weibull model to times data, with optional plots
weibull3_NOSE=function(times,plots=F){
  EPS = sqrt(.Machine$double.eps) # "epsilon" for very small numbers
  
  llik.weibull <- function(shape, scale, thres, x){ 
    sum(dweibull(x - thres, shape, scale, log=T))}
  
  # thetahat.weibull <- function(times,plots=T){
  x=times
  pred_t=seq(0,max(x)*1.5,length.out = 50)
  # require(numDeriv )
  if(any(x <= 0)) stop("x values must be positive")
  toptim <- function(theta) -llik.weibull(theta[1], theta[2], theta[3], x)
  mu = mean(log(x))
  sigma2 = var(log(x))
  shape.guess = 1.2 / sqrt(sigma2)
  scale.guess = exp(mu + (0.572 / shape.guess))
  thres.guess = 1
  res = nlminb(start=c(shape.guess, scale.guess, thres.guess), toptim, lower=EPS)
  # print(hessian(toptim,res$par,method = "Richardson",))
  # res = optim(par = c(shape.guess, scale.guess, thres.guess), toptim,hessian = TRUE)
  # res1 = optim(par = c(res$par[1], res$par[2], res$par[3]),
  #              upper =c(res$par[1], res$par[2], res$par[3])*1.1,
  #              lower = c(res$par[1], res$par[2], res$par[3])*0.9,
  #              method ="L-BFGS-B" , toptim,hessian = TRUE)
  # # c(shape=res$par[1], scale=res$par[2], thres=res$par[3])
  # SEs = sqrt(diag(solve(res1$hessian)))
  # # list(res,res1)
  # 
  shp=res$par[1]
  scl=res$par[2]
  trsh=res$par[3]
  # 
  # if(plots==TRUE){
  #   hist(x,xlim=c(min(pred_t),max(pred_t)),probability = T,xlab="Time",ylab="# Deaths",main="")
  #   lines(pred_t,dweibull3(x = pred_t,shape = shp,scale=scl,thres = trsh),col=2)
  #   
  #   Spred_t=seq(0,max(pred_t)*1.5,length.out = 50)
  #   plot(Surv(x),xlab="Time",ylab="Survival")
  #   lines(Spred_t,exp(-((Spred_t-trsh)/scl)^shp),col=2)
  # }
  # 
  out=data.frame(Parms=c("Shape","Scale","Shift"),Est=c(shape=res$par[1], scale=res$par[2], thres=res$par[3]),SEs=rep(NA,3))#,SEs)
  rownames(out)=NULL
  DF_S_pred=data.frame(times=sort(times),S=exp(-((sort(times)-trsh)/scl)^shp))
  out=list(out,rbind(paste("n",length(times),sep=" = "),
                     # paste("AIC",AIC(fit),sep=" = "),
                     paste("logLik",res$objective,sep=" = ")),
           DF_S_pred
  )
  return(out)
  # }
  # return(res)
}

# # Fitted Values from 1 to 150 for a smoother line in the plot
# surv_predDF150=NULL
# for (i in 1:length(dists)){ surv_predDF150=rbind(surv_predDF150,data.frame(model=dists_long[i],summary(fit[[i]],t=1:150),npars=fit[[i]]$npars))}
# surv_predDF150=rbind(surv_predDF150,
#                      data.frame(model="Weibull (3)",time=1:150,
#                                 est=exp(-(((1:150)-trsh)/scl)^shp),
#                                 lcl=0,ucl=0,npars=3),
#                      data.frame(model="Vitality_2009",time=1:150,
#                                 est=SurvFn(1:150,vit_mod[1],vit_mod[2],vit_mod[3],vit_mod[4]),
#                                 lcl=0,ucl=0,npars=4),
#                      data.frame(model="Vitality_2013",time=1:150,
#                                 est=SurvFn.4p(1:150,vit_alt[1],vit_alt[2],vit_alt[3],vit_alt[4]),
#                                 lcl=0,ucl=0,npars=4))
# 
# surv_predDF65=NULL
# for (i in 1:length(dists)){ surv_predDF65=rbind(surv_predDF65,data.frame(model=dists_long[i],summary(fit[[i]],t=seq(1,65,0.1)),npars=fit[[i]]$npars))}
# surv_predDF65=rbind(surv_predDF65,
#                     data.frame(model="Weibull (3)",time=seq(1,65,0.1),
#                                est=exp(-(((seq(1,65,0.1))-trsh)/scl)^shp),
#                                lcl=0,ucl=0,npars=3),
#                     data.frame(model="Vitality_2009",time=seq(1,65,0.1),
#                                est=SurvFn(seq(1,65,0.1),vit_mod[1],vit_mod[2],vit_mod[3],vit_mod[4]),
#                                lcl=0,ucl=0,npars=4),
#                     data.frame(model="Vitality_2013",time=seq(1,65,0.1),
#                                est=SurvFn.4p(seq(1,65,0.1),vit_alt[1],vit_alt[2],vit_alt[3],vit_alt[4]),
#                                lcl=0,ucl=0,npars=4))
# 

require(readr)

# Function for creating shinyServer function using submods
build_server <- function(vec_string,
                         server_obj_name="server",
                         server_file_name="server.R"){
  require(readr)
  top_line=paste(server_obj_name," <- shinyServer(function(input, output, session) {\n\n")
  write_lines(c(top_line,paste(vec_string,""),"})"), path = server_file_name)
}

# Function that adds a comma between vector of UI elements
ui_line_paste=function(x){
  require(readr)
  len=length(x)
  out=x
  if(len>1){out=paste(x,collapse = ",\n")}
  return(out)
}

# Function for organizing server elements
combine_server_elements=function(module_dirs,
                                 module_name,
                                 module_label,
                                 subscripts){
  require(readr)
  # test that vectors are the same length
  stopifnot(var(sapply(list(module_name,module_label,module_dirs),length))==0)
  
  server_eles=ele_nms=NULL
  for(i in 1:length(module_label)){
    for(j in 1:length(subscripts[[i]])){
      tmp_pth=file.path(module_dirs[i],subscripts[[i]][j])
      # testing if last added file exists in the directory
      stopifnot(file.exists(tmp_pth))
      ele_nms=c(ele_nms,tmp_pth)
      server_eles=c(server_eles,tmp_pth)
    }}
  
  out=lapply(server_eles,function(x){
    # labels the path
    paste0(
      "#### \n",
      paste("## PATH ## ",x,"\n",paste(read_lines(x),collapse="\n"),"\n",sep=""))}
  )
  names(out)=ele_nms
  print(length(ele_nms))
  return(out)
}

# Function for organizing user interface element
combine_ui_elements=function(module_dirs,
                             module_name,
                             module_label,
                             baseUI_ele){
  # test that vectors are the same length
  stopifnot(var(sapply(list(module_name,module_label,module_dirsORD),length))==0)
  # test that all directories have a "ui_page.R" file
  stopifnot(all(sapply(module_dirs,function(x){any(dir(x)=="ui_page.R")})))
  
  v=menuItems=list()
  for(i in 1:length(module_dirsORD)){
    v[[i]]=c(module_name[i],module_label[i],module_dirsORD[i])
    names(v[[i]])=c("module_name","module_label","module_pth")
    menuItems[[i]]=paste("menuItem(tabName = '",v[[i]][1],"', text = '",v[[i]][2],"')",sep="")}
  module_sbar=ui_line_paste(unlist(menuItems))
  
  module_pg_ls=sapply(file.path(module_dirsORD,"ui_page.R"),read_lines)
  len_blks=length(module_pg_ls)
  modul_pg_comb=c()
  for(i in 1:len_blks){
    if(i==len_blks){modul_pg_comb[i]=paste(module_pg_ls[[i]],collapse = "\n")}
    else{modul_pg_comb[i]=paste(paste(module_pg_ls[[i]],collapse = "\n"),",",sep="")}
  }
  module_pgs=paste(modul_pg_comb,collapse="\n")
  
  ui_line_paste(lapply(file.path(module_dirsORD,"ui_page.R"),read_lines))
  
  out_vec=c(
    # User-interface above sidebar names
    baseUI_ele$baseUI_top1,
    # Sidebar information for each page
    module_sbar,
    # User-interface between sidebar name and pages
    baseUI_ele$baseUI_top2,
    # User-interface for each page
    module_pgs,
    # User-interface for each page
    baseUI_ele$baseUI_bottom)
  
  return(out_vec)
}

# # helper function Function for creating "ui.R" 
# build_ui <- function(vec_string,
#                      ui_obj_name="ui",
#                      ui_file_name="ui.R"){
#   require(readr)
#   top_line=paste(ui_obj_name," <- ")
#   write_lines(c(top_line,paste(vec_string,""),""), path = ui_file_name)
# }

# Function for creating "ui.R" 
build_app <- function(prerun,server_code,
                      ui_eles,
                      app_path){
  # test that prerun file exists in directory
  # stopifnot(file.exists(prerun))
  top_line_ui="ui <- "
  top_line_server=paste("server <- shinyServer(function(input, output, session) {\n\n")
  top_mat=ui_els=c(top_line_ui,paste(UI_elements,""),"")
  server_els=c(top_line_server,paste(server_code,""),"")
  write_lines(c(prerun, # prerun code at the top
                "server <- shinyServer(function(input, output, session) {\n\n",paste(server_code,""),"})",
                paste("\n\nui <- "),paste(ui_eles,""),"\n\nshinyApp(server=server,ui=ui)"),
              file = paste(app_path,"app.R",sep = "/"))
}





# make_ui_ele <- function(obj,level,args_first="",contents,cat_wrap = FALSE,flow_lines=F,add_return=T){
#   if(args_first!=""){args_first <- paste0(args_first,",")}
#   obj_tab_level <- paste0(rep("\t",level),collapse="")
#   cont_tab_level <- paste0(rep("\t",level+1),collapse="")
#   ind <- seq_along(contents)
#   cont_str<- sapply(ind,function(ii){
#     if(flow_lines) {tmp <- paste0("\n",paste0(contents[[ii]]),collapse="")}
#     else{tmp <- paste0("\n",paste0(cont_tab_level,contents[[ii]]),collapse="")}
#     if(add_return) tmp <- gsub(x=tmp,pattern=",",paste0(",\n",cont_tab_level))
#     tmp
#   })
#   
#   
#   
#   # return(cont_str)
#   out <- paste0(obj_tab_level,obj,"(",
#                 args_first,
#                 paste0(cont_str,collapse=","),
#                 "\n",obj_tab_level,")",collapse="")
#   if(cat_wrap){return(cat(out))}
#   out
# }
# 
# ui_tab_concat <- function(tab_ls_in, cat_wrap = FALSE) {
#   objs_ind <- seq_along(tab_ls_in)
#   tabIt_as_strng <- sapply(objs_ind, function(ii) {
#     paste0("tabItem(tabName = ",
#            names(tab_ls_in)[ii],
#            ",\n\t",
#            tab_ls_in[[ii]],
#            ")")
#   })
#   out_str <-   paste0(tabIt_as_strng, collapse = ",\n")
#   if (cat_wrap) {
#     return(cat(out_str))
#   }
#   return(out_str)
# }

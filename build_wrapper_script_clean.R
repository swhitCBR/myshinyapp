# source(file.path(my_dir,"examp2source.R"))

my_dir <- "C:/Users/swhit/Desktop/test"

# moved 
# function definitions
# SERVER_elements
# prerun code

# create directories
if(!dir.exists(my_dir)) {dir.create(my_dir)}

sub_dirs <- c("server_elements","ui_elements","prerun","app/old","R")
sapply(seq_along(sub_dirs),function(ii){
  if(!dir.exists(file.path(my_dir,sub_dirs[ii]))) {
    dir.create(file.path(my_dir,sub_dirs[ii]),recursive = T)}
  })

source(file.path("C:/Users/swhit/Desktop/shiny_template","shiny_template_fxns.R"))
# source(file.path("C:/Users/swhit/Desktop/test/R","make_ui_ele.R"))
sapply( file.path(my_dir,"R",dir(file.path(my_dir,"R"),pattern=".R")),source)

######################################### #
# establish server and UI linkages
######################################### #


# source(file.path(my_dir,"ui_elements/base_ui_objs.R"))

SERVER_elements <- combine_server_elements(
  module_dirs = file.path(my_dir,"server_elements"),
  module_name = "test",
  module_label = "testLab",
  # first element in the list
  subscripts = list(dir(file.path(my_dir,"server_elements"))))

# named list
SERVER_elements

prerun_code=read_lines(file.path(my_dir,"/prerun/prerun_01.R"))

###################### #
# UI creation code
###################### #

ui_fls <- dir(file.path(my_dir,"ui_elements"),pattern="ui_")

# list of strings with scripts and lines of scripts
tmp_ls <- lapply(seq_along(ui_fls),function(ii){
  read_lines(file.path(my_dir,"ui_elements",ui_fls[ii]))})

# arguments at different levels various levels
in_ls <- list(
  obj_vec=c("fluidPage","fluidRow","column"),
  lvl_vec=c(0,1,2),
  args_first_vec=c("","","12"),
  flow_lines_vec=c(TRUE,TRUE,FALSE))


################################################### #
# Three-level ui grouping ui lines across two 
# scripts: ui_1.R,ui_2.R
################################################### #

full_ui <- paste0(
  "ui <- ",
  # page-level
  make_ui_ele(obj=in_ls[[1]][1],level=in_ls[[2]][1],
    contents = 
      make_ui_ele(
        obj=in_ls[[1]][2],level=in_ls[[2]][2],
          contents = 
            make_ui_ele(
              obj=in_ls[[1]][3],level=in_ls[[2]][3],
              args_first=in_ls[[3]][3],flow_lines = in_ls[[4]][3],
              contents=tmp_ls)))
)

cat(full_ui)

############################################# #
## Eliminating the lowest grouping level
############################################# #

full_ui <- paste0(
  "ui <- ",
  # page-level
  make_ui_ele(obj=in_ls[[1]][1],level=in_ls[[2]][1],
              contents = 
                make_ui_ele(
                  obj=in_ls[[1]][2],level=in_ls[[2]][2],
                  # contents = 
                  #   make_ui_ele(
                  #     obj=in_ls[[1]][3],level=in_ls[[2]][3],
                      args_first=in_ls[[3]][3],flow_lines = in_ls[[4]][3],
                      contents=
                        tmp_ls
                      )
                  # )
              )
)

cat(full_ui)

################################################### #
# Three-level ui grouping ui lines across two 
# scripts: ui_1.R,ui_2.R
################################################### #

in_ls[[1]]

full_ui <- paste0(
  "ui <- ",
  # page-level
  make_ui_ele(obj=in_ls[[1]][1],level=in_ls[[2]][1],
              contents = 
                make_ui_ele(
                  obj=in_ls[[1]][2],level=in_ls[[2]][2],
                  contents = 
                    c(
                      make_ui_ele(
                        obj=in_ls[[1]][3],level=in_ls[[2]][3],
                        args_first=in_ls[[3]][3],flow_lines = in_ls[[4]][3],
                        contents=tmp_ls[[1]]),
                      make_ui_ele(
                        obj=in_ls[[1]][3],level=in_ls[[2]][3],
                        args_first=in_ls[[3]][3],flow_lines = in_ls[[4]][3],add_return = T,
                        contents=tmp_ls[[2]],)
                    )
                  )
              )
)

cat(full_ui)


write_lines(
  full_ui,
  file = paste(my_dir,"app","ui.R",sep = "/"))


write_lines(
  c(prerun_code,
    "server <- shinyServer(function(input, output, session) {\n\n",paste(SERVER_elements,""),"})"),
  file = file.path(my_dir,"app","server.R"))

shiny::runApp(file.path(my_dir,"app"))



# build_app(prerun_code,
#           ui_elements,
#           server_elements,
#           app_dir)
# 
# 
# library(shiny)
# runApp(file.path(my_dir,'apps/proto_0_0'))
# 
# build_app(
#   prerun = prerun_code,
#   server_code = unlist(SERVER_elements),
#   ui_eles = UI_elements,
#   app_path = app_dir
# )
# 

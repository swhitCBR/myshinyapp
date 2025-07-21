
app_label_ls <- list(app_title="CVPAS Steelhead v0.0"
                     )

baseUI_objs=list(
# User-interface above sidebar names
baseUI_top1=paste0("dashboardPage(skin = 'green',
                        dashboardHeader(title = '",app_label_ls[["app_title"]],"',titleWidth = 300),
                        dashboardSidebar(
                        width = 300,
                          sidebarMenu("),
# User-interface between sidebar name and pages
baseUI_top2=")),
                        dashboardBody(
                          tags$head(
                            tags$style(HTML('
                            #rendertext {
                            color: black;
                            background: gray;
                            font-family: Arial, Helvetica, sans-serif;
                            font-size: 12px;
                            }'))),
                          shinyjs::useShinyjs(),
                          tags$head(tags$style(HTML('
                            #checkbox :after, #checkbox :before{
                            background-color:#bff442;
                            }'
                          ))),
                          tabItems(",

# CLOSING PARENTHESIS AT BOTTOM OF APP
baseUI_bottom=")))"
)
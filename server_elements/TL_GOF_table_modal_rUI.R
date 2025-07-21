
output$GOF_cont_clean <- renderDT({ GOF_tab()[["GOF_table"]][,c("model","npars","message","GOF")] }, 
                            options = list(dom = 't'))#list(scrollX=FALSE, scrollCollapse=FALSE,searching=FALSE,lengthChange = FALSE))

# Fitted taglife model ui that appears
output$post_GOF_rank <-renderUI({
    tagList(
    bsModal("modal_GOF_DT", "Model rankings", "TL_GOF_comp", size = "large",
            dataTableOutput('GOF_cont_clean')
            )
    )
    })

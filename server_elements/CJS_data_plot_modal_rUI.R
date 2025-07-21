# UI code inside the modal after upload
## User Interface that appears only after being triggered by uploading a CSV

output$CJS_contents <- renderDT({ CJSdata() }, options = list(pageLength = 5,scrollX=TRUE, scrollCollapse=TRUE,searching=FALSE))

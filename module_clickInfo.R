clickInfoUI <- function(id){
  ns <- NS(id)
  
  
}


ui<-fluidPage(
  moduleUI(moduleLab)
)

sever<-function(input, output, session){
  callModule(module, moduleLab)
}

shinyApp(ui, server)
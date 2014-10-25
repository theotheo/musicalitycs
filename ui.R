library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  mainPanel(      
    wellPanel(
      textOutput("number_unique"),
      textOutput("number_russian"),
      textOutput("p_factor")
    ),
    uiOutput("plot"),
    br(),
    tableOutput("names"),
    tableOutput("friends"),

    tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("redirect",
        function(a) {
          console.log("redirect")
          var redirect_uri = location.href
         location.replace("https://oauth.vk.com/authorize?client_id=4598768&scope=audio,friends&redirect_uri="+redirect_uri+"&response_type=code&v=5.25")

          //location.replace("https://oauth.vk.com/authorize?client_id=4596301&scope=audio,friends,photos&redirect_uri="+redirect_uri+"&response_type=token&v=5.25&display=page")
        }
      );
    ')))
  )
))
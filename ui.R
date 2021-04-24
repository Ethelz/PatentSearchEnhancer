library(rsconnect)
deployApp()

library(shiny)


shinyUI(fluidPage(

    titlePanel("Patent Search Enhancer"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("source_patent","What is your source patent number?"),
            numericInput("expected_return_num","How many number of patents are expected to return?",
                         value=0,min=0,max=50),
            textInput("website","Paste the URL here."),
            actionButton("submit","Submit")),
        mainPanel(
            tableOutput('table'),
            downloadButton(outputId="download",label="Download CSV")
        )
    )
 
    ))

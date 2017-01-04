#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(Rfacebook)
library(xlsx)

#generate facebook
function(input, output) {
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 

  facebookPage <- eventReactive(input$generate, {
    input$n
    fb_page <- getPage(page=input$n, token=fb_oauth)
    write.table(fb_page, "C:/Users/maranan.ld/Documents/MSTR/EmoTector/www/mydata.xls", sep="\t")
  })
  
  output$facebookPage <- renderText({
    facebookPage()
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
 
}

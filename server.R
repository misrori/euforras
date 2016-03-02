
library(shiny)
library(data.table)
library(pander)
shinyServer(function(input, output) {
  
  d2020 <-data.table( read.csv("eu.csv", header = T,sep = ",", fileEncoding = 'UTF-8'))
  
  g_by <- reactive({
    as.character(input$group_by)
  })
  
  g_by2 <- reactive({
    as.character(input$group_by2)
  })
  prog <- reactive({
    as.character(input$f_terv)
  })
  
  dd <- reactive({
    h<-g_by()
    h2<-g_by2()
    p <- prog()
    if (h==""&p=="") {
      return(d2020)
    } else if ( h==""& p!="") {
      return(d2020[f_program==p])
    }else if ( h!=""& p==""&h2=="") {
      return(d2020[,list(millio_Ft=round(sum(millio_Ft, na.rm = T), digits=3), nyertes_palyazatok_szama= .N),by=h])
    }else if ( h!=""& p!=""&h2=="") {
      return(d2020[f_program==p,list(millio_Ft=round(sum(millio_Ft, na.rm = T), digits=3), nyertes_palyazatok_szama= .N),by=h])
    }else if ( h!=""& p!="" &h2!="") {
      return(d2020[f_program==p,list(millio_Ft=round(sum(millio_Ft, na.rm = T), digits=3), nyertes_palyazatok_szama= .N),by=c(h, h2)])
    }else if ( h!=""& p==""&h2!="") {
      return(d2020[,list(millio_Ft=round(sum(millio_Ft, na.rm = T), digits=3), nyertes_palyazatok_szama= .N),by=c(h, h2)])
    }
    
    
  })
  
  output$distPlot <- renderDataTable({
    
    dd()
  }, options = list(orderClasses = TRUE, lengthMenu = c(10,50,500,5000, 10000, 100000, 150000, 1000000), pageLength = 5))
  
  
  
  output$downloadData <- downloadHandler(
    
    filename = 'data.csv' , content = function(file) {
      
      write.csv(dd(), file,  row.names = FALSE,  fileEncoding = "UTF-8")
    }
  )
  
  
})






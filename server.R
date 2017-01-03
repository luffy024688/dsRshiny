
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(jiebaRD)
library(jiebaR)
library(dplyr)
library(RCurl)
library(rjson)
library(plyr)
library(wordcloud)




pertype<-left_join(df.person.type,df.type,by="type")
colnames(df.match)[3]<-"matchtext"
colnames(df.match)[2]<-"matchtype"
pertypematch<-left_join(pertype,df.match,by="type")



shinyServer(function(input, output){
  output$type<-renderTable({
  if(input$name %in% pertype$name)
  {pertype[grepl(input$name,pertype$name,fixed = T),]}
  else
    {df.type}  
  })
  
  # output$matchlist<-renderTable({
  #   if(input$name=="請輸入"|input$name==""){data.frame()}
  #   else if(which(input$name %in% pertype$name))
  #   {mt<-pertypematch[grepl(input$name,pertypematch$name,fixed = T),]%>%select(matchtype)
  #     colnames(mt)[1]<-"type"
  #     aa<-semi_join(df.person.type,mt)%>%group_by(type)
  #     aa<-sample_n(aa,1)
  #     
  #       }
  # })
  
  # output$matchtitle<-renderText({
  #   if(input$name=="請輸入"|input$name==""){print("")}
  #   else if(which(input$name %in% pertype$name))
  #   {print("配對說明:")}
  #   
  # })
  
  output$match<-renderTable({
    if(input$name %in% pertype$name)
    {bb<-pertypematch[grepl(input$name,pertypematch$name,fixed = T),]%>%select(matchtype,matchtext)
      mt<-pertypematch[grepl(input$name,pertypematch$name,fixed = T),]%>%select(matchtype)
      colnames(mt)[1]<-"type"
      aa<-semi_join(df.person.type,mt)%>%group_by(type)
      aa<-sample_n(aa,1)
      colnames(aa)[2]<-"matchtype"
      colnames(aa)[1]<-"Ex."
      cc<-left_join(bb,aa,by="matchtype")%>%filter(!is.na(Ex.))
      type_explain<-df.type[c("type","sub")]
      colnames(type_explain)[1]<-"matchtype"
      dd<-right_join(type_explain,cc,by="matchtype")
      dd
    }
    else{data.frame()}
  })
  output$wordcloud <- renderPlot({
    if(input$name=="請輸入"|input$name==""){
      b <- result %>% filter(name=="林德昌")
      a<-filter(b,!is.na(message))%>%select(message)
      mixSeg<-worker()
      as <- segment(a$message, mixSeg)
      as<-table(as)
      aswodf<-data.frame(as)
      aswodf<-arrange(aswodf,desc(Freq))
      colnames(aswodf)[1]<-"word"
      aswodf<-aswodf[-c(1:3),]
      par(family = "STHeiti")
      palBlues <- brewer.pal(9, "Blues")
      palBlues <- palBlues[-(1:2)]
      palRd <- brewer.pal(9, "PuRd")
      palRd <- palRd[-(1:2)]
      wordcloud(
        aswodf$word,
        aswodf$Freq,
        scale = c(8, .3),
        min.freq = 2,
        max.words = 100,
        random.order = T,
        rot.per = .15,
        colors = palBlues
      )
    }
    else if(input$name %in% pertype$name){
    b <- result %>% filter(name==input$name)
    a<-filter(b,!is.na(message))%>%select(message)
    mixSeg<-worker()
    as <- segment(a$message, mixSeg)
    as<-table(as)
    aswodf<-data.frame(as)
    aswodf<-arrange(aswodf,desc(Freq))
    colnames(aswodf)[1]<-"word"
    aswodf<-aswodf[-c(1:3),]
    par(family = "STHeiti")
    palBlues <- brewer.pal(9, "Blues")
    palBlues <- palBlues[-(1:2)]
    palRd <- brewer.pal(9, "PuRd")
    palRd <- palRd[-(1:2)]
    wordcloud(
      aswodf$word,
      aswodf$Freq,
      scale = c(8, .3),
      min.freq = 2,
      max.words = 100,
      random.order = T,
      rot.per = .15,
      colors = palBlues
    )
    }
   
    
    })
  
  
  })

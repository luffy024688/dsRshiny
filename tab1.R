output$wordcloud <- renderPlot({
  b <- result %>% filter(name==input$name1)
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
})
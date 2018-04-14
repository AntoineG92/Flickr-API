function(input, output, session) {
  
  #charge les données des photos correspondant aux mots clés
  source("Flickr.photos.search.R")
  load_pics<-reactive ({
    req(input$keyword)
    key<-input$keyword
    data<-load_data(key,input$dates[1],input$dates[2])
    return(data)
  })
  
  #retient les données correspondant à l'intervalle sélectionné dans l'interface
  pics<-reactive ({
    data<-load_pics()
    tags_vect<-tags_df(data)
    data<-data[ which(data$hour >= input$range_hour[1] & data$hour <= input$range_hour[2]), ]
    if(input$theme != "select_theme"){
      data<-theme_selection(data,input$theme,"")
    }
    if(! is.null(input$theme_tag)){
      for(kw in input$theme_tag){
        data<-theme_selection(data,"",kw)
      }
    }
    
    return(data)
  })
  
  #nombre de photos en fonction de l'heure de la journée
  output$hist_hour<-renderPlot({
    data<-pics()
    agg_hour<-aggregate(data$views,by=list(data$hour),FUN=sum)[2]
    #agg_hour<-data.frame(table(data$hour))
    barplot(agg_hour$x,main = "Nombre de photos vues cumulées par heure", col = "grey",xlab="Heure",ylab="# de photos vues",
            names.arg = seq(1,nrow(agg_hour)) )
  })
  #popularité en fonction du mois de l'année
  output$hist_month<-renderPlot({
    data<-pics()
    agg_month<-aggregate(data$views,by=list(data$month),FUN=sum)[2]
    #agg_month<-data.frame(table(data$month))
    barplot(agg_month$x,main = "Nombre de photos vues cumulées par mois", col = "grey",xlab="Mois",ylab="# de photos vues",
            names.arg = seq(1,nrow(agg_month)) )
  })
  
  #crée une liste des tags à partir des données importées
  tags<-reactive({
    pics_df<-pics()
    tags_list<-tags_df(pics_df)
    return(tags_list)
  })
  
  observeEvent(input$action_theme,{
    tags_vect<-tags()
    updateSelectizeInput(session, "theme_tag", choices=tags()[1:15,1] ,server=TRUE,selected=input$theme_tag)
  })
  
  #word cloud des tags
  output$cloud<-renderPlot({
    word_df<-tags()
    wordcloud(words=word_df$list_tags, freq = word_df$Freq,
              max.words = 50,
              random.order = FALSE,
              colors=brewer.pal(8, "Dark2") )
  },width = "auto", height = "auto")
  #mapping des photos
  data_map<-reactive({
    pics_df<-pics()
    #recentre sur les photos avec coordonnées dans les quantiles (10%,90%)
    pics_df<-subset(pics_df, (latitude > quantile(latitude,0.05)) 
                    & (latitude < quantile(latitude,0.95))
                    & (longitude > quantile(longitude,0.05))
                    & (longitude < quantile(longitude,0.95)) )
    #permet le traçage des utilisateurs
    pics_df<-user_tracking(pics_df)
    #points<-cbind( pics_df$longitude,pics_df$latitude )
    return(pics_df)
  })
  
  output$map<-renderLeaflet({
    if( input$select_map == "trajets" ){
      pics<-data_map()
      m <- leaflet(pics) %>% 
        addTiles() %>%
        addMarkers(data = pics, 
                   lng = ~longitude, 
                   lat = ~latitude,
                   clusterOptions = markerClusterOptions(),
                   popup = ~paste0("<img src = ", url_small, ">"))
      for (i in unique(pics$label)) {
        m <- m %>% 
          addPolylines(data = pics[pics$label == i, ], 
                       lng = ~longitude, 
                       lat = ~latitude,
                       weight=4,
                       color="red")
      }
      m
    }
    else{
      pics_df<-pics()
      pics_df<-subset(pics_df, (latitude > quantile(latitude,0.05)) 
                      & (latitude < quantile(latitude,0.95))
                      & (longitude > quantile(longitude,0.05))
                      & (longitude < quantile(longitude,0.95)) )
      top_pics<-pics_df[order(-pics_df$views),]
      top_pics<-top_pics[1:300,]
      leaflet(top_pics) %>% 
        addTiles() %>% #addProviderTiles(providers$CartoDB.Positron) %>%
        addCircles(data = top_pics, 
                   lng = ~longitude, 
                   lat = ~latitude,
                   radius=~sqrt(views) ,
                   popup = ~paste0("<img src = ", url_small, ">") )
    }
  })
  
}#fin de server

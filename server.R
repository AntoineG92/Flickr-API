library(shiny)
source("Flickr.photos.search.R")

server<-function(input, output, session) {
  
  #créée la carte vierge
  output$map<-renderLeaflet({ leaflet() %>%
     addTiles() %>%
      setView(lng=35,lat=0,zoom=2)})
  
  #charge et retourne les données des photos correspondant aux mots clés
  load_pics<-eventReactive(c(input$keyword,input$dates),{
    req(input$keyword)
    key<-input$keyword
    data<-load_data(key,input$dates[1],input$dates[2])
    leafletProxy("map") %>%
      setView(lng=median(data$longitude),lat=median(data$latitude),zoom=12)
    return(data)
  })
  
  #update et retourne les données correspondant à l'intervalle sélectionné dans l'interface et les différents keywords
  pics<-eventReactive (c(input$keyword,input$range_hour,input$theme,input$theme_tag),{
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
    barplot(agg_hour$x,main = "Nombre de photos vues \n cumulées par heure", col = "blue",xlab="Heure",ylab="# de photos vues",
            names.arg = seq(1,nrow(agg_hour)) ,border=NA)
  })
  #popularité en fonction du mois de l'année
  output$hist_month<-renderPlot({
    data<-pics()
    agg_month<-aggregate(data$views,by=list(data$month),FUN=sum)[2]
    #agg_month<-data.frame(table(data$month))
    barplot(agg_month$x,main = "Nombre de photos vues \n cumulées par mois",xlab="Mois",ylab="# de photos vues",
            names.arg = seq(1,nrow(agg_month)),col = '#00DD00',border = 'white')
  })
  
  #crée une liste des tags à partir des données importées
  tags<-reactive({
    pics_df<-pics()
    tags_list<-tags_df(pics_df)
    return(tags_list)
  })
  
  # mise à jour de la map quand le tag séléctionné est modifié
  observeEvent(input$keyword,{
    updateSelectizeInput(session, "theme_tag", choices=tags()[,1] ,server=TRUE)
    leafletProxy("map")
  })
  
  #word cloud des tags
  output$cloud<-renderPlot({
    word_df<-tags()
    par(mar = rep(0, 4))
    wordcloud(words=word_df$list_tags, freq = word_df$Freq,
              max.words = 50,
              random.order = FALSE,
              colors=brewer.pal(8, "Dark2") )
  },width = 220, height = 220)
  
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
 
  #update la map en fonction du button selectionné
  observe({
    
    if(! is.null(input$select_map) ){
      if( input$select_map == "trajets" ){
        pics<-data_map()
        m <- leafletProxy("map",data=pics) %>% clearShapes() %>%
          addMarkers(data = pics, 
                     lng = ~longitude, 
                     lat = ~latitude,
                     clusterOptions = markerClusterOptions(), layerId = ~id,
                     popup = ~paste0("<img src = ", url_small, ">") )
        for (i in unique(pics$label)) {
          m <- m %>% 
            addPolylines(data = pics[pics$label == i, ], 
                         lng = ~longitude, 
                         lat = ~latitude,
                         weight=4, layerId = ~id,
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
        leafletProxy("map",data=top_pics) %>% clearShapes() %>%
          addCircles(data = top_pics, 
                     lng = ~longitude, 
                     lat = ~latitude,
                     radius=~sqrt(views) ,
                     popup = ~paste0("<img src = ", url_small, ">"), layerId = ~id )
      }
    }
  })
  
}#fin de server

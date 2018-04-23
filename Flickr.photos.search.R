#####################################################
# R code to query Flickr API and build a dataframe
# adapted from Mauricion Alarcon https://rpubs.com/rmalarc/74406
#####################################################

#packages pour les fonctions de recherche
library(RCurl)
library(XML)
library(httr)
library(httpuv)
library(lubridate)
library(ngram)
library(sp)
library(geosphere)

load_data<-function(keyword,date_min,date_max){
  
  #remplacer les espaces par des +
  keyword<-gsub(" ","+",keyword)
  
  if(keyword==""){
    return(data.frame("id"=0,"owner"=0,"datetaken"=0,"tags"=0,"longitude"=0,"latitude"=0,"views"=0,"page"=0,"hour"=0) )
  }

#permissions
api_key<-"6e04d787e6bac178fbf3adff28ffe7b5" 
baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,sep="")   #set base URL

pics<-NULL                              #creates empty object to store the data

year<-seq(year(date_min),year(date_max),1)
month_start<-month(date_min)
month_end<-month(date_max)
woeid<-"1"
hasgeo<-"1"                             #only return pictures that are geotagged
extras<-"date_taken,geo,tags,views,url_q"     #extra information to download
perpage<-"250"                           #number of results to return per page
format<-"rest"                          #format of results
sort_photos<-"relevance"
radius<-"32"

#API only returns 400 results per query so it is neccessary to loop through months to obtain all the results

mindate<-""
maxdate<-""

print(paste("keyword is:",keyword))

 for (y in 1:length(year)){                     #creates object dates
      for (m in ifelse(y==1,month_start,1):ifelse(y==length(year),month_end,12)){ daymin<-"01"
           ifelse ((m==4|m==6|m==9|m==11), daymax<-"30",daymax<-"31")
           if (m==2){
           ifelse (year[y]==2008|year[y]==2012|year[y]==2016, daymax<-29,daymax<-28)
                    }
           ifelse (m==10|m==11|m==12,
                    mindate<-as.character(paste(year[y],m,daymin,sep="-")),
                      mindate<-as.character(paste(year[y],paste("0",m,sep="")
                      ,daymin,sep="-")))
           ifelse (m==10|m==11|m==12,
                   maxdate<-as.character(paste(year[y],m,daymax,sep="-")),
                      maxdate<-as.character(paste(year[y],paste("0",m,sep="")
                      ,daymax,sep="-")))
           
          print( paste( "month ",m,"and year ",year[y]) )
           total_pages<-1

           pics_tmp<-NULL

           # loop through pages of photos and save the list in a DF
           for(i in c(1:total_pages)){

                #define the path witht the selected filters  
                getPhotos<-paste(baseURL,"&text=",keyword,
                                "&min_taken_date=",mindate,"&max_taken_date=",maxdate,
                                "&format=rest","&has_geo=1",
                                "&per_page=",perpage,
                                "&extras=",extras,
                                "&has_geo=1","&page=",i,sep="")
               
                 #get the XML tree of the path above, and find the root of the tree
                 getPhotos_data <- xmlRoot(xmlTreeParse(getURL
                  (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr")
                  ,useInternalNodes = TRUE ))

                  #get the data by specifying the nodes of the tree where the information is
                  #xmlGetAttr allows to retrieve the value of each attribute in a specific node
                  id<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"id")                 #extract photo id
                  owner<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"owner")           #extract user id
                  datetaken<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"datetaken")   #extract date picture was taken
                  tags<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"tags")            #extract tags
                  latitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"latitude")    #extract latitude
                  longitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"longitude")  #extract longitude
                  views<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"views")
                  url_small<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"url_q")
                  
                  #build the dataframe
                  tmp_df<-data.frame(cbind(id,owner,datetaken,tags,latitude,longitude,views,url_small),stringsAsFactors=FALSE)

                  tmp_df$page <- i
                  pics_tmp<-rbind(pics_tmp,tmp_df)
           }
                  pics<-rbind(pics,pics_tmp)
      }#end month loop
 }#end year loop

#remove duplicates
pics<-pics[!duplicated(pics$id),]
#####Date#####
pics$month<-month(ymd_hms(pics$datetaken),label=F)   #create variable month
pics$year<-year(ymd_hms(pics$datetaken))             #create variable year
pics$hour<-hour(ymd_hms(pics$datetaken))             #create variable hour
####Coordinates####
options(digits=9)
pics$latitude<-as.numeric(pics$latitude)              #set latitude as numeric
pics$longitude<-as.numeric(pics$longitude)            #set longitude as numeric
pics<-reframe_map(pics,50)
####Views####
pics$views<-as.numeric(pics$views)
###format to char####
pics$owner<-sapply(pics$owner,paste)
pics$id<-sapply(pics$id,paste)
pics$tags<-sapply(pics$tags,paste)
pics$datetaken<-as.POSIXct(as.character(pics$datetaken),format="%Y-%m-%d %H:%M:%S")

return(pics)
} #end of function load_data

#On cherche les tags les plus populaires. Empile tous les tags dans une liste sur l'ensemble des observations
tags_df<-function(pics){
  list_tags=c()
  for(i in 1:nrow(pics)){
    for(j in 1:wordcount( paste( pics$tags[i]) ) ){
      list_tags<-c(list_tags,strsplit( paste(pics$tags[i])," ")[[1]][j])
    }
  }
  #cherche les mots les plus représentés
  list_tags<-data.frame(list_tags)
  count_tags<-table(list_tags)
  count_tags<-data.frame(count_tags)
  count_tags<-count_tags[order(-count_tags$Freq),]
  #supprime le premier mot de la liste, équivalent au mot clé
  count_tags<-count_tags[2:nrow(count_tags),]
  count_tags$list_tags<-as.character(count_tags$list_tags)
  return(count_tags)
}

#Tracage du chemin des photographes à l'aide de leur coordonnées GPS
user_tracking<-function(pics){
  if (is.null(pics)){
    return()
  }
  photographers<-unique(pics$owner)              #ID photographers
  final<-list()                                 #Create OJB to store results
  for(i in 1:length(photographers)){            
    temp<-subset(pics,owner==photographers[i])     #Subset by photographer
    temp<-temp[order(temp$datetaken),]                   #Order by time
    breakpoints <- c(FALSE,diff(temp$datetaken)>10*60)    #Establish break points if diff > 8min
    temp$label <- as.numeric(cut.POSIXt(temp$datetaken, c(min(temp$datetaken), 
                                                          temp[breakpoints, "datetaken"], max(temp$datetaken)+1)))    #Convert factor to label
    temp$label<-paste(temp$owner,temp$label,sep="")  #Concatenate label with username
    final[[i]]<-temp                              #Store temp data
  }
  final<-do.call("rbind", final)
  
  count<-1
  realFinal<-list()
  id<-unique(final$label)
  for(i in 1:length(id)){
    temp<-subset(final,label==label[i])
    if(diff(range(temp$longitude))<0.005 & diff(range(temp$latitude))<0.005){
      realFinal[[count]]<-temp
      count<-count+1
    }
  }
  realFinal<-do.call("rbind", realFinal)
  realFinal<-subset(realFinal,select=-c(tags,views))
  
  return(realFinal)
}#end fct

#renvoie un dataframe de photos correspondant au theme considéré
theme_selection<-function(pics,theme,tag){
  if(theme=="night"){
    return(pics[which(pics$hour > 20),])
  }
  if(theme=="black & white"){
    bw_df=data.frame()
    bw_words=list("blackandwhite","bw","b&w","monochrome")
    for(word in bw_words){
      bw_df<-rbind(bw_df,pics[which(grepl(word,as.character(pics$tags))),])
    }
    bw_df<-bw_df[!duplicated(bw_df),]
    return( bw_df )
  }
  if(theme=="street"){
    kw_df=data.frame()
    keywords=list("street","streetphotography","urban")
    for(word in keywords){
      kw_df<-rbind(kw_df,pics[which(grepl(word,as.character(pics$tags))),])
    }
    kw_df<-kw_df[!duplicated(kw_df),]
    return( kw_df )
  }
  if(theme=="sunset"){
    kw_df=data.frame()
    keywords=list("sunset")
    for(word in keywords){
      kw_df<-rbind(kw_df,pics[which(grepl(word,as.character(pics$tags))),])
    }
    kw_df<-kw_df[!duplicated(kw_df),]
    return( kw_df )
  }
  if(theme=="sunrise"){
    kw_df=data.frame()
    keywords=list("sunrise")
    for(word in keywords){
      kw_df<-rbind(kw_df,pics[which(grepl(word,as.character(pics$tags))),])
    }
    kw_df<-kw_df[!duplicated(kw_df),]
    return( kw_df )
  }
  if(tag != ""){
    return( pics[which( grepl( tag, as.character(pics$tags) ) ),] )
  }
  kw_df=data.frame()
  kw_df<-rbind(kw_df,pics[which(grepl(theme,as.character(pics$tags))),])
  kw_df<-kw_df[!duplicated(kw_df),]
  return( kw_df )
  
  
  return(pics)
}

reframe_map<-function(pics,radius){
  centroid_long<-rep(1,nrow(pics))*median(pics$longitude)
  centroid_lat<-rep(1,nrow(pics))*median(pics$latitude)
  centroid_cd<-cbind(centroid_long,centroid_lat)
  pics$dist_centre<-diag(distm(x=centroid_cd,
                               y=pics[,c('longitude','latitude')], fun = distGeo) )
  return( pics[ which(pics$dist_centre < radius*1000) ,  ] )
}



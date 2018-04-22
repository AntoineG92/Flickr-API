#packages de viz
library(shiny)
library(shinyWidgets)
library(wordcloud)
library(tm)
library(memoise)
library(leaflet)
library(lubridate)


navbarPage("Flickr API", id="nav",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
           leafletOutput("map",height="1000px",width="100%"),  
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 150, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",h2("Data Exploration"),
                    dateRangeInput("dates", label = "Période",start=today()-365, end=today() ),
                    searchInput(
                      inputId = "keyword", label = "Entrer mots clés",
                      placeholder = "A city, theme, etc...",
                      btnSearch = icon("search"),
                      btnReset = icon("remove"),
                      width = "450px"),
                    h3("Filtres"),
                    sliderInput( "range_hour",label="Heure",min=0,max=23,value=c(0,23) ),
                    selectInput("theme",label="Sélectionne thème", 
                                choices = list("black & white"="black & white","night"="night","street"="street","sunset"="sunset","sunrise"="sunrise","--selection--"="select_theme"),
                                selected = "select_theme"),
                    selectizeInput('theme_tag','Sélectionne tag',choices=NULL,multiple=TRUE,selected=NULL),
                    radioButtons("select_map",label=h3("Map"),choices = c("Trajets"="trajets","Les plus vues"="vues"),
                                 selected="vues")
                          ),#end main panel
              absolutePanel(class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, bottom=20,left=20,width="250px",height="auto",style="opacity:0.70",h2("Visualisation"),
              plotOutput("cloud",height="240px",width="240px"),
              plotOutput("hist_hour",height="200px",width="240px"),
              plotOutput("hist_month",height="200px",width="240px") )#end second panel


)#end navbarPage
#end UI

#shinyApp(ui = ui, server = server)
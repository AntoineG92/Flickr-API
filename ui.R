fluidPage(headerPanel("Flickr interface"),
                sidebarLayout(
                  sidebarPanel(
                    dateRangeInput("dates", label = "Période"),
                    br(),
                    searchInput(
                      inputId = "keyword", label = "Entrer mots clés",
                      placeholder = "A placeholder",
                      btnSearch = icon("search"),
                      btnReset = icon("remove"),
                      width = "450px"),
                    br(),
                    sliderInput( "range_hour",label="Heure",min=0,max=23,value=c(0,23) ),
                    br(),
                    selectInput("theme",label="Sélectionne thème", 
                                choices = list("black & white"="black & white","night"="night","street"="street","sunset"="sunset","sunrise"="sunrise","--selection--"="select_theme"),
                                selected = "select_theme"),
                    selectizeInput('theme_tag','Sélectionne tag',choices=NULL,multiple=TRUE),
                    actionButton("action_theme",label="Update themes")
                  ),
                  mainPanel(
                    plotOutput("cloud"),
                    column(6,plotOutput("hist_hour", height = "300px") ),
                    column(6,plotOutput("hist_month",height = "300px") )
                  )),
                radioButtons("select_map",label=h3("Map"),choices = c("Photos & trajets"="trajets","Les plus vues"="vues"),
                             selected="trajets"),
                leafletOutput("map",height=600)
)#end fluidPage
#end UI
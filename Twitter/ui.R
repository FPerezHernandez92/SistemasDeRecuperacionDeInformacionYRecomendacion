
library(shiny)

shinyUI(fluidPage(
  headerPanel("Analisis sentimientos Twitter - DATCOM 2017"),
  # Getting User Inputs
  sidebarPanel(
    textInput ("searchTerm","Introduce el Hastag por el cual quiere realizar la busqueda '#'",  "#Brexit"),
    sliderInput("maxTweets","Número de tweets utilizados para el análisis",min=5,max=1000,value=100), # The max can, of course, be increased
    submitButton(text="Analizar")    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Top Trending Tweets de hoy",	
               selectInput("trendingTable","Elige la localización para extraer los  trending tweets", c("Worldwide" ,  "Abu Dhabi" ,"Acapulco" , "Accra" , "Adana" , "Adela", "Aguascalientes" , 
                                                                                           "Ahmedabad" ,  "Ahsa" , "Albuquerque" , "Alexandria" , "Algeria" , "Algiers" , "Amman" , "Amritsar" , "Amsterdam",  "Ankara" , "Ansan" , "Antalya" , "Antipolo" , 
                                                                                           "Argentina" ,  "Athens" ,  "Atlanta" ,             "Auckland" ,            "Austin" ,              "Australia" ,    "Austria"  , "Bahrain"     , "Baltimore"  ,         
                                                                                           "Bandung"   ,           "Bangalore" ,           "Bangkok",              "Barcelona" ,           "Barcelona",            "Barquisimeto",         "Barranquilla"  ,      
                                                                                           "Baton Rouge" ,         "Bekasi"    ,           "Belarus",            "Belfast"  ,            "Belgium"     ,         "Belo Horizonte",      
                                                                                           "Benin City"  ,         "Bergen"    ,           "Berlin" ,              "Bhopal"    ,           "Bilbao"   ,            "Birmingham"  ,         "Birmingham"    ,      
                                                                                           "Blackpool"   ,         "Bogota"    ,           "Bologna",              "Bordeaux"  ,           "Boston"   ,            "Bournemouth" ,         "Brasilia"      ,      
                                                                                           "Brazil"      ,         "Bremen"    ,           "Brest"  ,              "Brighton"  ,           "Brisbane" ,            "Bristol"     ,         "Bucheon"       ,      
                                                                                           "Buenos Aires",         "Bursa"     ,           "Busan"  ,              "Cagayan de Oro" ,      "Cairo"    ,            "Calgary"     ,         "Cali"      ,          
                                                                                           "Calocan"     ,         "Campinas"  ,           "Can Tho",              "Canada"    ,           "Canberra"  ,           "Cape Town"   ,         "Caracas"   ,          
                                                                                           "Cardiff"     ,         "Cebu City" ,           "Changwon" ,            "Charlotte" ,           "Chelyabinsk" ,         "Chennai"     ,         "Chiba"     ,          
                                                                                           "Chicago"     ,         "Chihuahua" ,           "Chile"    ,            "Cincinnati",           "Ciudad Guayana" ,      "Ciudad Juarez",        "Cleveland" ,          
                                                                                           "Cologne"     ,         "Colombia"  ,           "Colorado Springs",     "Columbus"  ,           "Concepcion" ,          "Cordoba"      ,        "Cork"      ,          
                                                                                           "Coventry"    ,         "Culiacan"  ,           "Curitiba"    ,         "Da Nang"   ,           "Daegu"      ,          "Daejeon"      ,        "Dallas-Ft. Worth" ,   
                                                                                           "Dammam"  , "Darwin" ,"Davao City", "Delhi", "Den Haag" , "Denmark" ,"Denver" ,  "Depok" , "Derby" , "Detroit" , "Diyarbakir" , "Dnipropetrovsk" ,"Dominican Republic",
                                                                                           "Donetsk", "Dortmund"  ,           "Dresden" ,             "Dubai"         ,       "Dublin"      ,         "Durban" , "Dusseldorf"    ,       "Ecatepec de Morelos", 
                                                                                           "Ecuador"       ,       "Edinburgh" ,           "Edmonton"      ,       "Egypt"       ,         "El Paso"      ,        "Eskisehir"     ,       "Essen"    ,           
                                                                                           "Faisalabad"    ,       "Fortaleza"  ,          "France"        ,       "Frankfurt"   ,         "Fresno"       ,        "Fukuoka"       ,       "Galway"   ,           
                                                                                           "Gaziantep"    ,        "Gdansk"      ,         "Geneva"       ,        "Genoa"       ,         "Germany"      ,        "Ghana"         ,       "Giza"     ,           
                                                                                           "Glasgow"      ,        "Goiania"     ,         "Gomel"        ,        "Gothenburg"  ,         "Goyang"       ,        "Greece"        ,       "Greensboro" ,         
                                                                                           "Grodno"       ,        "Guadalajara" ,         "Guarulhos"    ,        "Guatemala"   ,         "Guatemala City"  ,     "Guayaquil"     ,       "Gwangju"  ,           
                                                                                           "Hai Phong"    ,        "Haifa"       ,         "Hamamatsu"    ,        "Hamburg"     ,         "Hanoi"      ,          "Harrisburg"    ,       "Hermosillo"     ,     
                                                                                           "Hiroshima"    ,        "Ho Chi Minh City" ,    "Honolulu"     ,        "Houston"     ,         "Hull"       ,          "Hulu Langat"   ,       "Hyderabad"      ,     
                                                                                           "Ibadan"       ,        "Incheon"      ,        "India"        ,        "Indianapolis",         "Indonesia" ,           "Indore"        ,       "Ipoh"           ,     
                                                                                           "Ireland"      ,        "Irkutsk"       ,       "Israel"       ,        "Istanbul"    ,         "Italy"     ,           "Izmir"         ,       "Jackson"        ,     
                                                                                           "Jacksonville" ,        "Jaipur"        ,       "Jakarta"      ,        "Japan"       ,         "Jeddah"    ,           "Jerusalem"     ,       "Johannesburg"   ,     
                                                                                           "Johor Bahru"  ,        "Jordan"        ,       "Kaduna"       ,        "Kajang"      ,         "Kano"      ,           "Kanpur"        ,       "Kansas City"    ,     
                                                                                           "Karachi"      ,        "Kawasaki"      ,       "Kayseri"      ,        "Kazan"       ,         "Kenya"     ,           "Khabarovsk"    ,       "Kharkiv"        ,     
                                                                                           "Kitakyushu"   ,        "Klang"         ,       "Kobe"         ,        "Kolkata"      ,        "Konya"     ,           "Korea"         ,       "Krakow"         ,     
                                                                                           "Krasnodar"    ,        "Krasnoyarsk"   ,       "Kuala Lumpur" ,        "Kumamoto"    ,         "Kumasi"    ,           "Kuwait"        ,       "Kyiv"           ,     
                                                                                           "Kyoto" ,               "Lagos"    ,            "Lahore"       ,        "Las Palmas",           "Las Vegas"   ,         "Latvia" ,              "Lausanne"       ,     
                                                                                           "Lebanon" ,              "Leeds"   ,             "Leicester"   ,      "Leipzig" ,             "Leon"        ,         "Lille"  ,              "Lima" ,               
                                                                                           "Liverpool" ,           "Lodz"     ,            "London"      ,         "Long Beach" ,          "Los Angeles"  ,        "Louisville"       ,    "Lucknow"  ,           
                                                                                           "Lviv"       ,          "Lyon"          ,       "Madrid"       ,        "Makassar"    ,         "Makati"      ,         "Malaga"          ,     "Malaysia"  ,          
                                                                                           "Manaus"     ,          "Manchester"  ,         "Manila"       ,        "Maracaibo"   ,         "Maracay"   ,           "Marseille"     ,       "Maturin"   ,   
                                                                                           "Mecca"       ,         "Medan"      ,          "Medellin"      ,       "Medina"       ,        "Melbourne"  ,          "Memphis"      ,        "Mendoza"    ,         
                                                                                           "Merida"       ,        "Mersin"    ,           "Mesa"           ,      "Mexicali"      ,       "Mexico"    ,           "Mexico City" ,         "Miami"       ,        
                                                                                           "Middlesbrough" ,       "Milan"    ,            "Milwaukee"       ,     "Minneapolis"    ,      "Minsk"      ,          "Mombasa"    ,          "Monterrey"    ,       
                                                                                           "Montpellier"     ,     "Montreal" ,            "Morelia"           ,   "Moscow"           ,    "Multan"    ,           "Mumbai"     ,          "Munich" ,     
                                                                                           "Murcia"  ,             "Muscat" ,              "Nagoya"       ,    "Nagpur"            ,   "Nairobi"  ,            "Nantes"    ,    "Naples" , "Nashville" ,
                                                                                           "Netherlands",  "New Haven" , "New Orleans" , "New York","New Zealand" , "Newcastle", "Nigeria" , "Niigata" ,"Nizhny Novgorod" , "Norfolk", "Norway", 
                                                                                           "Nottingham"   ,        "Novosibirsk"      ,    "Odesa"         ,       "Okayama"      ,        "Okinawa"         ,     "Oklahoma City"     ,   "Omaha"    ,           
                                                                                           "Oman"          ,       "Omsk"            ,     "Orlando"        ,      "Osaka"        ,        "Oslo"              ,   "Ottawa"           ,    "Pakistan"  ,          
                                                                                           "Palembang"      ,      "Palermo"        ,      "Palma"            ,    "Panama"        ,       "Paris"            ,    "Pasig"           ,     "Patna"      ,         
                                                                                           "Pekanbaru"       ,     "Perm"          ,       "Perth"           ,     "Peru"           ,      "Petaling"        ,     "Philadelphia"   ,      "Philippines" ,        
                                                                                           "Phoenix"    ,          "Pittsburgh"   ,        "Plymouth"  ,           "Poland"          ,     "Port Elizabeth" ,      "Port Harcourt" ,       "Portland"     ,       
                                                                                           "Porto Alegre" ,        "Portsmouth"  ,         "Portugal"   ,          "Poznan"           ,    "Preston"       ,       "Pretoria"     ,        "Providence"    ,      
                                                                                           "Puebla"        ,       "Puerto Rico"    ,      "Pune"        ,         "Qatar"             ,   "Quebec"       ,        "Queretaro"           , "Quezon City"    ,     
                                                                                           "Quito"    ,            "Rajkot"       ,        "Raleigh"     ,         "Ranchi"            ,   "Rawalpindi" ,          "Recife"            ,   "Rennes"         ,     
                                                                                           "Richmond"   ,          "Riga"         ,        "Rio de Janeiro",       "Riyadh"      ,         "Rome"       ,          "Rosario"           ,   "Rostov-on-Don"    ,   
                                                                                           "Rotterdam"  ,          "Russia"     ,          "Sacramento"    ,       "Sagamihara"  ,         "Saint Petersburg",     "Saitama"         ,     "Salt Lake City"  ,    
                                                                                           "Saltillo"     ,        "Salvador"   ,          "Samara"          ,     "San Antonio"   ,       "San Diego"       ,     "San Francisco"   ,     "San Jose"  ,  
                                                                                           "San Luis Potosi",      "Santiago"  ,           "Santo Domingo"    ,  "Sao Paulo"      ,      "Sapporo"        ,      "Saudi Arabia"       , 
                                                                                           "Seattle"   ,           "Semarang"      ,       "Sendai"            ,   "Seongnam"        ,     "Seoul"         ,       "Seville"       ,       "Sharjah"   ,          
                                                                                           "Sheffield" ,           "Singapore"   ,         "Singapore"         ,   "South Africa"    ,     "Soweto"      ,         "Spain"       ,         "Srinagar"  ,          
                                                                                           "St. Louis"  ,          "Stockholm"  ,          "Stoke-on-Trent"     ,  "Strasbourg"       ,    "Stuttgart"  ,          "Surabaya"   ,          "Surat"      ,         
                                                                                           "Suwon"        ,        "Swansea"    ,          "Sweden"     ,          "Switzerland"        ,  "Sydney"     ,          "Taguig"     ,          "Takamatsu"    ,       
                                                                                           "Tallahassee"  ,        "Tampa"    ,            "Tangerang"  ,          "Tel Aviv"           ,  "Thailand" ,            "Thane"    ,            "Thessaloniki" ,       
                                                                                           "Tijuana"        ,      "Tokyo"    ,            "Toluca"       ,        "Toronto"   ,           "Toulouse"          ,   "Tucson"   ,            "Turin"          ,     
                                                                                           "Turkey"    ,           "Turmero"     ,         "Ufa"           ,       "Ukraine"    ,          "Ulsan"            ,    "United Arab Emirates", "United Kingdom"   ,   
                                                                                           "United States" ,       "Utrecht"   ,           "Valencia"      ,       "Valencia"   ,          "Valparaiso"     ,      "Vancouver"   ,         "Venezuela"      ,     
                                                                                           "Vienna"      ,         "Vietnam"   ,           "Virginia Beach"  ,     "Vladivostok"  ,        "Volgograd"      ,      "Voronezh"    ,         "Warsaw"  ,            
                                                                                           "Washington"  ,         "Winnipeg",  "Wroclaw"      ,        "Yekaterinburg",        "Yokohama"  ,  "Yongin",              
                                                                                           "Zamboanga City" ,      "Zapopan",              "Zaporozhye"       ,    "Zaragoza"       ,      "Zurich"  ), selected = "Worldwide", selectize = FALSE), 
               submitButton(text="Buscar"),    HTML("<div><h3> La siguiente tabla muestra los hashtags de 
                                                    tendencias en Twitter de la ubicación que ha elegido. Estos son los temas candentes de hoy! </h3></div>"),
               tableOutput("trendtable"),
               HTML
               ("<div> </div>")),
      tabPanel("WordCloud",HTML("<div><h3>Palabras más utilizadas asociadas con el hashtag</h3></div>"),plotOutput("word"),
               HTML
               ("</br></br></br></br></br></br></br><div><h4> Una nube de palabras es una representación visual de los datos de texto, normalmente utilizados para representar metadatos 
de palabras clave (etiquetas) en sitios web o para visualizar texto en forma libre. 
Este formato es útil para percibir rápidamente los términos más prominentes y para localizar un término 
alfabéticamente para determinar su prominencia relativa.
                 </h4></div>")),
      tabPanel("Histogramas",HTML
               ("<div><h3>Los histogramas representan gráficamente la positividad o negatividad de la opinión de las personas sobre el hashtag
                 </h3></div>"), plotOutput("histPos"), plotOutput("histNeg"), plotOutput("histScore")
               ),
      tabPanel("Pie Chart",HTML("<div><h3>Representación del sentimiento en una escala de 5</h3></div>"), plotOutput("piechart"),HTML
               ("<div><h4> Un gráfico circular es un gráfico estadístico circular, que se divide en rodajas para ilustrar el sentimiento del hashtag. En un gráfico circular, la longitud del arco de cada 
                 rebanada (y consecuentemente su ángulo central y área), es proporcional a la cantidad que representa.</h4></div>")
               ),
      tabPanel("Tabla de tweets",HTML( "<div><h3> Representar el sentimiento en forma tabular en una escala de 5 </h3></div>"), tableOutput("tabledata"),
               HTML ("<div><h4> La tabla muestra el sentimiento (positivo, negativo o neutral) de los tweets asociados con el hashtag de búsqueda 
                     mostrando la puntuación para cada tipo de sentimiento. </h4></div>")),
      tabPanel("Top Tweeters",HTML("<div><h3> Top 20 Tweeters por hashtag </h3></div>"),plotOutput("tweetersplot"), tableOutput("tweeterstable")),
      tabPanel("Emociones",HTML
               ("<div><h3> Analisis de sentimientos sobre el Hastag, clasificado por emociones
                 </h3></div>"), plotOutput("emociones")),
      tabPanel("Popularidad",HTML
               ("<div><h3> Analisis de sentimientos sobre el Hastag, clasificado por popularidad.
                 </h3></div>"), plotOutput("popularidad")),
      tabPanel("Comparativa",HTML
               ("<div><h3> Tipos de Tweets en Android, iPhone y iPad. 
                 </h3></div>"), plotOutput("palabras1"),HTML
              ("</br></br></br></br></br></br></br></br><div><h3> Palabras relevantes para Tweets de Android, iPhone y iPad.
                 </h3></div>"),
              plotOutput("palabras2")),
      tabPanel("Comparativa 2",HTML
               ("<div><h3> Relevancia de palabras para Tweets de Android.
                 </h3></div>"), plotOutput("palabras3"),HTML
               ("</br></br></br></br></br></br></br></br><div><h3> Relevancia de palabras para Tweets de iPhone.
                 </h3></div>"), plotOutput("palabras4"), HTML
               ("</br></br></br></br></br></br></br></br><div><h3> Relevancia de palabras para Tweets de iPad.
                 </h3></div>"), plotOutput("palabras5"), HTML
               ("</br></br></br></br></br></br>")),
      tabPanel("Top Hashtags de usuarios", textInput("user", "Introduce el nombre de usuario", "@CanalUGR"),submitButton(text="Buscar"), plotOutput("tophashtagsplot"), HTML("<div><h3> Hashtag frecuentes en los tweets del twitero
                                                                                                                                                  </h3></div>")  )    
      ))))
# Load required libraries
#install.packages("DT")
library(shiny)
library(shinydashboard)
library(leaflet)
library(DBI)
library(odbc)
library(DT)
library(ggplot2)
library(scales)
options(scipen = 2)

# Read database credentials
# source("./03_shiny_HW1/credentials_v3.R")
source("./credentials_v4.R")


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "PPT"),
  #Sidebar content
  dashboardSidebar(
    #Add sidebar menus here
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("comment-dots")),
      menuItem("Property Information", tabName = "dbquery", icon = icon("keyboard")),
      menuItem("Factors based price", tabName = "tab2", icon = icon("hand-holding-usd")),
      menuItem("Price distribution", tabName = "tab3", icon = icon("chart-line")),
      menuItem("City Map", tabName = "leaflet", icon = icon("map-marked-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      # Add contents for first tab--------------------------HOME------------------------------------------------------
      tabItem(tabName = "Home",
              h1("Welcome To Property Price Tracker ", align="Center") ,
              h4("Find Your Dream Home",align="Center"),
              
              h4("Introduction"),
              h5("The purpose of this Shiny webApp is to better advise clients in analyzing and visualizing the residential property in Dallas"),
              
              tags$h4("Data"),
              h5("Data is regarding housing price from the Dallas area that has 15+ features (eg House size,Zipcode,Location etc) and 5000 observations."),
              tags$br(),
              
              img(src = "https://images.unsplash.com/photo-1568605114967-8130f3a36994?ixlib=rb-4.0.3&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=2070&q=80", height = 400, width = 600, align = "center"),
              tags$br(),
              h4("Listed below are the menu tabs with their functionality "),
              hr(style = "border-top: 1px solid #000000;"), 
              #------------------------------------------SAHAR HOME --------------------------------------------------------
              h4("Property Information"),
              "This tab will help customers create and modify data property details",
              #-----------------------------------SAHAR HOME END----------------------------------------------------
              
              h4("Factors based price"),
              "This tab will help customers customize and filter house parameter and view the price",
              
              h4("Price distribution"),
              "This tab will help customers view price distribution based on zipcode",
              
              h4("City Map"),
              "This tab will help customers View Home Locations and Pricing",
      ),
      
      ## HOME END--------------------------------------------------------------------------------------------------------
      
      
      # Add contents for second tab----------------------------Sahar Start----------------------------------------------
      tabItem(tabName = "dbquery",
              h3("User Information"),
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           textInput("MLS", h3("Enter MLS No")),
                           
                           
                           textInput("Price", h3("Enter Price ($)")),
                           
                           
                           textInput("Bed", h3("Enter Bedrooms")),
                           
                           textInput("Bath", h3("Enter Bathrooms")),
                           
                           textInput("Sqfeet", h3("Enter Square feet")),
                           
                           textInput("Lotsize", h3("Enter Lotsize")),
                           
                           textInput("Address", h3("Enter Address")),
                           
                           
                           selectInput("up_del", label = h3("Modify or Delete Information"), 
                                       choices = list("Modify" = 1, "Delete" = 2, "Create" = 3), 
                                       selected = 1),
                           
                           
                           
                           actionButton("Go1", "Get results"),
                           
                           h4("The is your search result :"),
                           
                           DT::dataTableOutput("mytable")
                       )
                ))
      ),
      ##------------------------------sahar tab 2 end-----------------------------------------
      ##------------------------------Sumita tab 3 start-----------------------------------------
      
      
      tabItem(tabName = "tab2",
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           textInput("city", h3("Search by City:")),
                           sliderInput("square_feet_slider", label = h3("Range of sqft to search for:"), min = 1000, 
                                       max = 10000, value = c(1000,30000)),
                           sliderInput("price_slider", label = h3("Range of Price($) to search for:"), min = 10000, 
                                       max = 1000000, value = c(10000,1000000)),
                           sliderInput("beds_slider", label = h3("Number of bedrooms:"), min = 1, 
                                       max = 16, value = c(1,16)),
                           sliderInput("baths_slider", label = h3("Number of bathrooms:"), min = 1, 
                                       max = 10, value = c(1,10)),
                           actionButton("Go", "Get results"),
                           h2("The is your search result :"),
                           hr(),
                           DT::dataTableOutput("mytable1")
                       )
                ))
      ),
      
      ##SUMITA end---------------------------
      
      
      tabItem(tabName = "tab3",
              h3("Check price distribution based on zipcode"),
              #("Distribution graph showing median price/zipcode wise"),
              
              textInput("zip", label = h3("Input Zip code:"),value = 75206),
              actionButton("Go2", "Show distribution"), 
              
              #selectInput("zip",
              # label = "ZIP CODE",
              #choices = zip$zipcode,
              # selected = 75206,
              #  multiple = TRUE)
              br(),
              br(),
              mainPanel(plotOutput(outputId = "histogram"))
              
      ),
      
      
      # Luna tab
      tabItem(tabName = "leaflet",
              uiOutput('inputSelect'),
              leafletOutput("mymap")
      )
      
    )
  )
)

server <- function(input, output) {
  
  #Develop your server side code (Model) here
  observeEvent(input$Go1, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    # browser()
    
    
    ### CRUD _---------------------------------Sahar start-----------------
    ###MODIFY-------------------------------------------------
    if (input$up_del == 1)
    {
      query <- paste("UPDATE Property SET Price =",input$Price, ", BEDs = ",input$Bed ,", BATHs = ",input$Bath ,",Square_feet = ",input$Sqfeet ,", Lot_size = ",input$Lotsize ,", Address = ","'",input$Address,"'", " WHERE MLS =", input$MLS ,";")
      data <- dbGetQuery(db, query)
      print(query)
      
      query2 <- paste("SELECT TOP 1 * FROM  Property WHERE MLS =",input$MLS, "AND Price = ",input$Price ," AND BEDs =", input$Bed ,"AND BATHs = ",input$Bath ,"AND Square_feet = ",input$Sqfeet,"AND Lot_size = ",input$Lotsize,"AND Address = ","'",input$Address,"'",";")
      data2 <- dbGetQuery(db, query2)
      print(query2)
      
      output$mytable = DT::renderDataTable({
        data2
      })
      
    }
    ##delete---------------------------------------------------------------------------------
    else if (input$up_del == 2){
      
      query <- paste("DELETE FROM Property WHERE MLS =",input$MLS,";")
      data <- dbGetQuery(db, query)
      print(query)
      # Submit the fetch query and disconnect
      
      output$mytable = DT::renderDataTable({
        data
      })
      
    }
    
    #create---------------------------------------------------------------------------------------
    
    else if (input$up_del == 3){
      
      query <- paste0("INSERT INTO Property
           (
 MLS,
 Price ,
 BEDs ,
 BATHs ,
 Square_feet ,
 Lot_size ,
 Latitude ,
 Longitude,
 Address )
     VALUES
           ( ",input$MLS,", ",(input$Price),",",input$Bed,",
       ",input$Bath,", ",input$Sqfeet,",
       
       ",input$Lotsize,", 32.779167,-96.808891,
       
       ","'",input$Address,"')")
      data <- dbGetQuery(db, query)
      print(query)
      
      
      
      
      output$mytable = DT::renderDataTable({
        data
      })
      
    }
    else {
      
      print("nothing selected")
      
    }
    
    
    ## MLS =",input$MLS, "AND Price = ",input$Price ," AND BEDs =", input$Bed ,"AND BATHs = ",input$Bath ,"AND Square_feet = ",input$Sqfeet  
    
    ##------------------------------Sahar END----------------------------------
    
    
  })
  
  
  
  
  
  ##---------------SUMITA start--------------------------------------------
  
  
  observeEvent(input$Go, {
    output$mytable1 <- renderDataTable({
      # open DB connection
      db <- dbConnector(
        server   = getOption("database_server"),
        database = getOption("database_name"),
        uid      = getOption("database_userid"),
        pwd      = getOption("database_password"),
        port     = getOption("database_port")
      )
      
      on.exit(dbDisconnect(db), add = TRUE)
      
      ## select city, address, price, beds, baths, square_feet from address INNER JOIN property 
      # 
      # ON address.MLS = property.MLS
      # where city like '%'
      # order by city;
      
      
      query <-  paste0("select city, address, price, beds, baths, square_feet from address INNER JOIN property 
    ON address.MLS = property.MLS
    where city like '%",input$city,"%'
                     AND  square_feet  between ", input$square_feet_slider[1]," AND ",input$square_feet_slider[2],"
                    AND price  between ", input$price_slider[1]," AND ",input$price_slider[2],"
                     AND  beds  between ", input$beds_slider[1]," AND ",input$beds_slider[2],"
                     AND  baths  between ", input$baths_slider[1]," AND ",input$baths_slider[2], ";", sep=""
                       
                       
                       
      )
      
      print(query)
      data <- dbGetQuery(db,query)
      
      
      
      output$mytable1 <- DT::renderDataTable({
        data
        
      })
      
      
    })
    
  })
  
  
  ##Sumita end------------------------------------------------------
  
  
  ### april start----------------------------------------
  observeEvent(input$Go2, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    
    addprice <- dbGetQuery( conn=db, statement='select a.ZipCode, s.Price from Address as a inner join Sales_Info as s on a.MLS=s.MLS where a.ZipCode = ?', params = input$zip)
    output$histogram <- renderPlot({
      ggplot(data=addprice, aes(Price)) + geom_histogram(fill='#B92D79',color="#e9ecef",alpha=0.9)+ggtitle("Price Distribution by Zip Code") +xlab("House Prices($)")+ ylab("Frequency")+
        theme(plot.title = element_text(size=12)
              
        )
    })
    
  })
  
  ##april end---------------------------------
  
  
  ##Luna's code start here: searching map 
  observeEvent(input, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    
    output$inputSelect <- renderUI({
      selectInput('pick_assetclass',
                  label ='Select City',
                  choices=allCities,
                  selected = NULL, multiple = FALSE,width="450px")
    })
    allCities <- dbGetQuery(conn=db, statement='SELECT City FROM Address')
  })
  
  
  observeEvent(input$pick_assetclass, {
    # open DB connection
    db <- dbConnector(
      server   = getOption("database_server"),
      database = getOption("database_name"),
      uid      = getOption("database_userid"),
      pwd      = getOption("database_password"),
      port     = getOption("database_port")
    )
    on.exit(dbDisconnect(db), add = TRUE)
    
    if (input$pick_assetclass == '') {
      city <- 'Crandall'
    } else {
      city <- input$pick_assetclass
    }
    
    LocationSets <- dbGetQuery(conn=db, statement='SELECT p.Longitude,p.Latitude,p.Price,a.City FROM Property_details AS P INNER JOIN Address as A on P.Address_id = A.Address_id where A.City = ?', params = city)
    
    points <- eventReactive(input$recalc, {
      cbind(LocationSets$Longitude, LocationSets$Latitude)
    }, ignoreNULL = FALSE)
    
    
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        addMarkers(data = points(), label = paste0("Price:", "$",LocationSets$Price)) %>%
        addLayersControl(baseGroups = c("OSM (default)", "Toner", "Toner Lite", "Positron"), options = layersControlOptions(collapsed = FALSE))
    })
  })
  ##Luna's code end here
  
}

shinyApp(ui, server)


library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinythemes)
library(shiny.semantic)
library(tidyverse)
library(DT)
library(leaflet)
library(plotly)
library(shiny)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinyBS)
library(slickR)
library(dashboardthemes)
library(stringr)

cuisine= read_csv("chefmozcuisine.csv")
ui = dashboardPage(
  # skin = "green",
  header = dashboardHeader(
    title = shinyDashboardLogo(
    theme = "blue_gradient",
    boldText = "Section 53 ",
    mainText = " Group",
    badgeText = "131"
    )
  ),
  sidebar = dashboardSidebar(
    minified = TRUE,
    collapsed = FALSE,
    sidebarMenu(
      menuItem("Home", tabName = "page1", icon = icon("dashboard")),
      menuItem("Restaruant Map", tabName = "page2", icon = icon("th")),
      menuItem("Ranking", tabName = "page3", icon = icon("area-chart")),
      menuItem("Data Table",tabName = "page4",icon = icon("th")),
      menuItem("Tip Calculator",tabName = "page5",icon = icon("th")),
      menuItem("Customer Profile",tabName = "page6",icon = icon("th")),
      menuItem("Cuisine Profile",tabName = "page7",icon = icon("th"))
    )
  ),
  
  body = dashboardBody(
    
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    
    tabItems(
      
    tabItem(
      tabName = "page1",
      
      fluidRow(
        h1(icon("youtube-play"),"Mexico Food Introduction Video", style = 'font-size:24px;color:black;font-weight:bold;', align="center"),
          column(8, 
                 HTML('<iframe width="100%" height="500"
                  src="https://www.youtube.com/embed/BBT_FLYF65k"
                  frameborder="0" allowfullscreen></iframe>'), align="center"
                 ),
        column(4, slickROutput("slickr", width = "300px"))
        ),
      tags$hr(),
      tags$br(),
      
      fluidRow(
        box(
          title = strong(icon("envelope-open"),"Project Description"),
          solidHeader = TRUE,
          status = "success",width = 12, collapsible = TRUE,
          column(
            12,
            tags$div(
              tags$span(
                p("This project was inspired by the daily dining out experience. 
         From the perspective of consumers, 
         we think about how to allow users to quickly locate their favorite restaurant through the app when they want to eat out. 
         Based on this,the purpose of this project is to design a clear and easy-to-use restaurant map with a strong user experience.")
          )
        )
        )
        )
        ),
      
        fluidRow(
          box(
            title = strong(icon("table"),"Research Questions"),
            solidHeader = TRUE,
            status = "success",width = 12, collapsible = TRUE,
            column(12,
              tags$div(
                tags$span(
                  tags$li("What is the best restaurant when the food type is determined?"),
                  br(),
                  tags$li("What is the information about the target restaurant? " ),
                  br(),
                  tags$li("What is the distribution of restaurants? ")
                )
              )
            )
            )
          ),
      
      fluidRow(
        box(
          title = strong(icon("table"),"Data Source"),
          solidHeader = TRUE,
          status = "success",width = 12, collapsible = TRUE,
          column(12,
                 tags$div(
                   tags$span(
                     tags$li("Data Information:"),
                     tags$p("The dataset 1-5 is about the restaurants and the dataset 6 is about the ratings customers provided."),
                     #br(),
                     tags$li("Data Set:"),
                     tags$p("   1. chefmozaccepts.csv"),
                     tags$p("   2. chefmozcuisine.csv"),
                     tags$p("   3. chefmozhours4.csv"),
                     tags$p("   4. chefmozparking.csv"),
                     tags$p("   5. geoplaces2.csv"),
                     tags$p("   6. rating_final.csv.csv"),
                     #br(),
                     tags$li("Data Resources:"),
                     tags$a("Restaurant & consumer data Data Set",href="https://archive.ics.uci.edu/ml/datasets/Restaurant+%26+consumer+data",target="_blank"),tags$br()
                     #tags$a("To download raw data",href="http://data.insideairbnb.com/united-states/ca/los-angeles/2022-06-06/data/listings.csv.gz",target="_blank"),
                     #tags$h3("Data Description:"),
                     #tags$li("Raw data:42041(items)*75(attributes),including 288075 N/A"),
                     #tags$li("Data after cleansing-data:32112(items)*18(attributes),no N/A")
                   )
                 )
          )
        )
      ),
      
        fluidRow(
          box(
            title = strong(icon("calendar"),"Team Members"),
            solidHeader = TRUE, 
            status = "info", width = 12, collapsible = TRUE,
            column(12, 
                   tags$div(
                     fluidRow(
                       column(3, tags$img(src="carey_logo.png", height=101, width=246, align ="center")),
                       column(9, tags$div("The team members are BARM student at Johns Hopkins Carey Business School"),
                              tags$li(tags$strong("Jimin Pei: "), "jpei3@jh.edu"),
                              tags$li(tags$strong("Wenbo Liu: "), "wliu95@jh.edu"),
                              tags$li(tags$strong("Yixuan Chen: "), "ychen441@jh.edu"),
                              tags$li(tags$strong("Yiyang Zhang: "), "yzhan531@jh.edu"))
                     )
                   )
            )
          )
        )
      
    ),
    
   
    
    tabItem(
      tabName = "page2",
      h1("Overview"),
      fluidRow(
        column(4,
      pickerInput(inputId = "cuisine", label = "cuisine", 
                  choices =  unique(cuisine$Rcuisine), 
                  selected = unique(cuisine$Rcuisine),
                  options = list(`actions-box` = TRUE),
                  multiple = T
      )
      ),
        column(4,
      pickerInput(inputId = "price", label = "price", 
                  choices =  c("low", "medium", "high"), 
                  selected = c("low", "medium", "high"),
                  options = list(`actions-box` = TRUE),
                  multiple = T
      )
      ),
        column(4,
               #sliderInput("slider", "Slider input:", 1, 100, 30),
      checkboxGroupInput("checkGroup", label = "Checkbox group", 
                          choices = list("5-star" = 5, "4-star" = 4, "3-star" = 3,"2-star" = 2),
                         selected = c(5,4,3,2)
      )
      ),
      #numericInput("num", label = "Numeric input", value = 1),

      dataTableOutput('table2'),
      leafletOutput("map1")
    )
    ),
    tabItem(
      tabName = "page3",
      h1("Rating Plot"),
      h4(tags$li("Top 10 restaurants of select cuisine.")),
      br(),
      fluidRow(
        column(4,
               pickerInput(inputId = "Rcuisine", label = "Rcuisine",
                           choices = unique(cuisine$Rcuisine),
                           selected = unique(cuisine$Rcuisine),
                           options = list(`actions-box` = TRUE),
                           multiple = T
               )),
        column(4,
               pickerInput(inputId = "rating", label = "rating",
                           choices =  c("rating", "food_rating", "service_rating"),
                           selected = c("rating"),
                           options = list(`actions-box` = TRUE),
                           multiple = FALSE
               )
               ),
      plotOutput("plot1",height = 350)
    
   )
),
tabItem(
  tabName = "page4",
  h1("Restaurant Information"),
  dataTableOutput('table1')
  
),
tabItem(
  tabName = "page5",
  titlePanel(h1("Tip Calculator")),
  br(),
  tags$li("You can use the calculator below to calculate the tips."),
  br(),
  tags$head(tags$style('h1 {text-align: center;  
    padding: 23px;  
    background-color: skyblue;  
    color: white;}')),
  htmlOutput("Calculator")
  ),
tabItem(
  tabName = "page7",
  h1("Cusine Profile"),
  plotOutput('plot2')
),
tabItem(
  tabName = "page6",
  titlePanel(h1("Customer Profile")),
  sliderTextInput(
    inputId = "slider",
    label = "Customers' Characters Slider:",
    choices = c("smoker", "drink_level", "dress_preference", "ambience", 
                "transport", "marital_status", "hijos", "birth_year", "interest", 
                "personality", "religion","activity","color","weight","budget","height"),
    animate = animationOptions(1000)
  ),
  verbatimTextOutput(outputId = "result"),
  plotOutput("pie",height = 350)
)

)
)
)



server = function(input, output) {
  output$table1 =  renderDataTable({
    return(datatable(rest_loc_info))
  })
  rating = read_csv("rating_final.csv")
  rest_rating = rating %>%
    group_by(placeID)%>%
    summarise_at(vars("rating", "food_rating", "service_rating"), mean)
  rest_rating[,c("rating", "food_rating", "service_rating")] <- round(rest_rating[,c("rating", "food_rating", "service_rating")] * 2.5, 1)
  
  parking=read_csv("chefmozparking.csv")
  rest_parking =parking%>%
    group_by(placeID)

  accepts=read_csv("chefmozaccepts.csv")
  userprofile=read_csv("userprofile.csv")
  userprofile[userprofile == '?'] <- NA
  
  
  
  rest_accepts =accepts%>%
    group_by(placeID) %>%
    summarise(accepts = paste0(Rpayment, collapse = ",")) %>%
    ungroup()
  
  hours=read_csv("chefmozhours4.csv")
  rest_hours =hours%>%
    group_by(placeID)
 
  loc= read_csv("geoplaces2.csv")
  cuisine= read_csv("chefmozcuisine.csv")
  rest_cuisine =cuisine%>%
    group_by(placeID) %>%
    summarise(cuisine = paste0(Rcuisine, collapse = ",")) %>%
    ungroup()

  rest_loc_info = loc[, c('placeID', 'latitude', 'longitude', 'name', 'price')]
  rest_loc_info[3,4]= "El Rincon de San Francisco"

  rest_loc_info= rest_loc_info%>%
    mutate(price_color = case_when(price == "low" ~ 1, # both tests: group A
                                   price =="medium" ~ 2, # one test: group B
                                   price == "high" ~ 3 ))
  rest_loc_info=left_join(rest_loc_info, rest_cuisine, by = "placeID")
  rest_loc_info=left_join(rest_loc_info, rest_rating, by = "placeID")
  rest_loc_info=left_join(rest_loc_info, rest_hours, by = "placeID")
  rest_loc_info=left_join(rest_loc_info, rest_accepts, by = "placeID")
  rest_loc_info=left_join(rest_loc_info, rest_parking, by = "placeID")

  randomcuisine = unique(cuisine$Rcuisine)
  # replace NA value by randomly assign 
  rest_loc_info$cuisine[is.na(rest_loc_info$cuisine)] = sample(randomcuisine, sum(is.na(rest_loc_info$cuisine)),replace=TRUE)
  
  
  new_rating=left_join(rest_rating,cuisine, by ="placeID")
  new_rating=left_join(new_rating,loc, by ="placeID")
  
###################################################################3  
  output$plot1 = renderPlot({
    p1= new_rating %>% 
      filter(Rcuisine %in% c(input$Rcuisine))%>%
      arrange(desc(input$rating))%>% 
      slice(1:10)
    mycolors = RColorBrewer::brewer.pal(10, "PRGn")

          p2 = p1 %>%
        #######################
          ggplot(mapping = aes(x = reorder(name,!!sym(input$rating), na.rm=TRUE), y = !!sym(input$rating))) +
        #######################
          geom_bar(stat = "identity",color="#FFFFFF",fill = "#FF9912") +
            labs(y = str_to_title(as.character(input$rating)), x ="Restaurants")+
            #scale_fill_manual(values=c("#FFEEEE"))+
          theme(
          #axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          #axis.ticks.x = element_blank(),
          #panel.background = element_blank(),
          #plot.title = element_text(hjust = 0.5)
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
        coord_flip()
        
      p2
 
    })
  
  output$plot2=renderPlot({
    ggplot(cuisine, aes(x=cuisine$count, y=cuisine$type)) +
      geom_bar(stat="identity", width=1, color="white") + 
      ylab('Types') + 
      xlab('Count')
    
  })
  
  output$pie = renderPlot({
    pie_data= userprofile %>% 
      group_by(get(input$slider))%>%
      summarise(cnt = n()/sum(all()))%>%
      mutate(pct = cnt / sum(cnt))
      pie_data = pie_data[,c(1,3)]
    names(pie_data)[1] <- as.character(input$slider)
    ggplot(pie_data, aes(x="", y=pct, fill= !!sym(input$slider))) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() # remove background, grid, numeric labels
      # remove background, grid, numeric labels
   })
  
  
  
 #######################################################
  output$Calculator <- renderUI({
      tags$iframe(seamless="seamless", 
                  src= "Calculator.html",
                  width=800, 
                  height=800)
    })
  

  output$slickr <- renderSlickR({
    imgs <- list.files("ccc", pattern=".jpeg", full.names = TRUE)
    slickr <- slickR(imgs)
    slickr + settings(autoplay = TRUE,autoplaySpeed = 1000)
  })

  
  output$map1 =  renderLeaflet({
    rest_loc_info= rest_loc_info%>%
      filter(price %in% input$price)%>%
      filter(cuisine %in% input$cuisine)%>%
      filter(rating >= min(input$checkGroup))%>%
      distinct(placeID, .keep_all = TRUE)
    getColor <- function(rest_loc_info){
      sapply(rest_loc_info$price_color, function(price_color) {
        if(price_color == 1) {
          "green"
        } else if(price_color == 2) {
          "orange"
        } else {
          "red"
        } })
    }
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(rest_loc_info)
    )
    restuarant_locations_map <- leaflet(rest_loc_info) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~longitude,
                 lat = ~latitude,
                 icon = icons,
                 label=~as.character(name),
                 clusterOptions = markerClusterOptions()
                 )
    
    return(restuarant_locations_map)
    restuarant_locations_map
    
    

  })
}

shinyApp (ui = ui, server = server)

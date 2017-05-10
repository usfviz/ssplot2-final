setwd("~/Desktop/data_viz/ssplot2-")

packages <- c('shiny', 'shinydashboard','geojsonio', 
              "leaflet", "jsonlite", "rgdal", 
              "ggplot2", 'devtools', 'treemap', 'd3treeR',
              'tidyr', 'utils', 'dplyr')

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, require, character.only = TRUE)

devtools::install_github("gluc/data.tree")
devtools::install_github("timelyportfolio/d3treeR")
library(data.tree)
library(d3treeR)

crime_by_cat <- read.csv('data/crime_hood_category.csv', header = T)
crimes <- unique(sort(crime_by_cat$category[crime_by_cat$category != 'Category']))
DF <- read.csv("data/eight_crime.csv")
DF <- na.omit(DF)

# color_choices <- c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#b35806','#c7eae5','#bf812d')
color_choices <- c('#8dd3c7','#ffffb3', '#bebada', '#fb8072', '#80b1d3','#fdb462','#b3de69','#bf812d')

# heatmap stuff
DF_full <- read.csv("data/subcrime.csv")
DF_full$count <- 1
DF_full <- DF_full[,c('Category', 'PdDistrict', 'count')]
DF_group <- DF_full %>%
  group_by(Category, PdDistrict) %>%
  summarise(count_sum=sum(count))
DF_group <- na.omit(DF_group)

DF_pair <- expand.grid(Category=unique(DF_full$Category), PdDistrict = unique(DF_full$PdDistrict))

DF_fullpair <- merge(x=DF_pair, y=DF_group, by = c("Category","PdDistrict"), all.x = TRUE)
DF_fullpair <- DF_fullpair[-which(DF_fullpair$PdDistrict == '' | DF_fullpair$Category == ''),]
DF_fullpair[is.na(DF_fullpair)] <- 0
colnames(DF_fullpair) <- c("Category","District","Count")


ui <- dashboardPage(skin = 'black',
                    dashboardHeader(title = "SSPLOT2"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem('Intro', tabName = 'intro', icon = icon('hand-peace-o')),
                        menuItem('Map', tabName = 'choro', icon = icon('map-o')),
                        menuItem('Tree', tabName = 'tree', icon = icon('tree')),
                        menuItem('Time', tabName = 'time', icon = icon('clock-o')),
                        menuItem('Heat', tabName = 'heat', icon = icon('th'))
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "choro", 
                          fluidPage(
                            titlePanel("SF Crime Map"),
                            fluidRow(
                              column(width = 5,
                                     selectInput("crime",label= "Crime Type", crimes, selected=crimes[1:3], multiple=F)),
                              column(width = 12,
                                     leafletOutput('choro'))
                            )
                          )
                        ),
                        tabItem(
                          tabName = 'intro',
                          mainPanel(includeHTML('test.html'))
                        )
                        ,
                        tabItem(
                          tabName = 'tree',
                          fluidPage(
                            titlePanel("SF Crime Tree Map"),
                            fluidRow(
                              column(width = 12,
                                     d3tree2Output("tree")),
                              column(width = 12,
                                     dataTableOutput('district_table'))
                            )
                          )
                        ),
                        tabItem(
                          tabName = 'time',
                          fluidPage(
                            titlePanel("SF Crime Distributed Over Time"),
                            sidebarLayout(
                              
                              # Sidebar with a slider input
                              sidebarPanel(width = 3,
                                           sliderInput("month", 
                                                       "Month",
                                                       min = 1, 
                                                       max = 12, 
                                                       step = 1, 
                                                       value= 1, 
                                                       animate = animationOptions(interval = 200)),
                                           sliderInput("weekday", 
                                                       "Weekday",
                                                       min = 1, 
                                                       max = 7, 
                                                       step = 1, 
                                                       value= 1, 
                                                       animate = animationOptions(interval = 200)),
                                           checkboxGroupInput("hood", 
                                                              "Neighborhood:",
                                                              unique(DF$hood))
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput('volume')
                              )
                            )
                          )
                        ),
                        tabItem(
                          tabName = 'heat',
                          fluidPage(
                            
                            mainPanel(
                              tabsetPanel("Heatmap", 
                                          sidebarPanel(
                                            selectInput("category", "Crime Category:",unique(DF_group$Category), multiple = TRUE)
                                          ),
                                          
                                          mainPanel(
                                            plotOutput('heatmap',hover = "plot_hover", hoverDelay = 0),
                                            uiOutput("dynamic")
                                            #uiOutput("hover_info")
                                          )
                                          
                              )
                            )
                          )
                        )
                      )
                    )
)

server <- function(input, output) {
  # Choropleth Map
  map <- readOGR('data/geo_with_total_crime.geojson', "OGRGeoJSON")
  specific_crime <- read.csv('data/category_x_y.csv', header = T)
  
  subset_crime <- reactive({
    sub_crime <- specific_crime[specific_crime$Category == input$crime,]
  })
  
  pal <- colorQuantile("BuPu",NULL, n = 5)
  output$choro <- renderLeaflet({
    sub_crime <- subset_crime()
    leaflet(map)%>%
      addProviderTiles("Stamen.TonerBackground") %>%
      addPolygons(stroke = T, 
                  weight = 2,
                  color = "#000000", 
                  fill = T, 
                  fillColor = ~pal(map$totalCrime), 
                  fillOpacity = 1, 
                  opacity = 1,
                  label = map$neighborho) %>%
      addLegend("bottomright", 
                colors = c("#edf8fb","#b3cde3","#8c96c6","#8856a7","#810f7c"),
                labels= c("less","","","", "more"),
                title= "Total Crime (2016)",
                opacity = 1)%>%
      addCircles(lng = sub_crime$X, 
                 lat = sub_crime$Y, 
                 stroke = F,
                 fill = T,
                 fillOpacity = 100,
                 color = '#ffff33',
                 fillColor = '#ffff33',
                 radius = 20)
  })
  
  # Tree 
  output$tree <- renderD3tree2({
    df <- read.csv('data/treemap.csv', header = T)
    tm <- treemap(df,
                  index=c("hood", "category"),
                  vSize="count",
                  vColor="count",
                  type="value",
                  palette = 'GnBu',
                  format.legend = list(scientific = FALSE, big.mark = " "),
                  fontsize.legend = 6,
                  n=2)
    d3tree2(tm, rootname = 'Crime', width = '200%')
  })
  
  # Table for Tree tab
  districts <- read.csv('data/small_big_hood.csv', header = T)
  colnames(districts) <- c('Area', 'Neighborhoods')
  output$district_table <- renderDataTable({
    districts
  })
  
  # Time plot w/ bar charts
  input_category <- reactive({input$category})
  input_hood <- reactive({input$hood})
  
  output$volume <- renderPlotly({
    
    if (is.null(input_hood())){
      p <- ggplot(DF %>% filter(month == input$month & weekday == input$weekday), 
                  aes(x=hour, y=count, fill = category)) +
        theme_light() +
        geom_bar(stat = 'identity') + 
        ggtitle("\n Crime volume throughout day") + ylim(0,120) + xlim(0,24) +
        xlab("\n Hour of day") + ylab("Crime Count \n \n") +
        scale_fill_manual(values = color_choices) 
      
      ggplotly(p, width = 700, height = 400) %>%
        layout(legend = list(x = 1.05, y = 1))
    }
    else{
      
      p <- ggplot(DF %>% filter(month == input$month & weekday == input$weekday & hood %in% c(input$hood)), 
                  aes(x=hour, y=count, fill = category))+
        theme_light() +
        geom_bar(stat = 'identity') + 
        facet_grid(hood ~.) +
        ggtitle("\n Crime volume throughout day") + ylim(0,75) + xlim(0,24) +
        xlab("\n Hour of day") + ylab("Crime Count \n \n") +
        scale_fill_manual(values = color_choices)
      ggplotly(p, width = 700, height = 400+100*(length(input_hood())-1)) %>%
        layout(legend = list(x = 1.05, y = 1))
    }
  }
  )
  
  # Heatmap
  input_category <- reactive({input$category})
  
  output$heatmap <- renderPlot({
    
    if (is.null(input_category())){
      
      ggplot(DF_fullpair, aes(District, Category)) + 
        geom_tile(aes(fill = Count)) + 
        scale_fill_gradient(name="Crime Count", low = "#fef0d9", high = "#d7301f") +
        ggtitle("Crime Count by Category and District") +
        theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
              axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 8),
              axis.title=element_text(size=14,face="bold"), 
              legend.position="top", legend.direction = 'horizontal',
              legend.text = element_text(size = 8))
      
    }
    else{
      ggplot(DF_fullpair %>% filter(Category %in% c(input$category)) 
             %>% arrange (desc(Category)), 
             aes(District, Category)) + 
        geom_tile(aes(fill = Count)) + 
        scale_fill_gradient(name="Crime Count", low = "#fef0d9", high = "#d7301f") +
        ggtitle("Crime Count by Category and District") +
        theme(axis.title.x=element_blank(), axis.title.y = element_blank(),
              axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 8),
              axis.title=element_text(size=14,face="bold"), 
              legend.position="top", legend.direction = 'horizontal',
              legend.text = element_text(size = 8)) 
    }
  }, width = 800)
  
  output$dynamic <- renderUI({
    req(input$plot_hover) 
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderPrint({
    hover <- input$plot_hover 
    hover$x <- hover$x+1
    #print(hover$y)
    if (is.null(input_category())){
      point <- nearPoints(DF_fullpair, 
                          hover, threshold = 15, maxpoints = 1)
    }
    else{
      hover$y <- hover$y + 1
      point <- nearPoints(DF_fullpair %>% filter(Category %in% c(input$category)) 
                          %>% arrange (desc(Category)),
                          hover, xvar ='District', yvar = 'Category', 
                          maxpoints = 1)
    }
    req(nrow(point) != 0)
    HTML(paste("There were", point$Count, point$Category, "in", point$District, "in 2016"))
  })
  
}


shinyApp(ui, server)

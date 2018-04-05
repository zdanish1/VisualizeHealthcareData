cat('\014')
rm(list = ls())

if (!require("devtools")) install.packages("devtools")
devtools::install_github("pvictor/topogRam")

package_list <- c("treemap", 'd3treeR', 'ggplot2', 'GGally', 'plotly', 'ggplot2', 'maps', 'mapproj', 'shiny',
                  'shinythemes', 'RColorBrewer')

new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(treemap)
library(d3treeR)
library(ggplot2)
library(GGally)
library(plotly)
library(maps)
library(mapproj)
library(shiny)
library(shinythemes)
library(topogRam)
library(RColorBrewer)

setwd('~/Data_Viz/project') #set your own working directory

df <- read.csv('U.S._Chronic_Disease_Indicators__CDI_.csv')
df_mort <- read.csv('raw_data.csv')
df_cancer <- read.csv('df_cancer.csv')
df_final <- read.csv('df_final.csv')

df_mort <- merge(df_mort, subset(df, select = c('LocationAbbr','LocationDesc')), by.x = 'Location',by.y = 'LocationDesc')
df_mort <- unique(df_mort)
df_mort$hover <- with(df_mort, paste(Location))
df_overall <- df_final[df_final$StratificationCategory1 == 'Overall',]



tmLocate <- function(coor, tmSave) {
  tm <- tmSave$tm
  
  # retrieve selected rectangle
  rectInd <- which(tm$x0 < coor[1] &
                     (tm$x0 + tm$w) > coor[1] &
                     tm$y0 < coor[2] &
                     (tm$y0 + tm$h) > coor[2])
  
  return(tm[rectInd[1], ])
}


ui <- fluidPage(
  theme = shinytheme('flatly'),
  headerPanel("Interactive Visualization of Healthcare Data"),
  tabsetPanel(
    tabPanel("Indicators",
             sidebarLayout(
               position = 'right',
               sidebarPanel(uiOutput("click")),
               mainPanel(
                 plotlyOutput("plot1", width = '100%'))),
             sidebarLayout(
               position = 'right',
               sidebarPanel(uiOutput("record")),
                 mainPanel(plotOutput("treemap", click = 'click2'),
                           plotlyOutput("donut",width = '100%')))
                 # mainPanel(plotlyOutput("donut",width = '100%'))
               ),
    tabPanel("Cancer Cartogram",
             sidebarLayout(
              position = "right",
               sidebarPanel(selectInput("disease", "Select disease:",
                           choices = c(as.character(unique(df_cancer$Question))))),
               mainPanel(topogRamOutput("topo",width = "180%")))
    ))
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlotly({
    l <- list(color = toRGB("black"), width = 0.3)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    # specify some map projection/options
    p <- plot_geo(df_mort, locationmode = 'USA-states') %>%
      add_trace(
        z = ~Death.Rate.per.100.000, locations = ~LocationAbbr,
        color = ~Death.Rate.per.100.000, text = ~hover, colors = 'Blues',marker = list(line = l)
      ) %>%
      colorbar(title = "Deaths per 100,000") %>%
      layout(
        title = '2014 US Death Rate by State<br>(Hover for info; Click for Diseases)',
        geo = g
      )
  })
  d1 <- reactive({event_data("plotly_click")})
  output$click <- renderUI({
    if(is.null(d1()) == T) return(' ')
    diseases <- unique(as.character(df_final[df_final$Death.Rate.per.100.000 == as.numeric(d1()[3]),]$Topic))
    checkboxGroupInput("topic", "Choose Topic", diseases)
  })
  d <- reactive({event_data("plotly_click")})
  
  output$treemap <- renderPlot({
    if(is.null(d()) == T) return(' ')
    if(is.null(input$topic) == T) return(' ')
    df_overall2 <- df_overall[df_overall$Topic == input$topic,]
    df_sub <- df_overall2[df_overall2$Death.Rate.per.100.000 == as.numeric(d1()[3]),]
    .tm <<- treemap(df_sub,
                    index="Question",
                    vSize="DataValue",
                    vColor="LowConfidenceLimit",
                    type="value",
                    palette = "GnBu",
                    title = "Indicator Crude Prevalence (Click for breakdown options)",
                    fontsize.title = 20,
                    fontsize.labels = 12,
                    fontfamily.labels = 'sans',
                    fontfamily.title = 'serif',
                    fontface.labels = 'italic',
                    format.legend = list(scientific = FALSE, big.mark = " "))
    
  })
  getRecord <- reactive({
    x <- input$click2$x
    y <- input$click2$y
    
    x <- (x - .tm$vpCoorX[1]) / (.tm$vpCoorX[2] - .tm$vpCoorX[1])
    y <- (y - .tm$vpCoorY[1]) / (.tm$vpCoorY[2] - .tm$vpCoorY[1])
    x
    l <- as.character(tmLocate(list(x=x, y=y), .tm)[,1])
    l
  })
  
  output$record <- renderUI({
    if(is.null(input$click2) == T) return(' ')
    strats <- unique(as.character(df_final[df_final$Death.Rate.per.100.000 == as.numeric(d1()[3]) & as.character(df_final$Question) == getRecord(),]$StratificationCategory1))
    selectInput("strat", "Choose Stratification", strats)
  })
  
  donut_df <- reactive({
    df_final[df_final$Death.Rate.per.100.000 == as.numeric(d1()[3]) & as.character(df_final$Question) == getRecord() & df_final$StratificationCategory1 == input$strat,]
  })
  
  output$donut <- renderPlotly({
    colors <- c('rgb(241,238,246)','rgb(189,201,225)','rgb(116,169,207)','rgb(43,140,190)','rgb(4,90,141)')
    donut_df() %>%
      plot_ly(labels = ~Stratification1, values = ~DataValue,
              marker = list(colors = colors
                            # ,line = list(color = '#FFFFFF', width = 1)
              )) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Breakdown <br> (Hover for info)",  showlegend = T,
             autosize = T, width = 600, height = 500,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  final <- reactive({df_cancer[as.character(df_cancer$Question) == as.character(input$disease),]})
  
  output$topo <- renderTopogRam({
    topogRam(
      data = final(),
      key_var = "DataValue",
      shape = "usa-states",
      col = rev(brewer.pal("GnBu", n = 4))
    )
    
  })
}

shinyApp(ui, server)

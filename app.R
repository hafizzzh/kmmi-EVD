#Load the libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse) 
library(reshape2)
library(ggrepel)
library(tidyr)
library(lubridate)
library(plotly)
library(shinymaterial)

#Import the dataset
datanf <- read.csv("netflix_titles_fix.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Project Ujian Akhir Semester Kelompok G"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Anggota", tabName = "member", icon = icon("users")), 
      menuItem("Data", tabName = "data", icon = icon("database")), 
      menuItem("Visualisasi", tabName = "visual", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "member",
              h1("Anggota kelompok G"),
              h3(""),
              h3("Annisa Zahra"),
              h3("Ibrahim Hafizhan Witsqa"),
              h3("Maissy Ar Maghfiroh")
      ),
      
      # Second tab content
      tabItem(tabName = "data",
              h1("Input Data"),
              fluidPage(
                mainPanel(
                  tableOutput("contents")
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "visual",
              h1("Visualisasi Data"),
              fluidPage(
                tabsetPanel(
                  tabPanel("Statistika Deskriptif", 
                           verbatimTextOutput("statdes")), 
                  tabPanel("Stacked Bar Chart", plotOutput("barchart")), 
                  tabPanel("Bar Chart", plotOutput("scatplot")),
                  tabPanel("Box Plot", plotOutput("piechart")),
                  tabPanel("Histogram", plotOutput("histogram")),
                  tabPanel("Heat Map", plotOutput("heatmap")),
                )
              )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$contents <- renderTable({
    head(datanf)
  })
  output$statdes <- renderPrint({
    summary(datanf)
  })
  output$barchart <- renderPlot({
    ggplot(datanf,
           aes(fill = `rating`, 
               x = `type` )) +
      geom_bar(position = "fill")+
      theme(aspect.ratio=16/9)+
      labs(title="Perbandingan Rating pada TV Show dan Movie")
  })
  output$histogram <- renderPlot({
    ggplot(datanf,
           aes(x=`release_year`))+
      geom_histogram(fill="orange")
    
  })
  output$piechart <- renderPlot({
    ggplot(datanf,
           aes(x=`type`,
               y=`release_year`))+
      geom_boxplot(notch = TRUE,
                   fill="skyblue",
                   alpha=.7)
  })
  output$scatplot <-renderPlot({
    datanf %>% group_by(type) %>% mutate(country = fct_infreq(country)) %>% ggplot(aes(x = country)) + 
      geom_histogram(stat = 'count') + facet_wrap(~type, scales = 'free_x') + coord_cartesian(xlim = c(1,10)) + scale_x_discrete(labels = function(x){str_wrap(x,20)}, breaks = function(x) {x[1:10]})
  })
  output$heatmap <- renderPlot({
    df_show_categories <- datanf %>% 
      select(c('show_id','type','listed_in')) %>% 
      separate_rows(listed_in, sep = ',') %>%
      rename(Show_Category = listed_in)
    df_show_categories$Show_Category <- trimws(df_show_categories$Show_Category)
    
    df_unique_categories <- df_show_categories %>% group_by(type,Show_Category) %>%  summarise()
    df_category_correlations_movies <- data.frame(expand_grid(type = 'Movie', 
                                                              Category1 = subset(df_unique_categories, type == 'Movie')$Show_Category,
                                                              Category2 = subset(df_unique_categories, type == 'Movie')$Show_Category))
    
    df_category_correlations_TV <- data.frame(expand_grid(type = 'TV Show',
                                                          Category1 = subset(df_unique_categories, type == 'TV Show')$Show_Category,
                                                          Category2 = subset(df_unique_categories, type == 'TV Show')$Show_Category))
    
    df_category_correlations <- rbind(df_category_correlations_movies,df_category_correlations_TV)
    df_category_correlations$matched_count <- apply(df_category_correlations, MARGIN = 1,FUN = function(x) {
      length(intersect(subset(df_show_categories, type == x['type'] & Show_Category == x['Category1'])$show_id,
                       subset(df_show_categories, type == x['type'] & Show_Category == x['Category2'])$show_id))})
    
    df_category_correlations <- subset(df_category_correlations, (as.character(Category1) < as.character(Category2)) & (matched_count > 0))
    options(repr.plot.width=14, repr.plot.height=10)
    
    ggplot(subset(df_category_correlations, type == 'Movie'), aes(x = Category1, y = Category2, fill = matched_count)) + 
      geom_tile() + facet_wrap( ~type, scales = 'free') + scale_fill_distiller(palette = "Spectral") + 
      theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5) )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

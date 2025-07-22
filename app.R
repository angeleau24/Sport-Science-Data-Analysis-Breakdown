#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# install.packages(c("colourpicker", "plotly", "ggExtra"))
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggExtra)
library(shinyWidgets)
library(readxl)
library(DT)
library(gridExtra)
library(ggthemes)
library(ggpubr)
library(ggcorrplot)
library(shinyjs)
library(ggplotify)
library(grid)
library(tools)
library(shinyBS)
library(shinycssloaders)
library(FactoMineR)
library(factoextra)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Sports Science Data Analysis Breakdown"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose File"),
      checkboxInput("header", "Header", TRUE),
      tags$hr(),
      downloadButton("downloadPlotPDF", "Download All Plots (PDF)")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data", DTOutput("contents")),
        tabPanel("Summary Stats", dataTableOutput("summary_table")),
        
        tabPanel("Scatter Plots",
                 fluidRow(
                   column(4,
                          selectInput("x_scatter1", "X-axis:", choices = NULL),
                          selectInput("y_scatter1", "Y-axis:", choices = NULL),
                          selectInput("color_scatter1", "Color/Group by:", choices = NULL)
                   ),
                   column(8, plotOutput("scatter1", height = "500px"))
                 ),
                 fluidRow(
                   column(4,
                          selectInput("x_scatter2", "X-axis:", choices = NULL),
                          selectInput("y_scatter2", "Y-axis:", choices = NULL),
                          selectInput("color_scatter2", "Color/Group by:", choices = NULL)
                   ),
                   column(8, plotOutput("scatter2", height = "500px"))
                 )
        ),
        
        tabPanel("2D Bin Count",
                 fluidRow(
                   column(4,
                          selectInput("x_bin1", "X-axis:", choices = NULL),
                          selectInput("y_bin1", "Y-axis:", choices = NULL),
                          selectInput("fill_bin1", "Color/Group by:", choices = NULL)
                   ),
                   column(8, plotOutput("binplot1", height = "500px"))
                 )
        ),
        
        tabPanel("Histograms",
                 fluidRow(
                   column(4,
                          selectInput("x_hist", "X-axis:", choices = NULL),
                          selectInput("fill_hist", "Color/Group by:", choices = NULL),
                          sliderInput("bins", "Bin Size:", min = 5, max = 50, value = 20)
                   ),
                   column(8, plotOutput("histogram", height = "500px"))
                 )
        ),
        
        tabPanel("Bar Charts",
                 fluidRow(
                   column(4,
                          selectInput("x_bar", "X-axis:", choices = NULL),
                          selectInput("y_bar", "Y-axis:", choices = NULL),
                          selectInput("fill_bar", "Color/Group by:", choices = NULL)
                   ),
                   column(8, plotOutput("barchart", height = "500px"))
                 )
        ),
        
        tabPanel("PCA",
                 fluidRow(
                   column(4,
                          selectInput("group_pca", "Group by:", choices = NULL)
                   ),
                   column(8,
                          h4("Scree Plot (Variance Explained)"),
                          plotOutput("pca_scree", height = "300px"),
                          h4("PCA Individuals Plot"),
                          plotOutput("pca_ind", height = "400px"),
                          h4("PCA Variables Plot"),
                          plotOutput("pca_var", height = "400px"),
                          h4("PCA Biplot"),
                          plotOutput("pca_biplot", height = "500px")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    switch(tolower(ext),
           csv = read.csv(input$file1$datapath, header = input$header),
           tsv = read.delim(input$file1$datapath, header = input$header),
           txt = read.table(input$file1$datapath, header = input$header),
           xlsx = readxl::read_excel(input$file1$datapath),
           stop("Unsupported file type")
    )
  })
  
  observeEvent(dataset(), {
    vars <- names(dataset())
    updateSelectInput(session, "x_scatter1", choices = vars)
    updateSelectInput(session, "y_scatter1", choices = vars)
    updateSelectInput(session, "color_scatter1", choices = c("None", vars))
    
    updateSelectInput(session, "x_scatter2", choices = vars)
    updateSelectInput(session, "y_scatter2", choices = vars)
    updateSelectInput(session, "color_scatter2", choices = c("None", vars))
    
    updateSelectInput(session, "x_bin1", choices = vars)
    updateSelectInput(session, "y_bin1", choices = vars)
    updateSelectInput(session, "fill_bin1", choices = c("None", vars))
    
    updateSelectInput(session, "x_hist", choices = vars)
    updateSelectInput(session, "fill_hist", choices = c("None", vars))
    
    updateSelectInput(session, "x_bar", choices = vars)
    updateSelectInput(session, "y_bar", choices = vars)
    updateSelectInput(session, "fill_bar", choices = c("None", vars))
    
    updateSelectInput(session, "group_pca", choices = c("None", vars))
  })
  
  plot_scatter <- function(xvar, yvar, colorvar) {
    req(xvar, yvar)
    df <- dataset()
    ggplot(df, aes_string(x = xvar, y = yvar)) +
      geom_point(aes_string(color = ifelse(colorvar == "None", NULL, colorvar)), size = 4, alpha = 0.6) +
      theme_minimal(base_size = 14)
  }
  
  output$scatter1 <- renderPlot({ plot_scatter(input$x_scatter1, input$y_scatter1, input$color_scatter1) })
  output$scatter2 <- renderPlot({ plot_scatter(input$x_scatter2, input$y_scatter2, input$color_scatter2) })
  
  output$binplot1 <- renderPlot({
    req(input$x_bin1, input$y_bin1)
    df <- dataset()
    ggplot(df, aes_string(x = input$x_bin1, y = input$y_bin1)) +
      geom_bin2d(aes_string(fill = ifelse(input$fill_bin1 == "None", "..count..", input$fill_bin1)), bins = 30) +
      scale_fill_viridis_c() +
      theme_minimal(base_size = 14)
  })
  
  output$histogram <- renderPlot({
    req(input$x_hist)
    df <- dataset()
    ggplot(df, aes_string(x = input$x_hist)) +
      geom_histogram(aes_string(fill = ifelse(input$fill_hist == "None", NULL, input$fill_hist)),
                     bins = input$bins, color = "white", alpha = 0.7) +
      theme_minimal(base_size = 14)
  })
  
  output$barchart <- renderPlot({
    req(input$x_bar, input$y_bar)
    df <- dataset()
    
    ggplot(df, aes_string(x = input$x_bar, y = input$y_bar)) +
      geom_col(aes_string(fill = ifelse(input$fill_bar == "None", NULL, input$fill_bar)),
               position = "dodge", alpha = 0.8) +
      theme_minimal(base_size = 14)
  })
  
  output$summary_table <- renderDataTable({
    df <- dataset()
    num_cols <- sapply(df, is.numeric)
    df_num <- df[, num_cols, drop = FALSE]
    if (ncol(df_num) == 0) return(NULL)
    stats <- data.frame(
      Variable = names(df_num),
      Mean = sapply(df_num, function(x) round(mean(x, na.rm = TRUE), 2)),
      SD = sapply(df_num, function(x) round(sd(x, na.rm = TRUE), 2)),
      Min = sapply(df_num, function(x) round(min(x, na.rm = TRUE), 2)),
      Max = sapply(df_num, function(x) round(max(x, na.rm = TRUE), 2)),
      Median = sapply(df_num, function(x) round(median(x, na.rm = TRUE), 2)),
      NA_Count = sapply(df_num, function(x) sum(is.na(x))),
      stringsAsFactors = FALSE
    )
    datatable(stats)
  })
  
  output$contents <- renderDT({
    req(dataset())
    datatable(dataset(), options = list(scrollX = TRUE))
  })
  
  # PCA plots with FactoMineR and factoextra, filtering zero variance cols
  output$pca_scree <- renderPlot({
    df <- dataset()
    df_num <- df %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::select_if(~ var(., na.rm = TRUE) > 0) %>%
      na.omit()
    if (nrow(df_num) < 2 || ncol(df_num) < 2) return(NULL)
    res.PCA <- PCA(df_num, graph = FALSE)
    fviz_eig(res.PCA)
  })
  
  output$pca_ind <- renderPlot({
    df <- dataset()
    
    df_num <- df %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::select_if(~ var(., na.rm = TRUE) > 0) %>%
      na.omit()
    
    if (nrow(df_num) < 2 || ncol(df_num) < 2) return(NULL)
    
    res.PCA <- PCA(df_num, graph = FALSE)
    
    group_var <- NULL
    if (input$group_pca != "None") {
      full_group <- dataset()[[input$group_pca]]
      group_var <- full_group[complete.cases(df_num)]
    }
    
    fviz_pca_ind(res.PCA,
                 repel = TRUE,
                 habillage = group_var,
                 palette = "jco",
                 addEllipses = TRUE,
                 legend.title = ifelse(is.null(group_var), "", input$group_pca)
    )
  })
  
  output$pca_var <- renderPlot({
    df <- dataset()
    df_num <- df %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::select_if(~ var(., na.rm = TRUE) > 0) %>%
      na.omit()
    if (nrow(df_num) < 2 || ncol(df_num) < 2) return(NULL)
    res.PCA <- PCA(df_num, graph = FALSE)
    fviz_pca_var(res.PCA, repel = TRUE)
  })
  
  output$pca_biplot <- renderPlot({
    df <- dataset()
    
    df_num <- df %>%
      dplyr::select(where(is.numeric)) %>%
      dplyr::select_if(~ var(., na.rm = TRUE) > 0) %>%
      na.omit()
    
    if (nrow(df_num) < 2 || ncol(df_num) < 2) return(NULL)
    
    res.PCA <- PCA(df_num, graph = FALSE)
    
    group_var <- NULL
    if (input$group_pca != "None") {
      full_group <- dataset()[[input$group_pca]]
      group_var <- full_group[complete.cases(df_num)]
    }
    
    fviz_pca_biplot(res.PCA,
                    repel = TRUE,
                    habillage = group_var,
                    palette = "jco",
                    addEllipses = TRUE,
                    legend.title = ifelse(is.null(group_var), "", input$group_pca)
    )
  })
  
  output$downloadPlotPDF <- downloadHandler(
    filename = function() paste("EDA_Plots_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      pdf(file, width = 10, height = 8)
      print(plot_scatter(input$x_scatter1, input$y_scatter1, input$color_scatter1))
      print(plot_scatter(input$x_scatter2, input$y_scatter2, input$color_scatter2))
      dev.off()
    }
  )
}

shinyApp(ui, server)






# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("20m Resisted Sprint Times"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("Max Speed",
#                         "mph:",
#                         min = 10,
#                         max = 30,
#                         value = 50)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

library(shiny)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
require(scales)
require(viridis)
library(xtable)

app_url = 'http://something.com'
simulation_info_url <- 'http://something.com'

italy_box_y <- c(36, 47.5)
italy_box_x <- c(6.1, 19.5)  

load('app_data.RData')


ui <- fluidPage(
  tags$link(href="https://fonts.googleapis.com/css?family=Open+Sans", rel="stylesheet"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%;font-family: 'Open Sans', sans-serif;} .leaflet-bottom.leaflet-left {position: absolute; bottom: 50px} th, td {padding: 3px;} label {font-size: small} table {font-size: x-small} h3 {font-size: small; font-weight: 700} .selectize-input {font-size: small} h1 {font-size: 150%; font-weight: 900;} h1 {margin-bottom: 8px; margin-top: 8px} h3 {margin-bottom: 1px; margin-top: 3px} .table, #camera_sim_bar, #senato_sim_bar {margin-top: 3px; margin-bottom: 10px;} label.control-label, .selectize-control.single{ display: table-cell; vertical-align: middle; } .form-group { display: table-row;} .control-label {padding-right: 10px} #map_variable_dens{margin-bottom: 6px} .selectize-control {width: 250px} #footnote {font-size: 90%; padding: 0px; margin: 0px;}"),
  fluidRow(
    column(4,
           conditionalPanel(
             condition = "output.language == 'en'",
             tags$h1("Election 2018: District result simulation and seat distribution")),
           conditionalPanel(
             condition = "output.language != 'en'",
             tags$h1("Elezioni 2018: Simulazione risultato collegi e distribuzione seggi")),
           # uiOutput("which_maptype_ui"),
           uiOutput("map_variable_ui"),
           plotOutput("map_variable_dens", height = '130px'),
           uiOutput("which_map_ui"),
           hr(style = 'margin: 10px;'),
           conditionalPanel(
             condition = "output.language == 'en'",
             tags$h3("Distribution of seats based on simulation results")),
           conditionalPanel(
             condition = "output.language != 'en'",
             tags$h3("Distribuzione di seggi in base ai risultati della simulazione")),
           tableOutput('coalition_table'),
           conditionalPanel(
             condition = "output.language == 'en'",
             tags$h3("Chamber of Deputies")),
           conditionalPanel(
             condition = "output.language != 'en'",
             tags$h3("Camera dei Deputati")),
           plotOutput("camera_sim_bar", height = '100px'),
           conditionalPanel(
             condition = "output.language == 'en'",
             tags$h3("Senate of the Republic")),
           conditionalPanel(
             condition = "output.language != 'en'",
             tags$h3("Senato della Repubblica")),
           plotOutput("senato_sim_bar", height = '100px'),
           conditionalPanel(
             condition = "output.language == 'en'",
             tags$div(
               tags$p(tags$a(href=paste0(app_url, "?lang=it"), tags$span(style = 'background-color:white; font-size:120%', "Italiano")), tags$br(), tags$a(href = simulation_info_url, target = '_blank', "Simulation details"), " - Designed by ", tags$a(href="http://www.francescobailo.net/", target='_blank', "Francesco Bailo"), " - Hosted by ", tags$a(href = 'https://nectar.org.au/research-cloud/', target='_blank', 'Nectar Cloud'), " - ", tags$a(href = 'https://github.com/fraba/ige18_app', target='_blank', 'Code on GitHub')),
               id='footnote')),
           conditionalPanel(
             condition = "output.language != 'en'",
             tags$div(
               tags$p(tags$a(href=paste0(app_url, "?lang=en"), tags$span(style = 'background-color:white; font-size:120%', "English")), tags$br(), tags$a(href = simulation_info_url, target = '_blank', "Dettagli simulazione"), " - Creato da ", tags$a(href="http://www.francescobailo.net/", target='_blank', "Francesco Bailo"), " - Hosted by ", tags$a(href = 'https://nectar.org.au/research-cloud/', target='_blank', 'Nectar Cloud'), " - ", tags$a(href = 'https://github.com/fraba/ige18_app', target='_blank', 'Code on GitHub')),
               id='footnote'))),
    column(8,
           leafletOutput("map", height = '700px')))
)

server <- function(input, output, session) {
  
  
  output$language <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (length(query$lang) == 0) {
      return('it')
    } else {
      return(query$lang)
    }
  })
  outputOptions(output, 'language', suspendWhenHidden=FALSE)
  
  output$which_map_ui <- renderUI({
    query <- parseQueryString(session$clientData$url_search)
    if (length(query$lang) > 0) {
      if (query$lang == 'en') {
        return(tagList(
          radioButtons('which_map', 'Single-member district map',
                       choices = setNames(c('camera', 'senato'), nm = c('Chamber','Senate')), 
                       inline=T, selected = 'camera')))
      } else {
        return(tagList(
          radioButtons('which_map', 'Mappa collegi uninominali',
                       choices = setNames(c('camera', 'senato'), nm = c('Camera','Senato')), 
                       inline=T, selected = 'camera')))
      }
    } else {
      return(tagList(
        radioButtons('which_map', 'Mappa collegi uninominali',
                     choices = setNames(c('camera', 'senato'), nm = c('Camera','Senato')), 
                     inline=T, selected = 'camera')))
    }
  })
  
  # output$which_maptype_ui <- renderUI({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if (length(query$lang) > 0) {
  #     if (query$lang == 'en') {
  #       return(tagList(
  #         checkboxInput('which_maptype', 'Cartogram', value = FALSE)))
  #     } else {
  #       return(tagList(
  #         checkboxInput('which_maptype', 'Cartogramma', value = FALSE)))
  #     }
  #   } else {
  #     return(tagList(
  #       checkboxInput('which_maptype', 'Cartogramma', value = FALSE)))
  #   }
  # })
  
  output$map_variable_ui <- renderUI({
    query <- parseQueryString(session$clientData$url_search)
    if (length(query$lang) > 0) {
      if (query$lang == 'en') {
        return(tagList(
          selectInput('map_variable', 'Map variable', choices = en_map_variables)))
      } else {
        return(tagList(
          selectInput('map_variable', 'Variabile mappa', choices = it_map_variables)))
      }
    } else {
      return(tagList(
        selectInput('map_variable', 'Variabile mappa', choices = it_map_variables)))
    }
  })
  
  italy_map <- reactive({
    if (length(input$which_map)>0){
      if (input$which_map == 'camera') {
        if (!is.null(input$map_variable)) {
          # if(input$which_maptype == FALSE) {
          cu2017_sp$selected_var <- as.data.frame(cu2017_sp)[[input$map_variable]]
          # } else {
          #   cu2017_carto_sp$selected_var <- as.data.frame(cu2017_carto_sp)[[input$map_variable]]
          # }
        } else {
          cu2017_sp$selected_var <- 0
        }
        # if (input$which_maptype == FALSE) {
        return(cu2017_sp)
        # } else (
        #   return(cu2017_carto_sp)
        # )
      } else{
        if (!is.null(input$map_variable)) {
          su2017_sp$selected_var <- as.data.frame(su2017_sp)[[input$map_variable]]
        } else {
          su2017_sp$selected_var <- 0
        }
        return(su2017_sp)
      }
    } else {
      if (!is.null(input$map_variable)) {
        cu2017_sp$selected_var <- as.data.frame(cu2017_sp)[[input$map_variable]]
      } else {
        cu2017_sp$selected_var <- 0
      }
      return(cu2017_sp)
    }
  })
  
  # MAPPA
  output$map <- renderLeaflet({
    if (is.null(italy_map())) {
      leaflet(regone_sp) %>%
        addPolygons(stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5,
                    color = "black", opacity = .5, weight = .5,
                    popup = ~paste0(nome, " (", regione, ")"))
    } else {
      if (length(input$map_variable) > 0) {
        if (grepl("^leader$", input$map_variable)) {
          pal <- colorFactor(leader_var_colors, levels = leader_var_levels)
        } else if (grepl("majdiff$", input$map_variable)) {
          if (input$map_variable == 'min_majdiff') {
            bins <- c(0, -.01, -.02, -.03, -.05, -.1, -.2, -.4, -.6, -1)
            pal <- 
              colorBin(palette = viridis(11), 
                       domain = c(min(italy_map()$selected_var),max(italy_map()$selected_var)),
                       bins = bins)
          } else {
            bins <- rev(c(-.99, -.3, -.2, -.1, -.05, -.02 ,0, .02, .05, .1, .2, .3, .99))
            pal <- 
              colorBin(palette = brewer.pal(11, "PRGn"), 
                       domain = c(min(italy_map()$selected_var),max(italy_map()$selected_var)), 
                       bins = bins)
          }
        } else {
          pal <- 
            colorNumeric(palette = brewer.pal(7, "YlGnBu"), 
                         domain = c(min(italy_map()$selected_var),max(italy_map()$selected_var)))
        }
      } else {
        pal <- 
          colorNumeric(palette = brewer.pal(7, "YlGnBu"), 
                       domain = c(min(italy_map()$selected_var),max(italy_map()$selected_var)))
      } 
      
      composePopUp <- function(nome, regione, popup_label) {
        str <- paste0("<b>", nome, "</b>", ", <i>", regione, "</i><br/>", popup_label)
        query <- parseQueryString(session$clientData$url_search)
        if (length(query$lang) > 0) {
          if (query$lang == 'en') {
            str <- gsub("CDX", "Centre-right", str)
            str <- gsub("CSX", "Centre-left", str)
          } else {
            str <- gsub("CDX", "Centro-destra", str)
            str <- gsub("CSX", "Centro-sinistra", str)
          }
        } else {
          str <- gsub("CDX", "Centro-destra", str)
          str <- gsub("CSX", "Centro-sinistra", str)
        }
        return(str)
      }
      
      leafmap <- 
        leaflet(italy_map()) %>%
        addPolygons(data = regone_sp, color = "black", weight = 1.5) %>%
        addPolygons(stroke = TRUE, fillOpacity = 0.95, smoothFactor = 0.5,
                    fillColor = ~pal(selected_var),
                    color = "black", opacity = .35, weight = .3,
                    popup = ~composePopUp(nome, regione, popup_label),
                    highlight = highlightOptions(
                      weight = 4,
                      color = "black",
                      dashArray = "",
                      fillOpacity = 0.9,
                      bringToFront = TRUE)) %>%
        setView(lng= 13, lat=41.8, zoom=6) 
    }
    
    
    if (length(input$map_variable) > 0) {
      if (grepl("^leader$", input$map_variable)) {
        
        query <- parseQueryString(session$clientData$url_search)
        if(length(query$lang)>0) {
          if (query$lang == 'en') {
            legend_labs <- leader_var_labels_en
          } else {
            legend_labs <- leader_var_labels_it
          }
        } else {
          legend_labs <- leader_var_labels_it
        }
        
        leafmap %>%
          addLegend("bottomleft", colors = leader_var_colors, labels = legend_labs,
                    opacity = 1
          )
      } else {
        leafmap %>%
          addLegend("bottomleft", pal = pal, values = ~selected_var,
                    title = NULL,
                    labFormat = labelFormat(suffix = "%", transform = function(x){round(x*100, digits=0)}),
                    opacity = 1
          )
      }
    }
    
    
  })
  
  output$camera_sim_bar <- renderPlot({
    # Camera sim plot
    sim_result_df$cod <- 
      factor(sim_result_df$cod, 
             levels = sim_result_df$cod[order(sim_result_df$camera_seats)])
    ggplot(sim_result_df, aes(x=cod, y=camera_seats)) +
      geom_bar(stat="identity", alpha = 0.7) +
      geom_errorbar(aes(ymin=camera_seats+(camera_seats-camera_error_down), 
                        ymax=camera_seats+(camera_seats-camera_error_up)),
                    width = .5) +
      geom_hline(yintercept = 630/2+1, linetype = 2) + 
      labs(x=NULL, y=NULL) +
      theme_bw() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      coord_flip()
  })
  
  output$senato_sim_bar <- renderPlot({
    
    # Senato sim plot
    sim_result_df$cod <- 
      factor(sim_result_df$cod, 
             levels = sim_result_df$cod[order(sim_result_df$senato_seats)])
    ggplot(sim_result_df, aes(x=cod, y=senato_seats)) +
      geom_bar(stat="identity", alpha = 0.7) +
      geom_errorbar(aes(ymin=senato_seats+(senato_seats-senato_error_down), 
                        ymax=senato_seats+(senato_seats-senato_error_up)),
                    width = .5) +
      geom_hline(yintercept = 320/2+1, linetype = 2) + 
      labs(x=NULL, y=NULL) +
      theme_bw() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      coord_flip()
  })
  
  output$coalition_table <- renderTable({
    
    this_df <- sim_result_df[,c('cod','camera_seats','senato_seats')]
    this_df$camera_diff <- this_df$camera_seats - 630/2
    this_df$senato_diff <- this_df$senato_seats - 320/2
    
    this_df$camera_seats <- round(this_df$camera_seats, 0)
    this_df$senato_seats <- round(this_df$senato_seats, 0)
    
    this_df$camera_diff <- round(this_df$camera_diff, 0)
    this_df$senato_diff <- round(this_df$senato_diff, 0)
    
    this_df <- this_df[order(this_df$camera_seats, decreasing = T),]
    
    
    query <- parseQueryString(session$clientData$url_search)
    if(length(query$lang)>0) {
      if (query$lang == 'en') {
        require(dplyr)
        this_df$cod <- recode_factor(this_df$cod, 'CDX' = 'C-R', 'CSX' = 'C-L')
        this_df <- this_df[,c(1,2,4,3,5)]
        colnames(this_df) <- c("", "Chamber seats", "maj.", "Senate seats", "maj.")
        return(this_df)
      } else {
        this_df <- this_df[,c(1,2,4,3,5)]
        colnames(this_df) <- c("", "Seggi Camera", "magg.", "Seggi Senato", "magg.")
        return(this_df)
      }
    } else {
      this_df <- this_df[,c(1,2,4,3,5)]
      colnames(this_df) <- c("", "Seggi Camera", "magg.", "Seggi Senato", "magg.")
      return(this_df)
    }
    
    
  }, digits = 0, spacing = 'xs', align = 'c')
  
  output$map_variable_dens <- renderPlot({
    
    if (!is.factor(as.data.frame(italy_map())$selected_var) & !is.null(italy_map())) {
      ggplot(as.data.frame(italy_map()),  aes(selected_var)) + geom_density() +
        geom_vline(xintercept = median(as.data.frame(italy_map())$selected_var)) +
        theme_bw() +
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        labs(x=NULL,y=NULL) + 
        scale_x_continuous(labels = scales::percent)
    }
    
  })
  
}

shinyApp(ui, server)
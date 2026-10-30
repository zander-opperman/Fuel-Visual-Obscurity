# VERSION 1 #
library(shiny)
library(tidyverse)
library(bslib)
library(readr)
library(shinyWidgets)
library(bsicons)
library(thematic)

# tags$head(
#     tags$link(rel = "stylesheet", type = "text/css", href = "app_styles.css")
# )



page_sidebar(
    # includeCSS("www/app_styles.css"),
    
    # MAIN Sidebar ------------------------------------------------------------
    sidebar = sidebar(
        
        imageOutput("panelCalcImage",
                    fill = TRUE,
                    width = "auto",
                    height = "100%"),
        
        value_box(
            title = tags$h1("Percent Obstructed"),
            value = tags$p(textOutput("percentCover")),
            showcase = bs_icon("tree"),
            theme = "success",
            # class = "text-light"
            class = "accept-button-clicked"
        ),
        
        plotOutput("imageCropPixelMap")
        
    ),
    
    navset_card_underline(
        
        # CALIBRATE Panel ---------------------------------------------------------
        nav_panel(
            title = "Calibrate",
            # Display Calibrating Image
            layout_sidebar(

                # CALIBRATE Sidebar -------------------------------------------------------
                sidebar = sidebar(
                    position = "right",
                    # File input for Calibrating image
                    fileInput(inputId  = "selectImageCalibrate",
                              label    = "Select Image File"),
                    
                    # Calculate color histogram
                    actionButton(inputId  = "calculateColors", 
                                 label    ="Calculate Colors",
                                 disabled = TRUE),
                    
                    # Select color from Histogram
                    numericInput(inputId  = "colorSelect",
                                 label    = "Select Color",
                                 value = 1,
                                 min = 1, max = 27, step = 1),
                    
                    # Select Color Radius for Image Analysis
                    sliderInput( inputId = "colorSensitivity",
                                 label   ="Color Sensitivity [0-1]",
                                 min = 0.01, max = 0.99,
                                 value = 0.10, step = 0.01 ),
                    
                    # verbatimTextOutput("brush_info"),
                    div(
                        id = "panelSizeDiv",
                        class = "panel-size-default",
                        p(
                            uiOutput(outputId = "panelSizeText")
                            
                        )
                    ),
                    
                    # Accept panel Size
                    actionButton(inputId  = "acceptPanelSize",
                                 label    ="Accept",
                                 class    = "default",
                                 disabled = TRUE),
                    card(
                        numericInput("windowSize", "Window Size", 3),
                        numericInput("panelSens1", "Panel Sensitivity 1", 3),
                        numericInput("panelSens2", "Panel Sensitivity 2", 3)
                    )
                ),
                
                # layout_columns(
                    card(
                        imageOutput("imageCalibrate",
                                    fill = TRUE,
                                    width = "100%",
                                    height = "auto",
                                    brush = brushOpts( id = "image_brush",
                                                       resetOnNew = TRUE))
                        
                    ),
                    card(
                        imageOutput("imageCrop",
                                    fill = TRUE,
                                    width = "100%",
                                    height = "auto")
                    )
                    # card(
                    #     imageOutput(
                    #         "imageID",
                    #         width = "auto",
                    #         height = "100%",
                    #         fill = TRUE
                    #     )
                # ),
                # col_widths = c(6,6,-3,6,-3),
                # row_heights = c(6,6)
            )
        ),
        
        
        
        # CALCULATE Panel ---------------------------------------------------------
        
        nav_panel(
            title = "Calculate",
            
            layout_sidebar(
                sidebar = sidebar(
                    position = "right",
                    
                    fileInput("selectImage",
                              "Select Image File"),
                    
                    div(
                        style = "width: 200px; height: 200px; text-align: center;",
                        fluidRow(
                            column(12, actionBttn("dir_up", "↑", style = "material-circle"))
                        ),
                        br(),
                        fluidRow(
                            column(4, actionBttn("dir_left", "←", style = "material-circle")),
                            column(4, actionBttn("dir_reset", "⟳", style = "material-circle")),
                            column(4, actionBttn("dir_right", "→", style = "material-circle"))
                        ),
                        br(),
                        fluidRow(
                            column(12, actionBttn("dir_down", "↓", style = "material-circle"))
                        )
                    ),
                    
                    
                    
                ),
                
                imageOutput("imageCalc",
                            fill = TRUE,
                            width = "auto",
                            height = "100%"
                )
            )
            
        )
    )
)





# VERSION 2 #

library(shiny)
library(tidyverse)
library(bslib)
library(readr)
library(shinyWidgets)
library(shinyjs)
library(bsicons)
library(thematic)
source("www/info_text.R")

# Increase upload size to 10MB
options(shiny.maxRequestSize = 10 * 1024^2)

bslib::page_navbar(
    title = "Fuel Visual Obscurity",
    header = shiny::includeCSS("www/app_styles.css"),
    footer = useShinyjs(),
    theme = bslib::bs_theme(
        version = "5",
        bootswatch = "yeti"
    ),
    
    
    # CALIBRATE PANEL ---------------------------------------------------------
    bslib::nav_panel(
        title = "1. Calibrate",
        
        
        ## CALIBRATE SIDEBAR -------------------------------------------------------
        bslib::layout_sidebar(
            
            sidebar = bslib::sidebar(
                position = "left",
                ### Input file for calibrating image ----
                shiny::fileInput(inputId = "selectImageCalibrate",
                                 label = tooltip( trigger = list(
                                     "Select Image",
                                     bs_icon("info-circle")
                                 ), info_selectImageCalibrate)),
                
                ### Display Click Coords ----
                shiny::verbatimTextOutput("click_info"),
                
                ### Undo click assignment ----
                shiny::actionButton(inputId = "undoClick",
                             label   = "Undo",
                             class = "btn-warning") |> 
                    tooltip("Undo the previous click."),
                ### Panel Size Text ----
                shiny::verbatimTextOutput("panel_info"),
                
                ### Number of Quadrants ----
                shiny::selectInput(inputId = "numberQuadrants",
                                   label   = tooltip( trigger = list(
                                       "Number of Quadrants",
                                       bs_icon("info-circle")
                                   ), info_numberQuadrants),
                                    choices = list(1,2,3,4,
                                                   5,6,7,8),
                                   selected = 1),
                
                ### Calibrate Colors ----
                shiny::actionButton(inputId  = "calibrateColors", 
                             label    ="Calibrate Colors",
                             disabled = TRUE,
                             class = "btn-info") |> 
                    tooltip("Classify colors within the image."),
                ### Select color from Histogram ----
                shiny::numericInput(inputId  = "colorSelect",
                             label    = tooltip( trigger = list(
                                 "Select Color",
                                 bs_icon("info-circle")
                             ), info_colorSelect),
                             value = 1,
                             min = 1, max = 27, step = 1),
                
                ### Select Color Sensitivity ----
                # Used in color radius
                shiny::sliderInput( inputId = "colorSensitivity",
                             label = tooltip(trigger = list(
                                 "Color Sensitivity [0-1]",
                                 bs_icon("info-circle")
                             ), info_colorSensitivity),
                             min = 0.01, max = 0.99,
                             value = 0.10, step = 0.01 ),
                ### Accept Calibration ----
                shiny::actionButton(inputId = "acceptCalibration",
                             label = "Accept",
                             class = "btn-success"),
                div(class = "text-success", shiny::textOutput("acceptConfirm"))
            ),
            
            ## CALIBRATE MAIN DISPLAY --------------------------------------------------
            bslib::layout_columns(
                col_widths = c(9,3),
                
                bslib::layout_columns(
                    col_widths = c(3,9,12),
                    row_heights = c(10,2),
                    ### Image Meta Data ----
                    card(
                        fluidRow(
                            #### Panel Distance ----
                            column(8, numericInput(inputId = "panelDistance",
                                                   label   = "Panel Distance:",
                                                   value   = 0)),
                            #### Panel Dist Unit ----
                            column(4, textInput(inputId = "panelDistUnit",
                                                label   = "Unit",
                                                value   = "m"))
                        ),
                        div(
                            fluidRow(
                                #### Panel Width ----
                                column(4, numericInput(inputId = "panelWidth",
                                                       label = "Wd",
                                                       value = 0)),
                                #### Panel Height ----
                                column(4, numericInput(inputId = "panelHeight",
                                                       label = "Ht",
                                                       value = 0)),
                                #### Panel Dim. Units ----
                                column(4, textInput(inputId = "panelUnit",
                                                    label   = "Unit",
                                                    value   = "cm"))    
                            )
                        ),
                        #### Camera ID ----
                        textInput(inputId = "cameraId",
                                  label = "Camera Used"),
                        #### Calibrate Plot Name ----
                        textInput(inputId = "plotNameCalibrate",
                                  label = "Plot Name")
                    ),
                    ### Display Calibrate Image ----
                    card(
                        class = "align-items-center",
                        full_screen = TRUE,
                        # Allow clicking to select panel corners.
                        imageOutput(outputId = "imageCalibrate",
                                    dblclick = "image_click",
                                    height = "100%",
                                    width = "auto")
                    ),
                    ### Hist Plot Display ----
                    card(
                        plotOutput("colorHist_plot")
                    )
                ),
                card(
                    ### Display Panel ----
                    class = "align-items-center",
                    imageOutput(outputId = "calibrateInstruct",
                                width = "200px",
                                height = "auto"),
                    
                    ### Control Pad ----
                    div(
                        class = "control-pad",
                        div(class = "top-row",
                            actionBttn("dir_up_plus", "↑", style = "material-circle"),
                            actionBttn("dir_up_min", "↓", style = "material-circle")
                        ),
                        div(class = "center-stack",
                            actionBttn("dir_left_plus", "←", style = "material-circle"),
                            actionBttn("dir_left_min", "→", style = "material-circle")
                        ),
                        div(class = "center-bttn",
                            p("Adjust frame",
                              tooltip( bs_icon("info-circle"),
                                       "Adjust each edge of the panel independently.",
                                       position = "left"))
                        ),
                        div(class = "center-stack",
                            actionBttn("dir_right_plus", "→", style = "material-circle"),
                            actionBttn("dir_right_min", "←", style = "material-circle")
                        ),
                        div(class = "top-row",
                            actionBttn("dir_down_plus", "↓", style = "material-circle"),
                            actionBttn("dir_down_min", "↑", style = "material-circle")
                        )
                    )
                    
                )
                
            )
        )
    ),
    
    # CALCULATE PANEL ---------------------------------------------------------
    
    nav_panel(
        title = "2. Calculate",

        ## CALCULATE SIDEBAR -------------------------------------------------------
        layout_sidebar(
            sidebar = sidebar(
                position = "left",
                width = "400px",
                ### Select File ----
                disabled(
                    fileInput(inputId = "selectImageCalculate",
                              label = "Select Image")
                ),
                ### Plot name ----
                textInput(inputId = "plotName",
                          label = "Plot Name"),
                ### Color Sensitivity (2) ----
                # Select Color Radius for Image Analysis
                disabled(
                    shiny::sliderInput( inputId = "colorSensitivity2",
                                        label   ="Color Sensitivity [0-1]",
                                        min = 0.01, max = 0.99,
                                        value = 0.10, step = 0.01 )
                ),
                ### Percent obscured value box----
                value_box(
                    title = tags$h1("Percent Obscured"),
                    value = textOutput("percentCover"),
                    showcase = bs_icon("tree"),
                    theme = "bg-info"
                ),
                ### Percent Obscured: Quads ----
                verbatimTextOutput("percentCoverQuad"),
                ### Image meta text----
                verbatimTextOutput("imageMeta"),
                ### Save data ----
                actionButton(inputId = "saveCalc",
                             label = "Save Data",
                             icon = bs_icon("save"),
                             disabled = TRUE,
                             class = "btn-success"),
                
                div(class = "text-success", textOutput("saveConfirm")),
                ### Download data ----
                downloadButton(outputId = "downloadImage",
                               label = "Download Image",
                               class = "btn-success"),
                ### Toggle Image Annotation ----
                p("Annotate Panel?"),
                switchInput(inputId = "log_annoPanel",
                            size = "mini",
                            value = TRUE)
            ),
            
            ## CALCULATE MAIN DISPLAY --------------------------------------------------
            layout_columns(
                col_widths = c(8,4),
                card(
                    class = "align-items-center",
                    full_screen = TRUE,
                    ### Display Main Image ----
                    # Allow for dbl clicking to set panel location
                    imageOutput(outputId = "imageCalculate",
                                dblclick = "image_click_calc",
                                width = "100%"),
                ),
                card(
                    class = "align-items-center",
                    ### Display panel ----
                    imageOutput(outputId = "calcPanel",
                                width = "300px",
                                height = "auto"),
                    ### Control Pad ----
                    div(
                        class = "control-pad",
                        div(
                            class = "top-row",
                            actionBttn("move_up", "↑", style = "material-circle")
                        ),
                        div(class = "center-bttn",
                            actionBttn("move_left", "←", style = "material-circle"),
                        ),
                        p(class = "center-bttn",
                          "Adjust",br(),"frame"
                        ),
                        div(class = "center-bttn",
                            actionBttn("move_right", "→", style = "material-circle"),
                        ),
                        div(
                            class = "top-row",
                            actionBttn("move_down", "↓", style = "material-circle")
                        )
                    ),
                    shiny::sliderInput(inputId = "imageRotate",
                                       label   = "Rotate Image",
                                       min = -10, max = 10,
                                       step = 0.25, value = 0),
                )
            )
        )
    ),
    

# Data Summary & Export ---------------------------------------------------
    nav_panel(
        title = "3. Summary & Export",
        card(
            card_header(
                class = "d-flex justify-content-between",
                ## Table Name ----
                textInput(inputId = "tableName",
                          label = "Table Name",
                          value = "Visual Obscuration Data"),
                ## Export table ----
                downloadButton("downloadTable", 
                               "Export CSV",
                               class = "btn-info")
            ),
            ## Display Table ----
            card_body(
                DT::DTOutput("displayTable",width = "auto")
            )
        )
    ),

# Help Pane ---------------------------------------------------------------
    
    nav_spacer(),
    nav_panel(
        title = "Info",
        icon = icon("circle-info"),
        
        ## Calibration ----
        navset_card_underline(
            
            nav_panel(
                title = "Calibration",
                div(
                style = "width: 100%; text-align:justify;",
                div(
                    style = "margin-bottom: 50px; display: flex; gap: 30px;",
                    div(style = "width: 40%;",
                        p("Calibration sets the size of the panel and the colors to search
                  for when calculating obscurity. The app assumes that the calibration
                  photo and the calculation photos were taken at the same distance and
                  with the same camera."),
                        
                        p("Begin by uploading a photo for calibration in the upper left corner."),
                    ),
                    tags$img(src = "selectImage.png", height = "80px", width = "auto")
                ),
                div(
                    style = "margin-bottom: 50px; display: flex; gap: 30px;",
                    
                    div(style = "width: 40%;",
                           p("Once the photo has loaded, double-click on the upper left corner
                  and lower right corner of the panel. The image coordinates and 
                  panel dimensions within the photo will be displayed on the sidebar.
                  You will be able to precisely adjust each edge of your selection later."),
                           p("Clicked in the wrong spot? Click the \"Undo\" button to reset the
                  previous click."),
                    ),
                    div(style = "justify-content: right; display: flex;", 
                           tags$img(src = "CalibrateInstructions.gif", height = "160px", width = "auto"),
                           tags$img(src = "panelInfo.png", height = "160px", width = "auto"))
                ),
                div(
                    style = "margin-bottom: 50px; display:flex; gap: 30px; align-content:center;",
                p(style = "width:40%;", 
                "The panel can be subdivided into horizontal quadrants.
                  Quadrants will be evenly sized and distributed. Percent obscurity
                  will be measured for the entire panel and each quadrant. Note that
                  once you have accepted the calibration this cannot be changed because
                  of software limitations."),
                img(src = "quadExample.png", height = "160px")
                ),
                
                div(
                    style = "margin-bottom: 50px; display:flex; gap: 30px; align-content:center;",
                    
                p(style = "width: 40%;", "The image is now ready for calibrating colors. Once the image colors
                  have been assessed, select the color number that corresponds to the
                  color of the panel. The colors are plotted at the bottom of the screen
                  in order of most- to least-prevelent in the image. Once the panel
                  color has been selected, adjust the sensitivity. This adjusts how
                  similiar colors have to be for the app to select a pixel. A low value (0.01)
                  will select fewer pixels and discern between more colors.
                  High values (0.99) will select more pixels and group colors. You can
                  read more about the mechanics of color selection in the \"Mechanics\" pane."),
                img(src = "colorDisplay.png", width = "auto", height = "160px"),
                    img(src = "lowSens.png", width = "auto", height = "160px"),
                    img(src = "highSens.png", width = "auto", height = "160px"),
                ),
                
                div(
                    style = "margin-bottom: 50px; display:flex; gap: 30px; align-content:center;",
                p(style = "width: 40%;", "Make final adjustments to the frame position with the control pad
                  on the right side of the screen. Each pair of arrows adjusts one
                  side of the frame by one pixel. This provides the ability to precisely
                  align the selected frame with the actual panel."),
                img(src = "adustFrame1.png", height = "160px")
                ),
                
                p( style = "width: 40%;", "Complete the metadata about panel distance and size. This will
                  apply to all photos and will be be saved in the spreadsheet for 
                  your reference."),
                
                p( style = "width:40%;", "Save the calibration and begin collecting data!")
                )
                
            ),
            ## Calculation ----
            nav_panel(
                title = "Calculation",
                div(
                    style = "width: 100%; text-align:justify;",
                    
                    div(
                        style = "margin-bottom: 50px; display:flex; gap: 30px; align-content:center;",
                        div(
                            style = "width: 40%;", 
                            p("Select an image to upload from the upper left. You can change
                  the plot name. This name will be used in the spreadsheet and when
                  downloading the image."),
                            p("Double-click on the upper left corner of the panel. The program
                  will place the frame and compute obscurity. Obscurity is rounded to
                              the nearest whole percentage because precision does
                              not equate to accuracy."),
                        ),
                        img(src = "CalculateInstructions.gif", height = "160px", width = "auto")
                    ),
                    div(
                        style = "margin-bottom: 50px; display:flex; gap: 30px; align-content:center;", 
                        p(style = "width: 40%;", "Adjust the position of the frame using the control pad on the right.
                  Note, unlike the previous control pad, this does not change the frame
                  size, but moves the whole frame. If your image is a little crooked,
                  you can rotate the picture. After rotating, the frame position will
                  likely have to be adjusted. Simply double-click again on the upper left
                  corner."),
                        img(src = "adjustFrame2.png", height = "160px", width = "auto")
                    ),
                    div(
                        style = "margin-bottom: 50px; display:flex; gap: 30px; align-content:center;", 
                        p(style = "width: 40%;", "Once the frame is properly positioned, the data can be saved to
                  a spreadsheet (\"Save Data\") and the image can be downloaded.
                  The downloaded image will include the original picture and the
                  panel with annotated obscurity values."),
                        img(src = "panelExport.jpg", height = "200px", width = "auto")
                    ),
                p(style = "width: 40%;", "To calculate another photo, simply upload a new photo. Your data
                  will be saved and program will reset.")
                )
            ),
            ## Summary & Export ----
            nav_panel(
                title = "Summary & Export",
                
                p(style = "width: 40%; text-align: justify;", "Once you have completed all of the calculations, you can view
                  a table of the results. Change the name of the table and export
                  the results in the upper right corner. When the table is exported
                  the column names are \"cleaned\" using the ", code("janitor"), "package."),
                p("The datatable includes the following:"),
                tags$table(
                    class = "row-height",
                    tags$thead(
                        tags$tr(tags$th("Variable", style = "width: 10%;"), 
                                tags$th("Description", style = "width: 90%;"))
                    ),
                    tags$tbody(
                        tags$tr(
                            tags$td(strong("plot_name")), tags$td("Name of plot as set in calculate pane.")
                        ),
                        tags$tr(
                            tags$td(strong("percent_obscured")), tags$td("Percent of panel obscured for the whole panel.")
                        ),
                        tags$tr(
                            tags$td(strong("quad_{#}_obscured")), tags$td("Percent of quadrant obscured.")
                        ),
                        tags$tr(
                            tags$td(strong("panel_distance")), tags$td("The width and height of the panel
                                                                       in the real world as set on the calibration pane.")
                        ),
                        tags$tr(
                            tags$td(strong("panel_dim_unit",br(),"panel_dist_unit")), tags$td("Distance of camera from panel as set on the
                            calibration pane.")
                        ),
                        tags$tr(
                            tags$td(strong("panel_dim_unit",br(),"panel_dist_unit")), tags$td("Real world units of panel
                  dimensions and distance.")
                        ),
                        tags$tr(
                            tags$td(strong("camera_id")), tags$td("Camera ID as set in calibration pane.")
                        ),
                        tags$tr(
                            tags$td(strong("image_filepath",br(),"panel_filepath"), tags$td("File names of original image and exported panel."))
                        )
                        
                    )
                )
                
                
            ),
            ## Mechanics ----
            nav_panel(
                title = "Mechanics",
                
                div(
                    style = "width: 100%; text-align:justify;",
                    
                    div(
                        style = "margin-bottom: 50px; display:flex; gap: 30px; align-content:center;",
                        p( style = "width: 40%;",
                           "The app utilizes the ", code("countcolors"), " and ",
                           code("colordistance"), " packages. For an indepth explanation
                       of the packages, read ", tags$a(href = "https://cran.r-project.org/web/packages/countcolors/vignettes/Introduction.html",
                                                       "Introduction to countcolors package"), ".
                       In summary, the packages map each pixel's RGB color value
                       into a 3-D color space. To select colors, a center RGB value
                       is defined (this is the color selected in Calibration), then
                       a search radius is defined (the color sensitivity).", br(),br(),
                           "You probablly don't have the RGB value of your panel
                           memorized. To simplify the search process, the ", code("colordistance"),
                           " package is used. Each color channel is divided into three bins,
                           resulting in 27 unique bins. Bins are sorted based on the number
                           of pixels in each bin and the average value of the pixels.
                           While this is not precise for determining panel color, it
                           is simple. It is premised on the assumption that the panel
                           color will be unique enough to be in its own bin.", br(),br(),
                           "To increase processing time, the pixel selection is limited
                           to the area immediately around the frame using the ", code("magick"),
                           " package.", br(),br(),
                           "You can access the code on GitHub: ", 
                           tags$a(href = "https://github.com/zander-opperman/Fuel-Visual-Obscurity",
                                  "https://github.com/zander-opperman/Fuel-Visual-Obscurity"),
                           "."
                        ),
                        img(src = "Pixel_Plot.jpeg", height = "250px", width = "auto")
                    )
                )
            ),
            ## Citations ----
            nav_panel(
                title = "Citations",
                verbatimTextOutput("citations")
            )
        )
    )
    
)




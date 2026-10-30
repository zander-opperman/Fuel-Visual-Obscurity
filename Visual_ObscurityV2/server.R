# VERSION 2 #

library(shiny)
library(magick)
library(colordistance)
library(countcolors)
library(tidyverse)
library(shinyjs)
library(janitor)

# Functions for image processing
source("www/helpersV2.R")

# Temporary File path for calibrating image
tmp_fp_imageCalibrate      <- tempfile(fileext = ".jpg")
# Temporary File path for calibrating image with color ID
tmp_fp_imageCalibrateColor <- tempfile(fileext = ".jpg")
# Temporary file path for calibrating panel display
tmp_fp_panelCalibrate      <- tempfile(fileext = ".jpg")
# Temporary file path for calculating image
tmp_fp_imageCalculate      <- tempfile(fileext = ".jpg")
# Temporary file path for calculating image with color ID
tmp_fp_imageCalculateColor <- tempfile(fileext = ".jpg")
# Temporary file path for calculated panel
tmp_fp_panel               <- tempfile(fileext = ".jpg")
# Temporary file path for exporting image
tmp_fp_saveImage           <- tempfile(fileext = ".jpg")

function(input, output, session) {
    
# CALIBRATING IMAGE -------------------------------------------------------
    
    ## File Path ----
    # 1. File path to calibrating image. Reduced in size using function.
    fp_imageCalibrate <- reactive(
        reduceImageSize(
            normalizePath(file.path(req(input$selectImageCalibrate$datapath))),
            tmp_fp_imageCalibrate,
            rotate.angle = 0
        )
    )
    
    ## Display Main Image ----
    # 2. Output calibrating image.
    output$imageCalibrate <- renderImage({
        req(fp_imageCalibrate)
        
        # Display image w/ color ID after Calibrate Colors
        if(log_calibrateColors()){
            top_left = c(clickCoords$click1x, clickCoords$click1y)
            img.path = imageColorOutput(img.path = fp_imageCalibrate(),
                                        out.path = tmp_fp_imageCalibrateColor,
                                        rgb.mat  = req(colorHistDF()),
                                        colorSelect = input$colorSelect,
                                        colorSens   = input$colorSensitivity,
                                        panelDims   = panelSize,
                                        top_left    = top_left,
                                        n.quad = as.numeric(input$numberQuadrants))
        
        # Display original raw image (no alterations)
        }else{
            img.path = fp_imageCalibrate()
        }
        
        list(src = img.path,
             alt = "Failed to display.",
             height = "auto",
             width = "auto")
    }, deleteFile = FALSE) |> 
        bindEvent(input$calibrateColors, 
                  input$selectImageCalibrate,
                  input$colorSelect,
                  input$colorSensitivity,
                  input$undoClick,
                  input$dir_left_plus,
                  input$dir_left_min,
                  input$dir_right_plus,
                  input$dir_right_min,
                  input$dir_up_plus,
                  input$dir_up_min,
                  input$dir_down_plus,
                  input$dir_down_min)
    
    ## Instruction and Panel Display ----
    # Display Click Instructions
    output$calibrateInstruct <- renderImage({
        req(fp_imageCalibrate)
        
        # Display two color panel
        if(log_secondClick() & log_calibrateColors()){
            topleft <- c(clickCoords$click1x, clickCoords$click1y)
            img <- calculatePanel(req(fp_imageCalibrate()),
                                  tmp_fp_panelCalibrate,
                           req(colorHistDF()),
                           req(input$colorSelect),
                           req(input$colorSensitivity),
                           req(panelSize),
                           topleft,
                           as.numeric(input$numberQuadrants),
                           input$log_annoPanel)
            
            img.src <- img$src
            img.info <- image_info(image_read(img$src))
            if(img.info$height / img.info$width > 2){
            wd <- "125px"
            }else{
                wd <- "200px"
            }
        
        # Display instruction graphic before calibrating colors
        }else{
            wd <- "200px"
            img.src <- "www/CalibrateInstructions.gif"
        }
        list(src = img.src,
             alt = "",
             width = wd,
             height = "auto")
    }, deleteFile = FALSE)
    
    ## Panel Coordinates ----
    # Logicals for assigning values to clicks
    log_firstClick  <- reactiveVal(FALSE)
    log_secondClick <- reactiveVal(FALSE)
    
    # Reactive Value for storing click coords.
    clickCoords <- reactiveValues(
        click1x = 0,
        click1y = 0,
        click2x = 0,
        click2y = 0
    )
    
    ### Assign Panel Coords ----
    # 3. Assign click coords based on number of click inputs received.
    observe({
        if(!log_firstClick()){
            clickCoords$click1x <- round(input$image_click$x)
            clickCoords$click1y <- round(input$image_click$y)
            log_firstClick(TRUE)
        }else if(!log_secondClick()){
            clickCoords$click2x <- round(input$image_click$x)
            clickCoords$click2y <- round(input$image_click$y)
            log_secondClick(TRUE)
        }
    }) |> 
        bindEvent(input$image_click)
        
    ### Undo Click Assignment ----
    observe({
        if(log_secondClick()){
            log_secondClick(FALSE)
            clickCoords$click2x <- 0
            clickCoords$click2y <- 0
        }else if(log_firstClick()){
            log_firstClick(FALSE)
            clickCoords$click1x <- 0
            clickCoords$click1y <- 0
        }
        
        # Undo calibration if second-click is undone
        log_calibrateColors(FALSE)
        updateActionButton(inputId = "calibrateColors",
                           disabled = TRUE)
    }) |> 
        bindEvent( input$undoClick )
    
    ### Display Click coords ----
    output$click_info <- renderPrint({
        # Display instructions before click
        if(!log_firstClick()){
            cat("Double-click upper left \nand lower right corners",
                sep = "")
        }else{
        # Display click coords.
            cat( "1) ", clickCoords$click1x, ", ", clickCoords$click1y, "\n",
                 "2) ", clickCoords$click2x, ", ", clickCoords$click2y, "\n",
                 sep = "")
        }
        
    })
    
    ## Panel Size ----
    # Panel Size Calculated from Clicks
    panelSize <- reactiveValues()
    
    # Assign panel size values based on means of click coords
    observe({
        req(log_secondClick())
        
        panelSize$width = clickCoords$click2x - clickCoords$click1x
        
        panelSize$height = clickCoords$click2y - clickCoords$click1y
        
        updateActionButton(inputId = "calibrateColors", disabled = FALSE)
    })
    
    ### Micro Adjustments ----
    # Left side
    observe({
        clickCoords$click1x = clickCoords$click1x - 1
    }) |> 
        bindEvent( input$dir_left_plus )
    observe({
        clickCoords$click1x = clickCoords$click1x + 1
    }) |> 
        bindEvent( input$dir_left_min )
    # Right side
    observe({
        clickCoords$click2x = clickCoords$click2x + 1
    }) |> 
        bindEvent( input$dir_right_plus )
    observe({
        clickCoords$click2x = clickCoords$click2x - 1
    }) |> 
        bindEvent( input$dir_right_min )
    # Top side
    observe({
        clickCoords$click1y = clickCoords$click1y - 1
    }) |> 
        bindEvent( input$dir_up_plus )
    observe({
        clickCoords$click1y = clickCoords$click1y + 1
    }) |> 
        bindEvent( input$dir_up_min )
    # Bottom side
    observe({
        clickCoords$click2y = clickCoords$click2y + 1
    }) |> 
        bindEvent( input$dir_down_plus )
    observe({
        clickCoords$click2y = clickCoords$click2y - 1
    }) |> 
        bindEvent( input$dir_down_min )
    

    
    # Display panel Size
    output$panel_info <- renderPrint({
        req(log_secondClick())
        cat("Width: ", panelSize$width, "px \n",
            "Height: ", panelSize$height, "px", sep = "")
    })
    
    
    ## Find colors ----
    # Logical Value for when calibrating colors
    log_calibrateColors <- reactiveVal(FALSE)
    observe({
        log_calibrateColors(TRUE)
    }) |> 
        bindEvent(input$calibrateColors)
    
    ### Color DF ----
    # Data frame of color values.
    colorHistDF <- reactive({
        colordistance::getImageHist(fp_imageCalibrate(),
                                    bins = 3,
                                    bin.avg = TRUE,
                                    plotting = FALSE) |>
            dplyr::arrange(desc(Pct)) |>
            dplyr::mutate(
                fillColor = rgb(r,g,b),
                row = row_number()
            )
    }) |> 
        bindEvent(input$calibrateColors)
    
    
    ## Color Hist ----
    # Output plot of color histogram
    output$colorHist_plot <- renderPlot({
        req(colorHistDF)
        
        # Max color row for x-axis label breaks
        maxVal <- colorHistDF() |> 
            dplyr::filter(Pct > 1e-4) |> 
            dplyr::slice_max(row, n =1 ) |> 
            dplyr::pull(row)
        
        colorHistDF() |>
            dplyr::filter(Pct > 1e-4) |> 
            dplyr::mutate(
                # Used to outline selected color
                outline = ifelse(row == input$colorSelect, T, F)
            ) |> 
            ggplot2::ggplot(aes(row, 1)) +
            ggplot2::geom_col(aes(fill = fillColor,
                         colour = outline),
                     linewidth = 1.2) +
            # Colors the columns based on the value of fillColor
            ggplot2::scale_fill_identity() +
            ggplot2::theme(
                axis.title = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                axis.text.y = element_blank(),
                panel.background = element_rect(fill = "white"),
                legend.position = "none"
            ) +
            ggplot2::scale_x_continuous(
                breaks = seq(1,maxVal,1)
            ) +
            ggplot2::scale_color_discrete(
                palette = c("transparent", "firebrick1")
            )
    })
    
    
    observe({
        # Update input of Plot Name
        updateTextInput(inputId = "plotNameCalibrate",
                        value = tools::file_path_sans_ext(input$selectImageCalibrate$name))
        # Reset accept button if new file uploaded
        log_accept(FALSE)
        # Allow color sensitivity adjustment on the calibrate pane.
        enable("colorSensitivity")
        disable("colorSensitvity2")
    }) |> 
        bindEvent(input$selectImageCalibrate)

    ## Color Sens. Sliders ----
    # Toggles which slider is updating
    log_accept <- reactiveVal(FALSE)
    observe({
        log_accept(TRUE)
        # Disabled after accept because if this is changed after data has
        # been saved then the program breaks.
        disable("numberQuadrants")
        # Disable the color sensitivity slider on the calibrate pane.
        disable("colorSensitivity")
        enable("colorSensitivity2")
        enable("selectImageCalculate")
    }) |> 
        bindEvent(input$acceptCalibration)
    observe({
        if(log_accept()){
            updateSliderInput(inputId = "colorSensitivity",
                              value = input$colorSensitivity2)
        }else{
            updateSliderInput(inputId = "colorSensitivity2",
                              value = input$colorSensitivity)
        }
    }) |> 
        bindEvent(input$colorSensitivity2,
                  input$colorSensitivity)
    
    # Affirmation
    output$acceptConfirm <- renderText({
        req(input$acceptCalibration)
        "Calibration Saved!"
    })
# CALCULATING IMAGE -------------------------------------------------------
    ## File Path ----
    # File path to image for calculation
    fp_imageCalculate <- reactive({
        reduceImageSize(
            normalizePath(file.path(req(input$selectImageCalculate$datapath))),
            tmp_fp_imageCalculate,
            input$imageRotate
        )
    })
    
    ## Logical: Image clicked ----
    # Logical for when the image has been clicked
    log_dblClkCalculate <- reactiveVal(FALSE)
    observe({
        log_dblClkCalculate(TRUE)
        updateActionButton(inputId = "saveCalc",
                           disabled = FALSE)
    }) |> 
        bindEvent(input$image_click_calc)
    
    
    ## Click Coords ----
    # Coords of click for top left of panel
    calcTopLeft <- reactiveValues()
    observe({
        calcTopLeft$x <- round(input$image_click_calc$x)
        calcTopLeft$y <- round(input$image_click_calc$y)
    }) |> 
        bindEvent(input$image_click_calc)
    ### Micro adjustments ----
    observe({
        calcTopLeft$x <- calcTopLeft$x - 1
    }) |> 
        bindEvent( input$move_left)
    observe({
        calcTopLeft$x <- calcTopLeft$x + 1
    }) |> 
        bindEvent( input$move_right)
    observe({
        calcTopLeft$y <- calcTopLeft$y - 1
    }) |> 
        bindEvent( input$move_up)
    observe({
        calcTopLeft$y <- calcTopLeft$y + 1
    }) |> 
        bindEvent( input$move_down)
    
    
    ## Display Main Image ----
    # Output of calculation image
    output$imageCalculate <- renderImage({
        req(fp_imageCalculate, panelSize)
        
        if(log_dblClkCalculate()){
            # Display image with color ID
            topleft = c(calcTopLeft$x, calcTopLeft$y)
            imgpath = imageColorOutput(fp_imageCalculate(),
                                       tmp_fp_imageCalculateColor,
                                       colorHistDF(),
                                       input$colorSelect,
                                       input$colorSensitivity,
                                       panelSize,
                                       topleft,
                                       as.numeric(input$numberQuadrants))
        }else{
            # Display unaltered image
            imgpath = fp_imageCalculate()
        }
        
        list(
            src = imgpath,
            alt = "Failed to Display.",
            height = "auto",
            width = "auto"
        )
    }, deleteFile = FALSE)
    
    ## Panel Calculations ----
    # Outputs src, img.fraction, quad.fraction (list), pixel.idx
    panelCalc <- reactive({
        req(log_dblClkCalculate())
        
        topleft = c(calcTopLeft$x, calcTopLeft$y)
        calculatePanel(req(fp_imageCalculate()),
                       tmp_fp_panel,
                       req(colorHistDF()),
                       req(input$colorSelect),
                       req(input$colorSensitivity),
                       req(panelSize),
                       topleft,
                       as.numeric(input$numberQuadrants),
                       input$log_annoPanel)
    })
    
    
    ## Display Panel ----
    output$calcPanel <- renderImage({
        if(log_dblClkCalculate()){
            # Display two color panel
            img.src <- panelCalc()$src
            
            img.info <- image_info(image_read(img.src))
            if(img.info$height / img.info$width > 2){
                wd <- "150px"
            }else{
                wd <- "300px"
            }
        }else{
            # Display instructions
            img.src <- "www/CalculateInstructions.gif"
            wd <- "300px"    
        }
        
        list(
            src = img.src,
            alt = "Failed To Display.",
            height = "auto",
            width = wd
        )
    }, deleteFile = FALSE)
    
    ## Perc. Obscured Text ----
    output$percentCover <- renderText({
        req(panelCalc)
        val <- round((1-panelCalc()$img.fraction)*100)
        paste0(val, "%")
    })
    # Percent Obscured for Quadrants
    output$percentCoverQuad <- renderPrint({
        req(input$numberQuadrants > 1)
        cat("Quadrant Obscurities:")
        for(q in 1:length(panelCalc()$quad.fraction)){
            cat("\n",q,") ",round((1-panelCalc()$quad.fraction[q])*100),"%")
        }
    })
    # Archived until further development
    # output$panelHtAnalysis <- renderPlot({
    #     req(panelCalc)
    #     
    #     panelCalc()$pixel.idx |> 
    #         as.data.frame() |> 
    #         count(row) |> 
    #         ggplot(aes(1,row, fill = n)) +
    #         geom_tile() +
    #         scale_fill_gradient(
    #             high = "green",
    #             low = "red"
    #         ) +
    #         scale_y_continuous(
    #             transform = "reverse"
    #         ) +
    #         coord_fixed(0.05) +
    #         theme_void() +
    #         theme(
    #             legend.position = "none"
    #         )
    # })
    
    ## Image Meta Text ----
    output$imageMeta <- renderPrint({
        cat( "Panel Size: ", input$panelWidth, " ", input$panelUnit,
             " x ", input$panelHeight, " ", input$panelUnit, "\n",
             "Panel Distance: ", input$panelDistance, " ", input$panelDistUnit, "\n",
             "Camera: ", input$cameraId, sep = "")
    })
    
    ## Update values After Upload ----
    # Update input of Plot Name, save button, dbl click, rotation
    # after a new image is uploaded
    observe({
        updateTextInput(inputId = "plotName",
                        value = tools::file_path_sans_ext(input$selectImageCalculate$name))
        updateActionButton(inputId = "saveCalc",
                           disabled = TRUE)
        log_saveCalc(FALSE)
        log_dblClkCalculate(FALSE)
        updateSliderInput(inputId = "imageRotate",
                          value = 0)
    }) |> 
        bindEvent(input$selectImageCalculate)
    
    ## Save Affirmation ----
    log_saveCalc <- reactiveVal(FALSE)
    observe({
        log_saveCalc(TRUE)
    }) |> 
        bindEvent(input$saveCalc)
    output$saveConfirm <- renderText({
        req(log_saveCalc())
        "Saved!"
    })
    
    ## Image Save Name ----
    imageName_file <- reactiveVal()
    observe({
        imageName_file(
            paste0(input$plotName, "_obscurity.jpg", collapse = "")
        )
    }) |> 
        bindEvent(input$plotName)
    
    ## Download Image ----
    output$downloadImage <- downloadHandler(
        filename = imageName_file,
        content = function(file){
            topleft <- c(calcTopLeft$x, calcTopLeft$y)
            magick::image_read(
                combineImagePanel(
                    reduceImageSize(
                        normalizePath(file.path(req(input$selectImageCalculate$datapath))),
                        tmp_fp_imageCalculate,
                        rotate.angle = 0
                    ),
                    tmp_fp_panel,
                    panelCalc()$img.fraction,
                    input$plotName,
                    panelSize,
                    topleft,
                    as.numeric(input$numberQuadrants),
                    panelCalc()$quad.fraction,
                    input$imageRotate,
                    input$log_annoPanel,
                    tmp_fp_saveImage
                )
            ) |> 
               magick::image_write(file)
        },
        contentType = "image/jpg"
        
    )
# Data Summary & Export ---------------------------------------------------

    ## Data Table ----
    dataOutput <- reactiveValues(
        data = data.frame(
            `Plot Name` = character(),
            `Percent Obscured` = numeric(),
            `Panel Width` = numeric(),
            `Panel Height` = numeric(),
            `Panel Dim. Unit` = character(),
            `Panel Distance` = numeric(),
            `Panel Dist. Unit` = character(),
            `Camera ID` = character(),
            image_filepath = character(),
            panel_filepath = character(),
            
            check.names = FALSE
        )
    )
    ## Add Quad. Cols ----
    observe({
        if(as.numeric(input$numberQuadrants) > 1){
            
            for(q in seq_len(as.numeric(input$numberQuadrants))){
                
            dataOutput$data <- dataOutput$data |> 
                mutate("Quad. {q} Obscurity" := numeric(),
                       .before = `Panel Width`)
            }
        }
    }) |> 
        bindEvent(input$numberQuadrants)
    ## Add Data ----
    observe({
        new_input <- data.frame(
            `Plot Name` = input$plotName,
            `Percent Obscured` = round((1-panelCalc()$img.fraction)*100),
            `Panel Width` = input$panelWidth,
            `Panel Height` = input$panelHeight,
            `Panel Dim. Unit` = input$panelUnit,
            `Panel Distance` = input$panelDistance,
            `Panel Dist. Unit` = input$panelDistUnit,
            `Camera ID` = input$cameraId,
            image_filepath = input$selectImageCalculate$name,
            panel_filepath =  paste0(input$plotName, "_obscurity.jpg", collapse = ""),
            
            check.names = FALSE
        )
        if(as.numeric(input$numberQuadrants) > 1){
            for(q in seq_len(as.numeric(input$numberQuadrants))){
                
                new_input <- new_input |> 
                    mutate("Quad. {q} Obscured" := round((1-panelCalc()$quad.fraction[q])*100),
                           .before = `Panel Width`)
            }
        }
        dataOutput$data <- rbind(dataOutput$data, new_input)
    }) |> 
        bindEvent(input$saveCalc)
    
    ## Display Data table ----
    output$displayTable <- DT::renderDT({
        dataOutput$data |>
            select(-contains("filepath"))
    })
    
    # Save Name for data table
    tableName_file <- reactiveVal()
    observe({
        tableName_file(
            paste0(input$tableName, ".csv")
        )
    }) |> 
        bindEvent(input$tableName)
    ## Dowload Table ----
    output$downloadTable <- downloadHandler(
        filename = tableName_file,
        content = function(file){
            write.csv(dataOutput$data |> 
                          janitor::clean_names(), 
                      file, 
                      row.names = FALSE)
        }
    )
    

# Citations ---------------------------------------------------------------
    output$citations <- renderText({
        paste(readLines("www/citations.txt"),
              collapse = "\n")
    })
        
}

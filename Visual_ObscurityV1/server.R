# VERSION 1 #
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(magick)
library(colordistance)
library(countcolors)
library(tidyverse)
library(shinyjs)
library(Rcpp)
library(RcppArmadillo)

source("www/helpersV1.R")
sourceCpp("www/helpersV1.cpp")


# Define server logic required to draw a histogram
function(input, output, session) {
    # Temporary File for calibrating image size
    importCalibrate_image <- reactiveValues(path = tempfile(fileext = ".jpg"))
    # Temporary File for calculating image size
    importCalculate_image <- reactiveValues(path = tempfile(fileext = ".jpg"))
    # Temporary File for cropped image while Calibrating
    cropped_image <- reactiveValues(path = tempfile(fileext = ".jpg"))
    # Temporary file for isolating color panel when calculating
    isolate_image <- reactiveValues(path = tempfile(fileext = ".jpg"))
    # Temporary file for panel display
    panel_image <- reactiveValues(path = tempfile(fileext = ".jpg"))
    
    # Path to calibrating image
    filenameCalibrate <- reactive({
        reduceImageSize(
            normalizePath(file.path(input$selectImageCalibrate$datapath)),
            importCalibrate_image$path
        )
    }) |> 
        bindEvent(input$selectImageCalibrate)
    
    # Path to calculating image
    filenameImage <- reactive({
        reduceImageSize(
            normalizePath(file.path(input$selectImage$datapath)),
            importCalculate_image$path
        )
    }) |> 
        bindEvent(input$selectImage)
    
    # Create and path to isolated panel image.
    filenameIsolate <- reactive({
        req(filenameImage, input$colorSelect)
        isolatePanel(filenameImage(), isolate_image$path,
                     panelSize(), input$colorSelect,
                     colorHistDF(), input$colorSensitivity)
    })
    
    
    
    firstColorCalc <- reactive({
        req(filenameIsolate, input$colorSelect, input$colorSensitivity)
        imgColorClass <- jpeg::readJPEG(filenameIsolate())
        img <- countcolors::sphericalRange(imgColorClass,
                                    center = c(
                                        colorHistDF()$r[input$colorSelect],
                                        colorHistDF()$g[input$colorSelect],
                                        colorHistDF()$b[input$colorSelect]
                                    ),
                                    radius = input$colorSensitivity,
                                    target.color = "magenta",
                                    plotting = FALSE)
    })
    
    
    firstColorCalc_dims <- reactive({
        
        # dims <- panelCoords(findDensity(firstColorCalc()$pixel.idx))
        # xleft <- 
        
        
        
        dims <- calculatePanelSize(filenameIsolate(), req(firstColorCalc()$pixel.idx),
                                                          input$windowSize, 
                                                          input$panelSens1,
                                                          input$panelSens2)
        xleft <- dims$width_min
        ybottom <- dims$height_min + panelSize()$height
        xright <- dims$width_min + panelSize()$width
        ytop <- dims$height_min
        
        list(xleft = xleft, 
             ybottom = ybottom, 
             xright = xright, 
             ytop = ytop)
    })
    
    coordAdjust <- reactiveValues(x = 0, y = 0)
    
    observe({
        coordAdjust$x = coordAdjust$x + 1
    }) |> 
        bindEvent(input$dir_right)
    
    observe({
        coordAdjust$x = coordAdjust$x - 1
    }) |> 
        bindEvent(input$dir_left)
    
    observe({
        coordAdjust$y = coordAdjust$y - 1
    }) |> 
        bindEvent(input$dir_up)
    
    observe({
        coordAdjust$y = coordAdjust$y + 1
    }) |> 
        bindEvent(input$dir_down)
    
    observe({
        coordAdjust$x = 0
        coordAdjust$y = 0
    }) |> 
        bindEvent(input$dir_reset, input$selectImage)
    
    
    colorCalc_dims <- reactive({
        list(
            xleft = firstColorCalc_dims()$xleft + coordAdjust$x ,
            ybottom = firstColorCalc_dims()$ybottom + coordAdjust$y,
            xright = firstColorCalc_dims()$xright + coordAdjust$x ,
            ytop = firstColorCalc_dims()$ytop + coordAdjust$y
        )
    })
    
    panelCalc <- reactive({
        req(colorCalc_dims)
        calculatePanel(filenameImage(), panel_image$path,
                       colorCalc_dims(), input$colorSelect,
                       colorHistDF(), input$colorSensitivity)
    })
    
    output$panelCalcImage <- renderImage({
        req(panelCalc)
        list(
            src = panelCalc()$src,
            alt = "Failed to Display.",
            height = "auto",
            width = "width"
        )
        
    }, deleteFile = TRUE)
    
    output$percentCover <- renderText({
        req(panelCalc)
        val <- round((1-panelCalc()$img.fraction)*100)
        paste0(val, "%")
    })
    
    output$imageCalc <- renderImage({
        req(filenameIsolate)
        
        ext <- tools::file_ext(filenameImage())
        validate(need(ext == "jpg", "Please upload JPEG file."))
        
        tmp_imageCalc <- tempfile(fileext = ".jpg")
        
        # displayImage <- image_read(filenameImage()) |> 
        displayImage <- image_read(isolate_image$path) |> 
            image_draw()
        
        rect(
            colorCalc_dims()$xleft,
            colorCalc_dims()$ybottom,
            colorCalc_dims()$xright,
            colorCalc_dims()$ytop,
            border = "black"
        )
        dev.off()
        image_write(
            displayImage, 
            path = tmp_imageCalc)
        
        list(src = tmp_imageCalc,
             alt = "Failed to display.",
             height = "auto",
             width = "100%")
    }, deleteFile = FALSE) 
    
    
    
    # Send a pre-rendered image, and don't delete the image after sending it
    output$imageCalibrate <- renderImage({
        req(filenameCalibrate)
        
        ext <- tools::file_ext(filenameCalibrate())
        
        validate(need(ext == "jpg", "Please upload JPEG file."))
        # Return a list containing the filename and alt text
        list(src = filenameCalibrate(),
             alt = "Failed to Display.",
             height = "auto",
             width = "width"
        )
             
    }, deleteFile = FALSE)
        
    # output$imageInfo <- renderPrint(print(input$selectImage))

    # Capture and display a bounding box selection (brush)
    output$brush_info <- renderPrint({
        req(input$image_brush)
        cat("X min:", input$image_brush$xmin, "\n",
            "X max:", input$image_brush$xmax, "\n",
            "Y min:", input$image_brush$ymin, "\n",
            "Y max:", input$image_brush$ymax)
    })

    calculateColorLogic <- reactiveVal(value = FALSE)
    observe({
        calculateColorLogic(TRUE)
    }) |> 
        bindEvent(input$calculateColors)
    observe({
        calculateColorLogic(FALSE)
    }) |> 
        bindEvent(input$selectImageCalibrate)

    output$imageCrop <- renderImage({
        
        req(filenameCalibrate, input$image_brush)
        ext <- tools::file_ext(filenameCalibrate())
        validate(need(ext == "jpg", "Please upload JPEG file."))

        if(calculateColorLogic()){
            img <- imageColorID()$path
        }else{
            img <- imageCroppedOutput(filenameCalibrate(), 
                                      cropped_image$path,
                                      input$image_brush)
        }
        
        list(src = img,
             alt = "Failed to Display.",
             height = "auto",
             width = "100%")

    }, deleteFile = FALSE)

    # calculate Color histogram
    colorHistDF <- reactive({
        getImageHist(cropped_image$path,
                     bins = 3,
                     bin.avg = TRUE,
                     plotting = FALSE) |>
            arrange(desc(Pct)) |>
            mutate(fillColor = rgb(r,g,b),
                   row = row_number() )
    }) |> 
        bindEvent(input$calculateColors)
        
    # Plot of color histogram
    output$imageCropPixelMap <- renderPlot({
        req(colorHistDF)
        
        colorHistDF() |> 
            ggplot(aes(row, Pct)) +
            geom_col(aes(fill = fillColor)) +
            scale_fill_identity() +
            theme(
                axis.title = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                axis.text.x = element_blank(),
                panel.background = element_rect(fill = "white")
            ) +
            coord_flip() + 
            # scale_x_reverse() +
            scale_x_continuous(
                breaks = c(1:10),
                labels = c(1:10),
                limits = c(0.5,10),
                transform = "reverse"
            ) 
            
    })
    

    imageColorID <- reactive({
        req(colorHistDF)
        tmp_path <- tempfile(fileext = ".jpg")
        imageColorOutput(cropped_image$path,
                                tmp_path,
                                colorHistDF(),
                                input$colorSelect,
                                input$colorSensitivity,
                         input$windowSize,
                         input$panelSens1,
                         input$panelSens2
        )
    }) |> 
        bindEvent(input$colorSelect,
                  input$colorSensitivity,
                  input$calculateColors,
                  input$windowSize,
                  input$panelSens1,
                  input$panelSens2)
    
    output$panelSizeText <- renderUI({
        req(calculateColorLogic())
        
        val1 <- paste0("Panel Width: ", imageColorID()$width_dif)
        val2 <- paste0("Panel Height: ", imageColorID()$height_dif)
        
        HTML(paste(c(val1, val2), collapse = "<br>"))
        
    })  
    
    panelSize <- reactive({
        list(
            width = imageColorID()$width_dif,
            height = imageColorID()$height_dif
        )
    }) |> 
        bindEvent(input$acceptPanelSize)
    
    
    #Activate Accept Panel Size Button
    observe({
        req(input$calculateColors)
        updateActionButton(session = getDefaultReactiveDomain(),
                           "acceptPanelSize",
                           disabled = FALSE)
    })
    
    observe({
        shinyjs::addClass(id = "panelSizeDiv", class = "btn-success")
    }) |> 
        bindEvent(input$acceptPanelSize)
    
    # Activate Calculate Colors Button
    observe({
        req(input$image_brush)
        updateActionButton(
            inputId = "calculateColors",
            disabled = FALSE
        )
    })
    
    observe({
        updateActionButton( inputId = "calculateColors",
                            disabled = TRUE)
        updateActionButton(inputId = "acceptPanelSize",
                           disabled = TRUE)
        updateNumericInput(inputId = "colorSelect",
                           value = 1)
    }) |> 
        bindEvent(input$selectImageCalibrate)
}

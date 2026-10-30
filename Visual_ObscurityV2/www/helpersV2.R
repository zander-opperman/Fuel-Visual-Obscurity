
#' Alter Image Angle
#' 
#' Alters image angle using [magick] package returning image to 
#' a file path. 
#' 
#'
#' @param img.path File path to original image.
#' @param out.path File path to altered image.
#' @param rotate.angle Angle in degrees to rotate image clock-wise.
#'    Negative values are accepted. Default is 0.
#'
#' @returns [out.path] with newly assigned image.
#' @noRd
#' @examples
#' tmp.out <- tempfile(fileext = ".jpg")
#' frink_fp <-"https://jeroen.github.io/images/frink.png"
#' out.path <- reduceImageSize(frink_fp, tmp.out, 45)
#' image_read(out.path)
#' unlink(tmp.out, out.path)
reduceImageSize <- function(img.path, out.path,
                            rotate.angle = 0){
  magick::image_read(img.path) |> 
    image_rotate(rotate.angle) |> 
    # magick::image_resize(geometry = "1000x1000>") |> 
    magick::image_write(path = out.path)
  
  return(out.path)
}


#' Display frame and ID colors on image.
#' 
#' @description
#' Places the frame with pixel ID around the frame on the original
#' input image. 
#'
#' @details
#' This function takes an image (in the form of a file path) and
#' adds a frame with quadrants at the user selected locations. 
#' Additionally, it identifies selected colors using [countcolors::sphericalRange()]
#' in an area that is 10% larger than the selected frame.
#' 
#' By reducing the area of color identification, processing time is
#' reduced while providing the user with only the information they
#' are interested in. This is accomplished using the [magick] package. 
#' 
#' @param img.path File path to original image.
#' @param out.path File path to annotated image.
#' @param rgb.mat A data frame of colors with columns r, g, b with values [0,1].
#' @param colorSelect A number to index rgb.mat.
#' @param colorSens A number ([0,1]) used for search radius in [countcolors::sphericalRange()].
#' @param panelDims A list of panel dimensions with $width and $height.
#' @param top_left A vector of the (x,y) coordinates of the top-left position of the frame.
#' @param n.quad The number of quadrants to display.
#'
#' @returns [out.path] with newly annotated image
#' @export
#' 
imageColorOutput <- function(img.path, out.path,
                             rgb.mat, colorSelect,
                             colorSens, panelDims, 
                             top_left, n.quad){
  wd <- round(panelDims$width*1.2)
  ht <- round(panelDims$height*1.2)
  x.coord <- round(top_left[1]-panelDims$width*0.1)
  y.coord <- round(top_left[2]-panelDims$height*0.1)
  
  crp.geo <- paste0(wd,"x",ht,"+",x.coord,"+",y.coord)
  crp.tmp <- tempfile(fileext = ".jpg")

    img.crop <- image_read(img.path) |> 
    image_crop(geometry = crp.geo) |> 
    image_write(path = crp.tmp)
  
  
  img.id <- tryCatch( countcolors::sphericalRange(jpeg::readJPEG(crp.tmp),
                                        center = c(
                                          rgb.mat$r[colorSelect],
                                          rgb.mat$g[colorSelect],
                                          rgb.mat$b[colorSelect]
                                        ),
                                        radius = colorSens,
                                        target.color = "magenta",
                                        plotting = FALSE ),
                      warning = function(w){
                        message("Warning (2): ", conditionMessage(w))
                        list(indicator.img = crp.tmp)
                      }
  )
  
  displayImage <- image_composite(image_read(img.path), image_read(img.id$indicator.img),
                  offset = paste0("+",x.coord,"+",y.coord)) |>   
  magick::image_draw()
  rect(
    top_left[1] - 1,
    top_left[2] + panelDims$height - 1,
    top_left[1] + panelDims$width - 1,
    top_left[2] - 1,
    lwd = ceiling(panelDims$width/100)
  )
  if(n.quad > 1){
    seg.y.inc <- panelDims$height/n.quad
    
    for(i in 1:(n.quad-1)){
      segments(x0 = top_left[1] - 1,
               y0 = top_left[2] + round(seg.y.inc * i),
               x1 = top_left[1] + panelDims$width - 1,
               y1 = top_left[2] + round(seg.y.inc * i),
               lwd = ceiling(panelDims$width/150))
    }
  }
  grDevices::dev.off()
  magick::image_write(displayImage,
              path = out.path)
  
  return(out.path)
  
}


#' Render simplified two-color panel
#' 
#' @description
#' Produces a two-color image of the panel with identified pixels
#' highlighted.
#' 
#' @details
#' Utilizing the same functionality of imageColorOutput, this function
#' crops an image to just the frame then identifies pixels within
#' a color range. These pixels are then mapped onto a blank frame
#' producing a simplified two-color image of identified pixels. Lines
#' indicating quadrants can be toggled on or off.
#' 
#' img.fraction and quad.fraction return the number of identified pixels.
#' To calculate the obscurity, subtract these values from 1 (% obscured = 
#' (1 - img.fraction)*100).
#' 
#' @param img.path File path to original image.
#' @param out.path File path to write panel image to.
#' @param rgb.mat A data frame of colors with columns r, g, b with values [0,1].
#' @param colorSelect A number to index rgb.mat.
#' @param colorSens A number ([0,1]) used for search radius in [countcolors::sphericalRange()].
#' @param panelDims A list of panel dimensions with $width and $height.
#' @param top_left A vector of the (x,y) coordinates of the top-left position of the frame.
#' @param n.quad The number of quadrants to display.
#' @param anno.panel Whether quadrant lines should be displayed on the panel [TRUE/FALSE].
#'
#' @returns list with the following elements:
#'  *src [out.path] file path with two-color panel.
#'  *img.fraction Fraction of pixels from color identification.
#'  *quad.fraction List of fraction of pixels in each quadrant from color identification.
#'  *pixel.idx Two column (row, col) matrix with identified pixel coordinates.
#' @export
calculatePanel <- function(img.path, out.path,
                           rgb.mat, colorSelect,
                           colorSens, panelDims,
                           top_left, n.quad, anno.panel){
  crop_geo <- paste0(
    panelDims$width,"x",panelDims$height,
    "+", top_left[1], "+", top_left[2]
  )
  
  img <- image_read(img.path) |> 
    image_crop(geometry = crop_geo)
  
  img.dims <- image_info(img)
  
  tmp <- tempfile(fileext = ".jpg")
  image_write(img, tmp, format = "jpeg")
  
  imgJPG <- jpeg::readJPEG(tmp)
  colorID <- tryCatch( countcolors::sphericalRange(pixel.array = imgJPG,
                                         center = c(
                                           rgb.mat$r[colorSelect],
                                           rgb.mat$g[colorSelect],
                                           rgb.mat$b[colorSelect]
                                         ),
                                         radius = colorSens,
                                         target.color = "magenta",
                                         plotting = FALSE),
                       warning = function(w){
                         message("Warning (1): ", conditionMessage(w))
                         list(pixel.idx = NULL)
                       },
                       error = function(e){
                         message("Error (1): ", conditionMessage(e))
                         list(pixel.idx = NULL)
                       }
  
  )
  
  blk_img <- image_blank(width = panelDims$width,
                         height = panelDims$height,
                         color = "navy")
  image_write(blk_img, tmp, format = "jpeg")
  
  if(!is.null(colorID$pixel.idx)){
  comp_img <- changePixelColor(jpeg::readJPEG(tmp), colorID$pixel.idx,
                               target.color = "cadetblue1",
                               plotting = FALSE,
                               return.img = TRUE)
  }else{
    comp_img <- tmp
  }
  
  if(n.quad > 1){
    quad.frac <- numeric(n.quad)
    seg.y.inc <- img.dims$height/n.quad
    quad.size <- round(seg.y.inc * img.dims$width)
    for(q in 1:n.quad){
      if(is.null(colorID$pixel.idx)){
        quad.frac[q] <- 0
      }else{
        n.idx <- colorID$pixel.idx |> 
          as.data.frame() |> 
          filter(row >= round(seg.y.inc * (q - 1)) &
                   row < round(seg.y.inc * q) ) |> 
          nrow()
        if(n.idx > quad.size){n.idx = quad.size}
        quad.frac[q] <- n.idx / quad.size
      }
    }
    comp_img <- image_draw(image_read(comp_img))
    if(anno.panel){
      for(i in 1:(n.quad-1)){
        segments(x0 = 0,
                 y0 = round(seg.y.inc * i),
                 x1 = img.dims$width,
                 y1 = round(seg.y.inc * i),
                 lwd = ceiling(img.dims$width/150))
      }
    }
    dev.off()
  }else{
    comp_img <- image_read(comp_img)
    quad.frac <- colorID$img.fraction
  }
  
  comp_img |> 
    image_resize(geometry = "500x500>") |> 
    image_write(path = out.path, format = "jpeg")
  # image_write(image_read(colorID$indicator.img), out.path)
  
  
  return(
    list(
      src = out.path,
      img.fraction = colorID$img.fraction,
      quad.fraction = quad.frac,
      pixel.idx = colorID$pixel.idx
    )
  )
}

#' Combine original image and two-color panel with annotations
#' 
#' @description
#' Creates a new image with the original image and two-color panel
#' adjacent to each other with plot name and obscurity annotated. 
#' 
#' @details
#' Creates a new image with the original image and two-color panel
#' adjacent to each other. Includes the plot name and percent obscurity.
#' If [rotate.angle = 0], adds frame to image. If [anno.panel = TRUE], 
#' adds lines demarcating quadrants and percent obscured for each quadrant on
#' top of panel.
#' 
#' @param img.path File path to original image.
#' @param panel.path File path to two-color panel from calculatePanel().
#' @param prc.obs Fraction of identified pixels in whole panel [0,1].
#' @param plt.name Character string of plot name.
#' @param panelDims A list of panel dimensions with $width and $height.
#' @param top_left A vector of the (x,y) coordinates of the top-left position of the frame.
#' @param n.quad The number of quadrants to display.
#' @param quad.frac List of quadrant fraction of identified pixels.
#' @param rotate.angle Angle image was rotated by. Used as boolean [rotate.angle == 0]
#' @param anno.panel Whether quadrant lines should be displayed on the panel [TRUE/FALSE].
#' @param save.path File path to write new image to.
#'
#' @returns [save.path] with created image written to.
#' @export
combineImagePanel <- function(img.path, panel.path,
                              prc.obs,
                              plt.name,
                              panelDims,
                              top_left,
                              n.quad,
                              quad.frac,
                              rotate.angle,
                              anno.pannel,
                              save.path){
  img <- image_read(img.path)
  pan <- image_read(panel.path) 
  
  if(n.quad > 1 & anno.pannel){
    pan.dims <- image_info(pan)
    pan <- image_draw(pan)
    
    seg.y.inc <- pan.dims$height/n.quad
    
    pan <- image_draw(pan)
    for(i in seq_len(n.quad-1)){
      # segments(x0 = 0,
      #          y0 = round(seg.y.inc * i),
      #          x1 = pan.dims$width,
      #          y1 = round(seg.y.inc * i),
      #          lwd = ceiling(pan.dims$width/75)/2)
      rect(ceiling(pan.dims$width/20)-2,
           round(seg.y.inc*(i-1) + 0.05*seg.y.inc)+13,
           ceiling(pan.dims$width/20)+25,
           round(seg.y.inc*(i-1) + 0.05*seg.y.inc),
           col = "black")
      
      text(x = ceiling(pan.dims$width/20),
           y = round(seg.y.inc*(i-1) + 0.05*seg.y.inc),
           labels = paste0(round((1-quad.frac[i])*100),"%"),
           adj = c(0,1),
           font = 2,
           col = "white")
    }
    
    rect(ceiling(pan.dims$width/20)-2,
         round(seg.y.inc*(n.quad-1) + 0.05*seg.y.inc)+13,
         ceiling(pan.dims$width/20)+25,
         round(seg.y.inc*(n.quad-1) + 0.05*seg.y.inc),
         col = "black")
    
    text(x = ceiling(pan.dims$width/20),
         y = round(seg.y.inc*(n.quad-1) + 0.05*seg.y.inc),
         labels = paste0(round((1-quad.frac[n.quad])*100),"%"),
         adj = c(0,1),
         font = 2,
         col = "white")
    dev.off()
  }
  
  if(rotate.angle == 0){
  img <- image_draw(img)
  rect(
    top_left[1] - 1,
    top_left[2] + panelDims$height - 1,
    top_left[1] + panelDims$width - 1,
    top_left[2] - 1,
    lwd = ceiling(panelDims$width/25)
  )
  dev.off()
  }
  
  img_comb <- c(img, pan)
  img_comb <- image_resize(img_comb, geometry = "x1000")
  
  img_final <- image_append(img_comb)
  
  anno_text.prc <- paste0("Obscurity: ", round((1-prc.obs)*100),"%")
  anno_text.plt <- paste0("Plot: ", plt.name)
  
  img_final <- image_draw(img_final)
  rect(0,35,130,0, col = "white", border = "white")
  dev.off()
  
  img_final <- image_annotate(img_final, 
                              anno_text.plt,
                              location = "+5+5")
  img_final <- image_annotate(img_final,
                              anno_text.prc,
                              location = "+5+18")
  image_write(img_final, path = save.path)
  
  return(save.path)
}
reduceImageSize <- function(img.path, out.path){
  image_read(img.path) |> 
    image_resize(geometry = "500x500") |> 
    image_write(path = out.path)
  
  return(out.path)
}

calculatePanelSize_V1 <- function(pixel.idx){
  
  # Threshold = Number of Pixels / sensitivity
  sensitivity <- 4
  
  height_max <- pixel.idx |> 
    as.data.frame() |> 
    group_by(row) |> 
    summarise(count = n()) |> 
    summarise(mx_count = max(count)) |> 
    pull(mx_count)
  # 72
  
  height_dims <- pixel.idx |> 
    as.data.frame() |> 
    group_by(row) |> 
    summarise(count = n()) |> 
    filter(count > height_max/sensitivity) |> 
    summarise(
      min = min(row),
      max = max(row),
      dif = max - min
    )
  # Rows 129-200, ht 71
  
  
  width_max <- pixel.idx |> 
    as.data.frame() |> 
    group_by(col) |> 
    summarise(count = n()) |> 
    summarise(mx_count = max(count)) |> 
    pull(mx_count)
  # 72
  
  width_dims <- pixel.idx |> 
    as.data.frame() |> 
    group_by(col) |> 
    summarise(count = n()) |> 
    filter(count > width_max/sensitivity) |> 
    summarise(
      min = min(col),
      max = max(col),
      dif = max - min
    )
  
  return(
    list(
      height_min = height_dims$min,
      height_max = height_dims$max, 
      height_dif = height_dims$dif,
      width_min  =  width_dims$min,
      width_max  =  width_dims$max,
      width_dif  =  width_dims$dif
    )
  )
}

calculatePanelSize <- function(img.path, pixel.idx,
                               windowSize, panelSens1, panelSens2){
  
  img <- image_read(img.path) |> 
    image_info()
  calcPanel <- findDensity(pixel.idx, c(img$width, img$height), windowSize, panelSens1)
  coordPanel <- panelCoords(calcPanel, panelSens2)
  
  return(
    list(
      height_min = coordPanel[4],
      height_max = coordPanel[2], 
      height_dif = coordPanel[2] - coordPanel[4],
      width_min  = coordPanel[1],
      width_max  = coordPanel[3],
      width_dif  = coordPanel[3] - coordPanel[1]
    )
  )
}

isolatePanel <- function(img.path, out.path,
                         panelDims, colorSelect,
                         rgb.mat, colorSens){
  img <- jpeg::readJPEG(img.path)
  colorID <- countcolors::sphericalRange(pixel.array = img,
                                         center = c(
                                           rgb.mat$r[colorSelect],
                                           rgb.mat$g[colorSelect],
                                           rgb.mat$b[colorSelect]
                                         ),
                                         radius = colorSens,
                                         target.color = "magenta",
                                         plotting = FALSE)
  
  mn_row <- mean(colorID$pixel.idx[,1])
  mn_col <- mean(colorID$pixel.idx[,2])

  # panelEst <- calculatePanelSize(img.path, colorID$pixel.idx,
  #                    1, 2, 1)
  # mn_col <- mean(panelEst$width_min, panelEst$width_max)
  # mn_row <- mean(panelEst$height_min, panelEst$height_max)
  
  color_img <- image_read(img.path)
  gray_img <- image_modulate(color_img, saturation = 0)
  
  geo_txt <- paste0(
    panelDims$width * 2, "x", panelDims$height * 2,
    "+", mn_col - panelDims$width, "+", mn_row - panelDims$height
  )
  crop_img <- image_crop(color_img, 
                         geometry = geo_txt)
  
  
  geo_txt2 <- paste0(
    "+", mn_col - panelDims$width, "+", mn_row - panelDims$height
  )
  comp_img <- image_composite(gray_img, crop_img, offset = geo_txt2)
  comp_img <- image_flatten(comp_img)
  
  image_write(comp_img, out.path, format = "jpeg", flatten = TRUE)
  
  return(out.path)
}



calculatePanel <- function(img.path, out.path,
                           panelCoords, colorSelect,
                           rgb.mat, colorSens){
  crop_geo <- paste0(
    panelCoords$xright - panelCoords$xleft,
    "x", panelCoords$ybottom - panelCoords$ytop,
    "+", panelCoords$xleft, "+", panelCoords$ytop
  )
  
  img <- image_read(img.path) |> 
    image_crop(geometry = crop_geo)
  
  tmp <- tempfile(fileext = ".jpg")
  image_write(img, tmp, format = "jpeg")
  
  imgJPG <- jpeg::readJPEG(tmp)
  colorID <- countcolors::sphericalRange(pixel.array = imgJPG,
                                         center = c(
                                           rgb.mat$r[colorSelect],
                                           rgb.mat$g[colorSelect],
                                           rgb.mat$b[colorSelect]
                                         ),
                                         radius = colorSens,
                                         target.color = "magenta",
                                         plotting = FALSE)
  
  
  
  blk_img <- image_blank(width = panelCoords$xright - panelCoords$xleft,
              height = panelCoords$ybottom - panelCoords$ytop,
              color = "navy")
  image_write(blk_img, tmp, format = "jpeg")
  
  comp_img <- changePixelColor(jpeg::readJPEG(tmp), colorID$pixel.idx,
                               target.color = "green",
                               plotting = FALSE,
                               return.img = TRUE)
  
  image_read(comp_img) |> 
    image_resize(geometry = "200x200") |> 
    image_write(path = out.path, format = "jpeg")
  # image_write(image_read(colorID$indicator.img), out.path)
  
  
  return(
    list(
      src = out.path,
      img.fraction = colorID$img.fraction
    )
  )
}

imageCroppedOutput <- function(img.path, out.path,
                               brush.values){
  img <- image_read(img.path)
  
  xmin <- floor(brush.values$xmin)
  ymin <- floor(brush.values$ymin)
  width <- ceiling(brush.values$xmax) - floor(brush.values$xmin)
  height <- ceiling(brush.values$ymax) - floor(brush.values$ymin)
  
  geoText <- paste0(
    width, "x", height,
    "+",xmin,"+",ymin
  )
  
  image_crop(img, geometry = geoText) |> 
    image_write(path = out.path)
  
  return(out.path)
}

imageColorOutput <- function(img.path, out.path,
                             rgb.mat, colorSelect,
                             colorSens,
                             windowSize, panelSens1, panelSens2){
  img_id <- countcolors::sphericalRange(jpeg::readJPEG(img.path),
                                        center = c(
                                          rgb.mat$r[colorSelect],
                                          rgb.mat$g[colorSelect],
                                          rgb.mat$b[colorSelect]
                                        ),
                                        radius = colorSens,
                                        target.color = "magenta",
                                        plotting = FALSE )
  
  panelDims <- calculatePanelSize(img.path, img_id$pixel.idx,
                                  windowSize, panelSens1, panelSens2)
  
  displayImage <- image_read(img_id$indicator.img) |> 
    image_draw()
  rect(
    panelDims$width_min,
    panelDims$height_max,
    panelDims$width_max,
    panelDims$height_min,
    lwd = ceiling(panelDims$height_dif/100)
  )
  dev.off()
  image_write(displayImage,
              path = out.path)
  
  return(
    list(
      path = out.path,
      width_dif = panelDims$width_dif,
      height_dif = panelDims$height_dif
    )
  )
}
library(magick)
library(countcolors)
library(colordistance)
library(tidyverse)

img <- jpeg::readJPEG("SmallStickyNote.jpg")
# plotPixels("StickyNote.jpg")
# img <- jpeg::readJPEG("whipz.jpg")
plotPixels("SmallStickyNote.jpg")
plotPixels("Test Photos/PanelCalibrate.jpg",
           main = "Pixel Colors")




mag_img <- image_resize(magick::image_read(img), "500x500")

gray_img <- image_modulate(mag_img, saturation = 0)
crop_img <- image_crop(mag_img, geometry = "100x100+230+120")
comp_img <- image_composite(gray_img, crop_img, offset = "+230+120")
comp_img <- image_flatten(comp_img)

tmp_image <- tempfile( fileext = ".jpg")


image_write(comp_img, tmp_image, format = "jpeg", flatten = TRUE, 
            quality = 100, depth = 16)


image_read(tmp_image)

gray_img <- jpeg::readJPEG(tmp_image)
plotPixels(tmp_image)
image_read(gray_img)
img.rec.crop <- countcolors::rectangularRange(gray_img, 
                                              upper = c(0.8,0.9,0.99),
                                              lower = c(0.6,0.6,0.9),
                                              target.color = "magenta")

mag_ind <- magick::image_read(img.rec.crop$indicator.img)
head(img.rec.crop$indicator.img)
img.rec.crop$pixel.idx
image_composite(mag_img, mag_ind, operator = "copyMagenta")

height_median <- img.rec.crop$pixel.idx |> 
  as.data.frame() |> 
  group_by(row) |> 
  summarise(count = n()) |> 
  summarise(md_count = median(count)) |> 
  pull(md_count)
# 72

height_dims <- img.rec.crop$pixel.idx |> 
  as.data.frame() |> 
  group_by(row) |> 
  summarise(count = n()) |> 
  filter(count > height_median/4) |> 
  summarise(
    min = min(row),
    max = max(row),
    dif = max - min
  )
# Rows 129-200, ht 71
  

width_median <- img.rec.crop$pixel.idx |> 
  as.data.frame() |> 
  group_by(col) |> 
  summarise(count = n()) |> 
  summarise(md_count = median(count)) |> 
  pull(md_count)
# 72

width_dims <- img.rec.crop$pixel.idx |> 
  as.data.frame() |> 
  group_by(col) |> 
  summarise(count = n()) |> 
  filter(count > width_median/4) |> 
  summarise(
    min = min(col),
    max = max(col),
    dif = max - min
  )
# Cols 239 - 309, wd 70

mag_crop <- image_crop(mag_ind, geometry = "72x73+238+128")

image_quantize(mag_crop, max = 2)

unlink(tmp_image)


test_tmp <- tempfile(fileext = ".jpg")

isolatePanel("SmallStickyNote.jpg", test_tmp,
             list(width = 70, height = 70), )


library(countcolors)
library(colordistance)
library(tidyverse)
imgpath <- "SmallStickyNote.jpg"
colorHistDF <- colordistance::getImageHist(imgpath,
                              bins = 3,
                              bin.avg = TRUE,
                              plotting = FALSE) |>
    arrange(desc(Pct)) |>
    mutate(fillColor = rgb(r,g,b),
           row = row_number() ) 

colorSelect <- 4

colorHistDF |> 
  filter(Pct > 1e-4) |> 
  slice_max(row, n = 1)

colorHistDF |>
  filter(Pct > 1e-4) |> 
  mutate(outline = ifelse(row == colorSelect, T, F) ) |> 
  ggplot(aes(row, Pct)) +
  geom_col(aes(fill = fillColor, 
               color = outline,
               ), linewidth = 1.2) +
  scale_fill_identity() +
  theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    # axis.text.x = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  # coord_flip() +
  # scale_x_reverse() +
  scale_x_continuous(
    # breaks = scales::breaks_width(1)
    breaks = seq(1,,1)
    # breaks = c(1:10),
    # labels = c(1:10),
    # limits = c(0.5,10),
    # transform = "reverse"
  ) +
  scale_color_discrete(
    palette = c("transparent", "firebrick")
  )


imgcolor = imageColorOutput(imgpath,
                            tmp_fp_imageCalibrateColor,
                            colorHistDF,
                            9,
                            0.1,
                            list(width = 75, height = 75),
                            c(234,125))

imgcolor = sphericalRange(jpeg::readJPEG(imgpath),
                          center = c(colorHistDF$r[9],
                                     colorHistDF$g[9],
                                     colorHistDF$b[9]),
                          radius = 0.11,
                          plotting = FALSE)

imgcolor$pixel.idx |> 
  as.data.frame() |> 
  filter(row >= round(50 * (3 - 1)) &
           row < round(50 * 3) ) |> 
  nrow()
  

imgcolor$pixel.idx |> 
  as.data.frame() |> 
  count(row) |> 
  ggplot(aes(1,row, fill = n)) +
  geom_tile() +
  scale_fill_gradient(
    low = "green",
    high = "red",
    
  )+
  scale_y_continuous(
    transform = "reverse"
  ) +
  coord_fixed(0.01) +
  theme_void(
  ) +
  theme(
    legend.position = "none"
    
  )


nq <- 4

df <- data.frame(
  a = numeric(),
  b = character()
)

for(i in seq_len(nq)){
  df <- df |> 
    mutate("col_{i}" := numeric(),
           .before = a)
}

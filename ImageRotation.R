
img <- image_read("RotateStickyNote2.jpg")
img <- image_crop(img, geometry = "800x800+1300+2200")
img
tmp_img <- tempfile(fileext = ".jpg")
image_write(img, path = tmp_img)

imgColor <- jpeg::readJPEG(tmp_img)
colorHist <- getImageHist(tmp_img,
             bins = 3,
             bin.avg = TRUE,
             plotting = FALSE) |> 
  arrange(desc(Pct)) |> 
  filter(r > 0.05 & g > 0.05 & b > 0.05) |> 
  mutate(fillColor = rgb(r,g,b),
         row = row_number())

colorHist |> 
  ggplot(aes(row, Pct)) +
  geom_col(aes(fill = fillColor)) +
  scale_fill_identity() +
  theme_minimal() +
  coord_flip() +
  scale_x_reverse()
  


img_id <- countcolors::sphericalRange(imgColor, 
                                        center = c(
                                          colorHist$r[1],
                                          colorHist$g[1],
                                          colorHist$b[1]
                                        ),
                                        radius = 0.1,
                                        target.color = "magenta",
                                        plotting = TRUE)





img_id$pixel.idx |> 
  as.data.frame() |> 
  group_by(row) |> 
  count(row) |> 
  ungroup() |> 
  summarise(
    md = median(n)
  )

y1 <- img_id$pixel.idx |> 
  as.data.frame() |> 
  group_by(row) |> 
  count(row) |> 
  ungroup() |> 
  filter( n > 0.99 * median(n)) |> 
  arrange(row) |> 
  slice_head(n = 1) |> 
  pull(row)
# min row = 165
# y2 <- img_id$pixel.idx |> 
#   as.data.frame() |> 
#   group_by(row) |> 
#   count(row) |> 
#   ungroup() |> 
#   filter( n > 0.99 * median(n)) |> 
#   arrange(row) |> 
#   slice_tail(n = 1) |> 
#   pull(row)
# max row = 486
# 
x1 <- img_id$pixel.idx |> 
  as.data.frame() |> 
  group_by(col) |> 
  count(col) |> 
  ungroup() |> 
  filter( n > 0.99 * median(n)) |> 
  arrange(col) |> 
  slice_head(n = 1) |> 
  pull(col)
# min col = 162
x2 <- img_id$pixel.idx |>
  as.data.frame() |>
  group_by(col) |>
  count(col) |>
  ungroup() |>
  filter( n > 0.99 * median(n)) |>
  arrange(col) |>
  slice_tail(n = 1) |>
  pull(col)
# max col = 481
# 

x_high <- img_id$pixel.idx |>
  as.data.frame() |>
  filter(col > x2) |>
  group_by(col) |>
  count(col)

lm_x_high <- lm( n ~ col, x_high)

y_low <- img_id$pixel.idx |>
  as.data.frame() |>
  filter(row < y1) |>
  group_by(row) |>
  count(row)

lm_y_low <- lm( n ~ row, y_low )



rotateAngle <- tan( (y1 - (-coef(lm_y_low)[1]/coef(lm_y_low)[2] )) /
       ((-coef(lm_x_high)[1]/coef(lm_x_high)[2])-x1)) * 180/pi

rotateImage <- image_read(tmp_img)

rotateImage <- image_rotate(rotateImage, -rotateAngle)

rotateImage <- image_fill(image_border(rotateImage, color = "white"), "none")


image_write(rotateImage, tmp_img)

# img_id$pixel.idx |> 
#   ggplot(aes(row)) +
#   geom_histogram(binwidth = 1) +
#   geom_hline(yintercept = 456) +
#   geom_abline(intercept = coef(lm_upper)[1], slope = coef(lm_upper)[2] ) +
#   geom_abline(intercept = coef(lm_lower)[1], slope = coef(lm_lower)[2])

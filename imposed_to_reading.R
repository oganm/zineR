library(magick)
library(pdftools)
library(dplyr)
library(png)
library(glue)

file = 'jane.pdf'
density = 100

images = magick::image_read_pdf(file,density = density)



# first half
seq_len(length(images)) %>% lapply(function(i){
  img = images[i]
  inf = image_info(img)
  magick::image_crop(img,
                     magick::geometry_area(height = inf$height,
                                           width = inf$width/2,
                                           x_off = inf$width/2*(i%%2 == 1)))# %>%
    # magick::image_write(file.path('pages_reading',glue::glue('{i}.png')))
}) %>% do.call(c,.) -> first_half

# second half
seq_len(length(images)) %>% lapply(function(i){
  img = images[length(images)+1-i]
  inf = image_info(img)
  magick::image_crop(img,
                     magick::geometry_area(height = inf$height,
                                           width = inf$width/2,
                                           x_off = inf$width/2*(i%%2 == 1)))#  %>%
   #  magick::image_write(file.path('pages_reading',glue::glue('{i+length(images)}.png')))
}) %>% do.call(c,.) -> second_half

all = c(first_half,second_half)

image_write(all,glue::glue(fs::path_ext_remove(file),"_reading.pdf"),format = 'pdf',density = density)


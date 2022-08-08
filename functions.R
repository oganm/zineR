library(magick)
library(dplyr)
library(png)
library(glue)
library(pdftools)


# writing pdfs with magick required this fix
# https://stackoverflow.com/questions/52861946/imagemagick-not-authorized-to-convert-pdf-to-an-image

process_input = function(input,density_in){
    if('character' %in% class(input)){
        images = magick::image_read_pdf(input,density = density_in)
    } else if ('magick-image' %in% class(input)){
        images = input
    }

    info = magick::image_info(images)
    all_same_width = all(info$width == info$width[1])
    all_same_height = all(info$height == info$height[1])
    list(images = images,
         info = info,
         all_same_width = all_same_width,
         all_same_height = all_same_height)
}

imposed_to_reading = function(input, file_out = NULL, density_in = 100, density_out= density_in){

    info = process_input(input,density_in)
    images = info$images
    # checking for same height and width isn't important in this case.

    # first half
    seq_len(length(images)) %>% lapply(function(i){
        img = images[i]
        inf = info$info[i,]
        magick::image_crop(img,
                           magick::geometry_area(height = inf$height,
                                                 width = inf$width/2,
                                                 x_off = inf$width/2*(i%%2 == 1)))
    }) %>% do.call(c,.) -> first_half

    # second half
    seq_len(length(images)) %>% lapply(function(i){
        img = images[length(images)+1-i]
        inf = info$info[length(images)+1-i,]
        magick::image_crop(img,
                           magick::geometry_area(height = inf$height,
                                                 width = inf$width/2,
                                                 x_off = inf$width/2*(i%%2 == 1)))
    }) %>% do.call(c,.) -> second_half

    all = c(first_half,second_half)

    if(!is.null(file_out)){
        magick::image_write(all,file_out,format = 'pdf',density = density_out)
    }
    return(invisible(all))

}

reading_to_imposed = function(input, file_out, density_in = 100, density_out= density_in){
    # best works with multiples of 4 to avoid a blank in the middle. adds a blank page
    # if not a multiple of 2
    info = process_input(input,density_in)
    images = info$images
    assertthat::assert_that(info$all_same_width)
    assertthat::assert_that(info$all_same_height)

    # if it has an odd page number, add another to the end
    if (length(images)%%2 != 0){
        blank = magick::image_blank(info$info$width[1],info$info$height[1])
        images = c(images,blank)
    }

    target_size = length(images)/2

    seq_len(target_size) %>% lapply(function(i){
        image_n_minus_i = images[length(images)+1-i]
        image_i = images[i]
        if (i%%2 == 0){
            magick::image_append(c(image_i, image_n_minus_i))
        } else{
            magick::image_append(c(image_n_minus_i,image_i))
        }
    }) %>% do.call(c,.) -> all

    if(!is.null(file_out)){
        magick::image_write(all,file_out,format = 'pdf',density = density_out)
    }
    return(invisible(all))
}

reading_to_one_page = function(input, file_out = NULL, density_in = 100, density_out= density_in){
    # best works with multiples of 8. adds blanks if not multiples of 8
    info = process_input(input,density_in)
    images = info$images
    assertthat::assert_that(info$all_same_width)
    assertthat::assert_that(info$all_same_height)

    if (length(images)%%8 != 0){
        remainder = length(images)%%8
        blank = magick::image_blank(info$info$width[1],info$info$height[1])
        images = c(images,rep(blank,remainder))
    }

    target_size = length(images)/8
    seq_len(target_size) %>% lapply(function(i){
        image_set = images[(8*i-7):(8*i)]

        top = c(magick::image_rotate(image_set[5],180),
                magick::image_rotate(image_set[4],180),
                magick::image_rotate(image_set[3],180),
                magick::image_rotate(image_set[2],180)) %>%
            magick::image_append()

        bottom = c(image_set[6],
                   image_set[7],
                   image_set[8],
                   image_set[1]) %>%
            magick::image_append()

        magick::image_append(c(top,bottom),stack = TRUE)

    }) %>% do.call(c,.) -> all

    if(!is.null(file_out)){
        magick::image_write(all,file_out,format = 'pdf',density = density_out)
    }
    return(invisible(all))

}

one_page_to_reading = function(input, file_out = NULL, density_in = 100, density_out = density_in){
    info = process_input(input,density_in)
    images = info$images

    seq_len(length(images)) %>% lapply(function(i){
        img = images[i]
        inf = info$info[i,]
        page_5 = magick::image_crop(img,geometry =
                               magick::geometry_area(width = inf$width/4,
                                                     height = inf$height/2,
                                                     x_off = 0,y_off = 0)) %>%
            magick::image_rotate(180)
        page_4 = magick::image_crop(img,geometry =
                                        magick::geometry_area(width = inf$width/4,
                                                              height = inf$height/2,
                                                              x_off = inf$width/4,y_off = 0)) %>%
            magick::image_rotate(180)
        page_3 = magick::image_crop(img,geometry =
                                        magick::geometry_area(width = inf$width/4,
                                                              height = inf$height/2,
                                                              x_off = inf$width/4*2,y_off = 0)) %>%
            magick::image_rotate(180)

        page_2 = magick::image_crop(img,geometry =
                                        magick::geometry_area(width = inf$width/4,
                                                              height = inf$height/2,
                                                              x_off = inf$width/4*3,y_off = 0)) %>%
            magick::image_rotate(180)

        page_6 = magick::image_crop(img,geometry =
                                        magick::geometry_area(width = inf$width/4,
                                                              height = inf$height/2,
                                                              x_off = 0,y_off = inf$height/2))
        page_7 = magick::image_crop(img,geometry =
                                        magick::geometry_area(width = inf$width/4,
                                                              height = inf$height/2,
                                                              x_off = inf$width/4,y_off = inf$height/2))
        page_8 =  magick::image_crop(img,geometry =
                                         magick::geometry_area(width = inf$width/4,
                                                               height = inf$height/2,
                                                               x_off = inf$width/4*2,y_off = inf$height/2))

        page_1 =  magick::image_crop(img,geometry =
                                         magick::geometry_area(width = inf$width/4,
                                                               height = inf$height/2,
                                                               x_off = inf$width/4*3,y_off = inf$height/2))

        c(page_1,page_2,page_3,page_4,page_5,page_6,page_7,page_8)

        }) %>% do.call(c,.) -> all


    if(!is.null(file_out)){
        magick::image_write(all,file_out,format = 'pdf',density = density_out)
    }
    return(invisible(all))
}

nothing = function(input, file_out = NULL, density_in = 100, density_out= density_in){
    info = process_input(input,density_in)
    images = info$images
    if(!is.null(file_out)){
        magick::image_write(images,file_out,format = 'pdf',density = density_out)
    }
    return(invisible(images))
}


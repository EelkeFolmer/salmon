library(sf)
library(stars)
library(gdalUtils)
library(ggplot2)
library(tidyverse)
library(RStoolbox)
library(raster)
library(gridExtra)
library(magick)

library(torch)
library(torchvision)
library(pins)


# function for preparation of the dataset
#func_imgprep("salmon_20201021_PSCSal_11_Reach2-1_30m")
#layer = 'salmon_20201007_PSCSal_7_Reach2-1_30m'

func_imgprep <- function(layer=layer, size=10) {
  
  pathG  <- "/media/eelke/Samsung_T5/salmon/GIS/"
  pathD  <- "/media/eelke/Samsung_T5/salmon/data/consolidated/"
  ortho  <- brick(paste0(pathG, layer, '.tif') ) 
  
  labels <- st_read("~/work/projects/salmon/data/salmon_labels.gpkg", layer=layer) %>%
    st_transform(crs(ortho))
  
  river <- st_read(paste(pathG, "salmon.gpkg", sep='') , layer="riverbed") %>%
    st_transform(crs(ortho))
  
  grid <- st_make_grid(ortho, cellsize=size) %>%
    st_transform(crs(ortho)) %>%
    st_as_sf(agr="constant") %>%
    mutate(intersect_river = apply(st_intersects(., river, sparse = FALSE), 1, any)) %>%
    filter(intersect_river) 
  
  for (i in 1:dim(grid)[1]) {
    or  <- crop(ortho, grid[i, ])
    
    if (dim(labels)[1]>0) {
      has_salmon <- any(st_intersects(labels, grid[i, ], sparse = FALSE))
    } else {
      has_salmon <- FALSE
    }
    
    m        <- matrix(or[[1]])                  # to filter out images without variation
    is_black <- ifelse(sd(m) < 1, TRUE, FALSE)  
    p_valid  <- 1 - length(which(m == 255 ) ) / length(m) 
    dim_ok   <- (dim(or)[2]/dim(or)[1]>0.95 & dim(or)[2] > 224 & dim(or)[3] == 4) # check if the x and y dimensions are roughly the same and big enough
    img_ok   <- (dim_ok & !is_black & p_valid > 0.7)
      
    if (has_salmon & dim_ok & img_ok) {
      outname <- ifelse(has_salmon, paste0("pos/", layer, "_", i, "_T.jpg"), paste0("neg/", layer, "_", i, "_F.jpg") )
      sgdf    <- as(or, "SpatialGridDataFrame")
      rgdal::writeGDAL(sgdf, paste0(pathD, outname), drivername = "JPEG", type = "Byte", mvFlag = 255)
    }
    cat(i, '/', dim(grid)[1], 'has_salmon = ', has_salmon, '\n')
  }
  tmp_dir <- tempdir()
  files <- list.files(tmp_dir, full.names = T)
  file.remove(files)
}  

# functions for ML

copy_train_valid_test <- function(ptrain = 0.8, pvalid = 0.1, ptest = 0.1, out_resolution = "224x224!") {
  
  path <- "/media/eelke/Samsung_T5/salmon/data"

  for (group in c('pos', 'neg')) {
    f0      <- list.files(paste0(path, '/consolidated/', group), full.names = FALSE)
    fg      <- f0[!grepl('xml', f0)]
    f_train <- sample(fg, size=ptrain*length(fg))
    f_valid <- sample(fg[!fg %in% f_train], size=pvalid*length(fg) )
    f_test  <- fg[!fg %in% c(f_train, f_valid)]
    
    for (f in fg)  {
      img  <- image_read(paste0(path, '/consolidated/', group, '/', f) )
      imgt <- image_scale(img, geometry=out_resolution)
      if (f %in% f_train) image_write(imgt, path= paste0(path, '/train/', group, '/', f) ) 
      if (f %in% f_test)  image_write(imgt, path= paste0(path, '/test/', group, '/', f)  ) 
      if (f %in% f_valid) image_write(imgt, path= paste0(path, '/valid/', group, '/', f) )
      }
  }  

}

train_transforms <- function(img) {
  img %>%
    # first convert image to tensor
    transform_to_tensor() %>%
    # then move to the GPU (if available)
    (function(x) x$to(device = device)) %>%
    # data augmentation
    transform_random_resized_crop(size = c(224, 224)) %>%
    # data augmentation
    transform_color_jitter() %>%
    # data augmentation
    transform_random_horizontal_flip() %>%
    # normalize according to what is expected by resnet
    transform_normalize(mean = c(0.485, 0.456, 0.406), std = c(0.229, 0.224, 0.225))
}

valid_transforms <- function(img) {
  img %>%
    transform_to_tensor() %>%
    (function(x) x$to(device = device)) %>%
    #transform_resize(256) %>%
    transform_resize(size = c(224, 224)) %>%
    transform_center_crop(224) %>%
    transform_normalize(mean = c(0.485, 0.456, 0.406), std = c(0.229, 0.224, 0.225))
}

null_transforms <- function(img) {
  img %>%
    # first convert image to tensor
    transform_to_tensor() %>%
    # then move to the GPU (if available)
    #(function(x) x$to(device = device)) %>%
    # data augmentation
    transform_random_resized_crop(size = c(224, 224)) %>%
    # data augmentation
    #transform_color_jitter() %>%
    # data augmentation
    #transform_random_horizontal_flip() %>%
    # normalize according to what is expected by resnet
    transform_normalize(mean = c(0.485, 0.456, 0.406), std = c(0.229, 0.224, 0.225))
}

view_batch <- function(b) {
  images <- b[[1]]$to(device = "cpu") %>%
    as_array() %>%
    aperm(perm = c(1, 3, 4, 2))
  
  mean <- c(0.485, 0.456, 0.406)
  std <- c(0.229, 0.224, 0.225)
  images <- std * images + mean
  images <- images * 255
  images[images > 255] <- 255
  images[images < 0] <- 0
  
  par(mfcol = c(3,4), mar = rep(1, 4))
  
  images %>%
    purrr::array_tree(1) %>%
    purrr::set_names(class_names[as_array(classes)]) %>%
    purrr::map(as.raster, max = 255) %>%
    purrr::iwalk(~{plot(.x); title(.y)})
}

# fnames <- list.files("/media/eelke/Samsung_T5/salmon/data/train/neg", full.names = TRUE)
# 
# l <- fnames %>%
#   map(image_read) %>%
#   map_df(image_info)
# 
# fnames %>%
#   map(image_read) %>%
#   train_transforms(.)
# 
# sizel <- list()
# for (f in fnames) {
#   img <- image_read(f)
#   img_t <- train_transforms(img)
#   sizel[[f]] <- dim(img_t)
# }
# 
# df <- do.call(rbind, sizel)


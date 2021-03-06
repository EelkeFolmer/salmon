source('scripts/func_salmon.R')
source('scripts/EThemes.R')

# *********************************************************************************
# *********** 1. combine the point label gpkgs from Claudia and Kayley ************
# *********************************************************************************

# remove empty layers from the geopackage that Claudia made *************
lnames <- st_layers("~/work/projects/salmon/data/Labelled files.gpkg")[1] 
for (i in lnames$name) {
  labels <- st_read("~/work/projects/salmon/data/Labelled files.gpkg", layer=i)
  if (dim(labels)[1]>0) {
    st_write(labels, "~/work/projects/salmon/data/labels_Claudia.gpkg", layer=i, delete_layer = TRUE)
  }
}

# remove and renames the empty layers from the geopackage that Kayley made
lnames <- st_layers("~/work/projects/salmon/data/Kayley_PSCSal_project_review.gpkg")[1]
for (i in lnames$name) {
  labels <- st_read("~/work/projects/salmon/data/Kayley_PSCSal_project_review.gpkg", layer=i)
  if (dim(labels)[1]>0) {
    iname <- str_split(i, 's ', simplify = TRUE)[,2]
    st_write(labels, "~/work/projects/salmon/data/labels_Kayley.gpkg", layer=iname, delete_layer = TRUE)
  }
}

# combine Claudia and Kayley
layers1 <- st_layers("~/work/projects/salmon/data/labels_Claudia.gpkg")[c(1,4)] %>%
  data.frame() %>%
  set_names(c('name', 'features_C'))

layers2 <- st_layers("~/work/projects/salmon/data/labels_Kayley.gpkg")[c(1,4)] %>%
  data.frame() %>%
  set_names(c('name', 'features_K'))

lys <- left_join(layers1, layers2)

# read the layer from the correct gpkg and write to final label set
for (i in lys$name) {
    if (is.na(lys[lys$name == i,'features_K']) | lys[lys$name == i, 'features_C'] > lys[lys$name == i,'features_K']) {
      l <- st_read("~/work/projects/salmon/data/labels_Claudia.gpkg", layer=i)  
    } else {
      l <- st_read("~/work/projects/salmon/data/labels_Kayley.gpkg", layer=i) 
    }
  st_write(l, "~/work/projects/salmon/data/labels.gpkg", layer=i, delete_layer = TRUE)
}


# *********************************************************************************
# *********** 2. make the bounding boxes on the basis of the points ***************
# *********************************************************************************

st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y))
  do.call("c", lapply(x, f))
}

fnames_tif   <- list.files("/media/eelke/Samsung_T5/salmon/GIS", pattern="tif", full.names=FALSE) 
fnames_ortho <- str_split(fnames_tif, pattern='.tif', simplify=TRUE)[,1]
label_layers <- st_layers("~/work/projects/salmon/data/labels.gpkg") [c(1,4)] %>%
     data.frame()

for (i in label_layers$name) {
  if (i %in% fnames_ortho) {
  ortho <- brick(paste0("/media/eelke/Samsung_T5/salmon/GIS/", i, ".tif")  )
  bbox <- st_read("~/work/projects/salmon/data/labels.gpkg", layer=i) %>%
    st_transform(crs(ortho)) %>%
    st_buffer(., dist = 0.4) %>%
    st_bbox_by_feature(.) %>%
    st_as_sf(., crs=st_crs(ortho))
  } 
  st_write(bbox, "~/work/projects/salmon/data/labels_bbox.gpkg", layer=i, delete_layer = TRUE)
}


# *********************************************************************************
# *********** 3. make the bounding boxes on the basis of the lines  ***************
# *********************************************************************************

label_layers2 <- st_layers("~/work/projects/salmon/data/Labels_Lines_Completed.gpkg") [c(1,4)] %>%
  data.frame() %>% 
  arrange(features)

for (i in label_layers2$name) {
  st_read("~/work/projects/salmon/data/Labels_Lines_Completed.gpkg", layer=i) %>%
    st_transform(crs=st_crs(32610)) %>% 
    st_buffer(., dist = 0.05) %>% # if the line is vertical or horizontal, the bbox is too narrow, hence the buffer of 5 cm
    st_bbox_by_feature(.) %>%
    st_as_sf(., crs=st_crs(32610)) %>% 
    st_write("~/work/projects/salmon/data/labels_lines_bbox.gpkg", layer=i, delete_layer = TRUE)
}

# *********************************************************************************
# *********************** 3. clip geotifs and export jpgs *************************
# *********************************************************************************
# very difficult layers
lyr_poor <- c('salmon_20201021_PSCSal_12_Reach2-1_30m', 
              'salmon_20201019_PSCSal_1_Reach1-0_30m', 
              'salmon_20201002_30m', 
              'salmon_20201019_PSCSal_5_Reach2-1_30m',
              'salmon_20201026_PSCSal_6_Reach1-0_30m')

# reasonable layers
lyr_ok <- c('salmon_20201021_PSCSal_11_Reach2-1_30m', 
            'salmon_20201022_PSCSal_3_Reach2-1_30m', 
            'salmon_20201002_PSCSal_2_Reach2-1_30m', 
            'salmon_20201006_PSCSal_4_Reach2-1_30m', 
            'salmon_20201007_PSCSal_7_Reach2-1_30m', 
            'salmon_20201002_PSCSal_3_Reach2-1_30m',
            'salmon_20201019_PSCSal_1_Reach1-0_30m',
            'salmon_20201021_PSCSal_8_Reach2-1_30m',
            'salmon_20201021_PSCSal_3_Reach1-0_30m',
            'salmon_20201021_PSCSal_9_Reach2-1_30m',
            'salmon_20201019_PSCSal_11_Reach2-1_30m')

# subset layers for which to make a jpgs
layers_subset <- st_layers("~/work/projects/salmon/data/labels_lines_bbox.gpkg")[c(1,4)] %>% # labels.gpkg
  data.frame() %>%
  filter(features > 50 & name %in% lyr_ok) %>% # 
  arrange(desc(features))

# lyr_ok %in% layers_subset$name

map(layers_subset$name[4:10], func_imgprep)
#numb <- which(layers_subset == 'salmon_20201007_PSCSal_7_Reach2-1_30m')

# *********************************************************************************
# ************************** 4. make json with labels *****************************
# *********************************************************************************

# make a list "train" to be exported at the end; "train" consists of 4 elements
#  1. images (df with filename, height, width, id)
#  2. type "instances"
#  3. annotations (df with segments, bbox, etc.)
#  4. categories  (df with id, name, supercategory)

# 1. get filenames, id and extract width and height with imagemagick
all_jpgs  <- data.frame(filename = list.files( "/media/eelke/Samsung_T5/salmon/data/consolidated/pos", recursive = TRUE, full.names = TRUE)) %>% #"/home/eelke/work/projects/salmon/R/data/train"
  filter(!grepl('xml', filename) )

get_image_info <- function(x) {
  # extracts the metadata of an image based on filename
  cbind(x, image_info(image_read(x)) ) %>%
  dplyr::select('x', 'width', 'height') %>%
  set_names('filename', 'width', 'height') %>%
  mutate(file_name = substr(filename, start=54, stop=1000L),
         id0 = str_split(filename, '.jpg', simplify = TRUE)[1])
  }

images    <- map_df(all_jpgs[1:dim(all_jpgs)[1], ], get_image_info)
images$id <- as.integer(rownames(images))

# 3. annotations: get image_id, bbox, category_id, id (image_id bbox category_id  id ignore)
jpgs_pos <- images %>%
  filter(!grepl('_F', filename)) %>%
  mutate(image_id = id, 
         image_id2 =  substr(filename, start=54, stop=nchar(filename)-6),
         layername = paste0(str_split(image_id2, "m_", simplify = TRUE )[,1], 'm') ) 

func_get_bboxes <- function(r, bboxes) {
  # bboxes is a layer in a geopackage with bboxes
  # r is a raster with the bboxes
  
  dflist <- list()
  for (i in 1:dim(bboxes)[1]) {
    cds <- st_coordinates(bboxes[[1]][i])
    
    e <- extent(r)
    
    # c1 left top
    x1 <- cds[3,1] - e[1]
    y1 <- e[4] - cds[3,2]
    
    # c2 right top
    x2 <- cds[1,1] - e[1]
    y2 <- e[4] - cds[3,2]
    
    # c3 right bottom
    x3 <- cds[1,1] - e[1]
    y3 <- e[4] - cds[1,2]
    
    # c4 left bottom
    x4 <- cds[3,1] - e[1]
    y4 <- e[4] - cds[1,2]
    
    dx <- extent(r)[2] - extent(r)[1] # geographic size of image (m)
    dy <- extent(r)[4] - extent(r)[3]
    nx <- nrow(r)
    ny <- ncol(r)
    
    b_abs <- matrix(c(x1, y1, x2, y2, x3, y3, x4, y4), ncol=2, byrow = TRUE)
    b_rel <- b_abs / matrix(rep(c(dx, dy), 4), ncol=2, byrow = TRUE)
    
    b_out <- round(b_rel * matrix(rep(c(nx, ny), 4), ncol=2, byrow=TRUE))
    
    # xleft, ytop, width, height
    dflist[[i]] <- as.integer(c(b_out[2,1], b_out[1,2], b_out[1,1]-b_out[2,1], b_out[3,2]-b_out[2,2]) )
    
  }
  return(dflist)
}

func_get_annotations <- function(jpg_id) {
    
    cat(row.names(jpg_id), jpg_id$filename, '\n')
    
    r      <- brick(jpg_id$filename)
    bb1    <- st_bbox(r)
    pol    <- st_polygon(list(matrix(c(bb1$xmin, bb1$ymin, bb1$xmin, bb1$ymax, bb1$xmax, bb1$ymax, bb1$xmax, bb1$ymin, bb1$xmin, bb1$ymin), ncol=2, byrow=TRUE ) ) )
    
    bboxes <- st_read("~/work/projects/salmon/data/labels_lines_bbox.gpkg", layer=jpg_id$layername) %>% #labels_lines_bbox.gpkg
      st_transform(crs(r)) %>%
      cbind(st_centroid(.)) %>%
      st_set_geometry("geom.1") %>% # set the points as the active geometry
      mutate(int = apply(st_intersects(., pol, sparse = FALSE),1, any ) ) %>%
      dplyr::filter(int) %>%
      st_set_geometry("geom")

    bb <- func_get_bboxes(r=r, bboxes=bboxes) 

  out <- data.frame("image_id" = jpg_id$image_id, "bbox" = I(bb), "category_id" = as.integer(1), n=dim(jpg_id)[1]) 
  return(out)
}

annotations <- map_df(1:dim(jpgs_pos)[1], function(x) func_get_annotations(jpgs_pos[x, ]) ) %>%
  mutate(segmentation = list(integer(1)),
         area         = as.integer(round(100*runif(n=dim(.)[1]))),
         iscrowd      = as.integer(0),
         id           = as.integer(row.names(.)),
         ignore       = as.integer(0) ) %>%
  dplyr::select("segmentation", "area", "iscrowd", "image_id", "bbox", "category_id", "id", "ignore")

categories  <- data.frame(supercategory = "none", id=as.integer(1), name="s")

train <- list(images=images[,c('file_name', 'height', 'width', 'id')], type="instances", annotations = annotations, categories=categories)

str(train$images)
str(train$type)
str(train$annotations)

jsonlite::write_json(train, path="train.json")

plot_img_bbox <- function(img) {
  r      <- brick(img$filename)
  
  bboxes <- st_read("~/work/projects/salmon/data/labels_lines_bbox.gpkg", layer=img$layername) %>%
    st_transform(crs(r)) %>%
    st_crop(r)
  
  bb <- func_get_bboxes(r=r, bboxes=bboxes) 
  
  ggRGB(r, maxpixels=1000000, r=1, g=2, b=3) + scale_x_continuous(limits=extent(r)[1:2], expand = c(0,0)) + 
    scale_y_continuous(limits=extent(r)[3:4], expand = c(0,0)) + geom_sf(data=bboxes, fill=NA, col="blue")
}

'salmon_20201002_PSCSal_2_Reach2-1_30m_11_T.jpg'
which(jpgs_pos$file_name == 'salmon_20201002_PSCSal_2_Reach2-1_30m_11_T.jpg')
plot_img_bbox(img=jpgs_pos[1, ])

plot_img_bbox(img=jpgs_pos[5, ])

img <- image_read(jpgs_pos[5, ]$filename)

print(img)
image_contrast(img, sharpen = 100)
#img2 <- image_enhance(img)

image_modulate(img, brightness = 100, saturation = 100, hue = 100)

image_annotate(img, "O", size = 30, color = "red", boxcolor = "pink",
               degrees = 0, location = "+716+188")

copy_train_valid_test(ptrain = 0.8, ptest = 0.1, pvalid = 0.1, out_resolution = "224x224!")


lrs  <- st_layers("~/work/projects/salmon/data/salmon_labels.gpkg")$name 
lrs_line <- lrs[grepl('line', lrs)]  

layer <- lrs_line[1]

# function(layer=layer) {
#   lines <- st_read("~/work/projects/salmon/data/salmon_labels.gpkg", layer=layer) 
#   kk <- lapply(lines, f)
#   bbx   <- map(lines, st_bbox)
# }
# 

# mapview(lines) + st_bbox_by_feature(lines)


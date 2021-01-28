source('scripts/func_salmon.R')
source('scripts/EThemes.R')

# remove empty layers from the geopackage that Claudia made
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

layers1 <- st_layers("~/work/projects/salmon/data/labels_Claudia.gpkg")[c(1,4)] %>%
  data.frame() %>%
  set_names(c('name', 'features_C'))

layers2 <- st_layers("~/work/projects/salmon/data/labels_Kayley.gpkg")[c(1,4)] %>%
  data.frame() %>%
  set_names(c('name', 'features_K'))

lys <- left_join(layers1, layers2)

# read the layer from the right gpkg and write to final label set
for (i in lys$name) {
    if (is.na(lys[lys$name == i,'features_K']) | lys[lys$name == i, 'features_C'] > lys[lys$name == i,'features_K']) {
      l <- st_read("~/work/projects/salmon/data/labels_Claudia.gpkg", layer=i)  
    } else {
      l <- st_read("~/work/projects/salmon/data/labels_Kayley.gpkg", layer=i) 
    }
  st_write(l, "~/work/projects/salmon/data/labels.gpkg", layer=i, delete_layer = TRUE)
}

# make the bounding boxes
st_bbox_by_feature = function(x) {
  x = st_geometry(x)
  f <- function(y) st_as_sfc(st_bbox(y))
  do.call("c", lapply(x, f))
}

fnames_tif <- list.files("/media/eelke/Samsung_T5/salmon/GIS", pattern="tif", full.names=FALSE) 
fnames_ortho <- str_split(fnames_tif, pattern='.tif', simplify=TRUE)[,1]
for (i in lys$name) {
  if (i %in% fnames_ortho) {
  ortho <- brick(paste0("/media/eelke/Samsung_T5/salmon/GIS/", i, ".tif")  )
  bbox <- st_read("~/work/projects/salmon/data/labels.gpkg", layer=i) %>%
    st_transform(crs(ortho)) %>%
    st_buffer(., dist = 0.4) %>%
    st_bbox_by_feature(.)
  } 
  st_write(bbox, "~/work/projects/salmon/data/labels_bbox.gpkg", layer=i, delete_layer = TRUE)
}

# select the files for which to make a dataset
layers_subset <- st_layers("~/work/projects/salmon/data/labels.gpkg")[c(1,4)] %>%
  data.frame() %>%
  filter(features > 50)

numb <- which(layers_subset == 'salmon_20201007_PSCSal_5_Reach2-1_10m-40')

map(layers_subset$name[2:10], func_imgprep)

copy_train_valid_test(ptrain = 0.8, ptest = 0.1, pvalid = 0.1, out_resolution = "224x224")


lrs  <- st_layers("~/work/projects/salmon/data/salmon_labels.gpkg")$name 
lrs_line <- lrs[grepl('line', lrs)]  

layer <- lrs_line[1]

# function(layer=layer) {
#   lines <- st_read("~/work/projects/salmon/data/salmon_labels.gpkg", layer=layer) 
#   kk <- lapply(lines, f)
#   bbx   <- map(lines, st_bbox)
# }
# 

mapview(lines) + st_bbox_by_feature(lines)


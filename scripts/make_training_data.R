source('scripts/func_salmon.R')
source('scripts/EThemes.R')

# this removes the empty layers from the geopackage that Claudia made
# for (layer in layers) {
#   labels <- st_read("~/work/projects/salmon/data/Labelled files.gpkg", layer=layer) 
#   if (dim(labels)[1]>0) {
#     st_write(labels, "~/work/projects/salmon/data/salmon_labels.gpkg", layer=layer, delete_layer = TRUE) 
#   }
# }

path   <- "/media/eelke/Samsung_T5/salmon/GIS"
fnames <- list.files(path, pattern="tif", full.names=FALSE)
layers <- str_split(fnames, '.tif', simplify=TRUE)[,1]


numb <- which(layers == 'salmon_20200930_PSCSal_11_Reach2-1_30m')

map(layers, func_imgprep)

copy_train_valid_test()

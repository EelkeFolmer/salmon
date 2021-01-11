# https://blogs.rstudio.com/ai/posts/2020-09-29-introducing-torch-for-r/
  
library(torch)
library(torchvision)

train_ds <- kmnist_dataset(
  ".",
  download = TRUE,
  train = TRUE,
  transform = transform_to_tensor
)

test_ds <- kmnist_dataset(
  ".",
  download = TRUE,
  train = FALSE,
  transform = transform_to_tensor
)

train_ds[1]
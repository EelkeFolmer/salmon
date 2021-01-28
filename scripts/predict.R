source('scripts/func_salmon.R')

model  <- torch_load("model10.pt")
device <- "cpu"

my_ds <- image_folder_dataset(root="data/play/", transform = null_transforms)
my_dl <- dataloader(my_ds, batch_size = 10, shuffle=TRUE)

b <- enumerate(my_dl)[[1]]
#Predictor and target are returned in a list, to be accessed as batch[[1]] and batch[[2]] during training.

class_names <- train_ds$classes
classes <- b[[2]]

view_batch(b)

output <- model(b[[1]])
labels <- b[[2]]$to(device = device)
#loss   <- criterion(output, labels)
predicted <- torch_max(output$data(), dim = 2)[[2]]
# torch_max returns a list, with position 1 containing the values
# and position 2 containing the respective indices


test_batch <- function(b) {
  
  output <- model(b[[1]])
  labels <- b[[2]]$to(device = device)
  loss   <- criterion(output, labels)
  
  test_losses <<- c(test_losses, loss$item())
  # torch_max returns a list, with position 1 containing the values
  # and position 2 containing the respective indices
  predicted <- torch_max(output$data(), dim = 2)[[2]]
  total <<- total + labels$size(1)
  # add number of correct classifications in this batch to the aggregate
  correct <<- correct + (predicted == labels)$sum()$item()
  
}

# testing
model$eval()
criterion <- nn_cross_entropy_loss()

for (b in enumerate(test_dl)) {
  test_batch(b)
}




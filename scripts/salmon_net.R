  source('scripts/func_salmon.R')
  
  device <- if (cuda_is_available()) torch_device("cuda:0") else "cpu"
  
  train_ds <- image_folder_dataset(root="/media/eelke/Samsung_T5/salmon/data/train/", transform = train_transforms)
  valid_ds <- image_folder_dataset(root="/media/eelke/Samsung_T5/salmon/data/valid/", transform = valid_transforms)
  test_ds  <- image_folder_dataset(root="/media/eelke/Samsung_T5/salmon/data/test/",  transform = valid_transforms)
  
  train_ds$.length()
  valid_ds$.length()
  test_ds$.length()
  
  batch_size <- 12
  train_dl   <- dataloader(train_ds, batch_size = batch_size, shuffle = TRUE)
  valid_dl   <- dataloader(valid_ds, batch_size = batch_size)
  test_dl    <- dataloader(test_ds, batch_size = batch_size)
  
  class_names <- train_ds$classes
  
  batch <- train_dl$.iter()$.next()
  batch[[1]]$size()
  batch[[2]]$size()
  classes <- batch[[2]]
  
  model <- model_resnet18(pretrained = TRUE)
  model$parameters %>% 
    purrr::walk(function(param) param$requires_grad_(FALSE))
  
  num_features <- model$fc$in_features
  model$fc     <- nn_linear(in_features = num_features, out_features = length(class_names))
  model        <- model$to(device = device)
  criterion    <- nn_cross_entropy_loss()
  optimizer    <- optim_sgd(model$parameters, lr = 0.05, momentum = 0.9)
  
  num_epochs = 2
  
  scheduler <- optimizer %>% 
    lr_one_cycle(max_lr = 0.01, epochs = num_epochs, steps_per_epoch = train_dl$.length())
  
  train_batch <- function(b) {
    optimizer$zero_grad()
    output <- model(b[[1]])
    loss <- criterion(output, b[[2]]$to(device = device))
    loss$backward()
    optimizer$step()
    scheduler$step()
    loss$item()
  }
  
  valid_batch <- function(b) {
    output <- model(b[[1]])
    loss <- criterion(output, b[[2]]$to(device = device))
    loss$item()
  }
  
  # training
  for (epoch in 1:num_epochs) {
    
    model$train()
    train_losses <- c()
    
    for (b in enumerate(train_dl)) {
      loss <- train_batch(b)
      train_losses <- c(train_losses, loss)
    }
    
    model$eval()
    valid_losses <- c()
    
    for (b in enumerate(valid_dl)) {
      loss <- valid_batch(b)
      valid_losses <- c(valid_losses, loss)
    }
    
    cat(sprintf("\nLoss at epoch %d: training: %3f, validation: %3f\n", epoch, mean(train_losses), mean(valid_losses)))
  }
  torch_save(model, "model.pt")
  # flush gpu

model_ <- torch_load("model.pt")
# testing
model$eval()

test_batch <- function(b) {
  
  output <- model(b[[1]])
  labels <- b[[2]]$to(device = device)
  loss <- criterion(output, labels)
  
  test_losses <<- c(test_losses, loss$item())
  # torch_max returns a list, with position 1 containing the values
  # and position 2 containing the respective indices
  predicted <- torch_max(output$data(), dim = 2)[[2]]
  total <<- total + labels$size(1)
  # add number of correct classifications in this batch to the aggregate
  correct <<- correct + (predicted == labels)$sum()$item()
  
}

test_losses <- c()
total <- 0
correct <- 0

for (b in enumerate(test_dl)) {
  test_batch(b)
}

mean(test_losses)

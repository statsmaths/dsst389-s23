library(readr)
library(keras)
library(png)

flowers <- read_csv("../data/flowers_17.csv.bz2")
x64 <- read_rds("../data/flowers_17_x64.rds")
x64 <- x64[flowers$class %in% 0:9,,,]

X <- array(0L, dim = c(dim(x64)[1], 224, 224, 3))
tf <- tempfile(fileext = "png")

for (j in seq_len(dim(x64)[1]))
{
  writePNG(x64[j,,,], tf)
  image <- image_load(tf, target_size = c(224,224))
  image <- image_to_array(image)
  X[j,,,] <- image
  print(j)
}

resnet50 <- application_resnet50(weights = 'imagenet', include_top = TRUE)

model_avg_pool <- keras_model(inputs = resnet50$input,
                              outputs = get_layer(resnet50, 'avg_pool')$output)

X_pp <- imagenet_preprocess_input(X)
pred <- predict(model_avg_pool, x = X_pp)

write_rds(pred, "../data/flowers_17_resnet.rds")
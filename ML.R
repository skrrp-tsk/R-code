library(caret)
library(tidymodels)

set.seed(123)
data(iris)

train_index <- createDataPartition(iris$Species, p=0.8, list = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

cat("Training rows: ", nrow(train_data), "| Testing rows:", nrow(test_data), "\n")

model_tree <- train(Species~., data=train_data, method = "rpart") #recursive partioning 
print(model_tree)

predictions <- predict(model_tree, newdata = test_data)

print(confusionMatrix(predictions, test_data$Species))

# regression mtcars
print("===========================================================")
data(mtcars)
train_index_r <- createDataPartition(mtcars$mpg, p=0.8, list = FALSE)
train_mt <- mtcars[train_index_r, ]
test_mt <- mtcars[-train_index_r, ]

model_lm <- train(mpg ~ wt + hp, data = train_mt, method = "lm")
print(model_lm)

predictions_mt <- predict(model_lm, newdata = test_mt)
print(data.frame(Actual = test_mt$mpg, Predicted =predictions_mt))



#classification using tidymodel
print("===========================================================")
split <- initial_split(iris, prop = 0.8, strata = Species)
train_data_c <- training(split)
test_data_c <- testing(split)

tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_fit <- tree_spec %>%
  fit(Species ~., data = train_data_c)

predictions <- predict(tree_fit, test_data_c) %>%
  bind_cols(test_data_c)

predictions %>%
  metrics(truth = Species, estimate = .pred_class)
print(predictions)



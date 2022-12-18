# Create a function to calculate the performance
# RMSE: sqrt(mean((truth - prediction)^2))

sqrt(mean((testing$averageScore - predict(model_svm, testing))^2))
sqrt(mean((testing$averageScore - predict(model_rpart, testing))^2))

# for any variable
rmse <- function(variable, model) {
  sqrt(mean((testing[,variable] - predict(model, testing))^2))
}

rmse('averageScore', 'model_svm')
rmse('averageScore', model_rpart)
rmse('averageScore', lm)


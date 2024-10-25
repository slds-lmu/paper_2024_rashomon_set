


library("mlr3")
library("mlr3learners")
library("data.table")

set.seed(1)
themat <- matrix(rnorm(10000 * 3), ncol = 3, byrow = TRUE)

colnames(themat) <- c("x1", "x2", "y")
thedf <- as.data.table(themat)

# thedf[, y := x1 + 2 * x2 + y * .5]
thedf[, x2 := 2 * x1 + x2]
thedf[, y := x1 + x2 + y * .5]

ts <- as_task_regr(thedf, target = "y", id = "smallsynth")



trn <- ts$clone(deep = TRUE)$filter(1:100)

lr <- lrn("regr.glmnet")

betas <- lapply(seq(0, 1, length.out = 100), function(alpha) {
  ll <- exp(seq(-10, 10, length.out = 1000))
  lr$param_set$values$lambda = ll
  lr$param_set$values$intercept = FALSE
  lr$param_set$values$alpha = alpha
  lr$train(trn)
  as.data.table(t(as.matrix(lr$model$beta)))[, lambda := ll][, alpha := alpha][]
}) |> rbindlist()


plot(betas[alpha == 0, x1])
plot(betas[alpha == 0, x2])



library("ggplot2")
library("ggnewscale")

ggplot(betas, aes(x = x1, y = x2, color = alpha, group = as.factor(alpha))) + geom_path() + theme_bw()



# Define a grid of beta1 and beta2 values from 0 to 4
beta1_vals <- seq(0, 2, length.out = 100)
beta2_vals <- seq(0, 2, length.out = 100)

# Initialize an empty data.table to store RMSE values
rmse_grid <- CJ(beta1 = beta1_vals, beta2 = beta2_vals)

# Calculate RMSE for each combination of beta1 and beta2
rmse_grid[, rmse := {
  # Generate predictions with intercept set to zero
  preds <- beta1 * thedf$x1 + beta2 * thedf$x2
  sqrt(mean((preds - thedf$y)^2))  # Calculate RMSE
}, by = .(beta1, beta2)]


# Combined plot with separate color scales and legends
ggplot() +
  # Contour plot for RMSE values, using `fill` to avoid overlap with `color` in lines
  geom_contour(data = rmse_grid, aes(x = beta1, y = beta2, z = rmse, color = ..level..), bins = 20, show.legend = TRUE) +
  scale_color_viridis_c(name = "RMSE") +  # Viridis color scale for RMSE contour
  new_scale_color() +
  # Line plot from `betas` data with a custom light-to-dark blue color scale for `alpha`
  geom_path(data = betas, aes(x = x1, y = x2, color = alpha, group = as.factor(alpha)), show.legend = TRUE) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Alpha") +
  # Coordinate system and labels
  labs(title = "Combined Plot: RMSE Contours and Betas Data",
       x = "Coefficient for x1",
       y = "Coefficient for x2") +
  coord_fixed() +
  theme_bw()






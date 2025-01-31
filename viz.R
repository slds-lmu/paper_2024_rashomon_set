library("mlr3")
library("mlr3learners")
library("data.table")

set.seed(42)
themat <- matrix(rnorm(10000 * 3), ncol = 3, byrow = TRUE)

colnames(themat) <- c("x1", "x2", "y")
thedf <- as.data.table(themat)

# thedf[, y := x1 + 2 * x2 + y * .5]
thedf[, x2 := 2 * x1 + x2]
thedf[, y := x1 + x2 + y * .5]

ts <- as_task_regr(thedf, target = "y", id = "smallsynth")

thedfhead <- head(thedf, 30)

trn <- ts$clone(deep = TRUE)$filter(1:30)

lr <- lrn("regr.glmnet")

insampleerror <- function(beta1, beta2) {
  preds <- beta1 * thedfhead$x1 + beta2 * thedfhead$x2
  sqrt(mean((preds - thedfhead$y)^2))
}

betas <- lapply(seq(0, 1, length.out = 100), function(alpha) {
  ll <- exp(seq(-10, 10, length.out = 1000))
  lr$param_set$values$lambda = ll
  lr$param_set$values$intercept = FALSE
  lr$param_set$values$alpha = alpha
  lr$train(trn)
  ises <- sapply(seq_len(ncol(lr$model$beta)), function(j) insampleerror(lr$model$beta[1, j], lr$model$beta[2, j]))
  as.data.table(t(as.matrix(lr$model$beta)))[, lambda := ll][, alpha := alpha][, ise := ises][]
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
rmse_grid_subsample <- CJ(beta1 = beta1_vals, beta2 = beta2_vals)

# Calculate RMSE for each combination of beta1 and beta2
rmse_grid[, rmse := {
  # Generate predictions with intercept set to zero
  preds <- beta1 * thedf$x1 + beta2 * thedf$x2
  sqrt(mean((preds - thedf$y)^2))  # Calculate RMSE
}, by = .(beta1, beta2)]

range(rmse_grid$rmse)


# Calculate RMSE for each combination of beta1 and beta2
rmse_grid_subsample[, rmse := {
  # Generate predictions with intercept set to zero
  preds <- beta1 * thedfhead$x1 + beta2 * thedfhead$x2
  sqrt(mean((preds - thedfhead$y)^2))  # Calculate RMSE
}, by = .(beta1, beta2)]

# Combined plot with separate color scales and legends
ggplot() +
  # Contour plot for RMSE values, using `fill` to avoid overlap with `color` in lines
  geom_contour(data = rmse_grid, aes(x = beta1, y = beta2, z = rmse, color = ..level..), breaks = seq(0.5, 3.5, length.out = 20), show.legend = TRUE) +
  scale_color_viridis_c(name = "RMSE") +  # Viridis color scale for RMSE contour
  new_scale_color() +
  # Line plot from `betas` data with a custom light-to-dark blue color scale for `alpha`
  geom_path(data = betas, aes(x = x1, y = x2, color = alpha, group = as.factor(alpha)), show.legend = TRUE) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "Alpha") +
  # Coordinate system and labels
  labs(title = "Combined Plot: RMSE Contours and Betas Data",
       x = expression(paste("Coefficient for ", x[1])),
       y = expression(paste("Coefficient for ", x[2]))) +
  coord_fixed() +
  theme_bw()

range(rmse_grid_subsample$rmse)
range(betas$ise)


bestmodel <- betas[which.min(ise)]

pdf("rmse_contours.pdf", width = 7, height = 6)

ggplot() +
  # Contour plot for RMSE values, using `fill` to avoid overlap with `color` in lines
  geom_contour(data = rmse_grid, aes(x = beta1, y = beta2, z = rmse), breaks = c(0.5, 0.65, 4), show.legend = TRUE, color = "black") +
  geom_contour(data = rmse_grid_subsample, aes(x = beta1, y = beta2, z = rmse), breaks = c(min(betas$ise), min(betas$ise) + 0.15, 4), show.legend = TRUE, color = "red") +
  scale_color_viridis_c(name = "RMSE") +  # Viridis color scale for RMSE contour
  new_scale_color() +
  # Line plot from `betas` data with a custom light-to-dark blue color scale for `alpha`
  geom_path(data = betas, aes(x = x1, y = x2, color = alpha, group = as.factor(alpha), alpha = betas$ise < min(betas$ise) + 0.15), show.legend = TRUE) +
  guides(alpha = FALSE) +
  scale_color_gradient(low = "lightblue", high = "darkblue", name = "alpha") +
  # Coordinate system and labels
  labs(x = expression(paste("Coefficient for ", x[1])),
       y = expression(paste("Coefficient for ", x[2]))) +
  coord_fixed() +
  theme_bw() +
  geom_point(data = bestmodel,
             aes(x = x1, y = x2),
             color = "red",
             shape = 4,  # 4 is for "x" shape
             size = 3)   # adjust size as needed

dev.off()




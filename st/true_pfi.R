source("init.R")
library(iml)
library(ggbeeswarm)
library(xtable)

### Define task ---------------------------------------------------------------
task = generateCanonicalDataSplits(list.tasks[["st"]], ratio = 2 / 3, seed = 1)$validation
task


### Define new mlr3 learner with exact prediction ---------------------------
library(mlr3)
library(R6)

LearnerRegrDGP = R6Class("LearnerRegrDGP",
                         inherit = LearnerRegr,
                         public = list(
                           initialize = function() {
                             super$initialize(
                               id = "regr.dgp",
                               feature_types = c("numeric"),
                               predict_types = "response",
                               # We define the properties to show it handles interactions
                               properties = "missings", 
                               packages = character()
                             )
                           }
                         ),
                         private = list(
                           .train = function(task) {
                             # We return an empty list because the "model" is hardcoded in .predict
                             list() 
                           },
                           .predict = function(task) {
                             # Access the data from the task
                             dt = task$data()
                             
                             # Apply the exact formula from your data generating process
                             # Y = X.4 + X.5 + (X.4 * X.5)
                             # Note: We do NOT add the noise here because the model represents 
                             # the expected value, not the realization with error.
                             response = dt$X.4 + dt$X.5 + (dt$X.4 * dt$X.5)
                             
                             list(response = response)
                           }
                         )
)

## CHECK
dgp_lrn = LearnerRegrDGP$new()
dgp_lrn$train(task)
preds = dgp_lrn$predict(task)
data = task$data()
sum(preds$response != data$X.4 + data$X.5 + data$X.4*data$X.5)

### PFI ---------------------------------------------------------------------
set.seed(1)
X = task$data(cols = task$feature_names)
y = task$data(cols = task$target_names)
predictor = Predictor$new(model = dgp_lrn, data = X, y = y)
# PFI via ratio (default). Alternative: compare = "difference"
pfi = FeatureImp$new(predictor, loss = "rmse", compare = "difference",
                     n.repetitions = 10)

print(xtable(pfi$results, caption = "PFI based on DGP (st)", digits = 3), include.rownames = FALSE)

plot_true_pfi = ggplot() +
  geom_point(data = pfi$results, aes(x = importance, y = feature),
                   cex = 3, shape = 16) +
  labs(x = "Importance", y = "Feature", title = paste0("PFI values based on DGP (st)")) +
  theme_minimal(base_size = 15)+
  theme(legend.text = element_text(size=13))
plot_true_pfi

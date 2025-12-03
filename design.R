
source("init.R")

library(data.table)

# Base path for model files
base_path = "/media/external/rashomon/rashomon_models"

# Discover learnernames from the directory structure
learnernames = list.dirs(base_path, full.names = FALSE, recursive = FALSE)
learnernames = learnernames[learnernames != ""]  # Remove empty strings

# Discover tasknames and count files for each learnername/taskname combination
design_list = list()

for (learnername in learnernames) {
  learner_path = file.path(base_path, learnername)
  
  # Discover tasknames for this learner
  tasknames = list.dirs(learner_path, full.names = FALSE, recursive = FALSE)
  tasknames = tasknames[tasknames != ""]  # Remove empty strings
  
  # Count files for each taskname
  for (taskname in tasknames) {
    task_path = file.path(learner_path, taskname)
    
    # Get RDS files in the directory
    rds_files = list.files(task_path, pattern = "\\.rds$", full.names = FALSE, recursive = FALSE, ignore.case = TRUE)
    no.models = length(rds_files)
    
    # Add to design list
    design_list[[length(design_list) + 1]] = data.table(
      rn = taskname,
      learnername = learnername,
      no.models = no.models,
      rds_files = list(rds_files)  # Store as list column
    )
  }
}

# Combine all entries into a single data.table
pre_design = rbindlist(design_list)

# Create design with expanded rows, model.no column, and rds file names
design = pre_design[, .(model.no = sequence(no.models),
                        rds = rds_files[[1]][sequence(no.models)]), 
                    by = .(rn, learnername)]

save(pre_design, design, file = "data/design_all_but_TreeFARMS.RData")

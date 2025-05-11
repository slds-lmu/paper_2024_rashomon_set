
source("init.R")

infotable <- unwrap(getJobTable(reg = reg.treefarms)[,
  .(job.id, time.running, prob.pars, algo.pars, successful = !is.na(done) & is.na(error))
])

fwrite(infotable, "data/treefarms/treefarms_info.csv")


for (job.id in infotable[successful == TRUE, job.id]) {
  sourcename <- batchtools:::getResultFiles(reg.treefarms, job.id)  # nolint
  targetname <- sprintf("data/treefarms/treefarms_%s.rds", job.id)
  file.copy(sourcename, targetname)
}

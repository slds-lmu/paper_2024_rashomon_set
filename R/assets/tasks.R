
task.bh <- tsk("boston_housing")
task.bh$select(selector_invert(selector_cardinality_greater_than(2))(task.bh))


task.gc <- tsk("german_credit")
task.cs <- tsk("compas")
task.bs <- tsk("bike_sharing")
task.bs$select(selector_invert(selector_type("character"))(task.bs))


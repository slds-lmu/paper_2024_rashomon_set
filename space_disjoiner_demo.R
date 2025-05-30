
# render with knitr::spin("space_disjoiner_demo.R", knit = TRUE, report = FALSE)

parset <- ps(
  x = p_dbl(0, 1),
  y = p_dbl(0, 1),
  a = p_fct(c("a", "b", "c")),
  b = p_lgl(depends = a == "b"),
  c = p_lgl(depends = a %in% c("b", "c") && b == TRUE),
  d = p_fct(c("x", "y"), depends = a == "c"),
  only.in.a.ab = p_dbl(0, 1, depends = a %in% c("a", "b")),
  only.in.c.true = p_dbl(0, 1, depends = c == TRUE)
)

parset

sdx <- SpaceDisjoiner$new(parset)

sdx$param.set

sdx$subspaces

grid <- generate_design_grid(parset, 2)

grid$data

sdx$disjoinTable(grid$data)

sdx$disjoinTable(grid$data[c(7, 5, 4, 1)])




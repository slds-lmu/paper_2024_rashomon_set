
RashomonSamplerChain <- R6Class("RashomonSamplerChain",
  inherit = RashomonSampler,
  public = list(
    initialize = function(id, samplers, n.rashomon.samples) {
      assertList(samplers, types = "RashomonSampler", min.len = 1L)
      domain <- samplers[[1]]$domain
      minimize <- samplers[[1]]$minimize
      rashomon.epsilon <- samplers[[1]]$rashomon.epsilon
      rashomon.is.relative <- samplers[[1]]$rashomon.is.relative
      for (i in seq_along(samplers)) {
        assertTRUE(samplers[[i]]$domain == domain,
          .var.name = sprintf("samplers[[%d]]$domain == samplers[[1]]$domain", i)
        )
        assertTRUE(samplers[[i]]$minimize == minimize,
          .var.name = sprintf("samplers[[%d]]$minimize == samplers[[1]]$minimize", i)
        )
        assertTRUE(samplers[[i]]$rashomon.epsilon == rashomon.epsilon,
          .var.name = sprintf("samplers[[%d]]$rashomon.epsilon == samplers[[1]]$rashomon.epsilon", i)
        )
        assertTRUE(samplers[[i]]$rashomon.is.relative == rashomon.is.relative,
          .var.name = sprintf("samplers[[%d]]$rashomon.is.relative == samplers[[1]]$rashomon.is.relative", i)
        )
      }
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, 0, n.rashomon.samples)
    }
  )
)

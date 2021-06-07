set.seed(123L)
init_sim <- simulation$new(years = 200, N = 500, mutation_rate = 1e-2, period = 50)

# Run after all tests
withr::defer(remove(init_sim), teardown_env())
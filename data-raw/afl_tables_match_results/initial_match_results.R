library(fitzRoy)

match_results <- get_match_results()

# Write data using devtools
devtools::use_data(match_results, overwrite = TRUE)
save(match_results, file = "./data-raw//afl_tables_match_results/match_results.rda")

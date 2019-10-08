library(microbenchmark)
library(fst)
library(tidyverse)
library(data.table)
library(feather)
library(fitzRoy)


player_stats <- update_footywire_stats(check_existing = TRUE)

# Write data out
path_csv <- here::here("data-raw", "player_stats", "player_stats.csv")
path_rda <- here::here("data-raw", "player_stats", "player_stats.rda")
path_fst <- here::here("data-raw", "player_stats", "player_stats.fst")
path_fst_full <- here::here("data-raw", "player_stats", "player_stats_full.fst")
path_feather <- here::here("data-raw", "player_stats", "player_stats.feather")

write.csv(player_stats, path_csv)
save(player_stats, file = path_rda)
fst::write_fst(player_stats, path = path_fst)
fst::write_fst(player_stats, path = path_fst_full, compress = 100)
feather::write_feather(player_stats, path_feather)

# benchmark
benchmark <- microbenchmark(
  read_rda = load(path_rda),
  read_fst = fst::read_fst(path_fst),
  read_fst_full = fst::read_fst(path_fst_full),
  read.csv = read.csv(path_csv),
  read_csv = readr::read_csv(path_csv, progress = F),
  fread = data.table::fread(path_csv, showProgress = F),
  feather = feather::read_feather(path_feather)
)

print(benchmark, signif = 2)
ggplot2::autoplot(benchmark)

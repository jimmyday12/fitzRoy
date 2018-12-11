library(fitzRoy)

fixture <- get_fixture(2019)

# Write data using devtools
usethis::use_data(fixture, overwrite = TRUE)
save(fixture, file = "./data-raw//footywire_fixture/fixture.rda")

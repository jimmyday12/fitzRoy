library(fitzRoy)

fixture <- get_fixture()

# Write data using devtools
devtools::use_data(fixture, overwrite = TRUE)
save(fixture, file = "./data-raw//footywire_fixture/fixture.rda")

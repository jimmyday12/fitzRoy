library(plumber)
pr("routes.R") %>%
  pr_run(port=8000)

#* Example query :: http://localhost:8000/fixtures?source=footywire&season=1998
#* All routes accept the same arguments as their corresponding library functions
#* See routes.R for parameter details
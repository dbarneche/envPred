npp  <- read.csv("data-raw/npp.csv", header = TRUE, stringsAsFactors = FALSE)
npp$dates  <-  as.Date(npp$dates)
usethis::use_data(npp, overwrite = TRUE)

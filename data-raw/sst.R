sst  <- read.csv("data-raw/sst.csv", header = TRUE, stringsAsFactors = FALSE)
sst$dates  <-  as.Date(sst$dates)
usethis::use_data(sst, overwrite = TRUE)

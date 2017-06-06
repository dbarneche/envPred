sst              <- read.csv('data-raw/sst.csv', header = TRUE, stringsAsFactors = FALSE)
sst$datesVector  <-  as.Date(sst$datesVector)
devtools::use_data(sst, overwrite = TRUE)

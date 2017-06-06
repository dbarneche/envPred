npp              <- read.csv('data-raw/npp.csv', header = TRUE, stringsAsFactors = FALSE)
npp$datesVector  <-  as.Date(npp$datesVector)
devtools::use_data(npp, overwrite = TRUE)

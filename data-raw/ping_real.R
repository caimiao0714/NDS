library(dplyr)
library(randomNames)

d = readRDS("data-raw/OTR_sample_ping.rds") %>%
  data.table::data.table() %>%
  .[,trip_id := NULL]

dlist = d[,.N,driver] %>%
  .[order(-N)] %>%
  .[1:5] %>%
  .[,`:=`(drivername = randomNames(.N, ethnicity = "White", which.names = "last"),
          N = NULL)]

ping_real = d %>%
  .[dlist, on = "driver"] %>%
  .[,.(driver = drivername, ping_time, speed, lat, lon = long)] %>%
  .[!is.na(driver)]

usethis::use_data(ping_real, overwrite = TRUE)

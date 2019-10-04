trip_sim = data.frame(
  start_time = as.POSIXct(c("2015-05-21 06:31:30", "2015-05-21 13:00:30", "2015-05-21 18:31:30")),
  end_time = as.POSIXct(c("2015-05-21 10:39:30", "2015-05-21 16:50:30", "2015-05-21 21:17:47"))
)

usethis::use_data(trip_sim, overwrite = TRUE)

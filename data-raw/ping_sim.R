set.seed(123)

ping_diff = 1*60 # 1 minutes
shift_length = 15*60*60/ping_diff # 14 hours
tdiff = rpois(shift_length, ping_diff)
shift_start = as.POSIXct("2015-05-21 06:21:30", format="%Y-%m-%d %H:%M:%S", tzone = "UTC")
ping_time = shift_start + cumsum(tdiff)
ping_speed = round(rnorm(length(tdiff), 62, 1.5))


ping_speed[ping_time <= as.POSIXct("2015-05-21 06:31:30", format="%Y-%m-%d %H:%M:%S", tzone = "UTC")] = 0
ping_speed[ping_time >=
        as.POSIXct("2015-05-21 06:31:30", format="%Y-%m-%d %H:%M:%S", tzone = "UTC") &
                    ping_time <=
        as.POSIXct("2015-05-21 10:39:30", format="%Y-%m-%d %H:%M:%S", tzone = "UTC")] = 0
ping_speed[ping_time >=
        as.POSIXct("2015-05-21 16:31:30", format="%Y-%m-%d %H:%M:%S", tzone = "UTC") &
                    ping_time <=
        as.POSIXct("2015-05-21 18:31:30", format="%Y-%m-%d %H:%M:%S", tzone = "UTC")] = 0
ping_speed[length(ping_speed)-5:length(ping_speed)] = 0

ping_sim = data.frame(ping_time, ping_speed)

usethis::use_data(ping_sim, overwrite = TRUE)

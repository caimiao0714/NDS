pacman::p_load(dplyr, ggplot2, data.table)
d = readRDS("data/ping_test.rds")
source("R/segment.R")

d1 = d[shift_id == 33,
       .(driver, ping_time, speed)]

d1 = d %>%
  .[,.(driver, ping_time, speed)] %>%
  .[,diff := as.integer(difftime(ping_time, shift(ping_time, type = "lag",
                                                  fill = 0), units = "mins")), driver] %>%
  .[,diff := {diff[1] = 0L; diff}, driver]


speed = d1[,speed]
threshold = 8*60
time_diff = d1[,diff]
driver = d1[,driver]

speed1 = speed
# 1. Replace large jumps between two non-zero speed
speed[time_diff >= threshold] <- 0
# 2. Mark continuous states (driving or stopping) with "order_tmp"
r1 = rle(speed != 0)
r1$values <- replicate(length(r1$values), 1)
r1$values <- cumsum(r1$values)
order_tmp <- inverse.rle(r1)
dat_tmp1 <- data.table::data.table(d1[,ping_time], speed, order_tmp, time_diff,
                                   time_diff1 = time_diff, order_tmp1 = order_tmp)
dat_tmp1[,sum_speed := sum(speed), order_tmp]
dat_tmp1[sum_speed != 0,
         order_tmp1 := {order_tmp1[1] = order_tmp1[1] - 1;order_tmp1},
         order_tmp]
# 3. Length of each state "dat_tmp2"
group_0 <- dat_tmp1[,.(sumdiff = sum(time_diff)), order_tmp]
group_r <- dat_tmp1[,.(sumdiff_r = sum(time_diff)),
                    order_tmp1][,.(order_tmp = order_tmp1, sumdiff_r)]
dat_tmp2 = group_r[group_0, on = "order_tmp"]
dat_tmp2$sumdiff_r[is.na(dat_tmp2$sumdiff_r)] =
  dat_tmp2$sumdiff[is.na(dat_tmp2$sumdiff_r)]

# 4. Recode 0 speed but the length < threshold to non-zero
r2 = rle(speed != 0)
first_rle = r2$values[1]
r2$values[!(r2$values) & dat_tmp2$sumdiff_r < threshold] <- TRUE
r2$values[1] = first_rle # correct first short 0s in each trip
r2 <- rle(inverse.rle(r2))
# 5. Index moving trips
r2$values[r2$values] = cumsum(r2$values[r2$values])
id = inverse.rle(r2)
# 6. Replace leading non-zero speed with the following trip id
jump_speed = which(id == 0 & speed1 != 0)
id[jump_speed] = id[jump_speed + 1]

d[,shift_id1 := id]

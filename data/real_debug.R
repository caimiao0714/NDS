pacman::p_load(dplyr, ggplot2, data.table)
threshold = 8*60

d = readRDS("data/real_test.Rds") %>%
  .[,diff := as.integer(difftime(ping_time, shift(ping_time, type = "lag",
                                                  fill = 0), units = "mins")), driver] %>%
  .[,diff := {diff[1] = 0L; diff}, driver]






speed1 = speed
# 1. Replace large jumps between two non-zero speed
speed[time_diff >= threshold] <- 0
# 2. Mark continuous states (driving or stopping) with "order_tmp"
r1 = rle(speed != 0); r2 = r1
r1$values <- rep(1, length(r1$values))
r1$values <- cumsum(r1$values)
order_tmp <- inverse.rle(r1)
dat_tmp1 <- data.table::data.table(driver, speed, order_tmp, time_diff,
                                   order_tmp1 = order_tmp)
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
first_rle = r2$values[1]
r2$values[!(r2$values) & dat_tmp2$sumdiff_r < threshold &
            dat_tmp2$sumdiff_r >= 0] <- TRUE
r2$values[1] = first_rle # correct first short 0s in each trip
r2 <- rle(inverse.rle(r2))
# 5. Index moving trips
r2$values[r2$values] = cumsum(r2$values[r2$values])
id = inverse.rle(r2)
# 6. Replace leading non-zero speed with the following trip id
jump_speed = which(id == 0 & speed1 != 0)
id[jump_speed] = id[jump_speed + 1]

d[,shift_id1 := id]


orginal_speed = d[,speed]
d1 = d %>%
  .[diff >= threshold, speed := 0] %>% # 1. Replace large jumps between two !0 speed
  .[,.(ping_time, speed, diff),.(driver, rleid(speed != 0))] %>%
  .[,rleid1 := rleid] %>%
  .[,sum_speed := sum(speed), rleid] %>%
  .[sum_speed != 0, rleid1 := {rleid1[1] = rleid1[1] - 1L;rleid1}, rleid]
# 2. Mark continuous states (driving or stopping) with "rleid1"
# 3. Length of each state "d2"
g_0 <- d1[,.(N = .N, sumdiff = sum(diff), sum_speed = sum_speed[1]), .(driver, rleid)]
g_r <- d1[,.(sumdiff_r = sum(diff)), .(driver, rleid1)] %>%
  .[,.(driver, rleid = rleid1, sumdiff_r)]
d2 = g_r[g_0, on = c("driver", "rleid")]
d2$sumdiff_r[is.na(d2$sumdiff_r)] = d2[is.na(sumdiff_r),sumdiff]
# 4. Recode 0 speed but the length < threshold to non-zero
d3 = d2 %>%
  .[,row_id := sequence(.N),driver] %>%
  .[(sum_speed == 0) & (sumdiff_r < threshold) & (row_id != 1), sum_speed := 666] %>%
  .[,trip_id := ifelse(sum_speed > 0, 1, 0)] %>%
  .[,trip_id1 := cumsum(trip_id), driver] %>%
  .[,.(trip_id, new_id = rleid(trip_id), N),driver] %>%
  .[,.(trip_id = trip_id[1], N = sum(N)), .(driver, new_id)]
# 5. Index moving trips
d3 %>%
  .[,new_id1 := cumsum(trip_id), driver] %>%
  .[,new_id1 := ifelse(trip_id == 0, 0, new_id1)]
id = d3[,rep(new_id1, N)]
# 6. Replace leading non-zero speed with the following trip id
jump_speed = which(id == 0 & orginal_speed != 0)
id[jump_speed] = id[jump_speed + 1]





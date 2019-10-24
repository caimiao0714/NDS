pacman::p_load(dplyr, ggplot2, data.table)
d = structure(list(ping_time=structure(c(1428476560,1428476738,
1428476862,1428477108,1428559630,1428560518,1428560532,1428560544,
1428560832,1428560896,1428560908,1428561280,1428561416,1428561576,
1428562320,1428562332,1428563220,1428563262,1428564120,1428564130,
1428565020,1428565030,1428565496,1428565508,1428565668,1428565920,
1428565962,1428566202,1428566440,1428566682,1428566694,1428566820,
1428566830,1428567208,1428567422,1428567680,1428567720,1428568090,
1428568264,1428568620,1428568630,1428569328,1428569520,1428569532,
1428570308,1428570326,1428570328,1428570338,1428570628,1428570816,
1428571320,1428571358,1428571362,1428572232,1428572758,1428572958,
1428573142,1428573196,1428573776,1428573850,1428573866,1428574020,
1428574064,1428574852,1428574920,1428575294,1428575820,1428575838,
1428576128,1428576730,1428577078,1428577102,1428577216,1428577452,
1428577590,1428577620,1428578520,1428578532,1428579116,1428579216,
1428579420,1428579430,1428579748,1428579980,1428580040,1428580156,
1428580202,1428580320,1428580660,1428580724,1428581220,1428581230,
1428582120,1428582134,1428582622,1428582624,1428582638,1428582788,
1428583020,1428583032),class=c("POSIXct","POSIXt"),tzone="UTC"),
speed=c(0L,0L,0L,7L,37L,11L,6L,13L,4L,0L,0L,
0L,0L,0L,0L,0L,0L,0L,0L,0L,60L,60L,28L,28L,28L,
28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,
28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,
28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,
28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,
28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,
28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,28L,
28L,28L,28L),diff=c(0L,2L,2L,4L,1375L,14L,0L,
0L,4L,1L,0L,6L,2L,2L,12L,0L,14L,0L,14L,0L,14L,
0L,7L,0L,2L,4L,0L,4L,3L,4L,0L,2L,0L,6L,3L,4L,
0L,6L,2L,5L,0L,11L,3L,0L,12L,0L,0L,0L,4L,3L,
8L,0L,0L,14L,8L,3L,3L,0L,9L,1L,0L,2L,0L,13L,
1L,6L,8L,0L,4L,10L,5L,0L,1L,3L,2L,0L,15L,0L,
9L,1L,3L,0L,5L,3L,1L,1L,0L,1L,5L,1L,8L,0L,14L,
0L,8L,0L,0L,2L,3L,0L)),class=c("data.table",
"data.frame"))

speed = d[,speed]
threshold = 8*60
time_diff = d[,diff]




speed1 = speed
# 1. Replace large jumps between two non-zero speed
speed[time_diff >= threshold] <- 0
# 2. Mark continuous states (driving or stopping) with "order_tmp"
r1 = rle(speed != 0)
r1$values <- replicate(length(r1$values), 1)
r1$values <- cumsum(r1$values)
order_tmp <- inverse.rle(r1)
dat_tmp1 <- data.table::data.table(speed, order_tmp, time_diff,
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

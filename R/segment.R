#' @title `segment` NDS ping data by adding a trip or shift ID column.
#'
#' @description This function segments NDS ping data to shifts and trips
#' @param speed Real time speed of the ping
#' @param threhold The threshold you want to separate the ping (in minutes). It is recommended to use 30 minutes to separate into trips, and 8*60 minutes to separate into shifts.
#' @param time_diff The time difference between the nearest two pings.
#' @keywords ping
#' @keywords threshold
#' @keywords trip
#' @keywords shift
#' @export
#' @examples segment(dat$speed, 30, d$time_diff)

segment = function(speed, threshold, time_diff) {
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
  return(id)
}

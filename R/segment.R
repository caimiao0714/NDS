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
#' @examples `segment(dat$speed, 30, d$time_diff)`

segment = function(speed, threshold, time_diff) {
  ## Replace very long single points
  speed[time_diff >= threshold] <- 0
  ## First, replacing stretches of less than "threshold" consecutive 0 speeds by 1s
  r1 = rle(speed != 0)
  r1$values <- replicate(length(r1$values), 1)
  r1$values <- cumsum(r1$values)
  order_tmp <- inverse.rle(r1)
  dat_tmp1 <- data.table::data.table(speed, order_tmp, time_diff)
  dat_tmp2 <- dat_tmp1[,.(sumdiff = sum(time_diff)), by = order_tmp]
  r2 = rle(speed != 0)
  r2$values[r2$values == 0 & dat_tmp2$sumdiff < threshold] <- TRUE
  r2 <- inverse.rle(r2)
  r2 <- rle(r2)
  ## Then numbering consecutive stretches of non-zero values
  r2$values[r2$values] = cumsum(r2$values[r2$values])
  return(inverse.rle(r2))
}

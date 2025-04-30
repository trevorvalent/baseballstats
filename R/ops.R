#' On Base Plus Slugging Percentage
#'
#' @param h Number of Hits the batter hit
#' @param bb Number of times the batter Walked
#' @param hbp Number of Times the batter was Hit by a Pitch
#' @param sf Number of Sacrifice Flies hit by the batter
#' @param tb Total Bases accumulated by the batter
#' @param ab At Bats for the batter
#'
#' @returns The batter's On Base plus Slugging Percentage (OPS) is returned. OPS is a combination of OBP and SLG and can also be calculated by summing them together. An OPS of .700 is usually an average value, while an OPS of .900 or above is considered to be extraordinary.
#' @export
#'
#' @examples ops(176, 75, 18, 5, 334, 603)
#' @examples ops(h = 197, bb = 81, hbp = 6, sf = 5, tb = 411, ab = 636)
ops <- function(h, bb, hbp, sf, tb, ab){
  top <- ab*(h + bb + hbp) + tb*(ab + bb + sf + hbp)
  bot <- ab*(ab + bb + sf + hbp)
  return(top/bot)
}

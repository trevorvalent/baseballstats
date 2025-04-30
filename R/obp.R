#' Calculate a Batter's On Base Percentage
#'
#' @param h Number of Hits the batter hit
#' @param bb Number of times the batter Walked
#' @param hbp Number of Times the batter was Hit by a Pitch
#' @param sf Number of Sacrifice Flies hit by the batter
#' @param ab At Bats for the batter
#'
#' @returns The batter's On Base Percentage (OBP) is printed. OBP measures how often a batter reaches base.
#' @export
#'
#' @examples obp(176, 75, 18, 3, 603)
#' @examples obp(h = 197, bb = 81, hbp = 6, sf = 5, ab = 636)

obp <- function(h, bb, hbp, sf, ab){
  top <- h + bb + hbp
  bot <- ab + bb + hbp + sf
  return(top/bot)
}

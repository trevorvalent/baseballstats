#' Batting Average
#'
#' @param h Number of Hits the batter hit
#' @param ab At Bats for the batter
#'
#' @returns The batter's Batting Average (AVG) is returned.
#' @export
#'
#' @examples avg(197, 636)
#' @examples avg(h = 138, ab = 537)

avg <- function(h, ab){
  ba <- (h / ab)
  rba <- round(ba, digits = 3)
  return(rba)
}

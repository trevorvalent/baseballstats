#' Calculate Fantasy Points
#'
#' @param r Number of Runs a batter scored
#' @param tb Total Bases acquired by a batter
#' @param rbi Number of Runs Batted In a batter hit
#' @param bb Number of times a batter Walked
#' @param k Number of times a batter struck out
#' @param sb Stolen Bases acquired by a batter
#' @param ip Innings Pitched by a Pitcher
#' @param ha Hits Allowed by a Pitcher
#' @param er Earned Runs allowed by a Pitcher
#' @param bba Walks Allowed by a Pitcher
#' @param so Number of Strikeouts thrown by a Pitcher
#' @param w Number of Wins accumulated by a Pitcher
#' @param l Number of Losses accumulated by a Pitcher
#' @param sv Number of Saves accumulated by a Pitcher
#' @param hd Number of Holds accumulated by a Pitcher
#'
#' @returns The total Fantasy Points scored by a player is returned. Weights of values are based on the default fantasy scoring settings in an ESPN Fantasy Baseball league in the year 2025.
#' @export
#'
#' @examples fpts(ip = 6.1, ha = 7, er = 1, bba = 1, so = 9, w = 1)
#' @examples fpts(r = 128, tb = 328, rbi = 109, bb = 129, sb = 7, k = 119)

fpts <- function(r = 0, tb = 0, rbi = 0, bb = 0, k = 0, sb = 0, ip = NULL, ha = 0, er = 0, bba = 0, so = 0, w = 0, l = 0, sv = 0, hd = 0){
  if (is.null(ip) == TRUE){
    score <- (r + tb + rbi + bb + sb - k)
  } else {
    ipf <- fixip(ip)
    score <- (3*ipf - ha - 2*er - bba + so + 2*w - 2*l + 5*sv + 2*hd)
  }
  return(score)
}

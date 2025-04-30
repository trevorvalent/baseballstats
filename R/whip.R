#' Walks Plus Hits Per Innings Pitched
#'
#' @param bba Walks Allowed by a Pitcher
#' @param ha Hits Allowed by a Pitcher
#' @param ip Innings Pitched by a Pitcher
#'
#' @returns The pitcher's Walks plus Hits per Innings Pitched (WHIP) is returned. WHIP measures a pitcher's tendency to allow batters to reach base.
#' @export
#'
#' @examples whip(64, 112, 180.2)
#' @examples whip(bba = 99, ha = 115, ip = 180)

whip <- function(bba, ha, ip){
  ipf <- fixip(ip)
  top <- bba + ha
  rwhip <- round((top/ipf), digits = 3)
  return(rwhip)
}

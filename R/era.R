#' Earned Run Average
#'
#' @param er Earned Runs surrendered by a pitcher
#' @param ip Innings Pitched by a Pitcher
#'
#' @returns The pitcher's Earned Run Average (ERA) is returned. ERA measures how many runs are given up on a 9-inning basis.
#' @export
#'
#' @examples era(58, 228.2)
#' @examples era(er = 85, ip = 197.1)

era <- function(er, ip){
  ipf <- fixip(ip)
  eravg <- 9*er / ipf
  reravg <- round(eravg, digits = 2)
  return(reravg)
}

#' Fix Innings Pitched Value
#'
#' @param ip Innings Pitched by a Pitcher
#'
#' @returns The Innings Pitched by a pitcher, as expressed as a fraction. Innings in baseball are expressed in terms of thirds, but in recording them, the thirds are represented by 0.1, 0.2, and 1. This function changes that in order to perform proper analysis on the IP values.
#' @export
#'
#' @examples fixip(42.1)
#' @examples fixip(118.2)
#' @examples fixip(190)

fixip <- function(ip){
  ip1 <- rep(0, length(ip))
  ip2 <- rep(0, length(ip))
  for(i in 1:length(ip)){
    ip1[i] <- round(ip[i], digits = 0)
    ip2[i] <- round(ip[i] - ip1[i], digits = 1)
    if(ip2[i] == 0.2){
      ip[i] <- round(ip[i], digits = 0) + (2/3)
    } else if(ip2[i] == 0.1){
      ip[i] <- round(ip[i], digits = 0) + (1/3)
    } else {
      ip[i] <- ip[i]
    }
  }
  return(ip)
}

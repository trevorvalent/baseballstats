#' Slugging Percentage
#'
#' @param tb Total Bases accumulated by the batter
#' @param ab At Bats for the batter
#'
#' @returns The batter's Slugging percentage (SLG)  is returned. Slugging percentage measures the efficiency of the batter's hits, with extra weight given to more valuable hits. Slugging percentage can also be calculated by replacing Total Bases (TB) with Singles + 2 x Doubles + 3 x Triples + 4 x Home Runs.
#' @export
#'
#' @examples slg(334, 603)
#' @examples slg(tb = 411, ab = 636)

slg <- function(tb, ab){
  slug <- tb/ab
  return(slug)
}

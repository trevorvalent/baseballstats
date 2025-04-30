library(ggplot2)
library(dplyr)
library(forcats)
#' Statistics Chart
#'
#' @param df The data frame used for the creation of the chart
#' @param command The statistic desired to be visualized
#' @param n The number of players desired to be visualized
#'
#'
#' @returns Creates a bar graph showcasing the top N players at a chosen statistic from the baseballStats package. Statistics available are avg, era, fpts, obp, ops, slg, and whip.
#' @export
#'
#' @examples statChart(hitters, "slg", n = 10)
#' @examples statChart(pitchers, "era", n = 15)

statChart <- function(df, command, n){
  if (command == "avg"){
    df$avg <- avg(df$h, df$ab)
    df1 <- slice_max (df, order_by = avg, n = n)
    command <- df1$avg
    command <- round(command, digits = 3)
    name <- "Batting Average"
  } else if (command == "era"){
    df$era <- era(df$er, df$ip)
    df1 <- slice_min(df, order_by = era, n = n)
    command <- df1$era
    command <- round(command, digits = 2)
    name <- "Earned Run Average"
  } else if (command == "fpts"){
    df$fpts <- fpts(r = df$r, tb = df$tb, rbi = df$rbi, bb = df$bb, k = df$k, sb = df$sb, ip = df$ip, ha = df$ha, er = df$er, bba = df$bba, so = df$so, w = df$w, l = df$l, sv = df$sv)
    df1 <- slice_max(df, order_by = fpts, n = n)
    command <- df1$fpts
    name <- "ESPN Fantasy Points"
  } else if (command == "obp"){
    df$obp <- obp(df$h, df$bb, df$hbp, df$sf, df$ab)
    df1 <- slice_max(df, order_by = obp, n = n)
    command <- df1$obp
    command <- round(command, digits = 3)
    name <- "On Base Percentage"
  } else if (command == "ops"){
    df$ops <- ops(df$h, df$bb, df$hbp, df$sf, df$tb, df$ab)
    df1 <- slice_max(df, order_by = ops, n = n)
    command <- df1$ops
    command <- round(command, digits = 3)
    name <- "On Base Plus Slugging Percentage"
  } else if (command == "slg"){
    df$slg <-  slg(df$tb, df$ab)
    df1 <- slice_max(df, order_by = slg, n = n)
    command <- df1$slg
    command <- round(command, digits = 3)
    name <- "Slugging Percentage"
  } else if (command == "whip"){
    df$whip <- whip(df$bba, df$ha, df$ip)
    df1 <- slice_min(df, order_by = whip, n = n)
    command <- df1$whip
    command <- round(command, digits = 3)
    name <- "Walks Plus Hits per Innings Pitched"
  }
  #Create Bar graph
  ggplot(df1, aes(x = fct_reorder(player, command, .desc = T), y = command)) + geom_bar(stat = "Identity", fill = "seagreen3") + xlab("Player Name") + ylab(name) + ggtitle(paste("Top", n, "Best Players", sep = " "), name) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  geom_text(aes(label = command), vjust = 1.4)
}

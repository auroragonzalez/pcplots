#' @param data
#' @param groupVble
#' @param jit
#' @param jitFactor
#' @export f.parallelCoor
#' @return g
#'
#' @importFrom dplyr %>%
#' @importFrom tidyr gather
#' @importFrom plyr ldply
#' @import ggplot2


f.parallelCoor <- function(data, groupVble = "V1", jit=F, jitFactor =10){  # big jitFactor to make it visible
  # colour palete in ggplot2
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }


  gVI <-which(names(data)==groupVble)  # group variable index
  df2 <- data
  df2[,-gVI] <- apply(df2[,-gVI],2,scale)
  df2 <- df2 %>% gather(variable, value,-groupVble)

  df2$id <- 1:nrow(data) # it will be repeated within each variable
  if(jit){
    X <- by(df2$value, df2$variable, jitter, factor=jitFactor)
    XX2 <- ldply (X, data.frame)
    df2[c("variable", "value")] <- XX2
  }
  df2 <- as.data.frame(df2)
  pal <- gg_color_hue(length(levels(df2[,gVI])))

  g <- df2 %>%
    ggplot() +
    geom_line(aes(x = variable, y = value, group = id, color=df2[,gVI]) )+
    geom_point(aes(x=variable, y= value), color=pal[as.numeric(df2[,gVI])])
  g
  }

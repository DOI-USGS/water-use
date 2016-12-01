visualize.wuBars <- function(viz=getContentInfo('wuBars'), waterUse=readDepends(viz)$waterUse, outfile=viz[["location"]]) {
  
  # read and munge the data
  library(dplyr)
  waterTots <- waterUse %>%
    group_by(year, category) %>%
    summarize(value=sum(value, na.rm=TRUE)) %>%
    arrange(year, category)
    
  # prototype plot
  library(ggplot2)
  ggplot(waterTots, aes(x=year, y=value, fill=category)) + geom_bar(stat='identity') + theme_classic() + ylab('total water use')
  
  # gsplot (not ready yet)
  library(gsplot)
  gs.conc <- gsplot() %>% 
    rect(geom.df$x.left, geom.df$y.bottom, 
         geom.df$x.right, geom.df$y.top,
         lwd=0.5, col = geom.df$rect.col, 
         border = NA,
         ylab = "Plastic particles\nper 100 gallons",
         ylim=c(0,5)) %>% 
    axis(side = 2, at = seq(0, 5, by=1)) %>%
    axis(1, labels=FALSE, lwd.tick = 0)
}

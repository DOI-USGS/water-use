

visualize.states_svg <- function(viz){
  data <- readDepends(viz)
  state.map <- data$`state-map`
  state.name <- as.character(row.names(state.map)[state.map@plotOrder])
  library(svglite)
  library(sp)
  svglite::svglite(viz[['location']])
  par(mai=c(0,0,0,0), omi=c(0,0,0,0))
  sp::plot(state.map)
  library(xml2)
  svg <- read_xml(viz[['location']])
  
  
  # let this thing scale:
  xml_attr(svg, "preserveAspectRatio") <- "xMidYMid meet" 
  xml_attr(svg, "id") <- "matthew-svg"
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]
  
  r <- xml_find_all(svg, '//*[local-name()="rect"]')
  
  xml_add_sibling(xml_children(svg)[[1]], 'rect', .where='before', width=vb[3], height=vb[4], class='background')
  xml_add_sibling(xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])
  
  # clean up junk that svglite adds:
  .junk <- lapply(r, xml_remove)
  p <- xml_find_all(svg, '//*[local-name()="path"]')
  if (length(p) != length(state.map)){
    stop('something is wrong, the number of states and polys is different')
  }
  
  for (i in 1:length(state.name)){
    id.name <- gsub(state.name[i], pattern = '[ ]', replacement = '-')
    xml_attr(p[[i]], 'id') <- id.name
    xml_attr(p[[i]], 'class') <- 'state-polygon'
    xml_attr(p[[i]], 'style') <- NULL
    xml_attr(p[[i]], 'clip-path') <- NULL
  }
  write_xml(svg, viz[['location']])
  
}
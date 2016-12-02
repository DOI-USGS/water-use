

visualize.states_svg <- function(viz){
  data <- readDepends(viz)
  state.map <- data$`state-map`
  states <- state.map$states
  shifts <- state.map$shifted.states
  centroids <- state.map$state.centroids
  state.name <- as.character(row.names(states)[states@plotOrder])
  library(svglite)
  library(sp)
  #svglite::svglite(viz[['location']])
  svg <- svglite::xmlSVG({
    par(mai=c(0,0,0,0), omi=c(0,0,0,0))
    sp::plot(shifts, ylim=bbox(states)[2,], xlim=bbox(states)[1,], setParUsrBB = TRUE)
    sp::plot(centroids, add=TRUE, pch=1)
  }, width = 4.612979, height = 3.233984) # get height from bbox ratio!!
  
  library(xml2)

  
  
  # let this thing scale:
  xml_attr(svg, "preserveAspectRatio") <- "xMidYMid meet" 
  xml_attr(svg, "xmlns") <- 'http://www.w3.org/2000/svg' 
  xml_attr(svg, "xmlns:xlink") <- 'http://www.w3.org/1999/xlink'
  xml_attr(svg, "id") <- "water-use-svg"
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]

  r <- xml_find_all(svg, '//*[local-name()="rect"]')

  xml_add_sibling(xml_children(svg)[[1]], 'rect', .where='before', width=vb[3], height=vb[4], class='background')
  xml_add_sibling(xml_children(svg)[[1]], 'desc', .where='before', viz[["alttext"]])
  xml_add_sibling(xml_children(svg)[[1]], 'title', .where='before', viz[["title"]])

  # clean up junk that svglite adds:
  .junk <- lapply(r, xml_remove)
  p <- xml_find_all(svg, '//*[local-name()="path"]')
  cr <- xml_find_all(svg, '//*[local-name()="circle"]')
  if (length(p) != length(states)){
    stop('something is wrong, the number of states and polys is different')
  }
  
  defs <- xml_find_all(svg, '//*[local-name()="defs"]')
  # !!---- use these lines when we have css for the svg ---!!
  # xml_remove(defs) 
  # defs <- xml_add_child(svg, 'defs') 
  cp <- xml_add_child(defs[[1]], 'clipPath', id="svg-bounds")
  xml_add_child(cp, 'rect', width=vb[3], height=vb[4])
  gb <- xml_add_child(svg, 'g', 'id' = 'state-backgrounds')
  gf <- xml_add_child(svg, 'g', 'id' = 'state-foregrounds')
  
  for (i in 1:length(state.name)){
    id.name <- gsub(state.name[i], pattern = '[ ]', replacement = '_')
    id.use <- paste0(id.name,'-pattern')
    circle <- cr[[i]]
    transform <- sprintf('translate(%s,%s)', xml_attr(cr[[i]],'cx'), xml_attr(cr[[i]],'cy'))
    
    # why can't xml2 allow me to just move the node to be under the group?
    xml_add_child(xml_add_child(gb, 'g', transform=transform), 
                  'use', 'xlink:href'=paste0("#", id.use), id=paste0(id.name,'-background'), class='state-background', 
                  onmousemove=sprintf("hovertext('%s',evt);", state.name[i]),
                  onmouseout="hovertext(' ');")
    xml_add_child(xml_add_child(gf, 'g', transform=transform), 
                  'use', 'xlink:href'=paste0("#", id.use), id=id.name, class='state-foreground',
                  onmousemove=sprintf("hovertext('%s',evt);", state.name[i]),
                  onmouseout="hovertext(' ');")
    xml_add_child(defs, 'path', d = xml_attr(p[i], 'd'), id=id.use)

  }
  
  g.tool <- xml_add_child(svg,'g',id='tooltip-group')
  xml_add_child(g.tool, 'rect', id="tooltip-box", height="24", class="tooltip-box")
  xml_add_child(g.tool, 'use', id="tooltip-point", d="M-6,-11 l6,10 l6,-11", class="tooltip-box")
  xml_add_child(g.tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="svg-text", " ")
  
  xml_remove(p)
  xml_remove(cr)

  write_xml(svg, viz[['location']])
  
}
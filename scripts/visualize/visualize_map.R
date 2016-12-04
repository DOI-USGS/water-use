

visualize.states_svg <- function(viz){
  data <- readDepends(viz)
  state.map <- data$`state-map`
  category.names <- unique(data[['calc-scaleFactors']]$category)
  states <- state.map$states
  shifts <- state.map$shifted.states
  centroids <- state.map$state.centroids
  state.name <- as.character(row.names(states)[states@plotOrder])
  state.hovertext <- sprintf("%s", capwords(state.name))
  library(svglite)
  library(sp)
  size <- apply(state.map$bbox, 1, diff)/500000
  svg <- svglite::xmlSVG({
    par(mai=c(0,0,0,0), omi=c(0,0,0,0))
    sp::plot(shifts, ylim=bbox(states)[2,], xlim=bbox(states)[1,], setParUsrBB = TRUE)
    sp::plot(centroids, add=TRUE, pch=1)
  }, width = size[1], height = size[2])

  library(xml2)

  top.buffer <- 36
  # let this thing scale:
  xml_attr(svg, "preserveAspectRatio") <- "xMidYMid meet"
  xml_attr(svg, "xmlns") <- 'http://www.w3.org/2000/svg'
  xml_attr(svg, "xmlns:xlink") <- 'http://www.w3.org/1999/xlink'
  xml_attr(svg, "id") <- "water-use-svg"
  vb.num <- as.numeric(strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]])
  vb.num[4] <- vb.num[4] + top.buffer
  vb.num[2] <- -top.buffer
  xml_attr(svg, 'viewBox') <- paste(vb.num, collapse = ' ')
  vb <- strsplit(xml_attr(svg, 'viewBox'),'[ ]')[[1]]
  r <- xml_find_all(svg, '//*[local-name()="rect"]')

  xml_add_sibling(xml_children(svg)[[1]], 'rect', .where='before', width=vb[3], height=vb[4], class='map-background')
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
  xml_remove(defs)
  defs <- xml_add_child(svg, 'defs')
  cp <- xml_add_child(defs, 'clipPath', id="svg-bounds")
  patt <- xml_add_child(defs, 'pattern', id="nodata", patternUnits="userSpaceOnUse", width="12", height="12")
  xml_add_child(patt, 'rect', height='12',width='12', class='nodata-fill')
  xml_add_child(patt, 'path', d = "M-1,1 l2,-2 M0,12 l12,-12 M11,13 l2,-2", class='nodata-lines')
  
  xml_add_child(cp, 'rect', width=vb[3], height=vb[4])
  gb <- xml_add_child(svg, 'g', 'id' = 'state-backgrounds')
  gf <- xml_add_child(svg, 'g', 'id' = 'state-foregrounds')
  g.tool <- xml_add_child(svg,'g',id='tooltip-group')
  gm <- xml_add_child(svg, 'g', 'id' = 'state-mouseovers')

  for (i in 1:length(state.name)){
    id.name <- gsub(state.name[i], pattern = '[ ]', replacement = '_')
    id.use <- paste0(id.name,'-pattern')
    circle <- cr[[i]]
    transform <- sprintf('translate(%s,%s)', xml_attr(cr[[i]],'cx'), xml_attr(cr[[i]],'cy'))

    # why can't xml2 allow me to just move the node to be under the group?
    xml_add_child(xml_add_child(gb, 'g', transform=transform),
                  'use', 'xlink:href'=paste0("#", id.use), id=paste0(id.name,'-background'), class='state-background')


    xml_add_child(xml_add_child(gf, 'g', transform=transform),
                  'use', 'xlink:href'=paste0("#", id.use), id=id.name, class='state-foreground',
                  onmousemove=sprintf("hovertext('%s',evt);", state.hovertext[i]),
                  onmouseout="hovertext(' ');")
    xml_add_child(xml_add_child(gm, 'g', transform=transform), # this sits on top but only for mouseover
                  'use', 'xlink:href'=paste0("#", id.use), opacity='0',
                  onmousemove=sprintf("hovertext('%s',evt);", state.hovertext[i]),
                  onmouseout="hovertext(' ');")
    xml_add_child(defs, 'path', d = xml_attr(p[i], 'd'), id=id.use)

  }

  g.button <- xml_add_child(svg, 'g', 'id' = 'category-buttons', transform='translate(610,250)')
  y.button <- as.character(seq(0, by=25, length.out=length(category.names)))
  w.button <- "90"
  x.text <- as.character(as.numeric(w.button)/2)
  h.button <- "20"
  xml_add_child(g.button, 'text', x=x.text, dy='-1.5em', "Water withdrawal", 
                class='legend-title-text svg-text')
  xml_add_child(g.button, 'text', x=x.text, dy='-0.5em', "categories", 
                class='legend-title-text svg-text')
  for (name in category.names){
    id <- gsub(pattern = ' ','_',name)
    xml_add_child(g.button, 'rect',  y = y.button[1], height=h.button, width=w.button,
                  id=sprintf('%s-button',id))
    xml_add_child(g.button, 'text', x=x.text, y = y.button[1], dy='1.1em', name, 
                  class='cat-button-text svg-text')
    xml_add_child(g.button, 'rect', y = y.button[1], height=h.button, width=w.button,
                  class='cat-button', id=id,
                  onclick=sprintf("setCategory('%s')", id))
    y.button <- tail(y.button, -1)
  }

  clip <- xml_add_child(xml_add_child(g.tool, 'defs'), 'clipPath', id="tipClip")
  xml_add_child(clip, 'rect', x='-6', y='-11.5', height='11', width='12')
  xml_add_child(g.tool, 'rect', id="tooltip-box", height="24", class="tooltip-box")
  xml_add_child(g.tool, 'path', id="tooltip-point", d="M-6,-12 l6,10 l6,-10", class="tooltip-box", 'clip-path'="url(#tipClip)")
  
  xml_add_child(g.tool, 'text', id="tooltip-text", dy="-1.1em", 'text-anchor'="middle", class="svg-text", " ")

  xml_remove(p)
  xml_remove(cr)

  write_xml(svg, viz[['location']])

}

capwords <- function(s) {
  capword <- function(w) {
    paste(ifelse(w == 'of', w, paste0(toupper(substring(w, 1, 1)), substring(w, 2))), collapse=" ")
  }
  sapply(strsplit(s, split = " "), capword, USE.NAMES = FALSE)
}
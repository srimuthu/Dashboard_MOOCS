fonts_selector <- function(...) {
  req_fonts <- unlist(list(...))
  available <- intersect(req_fonts, names(grDevices::postscriptFonts()))
  if(length(available) == 0)
    stop("Non of the requested font were found: ", req_fonts)
  
  available[1]
}

#' CFI color palette 
#'
#' The first 5 colors are based on the CFI logo. The last 5 colors are supplements.
#' 
#' @return Vector of length 10 with hex values
#' @author Jasper Ginn
#' 

CFI_palette <- function() {
  colors= c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC", "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44", "#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788")
  return(colors)
}

#' CFI ggplot2 theme 1
#' 
#' Based on scientific theme taken from https://github.com/Bart6114/artyfarty/blob/master/R/themes.R
#' 

theme_cfi_scientific <- function() {
  # Define palette with grey colors
  palette <- c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373",
               "#525252", "#252525", "#000000") # = brewer.pal 'greys'
  # Store
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  color.axis = palette[5]
  
  theme_bw(base_size=11,
           base_family = fonts_selector("Helvetica","Arial", "sans", "sans-serif")) +
    theme(
      
      panel.border=element_rect(color=color.background),
      
      panel.grid.major=element_line(color=color.grid.major,size=.25, linetype=2),
      panel.grid.minor=element_blank(),
      axis.line.x=element_line(color=color.axis),
      axis.line.y=element_line(color=color.axis),
      axis.ticks=element_line(color=color.axis),
      
      legend.key = element_rect(fill=NA, color=NA),
      legend.text = element_text(size=rel(.8),color=color.axis.title),
      
      plot.title=element_text(color=color.title, size=rel(1.2), vjust=1.25),
      axis.text.x=element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y=element_text(size=rel(.9),color=color.axis.text),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()
    )
}

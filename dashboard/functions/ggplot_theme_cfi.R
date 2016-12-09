#' Font selector, internal helper function
#'
#' Will return the first available font
#'
#' @param ... list of fonts requested
#' @export
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
  colors <- RColorBrewer::brewer.pal(8, "Set1")
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
      axis.text.x=element_text(size=rel(.9),color=color.axis.text),
      axis.text.y=element_text(size=rel(.9),color=color.axis.text),
      axis.title.x=element_text(size=rel(1),color=color.axis.title, vjust=0),
      axis.title.y=element_text(size=rel(1),color=color.axis.title, vjust=1.25)
    )
}

#' CFI ggplot2 theme 2
#' 
#' Based on 538 theme
#' 

theme_cfi_538 <- function(grid_lines = "vertical") {
  
  color.background = "#F0F0F0"
  color.grid.major = "#D3D3D3"
  color.text = "#181818"
  
  if(grid_lines == "vertical"){
    grid.major.x = element_blank()
    grid.major.y = element_line()
  } else {
    grid.major.x = element_line()
    grid.major.y = element_blank()
  }
  
  theme_bw(base_size=12, base_family = fonts_selector("Helvetica","Arial", "sans", "sans-serif")) +
    theme(
      
      panel.background=element_rect(fill=color.background, color=color.background),
      plot.background=element_rect(fill=color.background, color=color.background),
      panel.border=element_rect(color=color.background),
      
      panel.grid.major=element_line(color=color.grid.major,size=.25),
      panel.grid.major.x=grid.major.x,
      panel.grid.major.y=grid.major.y,
      panel.grid.minor=element_blank(),
      axis.ticks=element_blank(),
      
      legend.background = element_rect(fill=color.background),
      legend.key = element_rect(fill=color.background, color=NA),
      legend.text = element_text(size=rel(.8),color=color.text),
      
      plot.title=element_text(color=color.text, size=rel(1.05), vjust=1.25),
      axis.text.x=element_text(size=rel(.95),color=color.text),
      axis.text.y=element_text(size=rel(.95),color=color.text, face="bold"),
      axis.title.x=element_text(size=rel(1),color=color.text, vjust=0),
      axis.title.y=element_text(size=rel(1),color=color.text, vjust=1.25),
      
      plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")
    )
  
}

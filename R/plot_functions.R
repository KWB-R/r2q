#' Plots the output of function [check_all_substances()]
#' 
#' @param hazards List created by [check_all_substances()]
#' @param title Optional title
#' @param xlabels Character vector of the same length as Hazards to manually
#' enter x axis labels (-> Substance names)
#' @param ylabels Character vector of the same length as one vector of the 
#' Hazards list entry to manually enter y axis labels (-> Landuse names)
#' 
#' @return Plot with coloured rectangles representing the three different output
#' options from function [check_all_substances()]
#' 
#' @importFrom grDevices dev.new 
#' @importFrom graphics axis legend par rect
#' 
#' @export
#' 
plot_hazards <- function(
    hazards, title = "", xlabels = names(hazards), ylabels = names(hazards[[1]])
)
{
  hl <- length(hazards)
  ll <- length(hazards[[1]])
  
  dev.new(noRStudioGD = T, width = 8, height = 4)
  par(mar = c(10.1, 12.1, 2.1, 0.1))
  plot(x = 0, y = 0, xlim = c(0.5, hl + 0.5), 
       ylim = c(0.5, ll + 0.5), main = title,
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
  
  for(j in 1:hl){
    x <- hazards[[j]]
    for(i in 1:ll){
      col <- ifelse(x[i] == Inf, yes = "forestgreen", no = "orange")
      shading <- ifelse(x[i] == -Inf, yes = 50, no = NA)
      rect(xleft = j - 0.4 , xright = j + 0.4 , ybottom = i - 0.4, ytop = i + 0.4, 
           col = col, border = NA, density = shading)
    }
  }
  axis(side = 1, at = 1:hl, labels = xlabels, las = 2, tick = FALSE)
  axis(side = 2, at = 1:ll, labels = ylabels, las = 1, tick = FALSE)
  
  legend(x = 0, y = 0, 
         legend = c("No hazard", 
                    "Problematic, high\nbackground concentration", 
                    "Problematic, constraint\nfor connectable area"),
         fill= c("darkgreen", "orange", "orange"), 
         xjust = 1, 
         yjust = 1,
         density = c(NA, 50, NA), 
         border = "black", 
         xpd =TRUE, 
         bty = "n", 
         cex = 0.8, 
         y.intersp = 2)
}

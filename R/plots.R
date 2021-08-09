#' Summary plot of the Placido irregularity indices
#'
#' Draw a three-part plot summarizing the corneal topography analysis, based on the Placido irregularity indices calculated by the function \link[rPACI]{computePlacidoIndices}.
#' @param dataset A dataset containing the read corneal topography.
#' @param PlacidoIndices A dataset of results as given by the function \link[rPACI]{computePlacidoIndices} or \link[rPACI]{analyzeFile}.
#' @param filename An optional character argument, corresponding to the file containing the analyzed data.
#' If specified, the filename is displayed on the plot.
#' @importFrom graphics barplot boxplot par plot rect text
#' @importFrom grDevices rgb
#' @export
#' @examples
#' dataset = readCornealTopography(system.file("extdata","K04.txt", package="rPACI"))
#' results = computePlacidoIndices(dataset)
#' plotSingleCornea(dataset, results)
plotSingleCornea <- function(dataset, PlacidoIndices, filename=NULL) {
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))
  x=dataset[,"x"]
  y=dataset[,"y"]
  
  
  GLPI = as.numeric(PlacidoIndices["GLPI"])
  color = if(GLPI>=70) "red" else if (GLPI>=30 && GLPI<70) "orange" else "green"
  
  par(mar=c(3,3,3,3))
  par(fig=c(0,5,0,10)/10)
  plot(x,y, pch=20, asp=1, cex=0.2, xlab = "X coordinate (mm)", ylab= "Y coordinate (mm)", main="Corneal topography data")
  text(0,min(y),paste("Diagnose:\n",PlacidoIndices["Diagnose"],sep=""), col=color, cex=1.4, font=2)
  if(!is.null(filename)) {
    text(0,max(y),paste("Filename:\n",basename(filename),sep=""), col=color, cex=1.0, font=2)
  }
  
  par(fig=c(5,7.5,0,10)/10)
  par(new=TRUE)
  plot(GLPI, xlim=c(1,1),ylim=c(0,100),pch=16, cex=1.5, cex.main=0.8, col=color,xaxt="n",main="GLPI index")
  rect(0, -20, 2, 30, col = rgb(0,1,0,alpha=0.15), lwd=0)
  rect(0, 30, 2, 70, col = rgb(1,0.5,0,alpha=0.18), lwd=0)
  rect(0, 70, 2, 120, col = rgb(1,0,0,alpha=0.15), lwd=0)
  
  par(fig=c(7.5,10,0,10)/10)
  par(new=TRUE)
  boxplot(as.numeric(PlacidoIndices[,3:6]), ylim=c(0,150), cex=1.5, cex.main=0.8, main="PI indices \n distribution")
  rect(0, -20, 2, 30, col = rgb(0,1,0,alpha=0.15), lwd=0)
  rect(0, 30, 2, 70, col = rgb(1,0.5,0,alpha=0.18), lwd=0)
  rect(0, 70, 2, 180, col = rgb(1,0,0,alpha=0.15), lwd=0)
  
}


#' Plot indices over time
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_point geom_line labs theme_bw scale_color_manual geom_rect scale_fill_manual guides guide_legend
#' @importFrom tidyr gather
#' @importFrom ggpubr ggarrange
plotEvolution <- function(df){
  df_l = tidyr::gather(df, 'var', 'value', 4:7)
  p1 = ggplot2::ggplot()+
    ggplot2::geom_boxplot(data = df_l, ggplot2::aes(x = Time, y = value, color = 'PI indices'))+
    ggplot2::geom_point(data = df_l, ggplot2::aes(x = Time, y = GLPI, color = 'GLPI'))+
    ggplot2::geom_line(data = df_l, ggplot2::aes(x = as.numeric(Time), y = GLPI, color = 'GLPI'))+
    ggplot2::labs(y = '', color = '')+
    ggplot2::theme_bw()+
    ggplot2::scale_color_manual(values = c('PI indices'="black", "GLPI"="red"))+
    ggplot2::geom_rect(ggplot2::aes(xmin=0,xmax=nrow(df)+1,ymin=0,ymax=30, fill = 'normal'))+
    ggplot2::geom_rect(ggplot2::aes(xmin=0,xmax=nrow(df)+1,ymin=30,ymax=70, fill = 'suspected'), color = NA)+
    ggplot2::geom_rect(ggplot2::aes(xmin=0,xmax=nrow(df)+1,ymin=70,ymax=max(150, df_l2$value), fill = "irregular"), color = NA)+
    ggplot2::scale_fill_manual(name="Diagnosis", values = c("normal"=rgb(0,1,0,alpha=0.15), "suspected"=rgb(1,0.5,0,alpha=0.18), 'irregular' = rgb(1,0,0,alpha=0.15)),
                      limits = c("normal", "suspected", "irregular"),
                      labels=c("Normal","Suspected","Irregular"))+
    ggplot2::guides(color = ggplot2::guide_legend(order = 1),
           fill = ggplot2::guide_legend(order = 2)) 
  
  df_l2 = tidyr::gather(df, 'var', 'value', 3:7)
  
  p2 = ggplot2::ggplot()+
    ggplot2::geom_point(data = df_l2, ggplot2::aes(x = as.numeric(Time), y = value, color = var))+
    ggplot2::geom_line(data = df_l2, ggplot2::aes(x = as.numeric(Time), y = value, color = var))+
    ggplot2::labs(y = '', x = 'Time', color = '')+
    ggplot2::theme_bw()+
    ggplot2::scale_x_continuous(breaks = as.numeric(df$Time))+
    ggplot2::geom_rect(ggplot2::aes(xmin=0.9,xmax=nrow(df)+0.1,ymin=0,ymax=30, fill = 'normal'), color = NA)+
    ggplot2::geom_rect(ggplot2::aes(xmin=0.9,xmax=nrow(df)+0.1,ymin=30,ymax=70, fill = 'suspected'), color = NA)+
    ggplot2::geom_rect(ggplot2::aes(xmin=0.9,xmax=nrow(df)+0.1,ymin=70,ymax=max(150, df_l2$value), fill = "irregular"), color = NA)+
    ggplot2::scale_fill_manual(name="Diagnosis", values = c("normal"=rgb(0,1,0,alpha=0.15), "suspected"=rgb(1,0.5,0,alpha=0.18), 'irregular' = rgb(1,0,0,alpha=0.15)),
                      limits = c("normal", "suspected", "irregular"),
                      labels=c("Normal","Suspected","Irregular"))+
    ggplot2::guides(color = ggplot2::guide_legend(order = 1),
           fill = ggplot2::guide_legend(order = 2)) 
  
  
  return(ggpubr::ggarrange(p1,p2))
}
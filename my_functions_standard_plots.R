




plot_scatter_w_highlighted_clusters<-function(df_toplot,x,y,cluster_assignments,
                                              myxlabel,myylabel,mytitle,col_vector) {
  
  TEXTSIZE=15
  p<-ggplot(data=df_toplot)+
    geom_point(aes_string(x=x,y=y),color=cluster_assignments)+#, color=cell_123_array)+
    ggtitle(mytitle)+
    xlab(myxlabel)+ylab(myylabel)+
    scale_color_manual(values=col_vector)+
    theme(#legend.position="none",
      text = element_text(size=TEXTSIZE),
      axis.text = element_text(size=TEXTSIZE),
      plot.title = element_text(size=TEXTSIZE),
      legend.text = element_text(size=TEXTSIZE))
  
  return(p)
  
}


my_title_row<-function(mytitle) {
  par(mar = c(0,0,0,0))
  p<-plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(mytitle), 
       cex = 1.6, col = "black", srt=90)
  return(p)
}

my_title_col<-function(mytitle) {
  par(mar = c(0,0,0,0))
  p<-plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste("My title"), 
       cex = 1.6, col = "black", srt=0)
  return(p)
}




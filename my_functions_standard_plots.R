




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

plot_scatter_w_highlighted_clusters_condition<-function(df_toplot,x,y,cluster_assignments,conditions,condition_names,condition_markers,
                                              myxlabel,myylabel,mytitle,col_vector) {
  
  TEXTSIZE=15
  p<-ggplot(data=df_toplot)+
    geom_point(aes_string(x=x,y=y,color=cluster_assignments,shape=conditions),size=2,alpha=.8)+#, color=cell_123_array)+
    ggtitle(mytitle)+
    xlab(myxlabel)+ylab(myylabel)+
    scale_color_manual(values=col_vector)+
    scale_shape_manual(values=condition_markers,labels=condition_names)+
    theme(#legend.position="none",
      text = element_text(size=TEXTSIZE),
      axis.text = element_text(size=TEXTSIZE),
      plot.title = element_text(size=TEXTSIZE),
      legend.text = element_text(size=TEXTSIZE))
  
  return(p)
  
}

plot_scatter_w_highlighted_clusters_condition_exprgrad<-function(df_toplot,x,y,cluster_assignments_varname,conditions_varname,condition_names,condition_markers,
                                                        myxlabel,myylabel,mytitle,col_vector,selected_gene_expression_varname,savelocation) {
  
  TEXTSIZE=15
  
  # The gene expression plot
  p1<-ggplot(data=df_toplot)+
    geom_point(aes_string(x=x,y=y,size=selected_gene_expression_varname,color=selected_gene_expression_varname))+
    scale_color_gradient(low="white", high="black")+
    scale_size_continuous(range = c(1, 10))+
    theme(
      panel.background = element_rect(fill = "transparent") # bg of the panel
      , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
      , panel.grid.major = element_blank() # get rid of major grid
      , panel.grid.minor = element_blank() # get rid of minor grid
      , legend.background = element_rect(fill = "transparent") # get rid of legend bg
      , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )+
    ggtitle(mytitle)+
    xlab(myxlabel)+ylab(myylabel)+
    theme(#legend.position="none",
      text = element_text(size=TEXTSIZE),
      axis.text = element_text(size=TEXTSIZE),
      plot.title = element_text(size=TEXTSIZE),
      legend.text = element_text(size=TEXTSIZE))
  
  # The usual clustered scatter plot
  p2<-ggplot(data=df_toplot)+
    geom_point(aes_string(x=x,y=y,color=cluster_assignments_varname,shape=conditions_varname),size=2,alpha=.8)+#, color=cell_123_array)+
    scale_shape_manual(values=condition_markers,labels=condition_names)+
    scale_color_manual(values=col_vector)+
    theme(
      panel.background = element_rect(fill = "transparent") # bg of the panel
      , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
      , panel.grid.major = element_blank() # get rid of major grid
      , panel.grid.minor = element_blank() # get rid of minor grid
      , legend.background = element_rect(fill = "transparent") # get rid of legend bg
      , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )+
    ggtitle(mytitle)+
    xlab(myxlabel)+ylab(myylabel)+
    theme(#legend.position="none",
      text = element_text(size=TEXTSIZE),
      axis.text = element_text(size=TEXTSIZE),
      plot.title = element_text(size=TEXTSIZE),
      legend.text = element_text(size=TEXTSIZE))
  
  # Combined plot needs to be saved while writing it
  # https://stackoverflow.com/questions/29708821/how-to-save-a-grid-plot-in-r
  pdf(savelocation, height = 6, width = 6, paper = "special")
  
  # Create the combined plot
  # https://stackoverflow.com/questions/11508902/plotting-discrete-and-continuous-scales-in-same-ggplot
  grid.newpage()
  pushViewport( viewport( layout = grid.layout( 1 , 1 , widths = unit( 1 , "npc" ) ) ) ) 
  print( p1 + theme(legend.position="none") , vp = viewport( layout.pos.row = 1 , layout.pos.col = 1 ) )
  print( p2 + theme(legend.position="none") , vp = viewport( layout.pos.row = 1 , layout.pos.col = 1 ) )
  
  # end of saving
  dev.off()
  
  return(list(p1,p2))
  
}

plot_scatter_gene_expression<-function(df_toplot,x,y,cluster_assignments_varname,conditions_varname,condition_names,condition_markers,
                                                                 myxlabel,myylabel,mytitle,col_vector,selected_gene_expression_varname) {
  
  TEXTSIZE=15
  #colorScheme<-brewer.pal(3,"Spectral")

  # The gene expression plot
  p1<-ggplot(data=df_toplot)+
    geom_point(aes_string(x=x,y=y,
                          color=selected_gene_expression_varname, 
                          shape=conditions_varname
                          ),
               size=2,alpha=0.8)+
    #scale_color_gradientn(colours = c('lightskyblue','red4'))+
    scale_color_distiller(  palette="Spectral")+
    #scale_color_gradient2(low=colorScheme[1], mid=colorScheme[2],  high=colorScheme[3], guide=guide_colorbar(barheight=15))+
    #scale_shape_manual(values=condition_markers,labels=condition_names)+
    ggtitle(mytitle)+
    xlab(myxlabel)+ylab(myylabel)+
    theme(#legend.position="none",
      text = element_text(size=TEXTSIZE),
      axis.text = element_text(size=TEXTSIZE),
      plot.title = element_text(size=TEXTSIZE),
      legend.text = element_text(size=TEXTSIZE))
  
  return(p1)
  
}

pump_out_freq_df<-function(p,gene_expression, color, shape) {
  NR_BINS=150 # 40
  
  mybinwidth=ceiling((max(gene_expression)+1)/NR_BINS)
  
  freq <- hist(x=as.numeric(gene_expression),
               breaks=seq(0,max(gene_expression)+mybinwidth,mybinwidth),
               plot=FALSE)
  
  freq_df <- data.frame(centers = freq$mids, counts = freq$counts, my_gene_nr=as.factor(ii))
  
  #geom_line(data=freq_df,
  #             stat="identity", 
  #             mapping=aes(x=centers, y=counts),
  #             color=color)
  #shape
  
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

barplot_differential_expression<-function(selected_data_df,
                                          centers_varname,
                                          differential_expression_varname,
                                          all_gene_names,
                                          lowcol,highcol,
                                          ylabtext, mytitle) {
  TEXTSIZE=15
  
  mybreaks<-selected_data_df[[centers_varname]]
  
  ggplot(data=selected_data_df, mapping=aes_string(x=centers_varname, y=differential_expression_varname))+#,fill=dataset_id)) +
    geom_bar(stat="identity", mapping=aes_string(fill=differential_expression_varname))+
    scale_x_discrete(breaks=mybreaks,
                     labels=all_gene_names[selected_data_df$original_nr])+
    coord_flip()+
    xlab("Genes")+
    ylab(ylabtext)+
    scale_fill_gradient(low=lowcol, high=highcol)+
    ggtitle(mytitle)+
    theme(legend.position="none",
          text = element_text(size=TEXTSIZE),
          axis.text = element_text(size=TEXTSIZE),
          plot.title = element_text(size=TEXTSIZE))
}

give_better_textsize_plot <- function(TEXTSIZE){
  theme(legend.position="none",
        text = element_text(size=TEXTSIZE),
        axis.text = element_text(size=TEXTSIZE),
        plot.title = element_text(size=TEXTSIZE))
}

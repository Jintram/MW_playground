library(edgeR)

mean_var_out = edgeR::binMeanVar(data_container$groups$HCM$dt$ndata, group = rep(1,dim(data_container$groups$HCM$dt$ndata)[2]))

mean_var_out$avemeans
mean_var_out$avevars

allmeanvar_df=data.frame(means=mean_var_out$means, vars=mean_var_out$vars, gene=strip__chrXX(names(mean_var_out$means)))
ggplot(data.frame(avemeans=mean_var_out$avemeans, avevars=mean_var_out$avevars))+
    #geom_point(aes(x=avemeans,y=avevars))+
    geom_point(data=allmeanvar_df,aes(x=means, y=vars))+
    geom_text_repel(data=allmeanvar_df[allmeanvar_df$vars>10000,],aes(x=means, y=vars, label=gene))+
    geom_line(aes(x=avemeans,y=avevars),color='red')+
    theme_bw()+xlim(c(0,250))+ylim(c(0,50000))


dev.off()
tomo_plot_gene_expression_heatmap(cfg, data_container, group_name = 'HCM', gene_set_name = 'test',
                                  gene_set = allmeanvar_df[allmeanvar_df$vars>10000,]$gene, printNA = T, show=T,
                                    annotation_colors_custom=col_darjeeling1)


        
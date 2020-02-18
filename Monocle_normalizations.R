
# how was it again, how is the Monocle data normalized?

plot_df<-melt(as.matrix(exprs_val[,1:10]))

ggplot(plot_df)+
    #geom_violin(aes(y=value,x=Var2))+
    geom_boxplot(aes(y=value,x=Var2))+
    ylim(c(-1,10))

# answer: it isn't
apply(as.matrix(exprs_val),2,sum) # cells
apply(as.matrix(exprs_val),1,sum) # genes

# but in the applicable Monocle functions, cell-cell normalization is performed
apply(as.matrix(expression_genes_normalized),2,sum)
apply(as.matrix(expression_genes_normalized),1,sum)


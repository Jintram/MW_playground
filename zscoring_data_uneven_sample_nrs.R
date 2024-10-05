
# Is it valid to look at z-scores if the data has 
# unequal replicates? --> I think so ..
#
# Though statistics are a bit murky, as 
# the distribution of values might be different
# due to the difference in sample size..

blabladata = matrix(runif(n = 100*100), nrow=100)

for (idx in 1:10) {
    blabladata[idx,1:20]     = blabladata[idx,1:20]+(1*runif(1))*10
    blabladata[idx+10,21:100]  = blabladata[idx+10,21:100]+1*runif(1)*10
}


colnames(blabladata) = paste0('cell', 1:100)
rownames(blabladata) = paste0('gene', 1:100)

pheatmap(blabladata, cluster_rows = F, cluster_cols = F)

blabladata_scaled = t(scale(t(blabladata)))
    # rowSums(blabladata_scaled)
    # apply(blabladata_scaled, 1, sd)
pheatmap(blabladata_scaled, cluster_rows = F, cluster_cols = F)

composite_expr = apply(blabladata_scaled, 2, mean)
pheatmap(composite_expr, cluster_rows = F, cluster_cols = F)

mean(composite_expr[1:20])
mean(composite_expr[21:100])

##########

m1=c(); m2=c()

for (rep in 1:1000) {
    
    blabladata = matrix(runif(n = 100*100), nrow=100)
    
    for (idx in 1:10) {
        blabladata[idx,1:20]     = blabladata[idx,1:20]+(1*runif(1))*10
        blabladata[idx+10,21:100]  = blabladata[idx+10,21:100]+1*runif(1)*10
    }
    
    
    colnames(blabladata) = paste0('cell', 1:100)
    rownames(blabladata) = paste0('gene', 1:100)
    
    # pheatmap(blabladata, cluster_rows = F, cluster_cols = F)
    
    blabladata_scaled = t(scale(t(blabladata)))
        # rowSums(blabladata_scaled)
        # apply(blabladata_scaled, 1, sd)
    # pheatmap(blabladata_scaled, cluster_rows = F, cluster_cols = F)
    
    composite_expr = apply(blabladata_scaled, 2, mean)
    # pheatmap(composite_expr, cluster_rows = F, cluster_cols = F)
    
    m1[rep] = mean(composite_expr[1:20])
    m2[rep] = mean(composite_expr[21:100])
}

plot_df = data.frame(expr=c(m1, m2), mycategory=as.factor(c(  rep(1, length(m1)), rep(2, length(m2))) )   )
ggplot(plot_df, aes(x=expr, color=mycategory, fill=mycategory))+
    geom_freqpoly()+
    theme_bw()

mean(m1)
mean(m2)

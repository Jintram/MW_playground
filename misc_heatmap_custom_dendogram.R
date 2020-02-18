

blabla<-matrix(c(1,1,1,1,0,1,
                 1,1,1,1,0,1,
                 1,0,1,1,0,1,
                 0,0,0,0,1,1,
                 0,0,0,0,1,1), nrow=6)

image(blabla)
heatmap(blabla)
heatmap.2(blabla)
pheatmap(blabla)

hclust_out_r <- hclust(dist(blabla))
hclust_out_c <- hclust(dist(t(blabla)))
plot(as.dendrogram(hclust_out_r))
plot(as.dendrogram(hclust_out_c))

the_example_clustering_c <- cutree(hclust_out_c, h=1.5)
col_the_example_clustering_c <- colors_random_100[the_example_clustering_c]
the_example_clustering_r <- cutree(hclust_out_r, h=1.5)
col_the_example_clustering_r <- colors_random_100[the_example_clustering_r]

heatmap(blabla,
    Colv=as.dendrogram(hclust_out_c), Rowv=as.dendrogram(hclust_out_r),
    ColSideColors = col_the_example_clustering_c, 
    RowSideColors = col_the_example_clustering_r,
    tracecol=NA)

heatmap.2(blabla,
    Colv=as.dendrogram(hclust_out_c), Rowv=as.dendrogram(hclust_out_r),
    ColSideColors = col_the_example_clustering_c, 
    RowSideColors = col_the_example_clustering_r,
    tracecol=NA)






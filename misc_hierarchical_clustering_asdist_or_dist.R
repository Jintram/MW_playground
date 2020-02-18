
########################################################################
# build a "correlation matrix"

test_matrix1 <- matrix(c(rep(c(.9,.9,.9,.1,.1,.1),3),rep(c(.1,.1,.1,.9,.9,.9),3)),nrow=6)
test_matrix1 <- matrix(  c(rep(c(.9,.9,.3,.3,.1,.1),2),
                          #    c(.3,.3,.5,.5,.4,.4),
                          #    c(.3,.3,.9,.9,.4,.4),
                          rep(c(.3,.3,.2,.2,.4,.4),2),
                          rep(c(.1,.1,.4,.4,.9,.9),2)),nrow=6)
diag(test_matrix1)<-1

dev.off()
pheatmap(test_matrix1)




########################################################################
# see how methods are different

# Set test matrix
test_matrix<-test_matrix1
# this is for example below, which is less relevant
#test_matrix<-cor_out_example 

# defualt way
hclust_out      <- hclust(dist(test_matrix))
# dev.off()
heatmap.2(test_matrix,
        Colv=as.dendrogram(hclust_out), Rowv=as.dendrogram(hclust_out), #Rowv=F, 
        dendrogram = 'column',
        tracecol=NA, col = viridis_pal(option = "D")(100))#c('#0000ff','#ff0000'))

# #####

# taking correlations as distances
hclust_out      <- hclust(as.dist(1-test_matrix),method='ward.D')
hclust_out      <- hclust(as.dist(1-test_matrix),method='complete') 
hclust_out      <- hclust(as.dist(1-test_matrix),method='single')
hclust_out      <- hclust(as.dist(1-test_matrix),method='ward.D2')
    # note that complete = max & single = min (closest)
    # Ward seems more logical, as it takes the average distance
    # see also https://towardsdatascience.com/understanding-the-concept-of-hierarchical-clustering-technique-c6e8243758ec
# dev.off()
heatmap.2(test_matrix,
        Colv=as.dendrogram(hclust_out), Rowv=as.dendrogram(hclust_out), #Rowv=F, 
        dendrogram = 'column',
        tracecol=NA, col = viridis_pal(option = "D")(100))#c('#0000ff','#ff0000'))








########################################################################
# a 2nd example, build a "gene expression dataset"

# clusters of genes
cl1 = rep(c(1,3,2),4)
cl2 = rep(c(1.4, 2.6,2),4)
cl3 = rep(c(8,4,2),4)
cl4 = rep(c(4,5,2),4)

coords=unlist(lapply(list(cl1,cl2,cl3,cl4), function(X) rep(X, 5)))
coords=coords+runif(length(coords),-.2,.2)
coords_mat=t(matrix(coords, nrow=12))
coords_mat

noisy_genes = matrix(runif(dim(coords_mat)[1]*dim(coords_mat)[2], min(coords_mat),max(coords_mat)), 
    nrow=dim(coords_mat)[1])

combined_mat=rbind(coords_mat,noise_cells)

ggplot(data.frame(x=combined_mat[,1],y=combined_mat[,2]),aes(x=x,y=y))+
    geom_point()

cor_out_example=cor(t(combined_mat))

pheatmap(cor_out_example,cluster_rows=F,cluster_cols=F)
pheatmap(cor_out_example,cluster_rows=T,cluster_cols=T)










library(stats)
library(ggplot2)
library(pheatmap)
library(patchwork)

# one example matrix
some_matrix = matrix(c(
                     1,  2,  0,  0,
                     2,  1,  0,  0,
                    -1, -2,  0,  0,
                    -1, -2,  0,  0,
                    .5, .5, .5,  0,
                    -1, -1, -1,  0), nrow=6, byrow = T)
# second example matrix
some_matrix = matrix(c(
                     2,  1.6,  0,  0,
                     2,  1.8,  0,  0,
                    2.2, 2.3,  0,  0,
                    -1.8, -2,  0,  0,
                    0, -.9, -1,  0,
                    0, -1, -.8,  0), nrow=6, byrow = T)
rownames(some_matrix) = c('gene_A','gene_B','gene_C','gene_D','gene_E','gene_F')
colnames(some_matrix) = c('sample_1','sample_2','sample_3','sample_4')
some_matrix=t(some_matrix)

some_matrix_scaled = scale(some_matrix, center = T, scale = T)
apply(some_matrix_scaled, 2, sum)
apply(some_matrix_scaled, 2, sd)

pheatmap(t(some_matrix_scaled), cluster_rows = F, cluster_cols = F, main = 'Expression')

# principal component analysis
pca_out = prcomp(some_matrix, scale. = T)
pca_out_scaled = prcomp(some_matrix_scaled)

pheatmap(t(pca_out$x), cluster_rows = F, cluster_cols = F, main = 'PCA')

# Also show samples on x,y with pc1 and pc2 as axis
ggplot(data.frame(pc1=pca_out$x[,1], pc2=pca_out$x[,2], label=rownames(some_matrix)), aes(x=pc1, y=pc2))+
    geom_point(color='red')+theme_bw()+
    geom_text(aes(label=label))+xlim(c(-2.5,2.5))+ylim(c(-2,2))


# compare scaled and unscaled data
round(pca_out$rotation,3)==round(pca_out_scaled$rotation,3)
    # interestingly, need for roundig illustrates that the scaling by PCA or
    # by the scale function makes a difference (probably PCA will do it again,
    # and thenn introduce errors).
    # PC4 is significantly different, but also has sdev ±0, so 
    # probably it's values are arbitrary 
ggplot(data.frame(x=as.vector(pca_out$rotation), y=as.vector(pca_out_scaled$rotation)))+
    geom_point(aes(x=x,y=y))

# reestablished_matrix = pca_out$rotation %*% pca_out$x

#reestablished_matrix = t(t(pca_out$x %*% t(pca_out$rotation)) + pca_out$center)
reestablished_matrix = t(t(pca_out$x %*% t(pca_out$rotation))*pca_out$scale + pca_out$center)
reestablished_matrix_scaled = t(t(pca_out_scaled$x %*% t(pca_out_scaled$rotation)))

# see sdevs
ggplot(data.frame(x=1:4, sdev=pca_out$sdev))+
    geom_point(aes(x, sdev))

# sanity check whether re-established matrix matches
ggplot(data.frame(x=as.vector(some_matrix), xprime=as.vector(reestablished_matrix)), )+
    geom_point(aes(x, xprime))+ggtitle('Comparison')
# and for the scaled one
ggplot(data.frame(x=as.vector(some_matrix_scaled), xprime=as.vector(reestablished_matrix_scaled)), )+
    geom_point(aes(x, xprime))


# check whether we see expected pc1 loadings
p1=ggplot(data.frame(x=rownames(pca_out$rotation), pc1loadings=pca_out$rotation[,1]), )+
    geom_bar(aes(x, pc1loadings), stat='identity')+theme_bw()+theme(axis.text.x = element_text(angle = 90))+xlab(element_blank())
    # so this is fully as expected

p2=ggplot(data.frame(x=rownames(pca_out$rotation), pc2loadings=pca_out$rotation[,2]), )+
    geom_bar(aes(x, pc2loadings), stat='identity')+theme_bw()+theme(axis.text.x = element_text(angle = 90))+xlab(element_blank())

p3=ggplot(data.frame(x=rownames(pca_out$rotation), pc3loadings=pca_out$rotation[,3]), )+
    geom_bar(aes(x, pc3loadings), stat='identity')+theme_bw()+theme(axis.text.x = element_text(angle = 90))+xlab(element_blank())

p4=ggplot(data.frame(x=rownames(pca_out$rotation), pc4loadings=pca_out$rotation[,4]), )+
    geom_bar(aes(x, pc4loadings), stat='identity')+theme_bw()+theme(axis.text.x = element_text(angle = 90))+xlab(element_blank())

(p1+p2)/
(p3+p4)

(p1+p2+p3+p4)+plot_layout(nrow=1)

################################################################################

# check whether we see expected pc1 loadings
p1_s=ggplot(data.frame(x=rownames(pca_out_scaled$rotation), pc1loadings=pca_out_scaled$rotation[,1]), )+
    geom_bar(aes(x, pc1loadings), stat='identity')+theme_bw()
    # so this is fully as expected

p2_s=ggplot(data.frame(x=rownames(pca_out_scaled$rotation), pc2loadings=pca_out_scaled$rotation[,2]), )+
    geom_bar(aes(x, pc2loadings), stat='identity')+theme_bw()

p3_s=ggplot(data.frame(x=rownames(pca_out_scaled$rotation), pc3loadings=pca_out_scaled$rotation[,3]), )+
    geom_bar(aes(x, pc3loadings), stat='identity')+theme_bw()

p4_s=ggplot(data.frame(x=rownames(pca_out_scaled$rotation), pc4loadings=pca_out_scaled$rotation[,4]), )+
    geom_bar(aes(x, pc4loadings), stat='identity')+theme_bw()

(p1_s+p2_s)/
(p3_s+p4_s)+plot_annotation(title='scaled')

################################################################################

# Now let's try to see what the matrix looks like with PC1 removed:

reestablished_matrix_nopc1 = t(t(pca_out$x[,-1] %*% t(pca_out$rotation[,-1])) + pca_out$center)

pheatmap(t(some_matrix_scaled), cluster_rows = F, cluster_cols = F, main = 'Original')
pheatmap(t(reestablished_matrix_nopc1), cluster_rows = F, cluster_cols = F, main = 'PC1 removed')


################################################################################
# Trying to make a little illustration

gene_colors = c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')

# fill with illustration data
# rows=samples, cols=genes
df_illustration = data.frame(gene=character(), sample=character(), gene_nr = integer())
some_matrix_noneg = some_matrix+abs(min(some_matrix))
nr_genes = dim(some_matrix_noneg)[2]
for (s in 1:dim(some_matrix_noneg)[1]) {
    for (g in 1:nr_genes) {
        
        if (some_matrix_noneg[s,g]>0) {
            df_illustration = rbind(df_illustration, 
                data.frame(gene=colnames(some_matrix_noneg)[g], sample=rownames(some_matrix_noneg)[s], gene_nr = rep(nr_genes-g+1,3^some_matrix_noneg[s,g])))
        }
        
    }
}

#View(df_illustration)

ggplot(df_illustration)+
    geom_jitter(aes(x=sample, color=gene, y=gene_nr), position = position_jitter(width = .2))+theme_void()+theme(legend.position = 'none')







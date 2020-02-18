
# correlation example to illustrate effect of non-normalized vs. normalized gene expression (ndata vs. zdata)

# libraries
library(reshape)
library(ggplot2)
library(ggpubr)
library(pheatmap)

source('/Users/m.wehrens/Documents/git_repos/SCS_RaceID3/Functions/MW_standard_plots.R')

# correlated but not very highly
n_expression_cell1 <- c(1, 1, 2, 2, 4, 1, 2, 3) # each entry would be the expression of a gene
n_expression_cell2 <- c(1, 1, 1, 1, 2, 3, 2, 4)
cor(z_expression_gene1, z_expression_gene2)

# now if a few genes are highly expressed, and correlated, this has a very strong effect
z_expression_cell1 <- c(10, 10, 2, 2, 4, 1, 2, 3)
z_expression_cell2 <- c(10, 10, 1, 1, 2, 3, 2, 4)
cor(z_expression_gene1, z_expression_gene2)

# example with actual normalization:
r_expression_cell1 <- c(10, 10, 2, 2, 4, 1, 2, 3) # each entry would be the expression of a gene
r_expression_cell2 <- c(10, 10, 1, 1, 2, 3, 2, 4)
r_expression_cell3 <- c( 0,  0, 1, 1, 2, 3, 2, 4)
gene_number        <- 1:length(r_expression_cell3)
# first we normalize such that the total transcript counts are equal per cell
df_expression_r <- as.data.frame(cbind(r_expression_cell1,r_expression_cell2,r_expression_cell3))
# cell-to-cell total transcript normalization
cs = colSums(df_expression_r, na.rm = TRUE)
df_expression_n = as.data.frame((t(t(df_expression_r / cs)) * median(cs)))
# gene expression normalization
myscale <- apply(df_expression_n,1,sum,na.rm=T)
df_expression_z <- as.data.frame(t(scale(t(df_expression_n),center=T,scale=myscale)))



p_r<-ggplot()+
    geom_bar(data=melt(cbind(df_expression_r, gene_number), id.vars='gene_number'), aes(x=gene_number,y=value,fill=variable), stat='identity',position = "dodge")+
    xlab('Gene #')+ylab('raw count')+
    give_better_textsize_plot(15)
p_n<-ggplot()+
    geom_bar(data=melt(cbind(df_expression_n, gene_number), id.vars='gene_number'), aes(x=gene_number,y=value,fill=variable), stat='identity',position = "dodge")+
    xlab('Gene #')+ylab('n expression')+
    give_better_textsize_plot(15)
p_z<-ggplot()+
    geom_bar(data=melt(cbind(df_expression_z, gene_number), id.vars='gene_number'), aes(x=gene_number,y=value,fill=variable), stat='identity',position = "dodge")+
    xlab('Gene #')+ylab('z expression')+
    give_better_textsize_plot(15)

ggarrange(p_r, p_n, p_z, nrow = 3)

# Now show the effects on the correlation matrices
p_cr<-pheatmap(cor(df_expression_r), fontsize = 15, main='R based raw counts')
p_cn<-pheatmap(cor(df_expression_n), fontsize = 15, main='R based total cell ct equal')
p_cz<-pheatmap(cor(df_expression_z), fontsize = 15, main='R based Z-score')

p_cr; p_cn; p_cz





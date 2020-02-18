# Quiz questions

a_matrix = matrix(c(1,2,3,4,5,6), nrow = 2, byrow = T)
a_matrix

# correct:
a_matrix[,c(1,2)]
a_matrix[1:2,1]
a_matrix[c(T,F),]
a_matrix[matrix(c(1,1,2,2),nrow=2)]
a_matrix[3:4]
# incorrect
a_matrix[,1,2]
a_matrix(2:3,)
a_matrix[99,]

for (idx in 1:5) {
    print(idx^2)
}

my_example_df = data.frame(
    observed_value = c(0.0535885400604457, 0.399725134251639, 0.0192380927037448, 0.826326637994498, 0.759387862402946, 0.681428464828059, 0.470854292390868, 0.0159712380263954, 0.985781421652064, 0.605334649095312, 0.583031141199172, 0.289758857805282),
    p_value = c(0.001, 0.001, 0.1, 0.4, 0.1, 0.8, 0.29, 0.23, 0.11, 0.99, 0.88, 0.6))

x=c(1.65, 4.1, 3.26, 2.44, 9.49, 6.13, 6.35, 0.4)
y=c(3.39, 4.32, 9.74, 1.86, 5.67, 3.88, 2.23, 3.56)

library(ggplot2)
ggplot(data.frame(x=c(1.65, 4.1, 3.26, 2.44, 9.49, 6.13, 6.35, 0.4),
                  y=c(3.39, 4.32, 9.74, 1.86, 5.67, 3.88, 2.23, 3.56)))+
    geom_point(x=x,y=y)

c(1,2,3)!=c(2,2,3)

################################################################################
# Some more questions

gene_list = 
    c('Ldoc1l__chr15', 'Stxbp5l__chr16', 'Sox8__chr17', 'Zfp955b__chr17', 'Ttc9c__chr19', 'Grn__chr11', 'Gm14378__chr8', 'Mroh2b__chr15', 'Atp2b4__chr1', 'Prl3d1__chr13', 'Pan2__chr10', 'Suz12__chr11', 'Gm14164__chr2', 'Eif4g2__chr7', 'F930015N05Rik__chr11', 'Naa11__chr5', 'Leprel2__chr6', 'Slc6a5__chr7', 'Sdhc__chr1', 'Strn__chr17', 'Atp2a2__chr5', 'Lmo2__chr2', 'Phf11c__chr14', '1700017B05Rik__chr9', 'Lrrcc1__chr3', 'Zfp759__chr13', 'Ndufb6__chr4', 'Nsg1__chr5', 'Cd300lg__chr11', 'E030025P04Rik__chr11')

# Gene expression matrix was saved and generated with:
#gene_counts_temp = apply(dt_pooled$rdata>0,1,sum)
#int_genes = which(gene_counts_temp>1000)
#example_gene_expression_matrix = dt_pooled$rdata[sample(int_genes,30),1:30]
#save(file='/Users/m.wehrens/Documents/Presentations/POWERPOINT/_R_workshop/example_expression_data.Rdata','example_gene_expression_matrix')

load(file='/Users/m.wehrens/Documents/Presentations/POWERPOINT/_R_workshop/example_expression_data.Rdata','example_gene_expression_matrix')

rownames(example_gene_expression_matrix)
mean(example_gene_expression_matrix['Tmed9__chr13',])

colnames(example_gene_expression_matrix)
sum(example_gene_expression_matrix[,'p1.0055'])







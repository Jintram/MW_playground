
library(ggplot2)


regulon_identity = as.factor(rep(1:10 , times=regulon_control_nrs))

##########
# 

gene_expression = generate_cell_expression(2)
ggplot(data.frame(n=1:length(gene_expression), gene_expression=gene_expression, regulon_identity=regulon_identity))+
    geom_bar(aes(x=n,y=gene_expression,fill=regulon_identity), stat='identity')+
    theme_bw()


##########
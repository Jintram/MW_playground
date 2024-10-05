library(ggplot2)

df_example = data.frame(derivative=c(-.2,-.3,-.25,0,0.2,-0.1), condition=c('WT M','WT M','WT M','mutant M','mutant M','mutant M'))

ggplot(df_example)+
    geom_boxplot(aes(x=condition, y=derivative))+
    geom_jitter(aes(x=condition, y=derivative))+
    ggtitle('Example')+theme_bw()

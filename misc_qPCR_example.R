library(openxlsx)
library(ggplot2)

datasheet =
    openxlsx::read.xlsx('/Users/m.wehrens/Data/__qPCR/Laura_Sheet/example_sheet_normalization_qPCR.xlsx')

ggplot(datasheet, aes(x=condition, y=2^-ddCT, color=as.factor(biological)))+
    geom_jitter(width=.25, size=3)+theme_bw()+
    #ylim(c(0,1.5))+
    give_better_textsize_plot(15)+
    ylab('Fold change')+xlab(element_blank())+theme(legend.position = 'none')+
    ggtitle('Fold changes, keeping biol. var')

ggplot(datasheet, aes(x=condition, y=-ddCT, color=as.factor(biological)))+
    geom_jitter(width=.25, size=3)+theme_bw()+
    #ylim(c(0,1.5))+
    give_better_textsize_plot(15)+
    ylab('-ddCT')+xlab(element_blank())+theme(legend.position = 'none')+
    ggtitle('ddCT, keeping biol. var')

ggplot(datasheet, aes(x=condition, y=-ddCT_bio, color=as.factor(biological)))+
    geom_jitter(width=.25, size=3)+theme_bw()+
    #ylim(c(0,1.5))+
    give_better_textsize_plot(15)+
    ylab('-ddCT')+xlab(element_blank())+theme(legend.position = 'none')+
    ggtitle('ddCT, removing biol. var')

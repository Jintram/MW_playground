
source('/Users/m.wehrens/Documents/git_repos/SCS_More_analyses/Functions_2021/copies_small_convenient_functions.R')

library(ggplot2)

# Confounding example

#x=1:30
#y=1:30
df_grid = data.frame(x=c(1,1,2,2),
                   y=c(1,2,1,2))
df_grid=df_grid[rep(c(1,2,3,4),times=c(30,3,3,30)),]

# x = batch effect
# y = biological condition

#df_grid = expand.grid(x=x,y=y)
df_grid$condition = as.factor(paste0('B=',df_grid$x,',C=',df_grid$y))
df_grid$z = 30*df_grid$y + 100*df_grid$x
df_grid$z = df_grid$z+runif(n = length(df_grid$z),min = -10,max=10)

#ggplot(df_grid)+
#    geom_tile(aes(x=x,y=y,fill=z),color='white')+theme_minimal()

ggplot(df_grid)+
    geom_jitter(aes(x=condition,y=z,color=y))+theme_minimal()

# show dependencies separately
ggplot(df_grid)+
    geom_violin(aes(x=factor(x),y=z))+theme_minimal()+xlab('Batch')
ggplot(df_grid)+
    geom_violin(aes(x=factor(y),y=z))+theme_minimal()+ylab('Biological')

ggplot(df_grid,aes(x=x,y=z,color=y))+
    geom_point()+theme_minimal()

lm_out = lm(df_grid$z ~ df_grid$x)
lm_out$coefficients
lm_out1 = lm(df_grid[df_grid$y==1,]$z ~ df_grid[df_grid$y==1,]$x)
lm_out2 = lm(df_grid[df_grid$y==2,]$z ~ df_grid[df_grid$y==2,]$x)
lm_out1$coefficients[2]
lm_out2$coefficients[2]
# --> y=3+3*x
# aim is to remove dependence on x
# --> y' = y-3x
df_grid$zprime=df_grid$z-df_grid$x*lm_out1$coefficients[2]

# show fitted lines 
ggplot(df_grid,aes(x=x,y=z,color=as.factor(y)))+
    geom_point()+theme_minimal()+
    geom_abline(intercept = lm_out1$coefficients[1],slope = lm_out1$coefficients[2])+
    geom_abline(intercept = lm_out2$coefficients[1],slope = lm_out2$coefficients[2])+
    xlab('Batch')+ylab('Value')+guides(color=guide_legend(title="Condition"))+
    scale_x_continuous(breaks=c(1,2))+give_better_textsize_plot(15)

# Batch-corrected plot
ggplot(df_grid)+
    geom_jitter(aes(x=condition,y=zprime,color=y))+theme_minimal()+
    ggtitle('Corrected plot')
ggplot(df_grid)+
    geom_jitter(aes(x=y,y=zprime,color=as.factor(x)))+theme_minimal()+
    ggtitle('Batch-corrected plot')+give_better_textsize_plot(15)+
    scale_x_continuous(breaks=c(1,2))+guides(color=guide_legend(title="Batch"))+
    xlab('Condition')+ylab('Value')
# Non-corrected reference
ggplot(df_grid)+
    geom_jitter(aes(x=y,y=z,color=as.factor(x)))+theme_minimal()+
    ggtitle('Not corrected')+give_better_textsize_plot(15)+
    scale_x_continuous(breaks=c(1,2))+guides(color=guide_legend(title="Batch"))+
    xlab('Condition')+ylab('Value')

# show dependencies separately
ggplot(df_grid)+
    geom_violin(aes(x=factor(x),y=zprime))+theme_minimal()+xlab('Batch')
ggplot(df_grid)+
    geom_violin(aes(x=factor(y),y=zprime))+theme_minimal()+ylab('Biological')





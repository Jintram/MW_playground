library(openxlsx)
library(reshape)

feedback_data = openxlsx::read.xlsx(xlsxFile = '/Users/m.wehrens/Documents/Presentations/POWERPOINT/_R_workshop/Feedback_results_session_1n2.xlsx')

# To see whether it has worked properly..
View(feedback_data)

# Plot violins+jitter over time
ggplot(melt(feedback_data))+
    geom_violin(aes(x=variable, y=value*100, color=variable))+
    geom_jitter(aes(x=variable, y=value*100, color=variable))+
    ylab('Response (percentages)')+
    xlab('Question')+
    ggtitle('Feedback')+
    theme_bw()+
    theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1),
        legend.position = 'none')+
    ylim(c(0,100))

# Plot scatter for session 1
ggplot(feedback_data)+
    geom_point(aes(x=Time_well_spent_s1,y=Understood_s1,color=Relevance_s1),size=3)+
    xlim(c(0,1))+ylim(c(0,1))+theme_bw()+
    theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1),
        legend.position = 'right')





    
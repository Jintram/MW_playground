library(ggplot2)
library(xlsx)

give_better_textsize_plot <- function(mytextsize){
  theme(#legend.position="none",
        text = element_text(size=mytextsize),
        axis.text = element_text(size=mytextsize),
        plot.title = element_text(size=mytextsize))
}

CoronaData = read.xlsx('/Users/m.wehrens/Documents/Personal/corona_table.xlsx', sheetIndex = 1)

# Calculate cumulative deaths
sel = max((which(is.na(CoronaData$Nr_Deaths)))+1):length(CoronaData$Nr_Deaths)
CoronaData$Cum_Nr_Deaths[sel] = cumsum(CoronaData$Nr_Deaths[sel])
# Calculate hospitalizations per day
CoronaData$Nr_hospitalizations = (CoronaData$Cum_hospitalization)-
                                    c(0,CoronaData$Cum_hospitalization[1:length(CoronaData$Cum_hospitalization)-1])

# exponential fit with all point
lm_out = lm(log(Cum_Nr_Deaths) ~ Date, CoronaData)
GR = exp(lm_out$coefficients[2])
C0 = exp(lm_out$coefficients[1])
    
# exp fit based on selection of points
lm_out_ = lm(log(Cum_Nr_Deaths) ~ Date, CoronaData[CoronaData$Date<=20&CoronaData$Nr_Deaths>10,])
GR_ = exp(lm_out_$coefficients[2])
C0_ = exp(lm_out_$coefficients[1])

# Plot cumulative nr deaths
p=ggplot(CoronaData, aes(x=Date, y=log(Cum_Nr_Deaths)))+
    geom_point()+
    theme_bw()+
    #give_better_textsize_plot(15)+
    ggtitle('Corona Related Deaths in NL')
p1=p+
    stat_function(fun = function(x) {log(C0*GR^x)},colour = "blue") +
    stat_function(fun = function(x) {log(C0_*GR_^x)},colour = "red") 
    #geom_point(data=CoronaData[CoronaData$Date<=20,],colour='grey')
p1
p2=p+
    geom_smooth(data=CoronaData[CoronaData$Nr_Deaths>10,],colour='grey')+xlim(c(10,31))
p2

ggplot(CoronaData, aes(x=Date, y=Cum_Nr_Deaths))+
    geom_point()+
    #geom_smooth(method='lm', formula= y~x)+
    #stat_function(fun = function(x) {log10(C0*GR^x)},colour = "grey") +
        # scale_y_log10 goes wrong here..?! --> stat_function not affected; log10 fixes it
    scale_y_log10()+
    theme_bw()+
    #give_better_textsize_plot(15)+
    ggtitle('Corona Related Deaths in NL')


#####



# with all points
lm_out_H = lm(log(Cum_hospitalization) ~ Date, CoronaData)
GR_H = exp(lm_out_H$coefficients[2])
C0_H = exp(lm_out_H$coefficients[1])
# with selection of points
lm_out_H2 = lm(log(Cum_hospitalization) ~ Date, 
    CoronaData[CoronaData$Date>10&CoronaData$Date<25,])
GR_H2 = exp(lm_out_H2$coefficients[2])
C0_H2 = exp(lm_out_H2$coefficients[1])
# linear fit
lm_out_Hl = lm(Cum_hospitalization ~ Date, 
    CoronaData[CoronaData$Date>25&CoronaData$Date<35,])
GR_Hl = lm_out_Hl$coefficients[2]
C0_Hl = lm_out_Hl$coefficients[1]

ggplot(CoronaData, aes(x=Date, y=Cum_hospitalization))+
    geom_smooth(method='lm', formula= y~x,color='blue')+
    geom_smooth(color='red')+
    geom_point()+
    #stat_function(fun = function(x) {log10(C0*GR^x)},colour = "grey") +
        # scale_y_log10 goes wrong here..?! --> stat_function not affected; log10 fixes it
    scale_y_log10()+
    theme_bw()+
    ggtitle('Corona Related Hospitalizations in NL')

# linear scale
ggplot(CoronaData, aes(x=Date, y=Cum_hospitalization))+
    geom_vline(xintercept = 25,color='gray')+
    geom_smooth(color='red')+
    stat_function(fun = function(x) {C0_H*GR_H^x},colour = "grey") +
    stat_function(fun = function(x) {C0_H2*GR_H2^x},colour = "black") +
    stat_function(xlim=c(20,38),fun = function(x) {C0_Hl+GR_Hl*x},colour = "black") +
    geom_point()+
        #scale_y_log10 goes wrong here..?! --> stat_function not affected; log10 fixes it
    #scale_y_log10()+
    theme_bw()+
    ggtitle('Corona Related Hospitalizations in NL')+
    give_better_textsize_plot(15)+ylim(c(0,1e4))
    

# Hospitalization per day
# with all points
lm_out_Hd = lm(log(Nr_hospitalizations) ~ Date, CoronaData)
GR_Hd = exp(lm_out_Hd$coefficients[2])
C0_Hd = exp(lm_out_Hd$coefficients[1])

ggplot(CoronaData, aes(x=Date, y=Nr_hospitalizations))+
    geom_point()+
    geom_smooth(method='lm', formula= y~x)+
    #stat_function(fun = function(x) {log10(C0*GR^x)},colour = "grey") +
        # scale_y_log10 goes wrong here..?! --> stat_function not affected; log10 fixes it
    scale_y_log10()+
    theme_bw()+
    ggtitle('Corona Related Hospitalizations in NL')

library(patchwork)
p1=ggplot(CoronaData, aes(x=Date, y=Nr_hospitalizations))+
    geom_bar(stat='identity')+theme_bw()+give_better_textsize_plot(12)
p2=ggplot(CoronaData, aes(x=Date, y=Nr_Deaths))+
    geom_bar(stat='identity')+theme_bw()+give_better_textsize_plot(12)
p1/p2



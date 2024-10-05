
library(ggplot2)
library(reshape)

data = read.csv('/Users/m.wehrens/Data/__misc_data/fun_other/covid_data_2021jan__ggd-meldingen-positief-geteste-personen-per-dag-vanaf-31-augustus-2020.csv', sep = ';')
View(data)

names(data)

data$nday = 1:dim(data)[1]

data_melted = reshape::melt(data, id.vars=c('nday','GGD.meldingsdatum'))

ggplot(data_melted)+
    geom_bar(aes(x=nday, y=value, fill=variable),stat='identity')

# now see if there's a weekly pattern
# First add days of week
data$weekday = rep(1:7,length.out=dim(data)[1]) # 31st august 2020 is 1st day, and also a Monday
data$weekday = factor(data$weekday, levels=1:7)
# Calculate total
data$total = data$nieuw+data$t.m.afgelopen.week
# Then calculate relative amounts compared surrounding week
data$total_relative = c(NA,NA,NA, 
                        sapply(4:(dim(data)[1]-3), function(idx) {data$total[idx]/mean(data$total[(idx-3):(idx+3)])}), 
                        NA, NA, NA)


ggplot(data)+
    geom_boxplot(aes(x=weekday, y=total_relative))+
    geom_point(aes(x=weekday, y=total_relative, color=nday))+
    #geom_jitter(aes(x=weekday, y=total_relative))+
    theme_bw()

# using only last 5 weeks
nrows = nrow(data)
sel_rows = ((nrows-(5*7-1)):nrows)-3
ggplot(data[sel_rows,])+
    #geom_boxplot(aes(x=weekday, y=total_relative))+
    geom_point(aes(x=weekday, y=total_relative, color=nday))+
    #geom_jitter(aes(x=weekday, y=total_relative))+
    theme_bw()

# Calculating means
means_df = aggregate(data$total_relative, by=list(data$weekday), mean, na.rm=T)
names(means_df) = c('weekday', 'relative_total_mean')
# using only last 5 weeks
means_df_last5 = aggregate(data[sel_rows,]$total_relative, by=list(data[sel_rows,]$weekday), mean, na.rm=T)
names(means_df_last5) = c('weekday', 'relative_total_mean')


ggplot(means_df)+
    geom_line(aes(x=as.numeric(weekday), y=relative_total_mean))+
    geom_line(data=means_df_last5, aes(x=as.numeric(weekday), y=relative_total_mean), color='red')+theme_bw()+
    xlab('Dag v.d. week')+ylab('Relatief aantal besmettingen ivm omliggende dagen')

data$correction = rep(1/means_df_last5$relative_total_mean, length.out=nrows)

data$total.corrected = data$total/data$correction

ggplot(data)+
    geom_bar(aes(x=nday, y=total.corrected),stat='identity')+theme_bw()

names(data)
ggplot(data)+
    geom_point(aes(x=weekday, y=total_relative),stat='identity')+
    geom_point(aes(x=weekday, y=correction),stat='identity',color='red')+theme_bw()

ggplot(data)+
    geom_point(aes(x=weekday, y=total_relative*correction))+theme_bw()


ggplot(data)+
    geom_line(aes(x=nday, y=total))+
    geom_line(aes(x=nday, y=total*correction),color='blue')+
    geom_line(aes(x=nday, y=correction*5000),color='red')+theme_bw()

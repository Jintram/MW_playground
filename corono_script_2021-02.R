

# Bit overkill but load my standard libraries
PATH_TO_DIRECTORY_WITH_SCRIPTS = '/Users/m.wehrens/Documents/git_repos/SCS_RaceID3/'
source('/Users/m.wehrens/Documents/git_repos/SCS_RaceID3/Functions/MW_load_libraries.R')


CovidData202101 = read.table('/Users/m.wehrens/Desktop/desktop_misc/covid202101__ggd-meldingen-positief-geteste-personen-per-dag--vanaf-27-februari-2020.csv',sep=';', header = 1)
# first day is a thursday so day 4
CovidData202101$weekday = c(4,5,6,7,rep(c(1,2,3,4,5,6,7), length.out=nrow(CovidData202101)-4))
# first week is week 9
CovidData202101$week = cumsum(CovidData202101$weekday==7)+9
# total cases
CovidData202101$total_cases = CovidData202101$t.m.afgelopen.week+CovidData202101$nieuw

# positive tests per week
positive_tests_per_week = aggregate(CovidData202101$total_cases, by=list(CovidData202101$week ), FUN=sum)
weekdays_reported = aggregate(rep(1,length(CovidData202101$total_cases)), by=list(CovidData202101$week ), FUN=sum)$x
names(positive_tests_per_week) = c('week','cases')
positive_tests_per_week$days_reported = weekdays_reported

# now look at british variety
# 1,1% in week 51
# 19,8% in week 2
CovidData202101_British = data.frame(week=c(51,52+2), fraction=c(0.011,0.198))
CovidData202101_British$cases_total = positive_tests_per_week[positive_tests_per_week$week %in% CovidData202101_British$week,]$cases *
                                        CovidData202101_British$fraction
# And according to NOS
CovidData202101_British_NOS = data.frame(week=c(52+4), fraction=c(0.5)) # 2-feb: "66% is British"
CovidData202101_British_NOS$cases_total = positive_tests_per_week[positive_tests_per_week$week %in% CovidData202101_British_NOS$week,]$cases *
                                        CovidData202101_British_NOS$fraction

# show curve
library(ggplot2)
ggplot(positive_tests_per_week[positive_tests_per_week$days_reported==7,])+
    geom_line(aes(x=week,y=cases),color='gray')+theme_bw()+
    geom_point(data = CovidData202101_British, aes(x=week,y=cases_total), color='red')+
    geom_point(data = CovidData202101_British_NOS, aes(x=week,y=cases_total), color='red', shape=15)+
    geom_line(data = rbind(CovidData202101_British,CovidData202101_British_NOS),aes(x=week,y=cases_total), size=.5, color='red') +
    geom_smooth(data = CovidData202101_British,method="lm", aes(x=week,y=cases_total), 
        formula= (y ~ exp(x)), se=FALSE, fullrange=T, size=.5, color='red', linetype = "dashed") +
    geom_smooth(data = rbind(CovidData202101_British,CovidData202101_British_NOS),method="lm", aes(x=week,y=cases_total), 
        formula= (y ~ exp(x)), se=FALSE, fullrange=T, size=.5, color='red', linetype = "dashed") +
    #geom_point(data = rbind(CovidData202101_British,CovidData202101_British_NOS),aes(x=week,y=cases_total),color='black')+
    ylim(c(0,1e5))+xlab('Week')+ylab('# Positieve testen')+give_better_textsize_plot(15)+xlim(c(40,60))

###

# log scale (note: geom_smooth doesn't handle this well, need edit)
ggplot(positive_tests_per_week[positive_tests_per_week$days_reported==7,])+
    geom_line(aes(x=week,y=cases))+theme_bw()+
    geom_point(data = CovidData202101_British, aes(x=week,y=cases_total), color='red')+
    geom_point(data = CovidData202101_British_NOS, aes(x=week,y=cases_total), color='purple')+
    geom_smooth(data = CovidData202101_British,method="lm", aes(x=week,y=cases_total), 
        formula= (y ~ x), se=FALSE, fullrange=T, size=.5, color='red') +
    geom_smooth(data = rbind(CovidData202101_British,CovidData202101_British_NOS),method="lm", aes(x=week,y=cases_total), 
        formula= (y ~ x), se=FALSE, fullrange=T, size=.5, color='purple') +
    #geom_point(data = rbind(CovidData202101_British,CovidData202101_British_NOS),aes(x=week,y=cases_total),color='black')+
    xlab('Week')+ylab('# Positieve testen')+give_better_textsize_plot(15)+
    scale_y_continuous(trans = 'log10', limits = c(1,1e5))
    





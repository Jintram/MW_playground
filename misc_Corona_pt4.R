
# description of data can be found here:
# https://acceptatie.data.rivm.nl/meta/srv/api/records/45f911c4-3a62-42f1-b594-524a75db2c94
#
# Excerpt
# "IC_admission_notification: Het aantal nieuwe, bij de NICE registratie gemelde, COVID-19 patiënten dat op de IC is opgenomen per datum waarop de IC opname is gemeld [Date_of_statistics].
# IC_admission: Het aantal nieuwe, bij de NICE registratie gemelde, COVID-19 patiënten dat op de IC is opgenomen per IC opnamedatum [Date_of_statistics]."
# So difference is time of event vs. time of reporting of event.

# Data can be downloaded here:
# https://data.rivm.nl/covid-19/

# Additional information (presentation v. dissel)
# https://www.tweedekamer.nl/kamerleden-en-commissies/commissies/volksgezondheid-welzijn-en-sport/thema-coronavirus

# Hospital lenght of stay at ICU, first google hit article, estimated 12.9 days - 18.9 days
# https://bmcinfectdis.biomedcentral.com/articles/10.1186/s12879-021-06371-6



################################################################################

library(ggplot2)
library(reshape2)
library(plyr)

################################################################################

source('/Users/m.wehrens/Documents/git_repos/Convenient_functions/collection_convenient_functions.R')

################################################################################

data_test = read.csv('/Users/m.wehrens/Data/__misc_data/fun_other/2021-11-18__COVID-19_aantallen_gemeente_per_dag.csv', sep = ';')
data_IC = read.csv('/Users/m.wehrens/Data/__misc_data/fun_other/2021-11-18__COVID-19_ic_opnames.csv', sep = ';')

# Pre-processing data
ref_day = as.Date(data_test$Date_of_publication[1])

data_test$Date_of_publication = as.Date(data_test$Date_of_publication)
data_test$days_since_start = 
    as.numeric(data_test$Date_of_publication - ref_day) # assuming sorted

data_IC$Date_of_statistics = as.Date(data_IC$Date_of_statistics)
data_IC$days_since_start = 
    as.numeric(data_IC$Date_of_statistics - ref_day) # assuming sorted

rownames(data_IC) = data_IC$days_since_start

#ggplot(data_test, aes(x=Date_of_publication, y=Total_reported, fill=Municipality_code))+
#    geom_bar(stat='identity')+theme(legend.position = 'none')

totals_per_day = 
    aggregate(x = list(total=data_test$Total_reported), by = list(days_since_start=data_test$days_since_start), FUN = sum)

# Combined data frame
df_both = join(totals_per_day, data_IC, by='days_since_start')

# Some normalization / smoothing
df_both$IC_admission_norm=df_both$IC_admission/sum(df_both$IC_admission, na.rm=T)
df_both$total_norm=df_both$total/sum(df_both$total)

df_both$IC_admission_norm_smooth  = shorthand_smooth(Y = df_both$IC_admission_norm)
df_both$total_norm_smooth         = shorthand_smooth(Y = df_both$total_norm)

df_both$IC_admission_smooth  = shorthand_smooth(Y = df_both$IC_admission)
df_both$total_smooth         = shorthand_smooth(Y = df_both$total)

# cross corr, ignoring 1st 150 days
cross_corr = shorthand_crosscorr(X=df_both$total[df_both$days_since_start>150], Y=df_both$IC_admission[df_both$days_since_start>150])
# cross corr, ignoring 1st 475 days
cross_corr = shorthand_crosscorr(X=df_both$total[df_both$days_since_start>450], Y=df_both$IC_admission[df_both$days_since_start>450])


# Tests
ggplot(df_both, aes(x=days_since_start, y=total_smooth))+
    geom_line()+theme_bw()
# IC
ggplot(df_both, aes(x=days_since_start, y=IC_admission_smooth))+
    geom_line()+theme_bw()

# show corr
ggplot(as.data.frame(cross_corr))+
    geom_line(aes(x=tau, y=corr))+theme_bw()
ggplot(as.data.frame(cross_corr))+
    geom_line(aes(x=tau, y=corr))+theme_bw()+xlim(c(-50,50))
ggplot(as.data.frame(cross_corr))+
    geom_line(aes(x=tau, y=corr))+theme_bw()+xlim(c(-25,25))

# calculate time shift
cross_corr$tau[which(cross_corr$corr==max(cross_corr$corr[cross_corr$tau> -25&cross_corr$tau< 25]))]
time_shift = cross_corr$tau[which(cross_corr$corr==max(cross_corr$corr[cross_corr$tau> -25&cross_corr$tau< 25]))]
# manual time shift
# time_shift = 14

# Create melted frame
df_both_melted = reshape2::melt(df_both, measure.vars=c('total_norm_smooth','IC_admission_norm_smooth'))#,'IC_admission_notification'))
df_both_melted$days_since_start_timeshift = df_both_melted$days_since_start+time_shift
df_both$days_since_start_timeshift = df_both$days_since_start-time_shift
df_both$days_since_start_timeshift14 = df_both$days_since_start-14

# Plotting smoothed both, linear scale
ggplot(df_both_melted)+
    geom_line(aes(x=days_since_start, y=value, color=variable))+theme_bw()
# Plotting smoothed both, log10 scale
ggplot(df_both_melted)+
    geom_line(aes(x=days_since_start, y=value, color=variable))+theme_bw()+
    scale_y_log10()
# Selection for only last days
ggplot(df_both_melted)+
    geom_line(aes(x=days_since_start, y=value, color=variable))+theme_bw()+
    scale_y_log10()+xlim(c(475,600))

# Conversion rate of IC beds to positive test
N=length(df_both$IC_admission_smooth)
df_both$conversion = rep(NA, N) 
df_both$conversion[(1):(N-time_shift)] = df_both$IC_admission_smooth[(1+time_shift):(N)]/df_both$total_smooth[(1):(N-time_shift)]
# For scatter later, also create matching x,y params
df_both$IC_admission_smooth_shifted = c(df_both$IC_admission_smooth[(1+time_shift):(N)] , rep(NA, time_shift))
df_both$total_smooth_shift_ref      = c(df_both$total_smooth[(1):(N-time_shift)], rep(NA, time_shift))
ggplot(df_both[df_both$days_since_start>150,])+
    geom_line(aes(x=days_since_start, y=conversion))+theme_bw()
ggplot(df_both[df_both$days_since_start>500,])+
    geom_line(aes(x=days_since_start, y=conversion))+theme_bw()

# Day vaxx campaign started
day_vaxx_start = df_both$days_since_start[(df_both$Date_of_statistics=='2021-01-10')&(!is.na(df_both$Date_of_statistics))]

# Timeshift adjusted plot
ggplot(df_both)+
    geom_line(aes(x=days_since_start, y=total_norm_smooth), color='black')+
    geom_line(aes(x=days_since_start_timeshift, y=IC_admission_norm_smooth), color='blue')+
    theme_bw()+xlab('Days since measurements started')+ylab('Total infections measured / Total IC admissions (normalized)')+
    give_better_textsize_plot_shorthand(12)+
    geom_vline(xintercept = day_vaxx_start)+theme(legend.position = 'inside')#+scale_y_log10()

# Questions remaining:
# at max IC capacity, what will be # positive people?
# though this of course also depends on how many days people are at IC ...

# Quick calculation
factor = 1/mean(df_both[df_both$days_since_start>525,]$conversion, na.rm=T)
factor_dissel = 400
time_of_stay = 14 # hand waving assumption
max_capacity = 1000
# people allowed infected per day, given max IC capacity
allowed_infected_per_day = max_capacity/time_of_stay*factor
allowed_infected_per_day

allowed_infected_per_day_dissel = max_capacity/time_of_stay*factor_dissel

# Showing in earlier plot
ggplot(df_both)+
    geom_line(aes(x=days_since_start, y=total_smooth), color='black')+
    geom_line(aes(x=days_since_start_timeshift, y=IC_admission_smooth*factor), color='red')+
    geom_hline(yintercept = allowed_infected_per_day)+
    geom_vline(xintercept = day_vaxx_start)+
    theme_bw()+xlab('Days since measurement started')+ylab('Number of detected infections')+
    give_better_textsize_plot_shorthand(12)
#+scale_y_log10()

# Edited version
# Showing in earlier plot
p1=ggplot(df_both)+
    geom_line(aes(x=days_since_start, y=IC_admission_smooth*factor), color='grey')+#, linetype='dotted')+
    geom_line(aes(x=days_since_start, y=total_smooth), color='black')+
    geom_hline(yintercept = allowed_infected_per_day)+
    geom_vline(xintercept = day_vaxx_start)+
    theme_bw()+ylab('Detected infections')+# xlab(element_blank())+#
    xlab('Days since measurement started')+
    give_better_textsize_plot_shorthand(10)
ggsave(plot = p1,filename = paste0('/Users/m.wehrens/Data/__misc_data/fun_other/','tests_IC_plot.pdf'), width = 10, height = 5, units='cm', device = cairo_pdf)
p2=ggplot(df_both)+
    geom_line(aes(x=days_since_start, y=IC_admission_smooth), color='blue')+#, linetype='dotted')+
    geom_vline(xintercept = day_vaxx_start)+
    theme_bw()+xlab('Days since measurement started')+ylab('IC admissions')+
    give_better_textsize_plot_shorthand(10)
p2
ggsave(plot = p2,filename = paste0('/Users/m.wehrens/Data/__misc_data/fun_other/','IC_plot.pdf'), width = 10, height = 3, units='cm', device = cairo_pdf)
p=p1+p2+plot_layout(nrow=2, heights = c(.66,.33))
ggsave(plot = p,filename = paste0('/Users/m.wehrens/Data/__misc_data/fun_other/','tests_IC_plot_2panels.pdf'), width = 10, height = 8, units='cm', device = cairo_pdf)

#+scale_y_log10()

# Estimated days at max capacity to have 20% unvaccinated all infected
end_of_problem = (17e6 * .2) / allowed_infected_per_day
end_of_problem
    # note total population might overestimate those who will get tested
    # but people that get tested is probably ±80% of population
    # other caveat is of course that unvaccinated people won't get tested, so the actual number of infected might be much higher
    #   ^ although, my estimate does seem to work also for before vaccinations got going
    #     does that mean we can really assume this is currently pandemic under unvaxxed?

# How many people have been infected based on IC admissions
sum(df_both$IC_admission, na.rm=T)*factor_dissel

##########
# Relation IC-positive tests
# Scatter

# First plot with both timelines
ggplot(df_both)+
    geom_line(aes(x=days_since_start_timeshift, y=IC_admission_smooth*factor), color='grey')+#, linetype='dotted')+
    geom_line(aes(x=days_since_start, y=IC_admission_smooth_shifted*factor), color='black', linetype='dotted')+
    geom_line(aes(x=days_since_start, y=total_smooth), color='black')+
    geom_hline(yintercept = allowed_infected_per_day)+
    geom_vline(xintercept = day_vaxx_start)+
    theme_bw()+ylab('Detected infections')+# xlab(element_blank())+#
    xlab('Days since measurement started')+
    give_better_textsize_plot_shorthand(10)
    
p.a=ggplot(df_both[df_both$days_since_start>450,])+
    #geom_point(aes(x=days_since_start_timeshift, y=IC_admission_smooth*factor), color='grey')+#, linetype='dotted')+
    #geom_point(aes(x=days_since_start, y=IC_admission_smooth_shifted*factor), color='black', linetype='dotted')+
    geom_point(aes(x=days_since_start, y=total_smooth, color=days_since_start))+
    geom_hline(yintercept = allowed_infected_per_day)+
    geom_vline(xintercept = day_vaxx_start)+
    theme_bw()+ylab('Detected infections')+# xlab(element_blank())+#
    xlab('Days since measurement started')+
    give_better_textsize_plot_shorthand(10)+
    scale_color_gradientn(colours = rainbow(5))
p.a

# Now scatter
ggplot(df_both)+
    geom_point(aes(x=IC_admission_smooth_shifted, y=total_smooth, color=days_since_start))+
    geom_abline(slope = factor, intercept = 0)+
    geom_abline(slope = factor_dissel, intercept = 0, linetype='dotted')+
    theme_bw()+
    scale_color_gradientn(colours = rainbow(5))

p.b = ggplot(df_both[df_both$days_since_start>450,])+
    geom_point(aes(x=IC_admission_smooth_shifted, y=total_smooth, color=days_since_start))+
    geom_abline(slope = factor, intercept = 0)+
    geom_abline(slope = factor_dissel, intercept = 0, linetype='dotted')+
    theme_bw()+
    scale_color_gradientn(colours = rainbow(5))
p.b

p.c = ggplot(df_both[df_both$days_since_start>450,])+
    geom_point(aes(x=IC_admission_smooth_shifted*14, y=total_smooth, color=days_since_start))+
    geom_abline(slope = factor/14, intercept = 0)+
    geom_abline(slope = factor_dissel/14, intercept = 0, linetype='dotted')+
    theme_bw()+
    scale_color_gradientn(colours = rainbow(5))+
    xlim(c(0,2000))+ylim(c(0,25e3))
p.c

library(patchwork)
p.a+theme(legend.position = 'none')+p.c+theme(legend.position = 'none')




sales_data =
    openxlsx::read.xlsx('/Users/m.wehrens/Documents/Personal/kopen_huis/kadaster_berkenhof.xlsx')

sales_data$date = as.Date(paste0(sales_data$year, '-', sales_data$month, '-', sales_data$day))

    #as.Date("2017-06-14")
    #sales_data$days_since

ggplot(sales_data, aes(x=date, y=price))+
    geom_point()+xlim(as.Date(c('2018-01-01','2022-12-31')))+theme_bw()+ylim(c(0,300))+ylab('Sold for (k€)')+ggtitle('Berkenhof chalets')


library(plyr)

filepath='/Users/m.wehrens/Documents/Personal/auto/cardata.txt'

con = file(filepath, "r")

car_db = list()
counter = 0

while ( TRUE ) {
    line = readLines(con, n = 1)
    
     if ( length(line) == 0 ) {
      break
    }
    
    grep_out = grep(pattern = "^\\{\"id\":", x = line)
    if (length(grep_out)>0) {
        if (grep_out) {
            print(c('Next car identified at: ', line))
            
            counter = counter + 1
            next
        }
    }
    
    splitted = strsplit(x = line, split = ':')[[1]]
    name=gsub('\"|,', '', splitted[1])
    value=gsub('\"|,', '', splitted[2])
    
    car_db[[paste0('car',counter)]][[name]] = value
    
    #print(line)
}

close(con)

View(car_db)

Reduce(rbind, car_db)


lapply(car_db, length)
lapply(car_db, names)

df_list = lapply(car_db, as.data.frame)

car_db_df = Reduce(rbind,df_list)

car_db_df = as.data.frame(df_list[[1]])
for (idx in 2:length(df_list)) {
    car_db_df = rbind.fill(car_db_df, df_list[[idx]])
}

View(car_db_df)

View(car_db_df[car_db_df$model=='Auris',])


################################################################################

library(ggplot2)
library(openxlsx)
library(ggrepel)
library(RColorBrewer)
rainbow_colors = rev( brewer.pal(n = 11, name = "Spectral") )

cardata_manual_all = read.xlsx('/Users/m.wehrens/Documents/Personal/auto/cardata_manual3.xlsx')
cardata_manual_all$age = cardata_manual_all$ref_year-cardata_manual_all$year

cardata_manual = cardata_manual_all[cardata_manual_all$model=='auris',]

View(cardata_manual)

cardata_manual_avg_price_age = aggregate(list(price=cardata_manual$price), by = list(age=cardata_manual$age), FUN=mean)

lm(formula = price ~ age, cardata_manual)


ggplot(cardata_manual, aes(x=age,y=KM,size=price, color=price))+
    geom_smooth(method='lm', formula= y~x, color='black',se = F)+
    geom_point()+
    geom_point(data=cardata_manual[cardata_manual$special==1,,drop=F], size=2, color='black', shape=3)+
    geom_text_repel(aes(label=price), color='grey', size=5)+
    theme_bw()+
    scale_color_gradientn(colours=rainbow_colors)+
    ggtitle('Axis positive=positive')#, limits=c(0,expr_limits[2]), oob=squish)

ggplot(cardata_manual, aes(x=-age,y=-KM,size=price, color=price))+
    geom_point()+
    geom_point(data=cardata_manual[cardata_manual$special==1,,drop=F], size=2, color='black', shape=3)+
    geom_text_repel(aes(label=price), color='grey', size=5)+
    theme_bw()+
    scale_color_gradientn(colours=rainbow_colors) #, limits=c(0,expr_limits[2]), oob=squish)

ggplot(cardata_manual, aes(x=-KM,y=price,size=-age, color=-age))+
    geom_smooth(method='lm', formula= y~x, color='black',se = F)+
    geom_point()+theme_bw()+
    geom_point(data=cardata_manual[cardata_manual$special==1,,drop=F], size=2, color='black', shape=3)+
    geom_text_repel(aes(label=price), color='grey', size=5)+
    scale_color_gradientn(colours=rainbow_colors) #, limits=c(0,expr_limits[2]), oob=squish)

ggplot(cardata_manual, aes(x=-age,y=price,size=-KM, color=-KM))+
    geom_smooth(method='lm', formula= y~x, color='black',se = F)+
    geom_point()+theme_bw()+
    geom_point(data=cardata_manual[cardata_manual$special==1,,drop=F], size=2, color='black', shape=3)+
    geom_point(data=cardata_manual_avg_price_age, size=4, color='black')+
    geom_text_repel(aes(label=price), color='grey', size=5)+
    scale_color_gradientn(colours=rainbow_colors) #, limits=c(0,expr_limits[2]), oob=squish)


ggplot(cardata_manual_all, aes(x=-age,y=price, color=model))+
    geom_smooth(data=cardata_manual,method='lm', formula= y~x, color='black',se = F)+
    geom_point()+theme_bw()+geom_vline(xintercept = -1)
    #geom_point(data=cardata_manual_avg_price_age, size=4, color='black')+
    #geom_text_repel(aes(label=price), color='grey', size=5)+
    #scale_color_gradientn(colours=rainbow_colors) #, limits=c(0,expr_limits[2]), oob=squish)

ggplot(cardata_manual_all, aes(x=-KM,y=price, color=model))+
    geom_smooth(data=cardata_manual, method='lm', formula= y~x, color='black',se = F)+
    geom_point()+theme_bw()+geom_vline(xintercept = -27)






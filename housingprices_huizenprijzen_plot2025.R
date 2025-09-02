
# Script to assess housing prices in specific areas, based on "kadaster" data
# https://www.kadaster.nl/winkel (data on sales)
# https://www.kadaster.nl/zakelijk/vastgoedinformatie/vastgoedcijfers/vastgoeddashboard/prijsindex (price index)

################################################################################

library(openxlsx)
library(ggplot2)
library(tidyverse)

# Set and create outputfolder
outputfolder <- '/Users/m.wehrens/Documents/Personal/huis/huizenprijzen/figures/'
if (!dir.exists(outputfolder)) { dir.create(outputfolder, recursive = TRUE) }

################################################################################

# a function to set all ggplot font sizes to 8 and font face to arial, 
correct_font_sizes_MW <- function(mysize=8, myface="Arial") {
        theme(
            text = element_text(size = mysize, family = myface),
            axis.text = element_text(size = mysize, family = myface),
            axis.title = element_text(size = mysize, family = myface),
            legend.text = element_text(size = mysize, family = myface),
            legend.title = element_text(size = mysize, family = myface)
        )
}
################################################################################

# prijsontwikkeling
# bron: 
# https://www.kadaster.nl/zakelijk/vastgoedinformatie/vastgoedcijfers/vastgoeddashboard/prijsindex
data_prijsontwikkeling <- openxlsx::read.xlsx('/Users/m.wehrens/Documents/Personal/huis/huizenprijzen/kadaster_downloads/vgd_Prijsindex_Prijsindex_maand_T_10.xlsx',
                                              sheet = 1, startRow = 7)

data_prijsontwikkeling <- data_prijsontwikkeling %>% 
    # separate YYYYMM into YYYY and MM columns
    separate('Periode.(maand)', into = c("year", "month"), sep = 4, remove=F) %>% 
    # add YYYY-MM-DD date column
    mutate(date = as.Date(paste(year, month, "15", sep = "-")))

ggplot(data_prijsontwikkeling) +
    geom_line(aes(x = date, y = Prijsindex), color='blue') +
    geom_point(aes(x = date, y = Prijsindex), color='blue') +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
    xlab('Datum') +
    ylab('Prijsindex (2015=100)') +
    ggtitle('Prijsindex huizen (bron: Kadaster)')


# Downloaded data
data_huizen <- openxlsx::read.xlsx('/Users/m.wehrens/Documents/Personal/huis/huizenprijzen/huizenprijzen_2025.xlsx')

data_huizen <- data_huizen %>% 
    mutate(data_conv = as.Date(datum, origin = "1899-12-30")) %>% 
    separate(data_conv, into = c("year", "month", "day"), sep = "-", remove=F) 
View(data_huizen)

# now let's interpolate the data from data_prijsontwikkeling onto timepoints of data_huizen
data_huizen <- data_huizen %>% 
    # filter(!is.na(m2.prijs)) %>% 
    mutate(prijsindex = approx(x = data_prijsontwikkeling$date, 
                           y = data_prijsontwikkeling$Prijsindex, 
                           xout = data_conv)$y)

# calculate linear fit line to downloaded data, for data after 2013
lm_fit <- lm(m2.prijs ~ data_conv, data = data_huizen %>% filter(year>2013))
# now get a+b*x, a and b values
fit_offset <- coef(lm_fit)[1]
fit_coeff  <- coef(lm_fit)[2]

# now fit the prijsindex to the m2.prijs data
lm_fit_prijsidx <- lm(m2.prijs ~ prijsindex, data = data_huizen %>% filter(year>2013))
fit_offset_prijsidx <- coef(lm_fit_prijsidx)[1]
fit_coeff_prijsidx  <- coef(lm_fit_prijsidx)[2]

# now let's fit data_prijsontwikkeling$Prijsindex with data_huizen$m2.prijs
# to be able to project it onto the plot


p <- ggplot(data_huizen %>% filter(year>2013)) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    # set minor y grid lines at 100
    scale_y_continuous(breaks = seq(2000, 6000, by = 500), 
                       minor_breaks = seq(0, 6000, by = 100), 
                       limits = c(1500,5500)) +
    ### actual data
    # add current date as vertical line
    geom_vline(xintercept=as.numeric(Sys.Date()), linetype="solid", color = "#e69f00", linewidth=1.5) +
    # add the trend line
    geom_abline(aes(color='Lineare fit', slope = fit_coeff, intercept = fit_offset), linetype = "solid", linewidth=1.5)+ # color = 'blue', 
    # add price index data 
    geom_line(data = data_prijsontwikkeling, aes(x = date, y = fit_offset_prijsidx + fit_coeff_prijsidx * Prijsindex, 
                                                 color='Kadaster Prijsindex'), linewidth=1.5) + # , color='red'
    # data points
    geom_point(aes(x = data_conv, y =  m2.prijs, color='Verkoop (kadaster)'), size=2) +
    ###
    scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months", limits=as.Date(c("2015-01-01", "2025-12-31"))) +
    xlab('Datum')+ylab('Prijs per m2 (euro)')+
    # put legend inside and left top
    theme(legend.position = c(0.15, 0.85),
          legend.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey30", size = 0.5),
          panel.grid.minor = element_line(color = "grey70", size = 0.25))+
    labs(color = "Data")+ 
    scale_color_manual(values = c('Verkoop (kadaster)' = '#000000', 'Lineare fit' = '#0072b2', 'Kadaster Prijsindex' = '#009e73')) + # #e69f00
    correct_font_sizes_MW(mysize=8, myface="Arial") 
      
print(p)  
# save to pdf
ggsave(paste0(outputfolder, "huizenprijzen_bilt.pdf"), 
       plot = p, width = 15, height = 10, units = "cm", dpi = 300, device = cairo_pdf)

    
    
toString(unique(data_huizen$postcode))
    
lapply(data_huizen$datum, convert_excel_date)

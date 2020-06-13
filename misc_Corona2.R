


patienten_ziekenhuis=read.csv('/Users/m.wehrens/Desktop/desktop_misc/corono_ziekenhuis_In het ziekenhuis opgenomen COVID-19 patienten.csv', sep=';')

gemeentes_65plus=read.csv('/Users/m.wehrens/Desktop/desktop_misc/corono_65plus_klikfile65plus-v2.csv', sep=';')


rownames(patienten_ziekenhuis)=patienten_ziekenhuis$Category
rownames(gemeentes_65plus)=gemeentes_65plus$Gemeente
    
gemeentes_overlap = patienten_ziekenhuis$Category[patienten_ziekenhuis$Category %in% gemeentes_65plus$Gemeente]

patienten_ziekenhuis_sync=patienten_ziekenhuis[gemeentes_overlap,]
gemeentes_65plus_sync= gemeentes_65plus[gemeentes_overlap,]

library(stringr)
gemeentes_65plus_sync$Percentage =
    str_replace(gemeentes_65plus_sync$Percentage, ',', '.')
gemeentes_65plus_sync$Percentage = 
    as.double(gemeentes_65plus_sync$Percentage)


library(ggplot2)
ggplot(data.frame(patienten = patienten_ziekenhuis_sync$Meldingen.per.100.000,
                    plus65 = gemeentes_65plus_sync$Percentage), 
    aes(x=plus65,y=patienten))+
    geom_point()+
    geom_smooth(method='lm')+
    xlab('Percentage 65% in gemeente')+ylab('Zkh opnames per 100K')+
    theme_minimal()




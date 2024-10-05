

library(openxlsx)
library(pheatmap)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)

standpunten = openxlsx::read.xlsx(xlsxFile = '/Users/m.wehrens/Desktop/partijen_standpunten.xlsx', rowNames = T)
uitslagen   = openxlsx::read.xlsx(xlsxFile = '/Users/m.wehrens/Desktop/verkiezingen_uitslag.xlsx', rowNames = T)
uitslagen_EK   = openxlsx::read.xlsx(xlsxFile = '/Users/m.wehrens/Desktop/verkiezingen_EK.xlsx', rowNames = T)

if (F) { 
    uitslagen$zetels=1
    uitslagen['BIJ1',]=150-sum(uitslagen$zetels)+1
}

gemiddelde_kiezer = (uitslagen$zetels %*% as.matrix(standpunten[rownames(uitslagen),]))/150

afstand_partij_gemiddelde_kiezer = 
    apply(standpunten, 1, function(X) {sqrt(sum((X-gemiddelde_kiezer)^2))})

View(afstand_partij_gemiddelde_kiezer)

pheatmap(standpunten)

pca_standpunten = prcomp(standpunten)

View(pca_standpunten$rotation)

gemiddelde_kiezer_pca = scale(gemiddelde_kiezer, pca_standpunten$center, pca_standpunten$scale) %*% pca_standpunten$rotation 

# sanity check (is equal)
gemiddelde_kiezer_pca_2 = c(sum(pca_standpunten$x[,1]*uitslagen[rownames(pca_standpunten$x),])/150,
                            sum(pca_standpunten$x[,2]*uitslagen[rownames(pca_standpunten$x),])/150)
# sanity check CoM (equal weight both sides voter)
sum(uitslagen[pca_standpunten$x[,1]>gemiddelde_kiezer_pca_2[1],])
sum(uitslagen[pca_standpunten$x[,1]<gemiddelde_kiezer_pca_2[1],])
#
sum(uitslagen[pca_standpunten$x[,2]>gemiddelde_kiezer_pca_2[2],])
sum(uitslagen[pca_standpunten$x[,2]<gemiddelde_kiezer_pca_2[2],])

sum(uitslagen[pca_standpunten$x[,1]>gemiddelde_kiezer_pca_2[1],])

plot_df = data.frame(pc1=pca_standpunten$x[,1],
                       pc2=pca_standpunten$x[,2],
                       pc3=pca_standpunten$x[,3], 
                       pc4=pca_standpunten$x[,4], 
                       naam=rownames(pca_standpunten$x),
                       uitslag=uitslagen[rownames(standpunten),])
plot_df_kiezer = data.frame(pc1=gemiddelde_kiezer_pca[1], pc2=gemiddelde_kiezer_pca[2], naam='KIEZER')

ggplot(data=plot_df, mapping=aes(x=pc1, y=pc2))+
    geom_point()+
    geom_point(data=plot_df_kiezer)+
    geom_label_repel(aes(label=naam))+
    geom_label_repel(data=plot_df_kiezer,aes(label=naam))+theme_bw()

ggplot(data=plot_df, mapping=aes(x=pc1, y=pc2))+
    geom_hline(yintercept = gemiddelde_kiezer_pca[2], color='grey')+
    geom_vline(xintercept = gemiddelde_kiezer_pca[1], color='grey')+
    #geom_hline(yintercept = gemiddelde_kiezer_pca_2[2], color='grey')+
    #geom_vline(xintercept = gemiddelde_kiezer_pca_2[1], color='grey')+
    geom_point(data=plot_df_kiezer,color='black', shape=18, size=6)+
    geom_point(aes(size=uitslag))+
    geom_label_repel(aes(label=naam))+
    geom_label_repel(data=plot_df_kiezer,aes(label=naam),color='red')+
    theme_void()+theme(legend.position = 'none')+
    scale_size_continuous(range = c(3, 15))

################################################################################

# What are the possible coalitions?
total_list = list()
party_count = c()
for (coalition_size in 1:dim(uitslagen)[1]) {
    
    current_combinations = combn(rownames(uitslagen), m=coalition_size)
    current_list = lapply(1:(dim(current_combinations)[2]),function(X){ current_combinations[,X] })
    names(current_list) = sapply(current_list,paste, collapse='_')
    total_list = append(total_list, current_list)
    party_count = c(party_count, rep(coalition_size, length(current_list)))
    
}
names(party_count) = names(total_list)

# Now calculate number of seats coalitions would have
seats_coalitions_TK = sapply(total_list, function(X) {sum(uitslagen[X,])})
seats_coalitions_EK = sapply(total_list, function(X) {sum(uitslagen_EK[X,])})

seat_summary_df = data.frame(coalitie_naam = sapply(total_list,paste, collapse='_'), TK_zetels=seats_coalitions_TK, EK_zetels=seats_coalitions_EK, aantal_partijen = party_count)


View(seat_summary_df[seat_summary_df$aantal_partijen<7 & seat_summary_df$TK_zetels>75 & seat_summary_df$EK_zetels>37,])

coalition_selection_df = seat_summary_df[seat_summary_df$aantal_partijen<8 & seat_summary_df$TK_zetels>75 & seat_summary_df$EK_zetels>37,] 
coalition_selection_partynames = total_list[rownames(coalition_selection_df)]


# sanity check
coalition_selection_df$coalitie_naam == sapply(coalition_selection_partynames,paste,collapse='_')

# now calculate weighed position on PC plot
shorthand_coalition_position = function(coalition_list) {
    coalition_position = 
        uitslagen[coalition_list,] %*% as.matrix(standpunten[coalition_list,]) / sum(uitslagen[coalition_list,]) 
        #uitslagen[coalition_list,] * as.matrix(standpunten[coalition_list,])
    
    coalition_position_PCA = 
        scale(coalition_position, pca_standpunten$center, pca_standpunten$scale) %*% pca_standpunten$rotation 
    
    return(coalition_position_PCA[1:2])
}

selection_coalition_positions = lapply(coalition_selection_partynames, shorthand_coalition_position)

final_coalition_df = 
    cbind(coalition_selection_df,
        data.frame(pc1=sapply(selection_coalition_positions, function(X){X[1]}), 
                   pc2=sapply(selection_coalition_positions, function(X){X[2]})))

final_coalition_df$excluded = grepl('PVV',final_coalition_df$coalitie_naam)|grepl('FVD',final_coalition_df$coalitie_naam)

# now overview plot
ggplot(data=plot_df, mapping=aes(x=pc1, y=pc2))+
    geom_point(aes(size=uitslag))+
    geom_point(data=plot_df_kiezer,color='black', shape=18, size=6)+
    geom_label_repel(aes(label=naam))+
    geom_label_repel(data=plot_df_kiezer,aes(label=naam),color='red')+
    #geom_label_repel(data=final_coalition_df,aes(label=coalitie_naam),color='red')+
    geom_point(data=final_coalition_df,color='blue',alpha=.5)+
    theme_void()+theme(legend.position = 'none')

# now calculate distances kiezers
selected_coalition_distances = apply(as.matrix(final_coalition_df[,c('pc1','pc2')]), 1, function(X) {sqrt(sum((X-gemiddelde_kiezer_pca[1:2])^2))})
final_coalition_df$distances = apply(as.matrix(final_coalition_df[,c('pc1','pc2')]), 1, function(X) {sqrt(sum((X-gemiddelde_kiezer_pca[1:2])^2))})

#debugging
#final_coalition_df$distances00 = apply(as.matrix(final_coalition_df[,c('pc1','pc2')]), 1, function(X) {sqrt(sum(X^2))})
#final_coalition_df$distancesXY = apply(as.matrix(final_coalition_df[,c('pc1','pc2')]), 1, function(X) {sqrt(sum((X-c(-1.25,-1))^2))})

top15_coalities =
    final_coalition_df[order(final_coalition_df$distances, decreasing = F),]$coalitie_naam[1:15]

selected_coalition_distances_uitsluitingen = selected_coalition_distances[!(grepl('PVV',names(selected_coalition_distances))|grepl('FVD',names(selected_coalition_distances)))]

excluded_coalitions = names(selected_coalition_distances)[(grepl('PVV',names(selected_coalition_distances))|grepl('FVD',names(selected_coalition_distances)))]

View(selected_coalition_distances_uitsluitingen)

party_count[names(selected_coalition_distances_uitsluitingen)]

selected_coalitions_summary_df = 
    data.frame( coalitie=names(selected_coalition_distances_uitsluitingen), 
                nr_partijen=as.double(party_count[names(selected_coalition_distances_uitsluitingen)]),
                afstand_kiezer = selected_coalition_distances_uitsluitingen,
                zetels = final_coalition_df[names(selected_coalition_distances_uitsluitingen),]$TK_zetels)

selected_coalitions_names = rownames(selected_coalitions_summary_df[(selected_coalitions_summary_df$nr_partijen==5&selected_coalitions_summary_df$afstand_kiezer<.9)|
                                                         (selected_coalitions_summary_df$nr_partijen==6&selected_coalitions_summary_df$afstand_kiezer<.7),])

ggplot(selected_coalitions_summary_df, mapping=(aes(x=zetels, y=afstand_kiezer, label=coalitie, color=nr_partijen)))+
    geom_point()+
    geom_label_repel(data=selected_coalitions_summary_df[selected_coalitions_summary_df$zetels<85&selected_coalitions_summary_df$afstand_kiezer<.6,], color='black')+
    scale_color_gradientn(colors=brewer.pal(8, 'Spectral'))+theme_bw()
    #scale_color_brewer(palette="Spectral")
    #geom_label_repel(data=selected_coalitions_summary_df[(selected_coalitions_summary_df$nr_partijen==5&selected_coalitions_summary_df$afstand_kiezer<.9)|
    #                                                    (selected_coalitions_summary_df$nr_partijen==6&selected_coalitions_summary_df$afstand_kiezer<.7),])

top5_5partijen =    
    rownames(selected_coalitions_summary_df[selected_coalitions_summary_df$nr_partijen==5,][order(selected_coalitions_summary_df[selected_coalitions_summary_df$nr_partijen==5,]$afstand_kiezer, decreasing = F),][1:5,])
top5_6partijen =    
    rownames(selected_coalitions_summary_df[selected_coalitions_summary_df$nr_partijen==6,][order(selected_coalitions_summary_df[selected_coalitions_summary_df$nr_partijen==6,]$afstand_kiezer, decreasing = F),][1:5,])
top5_7partijen =    
    rownames(selected_coalitions_summary_df[selected_coalitions_summary_df$nr_partijen==7,][order(selected_coalitions_summary_df[selected_coalitions_summary_df$nr_partijen==7,]$afstand_kiezer, decreasing = F),][1:5,])

# now overview plot with selected names, highlighted are top 10 coalitions (incl FVD and PVV)
ggplot(data=plot_df, mapping=aes(x=pc1, y=pc2))+
    geom_point(data=final_coalition_df,color='grey')+
    geom_point(data=final_coalition_df[c(top15_coalities[1:10]),],color='black')+
    geom_point(aes(size=uitslag))+
    geom_point(data=plot_df_kiezer,color='black', shape=18, size=6)+
    # partijnaam
    geom_label_repel(aes(label=naam))+
    geom_label_repel(data=final_coalition_df[c(top15_coalities[1:10]),],aes(label=coalitie_naam, color=aantal_partijen))+#,color='black',alpha=.5)+
    coord_fixed(ratio = 1)+
    theme_void()+theme(legend.position = 'none')

# now overview plot with selected names
ggplot(data=plot_df, mapping=aes(x=pc1, y=pc2))+
    geom_hline(yintercept = gemiddelde_kiezer_pca[2])+
    geom_vline(xintercept = gemiddelde_kiezer_pca[1])+
    geom_point(data=final_coalition_df,aes(color=aantal_partijen))+
    geom_point(data=final_coalition_df[c(top15_coalities[1:10]),],color='black')+
    geom_point(aes(size=uitslag))+
    geom_point(data=plot_df_kiezer,color='black', shape=18, size=6)+
    # partijnaam
    geom_label_repel(aes(label=naam))+
    geom_label_repel(data=final_coalition_df[c(top5_5partijen,top5_6partijen, top5_7partijen),],aes(label=coalitie_naam, color=aantal_partijen))+#,color='black',alpha=.5)+
    scale_color_gradientn(colors=brewer.pal(3, 'Dark2'))+
    coord_fixed(ratio = 1)+
    theme_void()#+theme(legend.position = 'none')

# with only 5 parties, excl FVD, PVV
p=ggplot(mapping=aes(x=pc1, y=pc2))+
    # axis
    #geom_hline(yintercept = 0)+
    #geom_vline(xintercept = 0)+
    # gem. kiezer
    geom_hline(yintercept = gemiddelde_kiezer_pca[2], color='black')+
    geom_vline(xintercept = gemiddelde_kiezer_pca[1], color='black')+
    # coalities
    geom_point(data=final_coalition_df[final_coalition_df$aantal_partijen==5,],aes(color=aantal_partijen))+
    geom_point(data=final_coalition_df[final_coalition_df$aantal_partijen==5&final_coalition_df$excluded==T,],shape=4,size=4)+
    # partijen + uitslag
    geom_point(data=plot_df,aes(size=uitslag))+
    # kiezer
    geom_point(data=plot_df_kiezer,color='black', shape=18, size=6)+
    # partijnaam
    geom_label_repel(data=plot_df, aes(label=naam))+
    # cosmetics
    scale_color_gradientn(colors=brewer.pal(3, 'Dark2'), limits=c(5,7))+
    coord_fixed(ratio = 1)+
    theme_void()+#+theme(legend.position = 'none')
    # labeling
    geom_point(data=final_coalition_df[c(top5_5partijen),],color='black')+
    geom_label_repel(data=final_coalition_df[c(top5_5partijen),],aes(label=coalitie_naam, color=aantal_partijen))+
    theme(legend.position = 'none')+
    ggtitle('Met 5 partijen')
p
ggsave(plot = p, filename = '/Users/m.wehrens/Desktop/coalities_5partijen.pdf', width = 20, height=15, units='cm')

# with only 6 parties, excl FVD, PVV
p=ggplot(mapping=aes(x=pc1, y=pc2))+
    # axis
    #geom_hline(yintercept = 0)+
    #geom_vline(xintercept = 0)+
    # gem. kiezer
    geom_hline(yintercept = gemiddelde_kiezer_pca[2], color='black')+
    geom_vline(xintercept = gemiddelde_kiezer_pca[1], color='black')+
    # coalities
    geom_point(data=final_coalition_df[final_coalition_df$aantal_partijen==6,],aes(color=aantal_partijen),alpha=.2)+
    geom_point(data=final_coalition_df[final_coalition_df$aantal_partijen==6&final_coalition_df$excluded==T,],shape=4,color='grey')+
    # partijen + uitslag
    geom_point(data=plot_df,aes(size=uitslag))+
    # kiezer
    geom_point(data=plot_df_kiezer,color='black', shape=18, size=6)+
    # partijnaam
    geom_label_repel(data=plot_df, aes(label=naam))+
    # cosmetics
    scale_color_gradientn(colors=brewer.pal(3, 'Dark2'), limits=c(5,7))+
    coord_fixed(ratio = 1)+
    theme_void()+#+theme(legend.position = 'none')
    # labeling
    geom_point(data=final_coalition_df[c(top5_6partijen),],color='black')+
    geom_label_repel(data=final_coalition_df[c(top5_6partijen),],aes(label=coalitie_naam, color=aantal_partijen))+
    theme(legend.position = 'none')+
    ggtitle('Met 6 partijen')+
    xlim(c(min(final_coalition_df[final_coalition_df$aantal_partijen==6,]$pc1), max(final_coalition_df[final_coalition_df$aantal_partijen==6,]$pc1)))+
    ylim(c(min(final_coalition_df[final_coalition_df$aantal_partijen==6,]$pc2), max(final_coalition_df[final_coalition_df$aantal_partijen==6,]$pc2)))
p
ggsave(plot = p, filename = '/Users/m.wehrens/Desktop/coalities_6partijen.pdf', width = 20, height=15, units='cm')


# with only 7 parties, excl FVD, PVV
p=ggplot(mapping=aes(x=pc1, y=pc2))+
    # axis
    #geom_hline(yintercept = 0)+
    #geom_vline(xintercept = 0)+
    # gem. kiezer
    geom_hline(yintercept = gemiddelde_kiezer_pca[2], color='black')+
    geom_vline(xintercept = gemiddelde_kiezer_pca[1], color='black')+
    # coalities
    geom_point(data=final_coalition_df[final_coalition_df$aantal_partijen==7,],aes(color=aantal_partijen),alpha=.2)+
    geom_point(data=final_coalition_df[final_coalition_df$aantal_partijen==7&final_coalition_df$excluded==T,],shape=4,color='grey')+
    # partijen + uitslag
    geom_point(data=plot_df,aes(size=uitslag))+
    # kiezer
    geom_point(data=plot_df_kiezer,color='black', shape=18, size=6)+
    # partijnaam
    geom_label_repel(data=plot_df, aes(label=naam))+
    # cosmetics
    scale_color_gradientn(colors=brewer.pal(3, 'Dark2'), limits=c(5,7))+
    coord_fixed(ratio = 1)+
    theme_void()+#+theme(legend.position = 'none')
    # labeling
    geom_point(data=final_coalition_df[c(top5_7partijen),],color='black')+
    geom_label_repel(data=final_coalition_df[c(top5_7partijen),],aes(label=coalitie_naam, color=aantal_partijen))+
    theme(legend.position = 'none')+
    ggtitle('Met 7 partijen')+
    xlim(c(min(final_coalition_df[final_coalition_df$aantal_partijen==7,]$pc1), max(final_coalition_df[final_coalition_df$aantal_partijen==7,]$pc1)))+
    ylim(c(min(final_coalition_df[final_coalition_df$aantal_partijen==7,]$pc2), max(final_coalition_df[final_coalition_df$aantal_partijen==7,]$pc2)))

p
ggsave(plot = p, filename = '/Users/m.wehrens/Desktop/coalities_7partijen.pdf', width = 20, height=15, units='cm')

# only top choices displayed
ggplot(mapping=aes(x=pc1, y=pc2))+
    # axis
    #geom_hline(yintercept = 0)+
    #geom_vline(xintercept = 0)+
    # gem. kiezer
    geom_hline(yintercept = gemiddelde_kiezer_pca[2], color='black')+
    geom_vline(xintercept = gemiddelde_kiezer_pca[1], color='black')+
    # coalities
    geom_point(data=final_coalition_df[c(top5_5partijen,top5_6partijen,top5_7partijen),],aes(color=aantal_partijen))+
    # partijen + uitslag
    geom_point(data=plot_df,aes(size=uitslag))+
    # kiezer
    geom_point(data=plot_df_kiezer,color='black', shape=18, size=6)+
    # partijnaam
    geom_label_repel(data=plot_df, aes(label=naam))+
    # cosmetics
    scale_color_gradientn(colors=brewer.pal(3, 'Dark2'), limits=c(5,7))+
    coord_fixed(ratio = 1)+
    theme_void()+#+theme(legend.position = 'none')
    # labeling
    #geom_point(data=final_coalition_df[c(top5_5partijen,top5_6partijen,top5_7partijen),],color='black')+
    geom_label_repel(data=final_coalition_df[c(top5_5partijen,top5_6partijen,top5_7partijen),],aes(label=coalitie_naam, color=aantal_partijen))

openxlsx::write.xlsx(x=final_coalition_df[c(top5_5partijen,top5_6partijen,top5_7partijen),], 
    file='/Users/m.wehrens/Desktop/coalities.xlsx')

# debugging
ggplot(data=plot_df, mapping=aes(x=pc1, y=pc2))+
    geom_point(aes(size=uitslag))+
    geom_point(data=plot_df_kiezer,color='black', shape=18, size=6)+
    geom_label_repel(aes(label=naam))+
    #geom_point(data=final_coalition_df,color='black')+
    geom_point(data=final_coalition_df,aes(color=distances))+
    #geom_point(data=final_coalition_df[final_coalition_df$distances<.3,],color='black',shape=1)+
    scale_color_gradientn(colors=brewer.pal(8, 'Spectral'), values = c(0,.3))+
    coord_fixed(ratio = 1)+theme_bw()
    #geom_point(data=final_coalition_df[final_coalition_df$distances<.2,],color='green')
    


library(openxlsx)

scheduling_df = openxlsx::read.xlsx('/Users/m.wehrens/Documents/Project_files/_courses_R/2020_01_scheduling.xlsx')

View(scheduling_df)

nr_persons = dim(scheduling_df)[1]


strsplit(scheduling_df$`select.days/times.where.you.most.likely.CAN.make.it.[Monday]`,split=', ')

time_levels = c('09:00-11:00','10:00-12:00','13:30-15:30','14:00-16:00','14:30-16:30','15:00-17:00','15:30-17:30')
time_conversion = 1:length(time_levels)
names(time_conversion) = time_levels
time_template = rep(0, length(time_levels))

convert_to_yesno = function(x) {
    observed_times = strsplit(x,', ')
    time_list = time_template
    time_list[time_conversion[unlist(observed_times)]]=1
    return(time_list)
}

overview_matrix=matrix(rep(NA, 5*length(time_levels)), nrow=5)
rownames(overview_matrix) = c('m','t','w','t','f')
colnames(overview_matrix) = time_levels
# rows are days
# columns are time-slots
colnames(scheduling_df[,3:7])
for (idx in c(3:7)) {

    data_column = scheduling_df[,idx]
    #convert_to_yesno(data_column[1])
    
    current_time_matrix = matrix(unlist(lapply(data_column,convert_to_yesno)), nrow=dim(scheduling_df)[1], byrow=T)
    colnames(current_time_matrix) = time_levels
    
    overview_matrix[idx-2,] = apply(current_time_matrix,2,sum)
    
}
overview_matrix

library(pheatmap)
pheatmap(overview_matrix, cluster_rows = F, cluster_cols = F)


########

overview_matrix_NOT=matrix(rep(NA, 5*length(time_levels)), nrow=5)
rownames(overview_matrix_NOT) = c('m','t','w','t','f')
colnames(overview_matrix_NOT) = time_levels
# rows are days
# columns are time-slots
colnames(scheduling_df[,8:12])
for (idx in c(8:12)) {

    data_column = scheduling_df[,idx]
    #convert_to_yesno(data_column[1])
    
    current_time_matrix = matrix(unlist(lapply(data_column,convert_to_yesno)), nrow=dim(scheduling_df)[1], byrow=T)
    colnames(current_time_matrix) = time_levels
    
    overview_matrix_NOT[idx-2-5,] = apply(current_time_matrix,2,sum)
    
}
overview_matrix_NOT

library(pheatmap)
overview_matrix_NOT[overview_matrix_NOT==0]=NA
pheatmap(overview_matrix_NOT, cluster_rows = F, cluster_cols = F)

#######

pheatmap(overview_matrix, cluster_rows = F, cluster_cols = F)
pheatmap(overview_matrix_NOT, cluster_rows = F, cluster_cols = F)
overview_matrix_=overview_matrix
overview_matrix_[!is.na(overview_matrix_NOT)]=NA
pheatmap(overview_matrix_, cluster_rows = F, cluster_cols = F)










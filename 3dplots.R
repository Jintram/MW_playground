library("scatterplot3d") # install.packages("scatterplot3d") 

data(iris)
scatterplot3d(iris), pch = 16, type="h", color=colors)

colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]

scatterplot3d(iris[,1:3], pch = 16, type="h", color=colors)

#scatterplot3d(data_container$groups$HCM$dt$pca_cells_zdata$x[,1:3], type="h", color=
#        col_vector_60[data_container$groups$HCM$dt$identities[data_container$groups$HCM$dt$sel_s]])

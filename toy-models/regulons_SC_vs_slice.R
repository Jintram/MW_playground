
################################################################################
# Let's write a small simple toy model for gene expression
# 
# m.wehrens@hubrecht.eu, 03-2020

library(umap)
library(pheatmap)

# shorthand function to generate a bunch of rounded random numbers
runif_rnd = function(N) {round(runif(N)*10,2) }

# Now let's define cell types based on 10 regulons
celltypes =
    matrix(c(   runif_rnd(2), 0, 0, 0,      runif_rnd(5), 
                0, runif_rnd(2), 0, 0,      runif_rnd(5),
                0, 0, 0, runif_rnd(1), 0,   runif_rnd(5), 
                0, 0, 0, 0, runif_rnd(1),   runif_rnd(5),
                0, 0, 0, 0, runif_rnd(1),   runif_rnd(4),0),
            nrow=5, byrow = T)
# Show user active regulons per type
(celltypes>0)*1

# Decide how many genes each regulon controls:
regulon_control_nrs_ = round(10+runif(10)*100)
regulon_control_nrs_ = round(regulon_control_nrs_/sum(regulon_control_nrs_)*1000)
# make sure total controlled genes is 1000
to_correct = 1000-sum(regulon_control_nrs_) 
regulon_control_nrs_[1:abs(to_correct)]=regulon_control_nrs_[1:abs(to_correct)]+1*sign(to_correct)
regulon_control_nrs=regulon_control_nrs_
# show:
regulon_control_nrs
sum(regulon_control_nrs)

# create a param that annotates genes to their corresponding regulons
regulon_identity = as.factor(c(rep(1:10 , times=regulon_control_nrs),rep(11,5)))
names(regulon_identity) = paste0('g.',1:1005)

# define baseline gene expression 
baseline_expression = runif_rnd(1000)

# Now create a function that generates gene expression for a cell
default_regulon_control_nrs=regulon_control_nrs
default_celltypes=celltypes
default_baseline_expression=baseline_expression
default_marker_baseline = c(3.42, 6.15, 0.6, 4.05, 7.67)
generate_cell_expression = function(desired_celltype, 
        regulon_control_nrs=default_regulon_control_nrs, 
        celltypes=default_celltypes, 
        baseline_expression=default_baseline_expression, 
        marker_baseline = default_marker_baseline) {
 
    regulon_control = rep(celltypes[desired_celltype,], times=regulon_control_nrs)
    
    # ratio defined here is perhaps important?
    regulon_noise   = rep(1-rnorm(10)/10, times=regulon_control_nrs)
    gene_noise      = 1-rnorm(1000)/10
    
    # determine gene expression
    gene_expression = baseline_expression*regulon_control*regulon_noise*gene_noise
    
    # now also slap a few marker genes on there
    marker_expression = rep(0, 5)
    marker_expression[desired_celltype] = marker_baseline[desired_celltype]*(1-rnorm(1)/10)
    
    return(c(gene_expression,marker_expression))
    # 1-rnorm(1)/10
       
}

################################################################################
# OK now fill a single cell count matrix

# Let's say there's 1000 cells
desired_cell_types = sort(ceiling(runif(1000)*5)) # i was lazy
names(desired_cell_types) = paste0('c.', 1:1000)

# generate the count matrix
my_sc_count_matrix = 
    matrix(sapply(desired_cell_types, generate_cell_expression), nrow=1005, byrow=F)
rownames(my_sc_count_matrix) = paste0('g.',1:1005)
colnames(my_sc_count_matrix) = paste0('c.',1:1000)

# umap the count matrix
umap_out = umap(t(my_sc_count_matrix))
# show the umap
ggplot(data.frame(umap1=umap_out$layout[,1], umap2=umap_out$layout[,2], true_type = as.factor(desired_cell_types)))+
    geom_point(aes(x=umap1, y=umap2, color=true_type))+theme_bw()

# calculate gene-gene correlations
cor_out = cor(t(my_sc_count_matrix))
# show 'm
#pheatmap(cor_out, cluster_rows = F, cluster_cols = F, annotation_row = data.frame(regulon_identity))
pheatmap(cor_out, cluster_rows = T, cluster_cols = T, annotation_row = data.frame(regulon_identity))
#image(cor_out, useRaster=F, axes=FALSE, col = heat.colors(10))
cor_out[1:10, 1:10]

# calculate cell-cell correlations
cor_out_cells = cor(my_sc_count_matrix)
# show 'm
#pheatmap(cor_out, cluster_rows = F, cluster_cols = F, annotation_row = data.frame(regulon_identity))
pheatmap(cor_out_cells, cluster_rows = T, cluster_cols = T, annotation_row = data.frame(as.factor(desired_cell_types)))
#image(cor_out, useRaster=F, axes=FALSE, col = heat.colors(10))
cor_out[1:10, 1:10]

# sanity checks with the "markers"
my_sc_count_matrix[1001:1005,1:10]

###
# So, as expected, both regulons and cell types are retreived from "single cell" data




################################################################################
################################################################################
# Now generate some "tomoseq" data

# let's manually set the cellular contributions to the slices:
cell_types_per_slice = matrix(c(
    8000, 900, 900, 200,   0,
    8000, 900, 900, 200,   0,
    8000, 900, 900, 200,   0,
    8000, 900, 900, 200,   0,
    8000, 900, 900, 200,   0,
    8000, 900, 900, 200,   0,
    8000, 900, 900, 200,   0,
    8000, 900, 900, 200,   0,
    8000, 900, 900, 200,   0,
    8000, 900, 900, 200,   0,

    9000, 450, 450, 100,   0,
    9000, 450, 450, 100,   0,
    9000, 450, 450, 100,   0,
    9000, 450, 450, 100,   0,
    9000, 450, 450, 100,   0,
    9000, 450, 450, 100,   0,
    9000, 450, 450, 100,   0,
    9000, 450, 450, 100,   0,
    9000, 450, 450, 100,   0,
    9000, 450, 450, 100,   0,
    
    8000, 900, 900, 100, 100,
    8000, 900, 900, 100, 100,
    8000, 900, 900, 100, 100,
    8000, 900, 900, 100, 100,
    8000, 900, 900, 100, 100,
    8000, 900, 900, 100, 100,
    8000, 900, 900, 100, 100,
    7600, 900, 900, 500, 100,
    7600, 900, 900, 500, 100,
    7600, 900, 900, 500, 100), ncol=5, byrow=T)
    
# 30 slices, each 10k cells
slice_expressions = matrix(rep(NA, 1005*30), nrow=1005)
for (slice_nr in 1:30) {
    print(paste0('Calculating slice ', slice_nr))
    
    desired_cell_types_tomo = rep(1:5, times=cell_types_per_slice[slice_nr,])
    
    current_tomo_count_matrix = 
        matrix(sapply(desired_cell_types_tomo, generate_cell_expression), nrow=1005, byrow=F)
    rownames(current_tomo_count_matrix) = paste0('g.',1:1005)
    colnames(current_tomo_count_matrix) = paste0('c.',1:10000)

    average_gene_expression = 
        apply(current_tomo_count_matrix, 1, mean)
    
    slice_expressions[,slice_nr] = average_gene_expression
}
rownames(slice_expressions) = paste0('g.',1:1005)

################################################################################
# Now repeat calculating correlation matrices..

# calculate gene-gene correlations
cor_out_tomo = cor(t(slice_expressions))
# show 'm
#pheatmap(cor_out, cluster_rows = F, cluster_cols = F, annotation_row = data.frame(regulon_identity))
pheatmap(cor_out_tomo, cluster_rows = T, cluster_cols = T, annotation_row = data.frame(regulon_identity))
#image(cor_out, useRaster=F, axes=FALSE, col = heat.colors(10))
cor_out[1:10, 1:10]

# calculate slice-slice correlations
cor_out_slices_tomo = cor(slice_expressions)
# show 'm
pheatmap(cor_out_slices_tomo, cluster_rows = F, cluster_cols = F)
#pheatmap(cor_out_cells_tomo, cluster_rows = T, cluster_cols = T, annotation_row = data.frame(as.factor(desired_cell_types)))
#image(cor_out, useRaster=F, axes=FALSE, col = heat.colors(10))
cor_out[1:10, 1:10]

################################################################################
# Now can we extrapolate the number of cell types from the tomo data?

# we can plot the regulons first on the slices
regulon_identity_int = c(rep(1:10 , times=regulon_control_nrs),rep(11,5))
regulon_identity_int

regulon_expression_tomo = matrix(rep(NA, 10*30), nrow=10)
for (reg_idx in 1:10) {
    
    current_expression = 
        apply(slice_expressions[regulon_identity_int == reg_idx,],2,sum)
    
    regulon_expression_tomo[reg_idx, ] = current_expression/sum(current_expression)
}
rownames(regulon_expression_tomo) = paste0('r.',1:10)
colnames(regulon_expression_tomo) = paste0('s.',1:30)
pheatmap(regulon_expression_tomo, cluster_rows = F, cluster_cols = F)


#####
# back-calculation would be easiest based on some marker genes
slice_expressions[1001:1005,]
pheatmap(slice_expressions[1001:1005,], cluster_rows = F, cluster_cols = F)

# now, if all markers have equal expression levels things are simple:
slices_marker_expression = slice_expressions[1001:1005,]
# calculate fraction of marker expression and times total #cells, et voila
10000*slices_marker_expression/apply(slices_marker_expression,2,sum) 

# but, if markers have different expression levels, the level in a slice is 
# determined by:
fraction_cells * marker_baseline * nr_cells = expression
fraction_cells * marker_baseline = marker_expression

# so if we know the baselines, it's easy again (and we know them from single cell matrix)
hist(my_sc_count_matrix[1001,])
median(my_sc_count_matrix[1001,][my_sc_count_matrix[1001,]>1])
    # calculated base line of 3.43, whilst actual value was 3.42 
# though we can calculate it, let's use known baseline "default_marker_baseline" here
# fraction_cells = marker_expression/marker_baseline
slices_marker_expression/default_marker_baseline*10000
# or, if we only know relative values
default_marker_baseline_norm  = default_marker_baseline/sum(default_marker_baseline)
xx = slices_marker_expression / default_marker_baseline_norm
xx/apply(xx,2,sum)*10000
    # indeed, this is what we had

# but we could use the regulon "signature" as marker also
# however, we'd then need to determine a signature per cell type first










# Convert the data first to a matrix and then to double
tomoseq_data = as.matrix(tomoseq_data_)
rownames(tomoseq_data) = rownames(tomoseq_data_)
# Remove the temporary parameter from the workspace (rm stands for remove)
rm(tomoseq_data_)

# Alternative way to do it
    ## Convert the data to numeric format
    #tomoseq_data = as.matrix(as.data.frame(lapply(tomoseq_data_, as.numeric)))
    #rownames(tomoseq_data) = rownames(tomoseq_data_)
    #    # this contains multiple conversion steps for technical reasons,
    #    # and the function lapply, which will be introduced later.
    #    # (lapply can be applied to a dataframe, because technically,
    #    # a dataframe is simply a list of parameters of equal lengths.)

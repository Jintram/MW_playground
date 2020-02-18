
all_object_names <- ls()
all_object_sizes <- list()
types_objects    <- list()
for (idx in 1:length(all_object_names)) {

    object_name <- all_object_names[[idx]]
    all_object_sizes[[idx]] <- object.size(eval(parse(text=object_name)))
    types_objects[[idx]] <- typeof(eval(parse(text=object_name)))
    
}

df_object_sizes <- data.frame(name=all_object_names, size=unlist(all_object_sizes)/1024^2, type=unlist(types_objects))

df_object_sizes[order(df_object_sizes$size, decreasing = T),][1:30,]

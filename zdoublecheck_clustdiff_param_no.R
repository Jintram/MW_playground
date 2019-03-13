

x     <- object@ndata
y     <- object@expdata[,names(object@ndata)]

# Can I do the normalization myself?
total_median_transcript_count <- median(apply(y,2,sum))
total_transcript_counts_per_cell <- apply(y,2,sum)
x / total_transcript_counts_per_cell
View(apply(x,1,'/',total_transcript_counts_per_cell))


# manual calculation
no_mw_pure <- (median(apply(y[,part == 1],2,sum))) / (median(apply(y,2,sum)))
no_mw_01   <- (median(apply(y[,part == 1],2,sum))) / (median(apply(y,2,sum)))

# grun calculation
x0 <- x-.1
no            <- (median(apply(y[,part == 1],2,sum))) / (median(apply(x[,part == 1],2,sum)))
no_corrected  <- (median(apply(y[,part == 1],2,sum))) / (median(apply(x0[,part == 1],2,sum)))
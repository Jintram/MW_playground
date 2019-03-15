

i<-1

# parameters as defined by grun =======================================================================
x     <- object@ndata
y     <- object@expdata[,names(object@ndata)]




# Let's see if we can do the normalization manually ===================================================
overall_median_transcript_count <- median(apply(y,2,sum))
overall_min_transcript_count <- min(apply(y,2,sum))
transcript_count_per_cell <- apply(y,2,sum)

# Calculate normalization, using overall median and minimum (latter is done in raceid code)
xprime<-sweep(y,2,transcript_count_per_cell,'/')*overall_median_transcript_count+.1
xprime_min<-sweep(y,2,transcript_count_per_cell,'/')*overall_min_transcript_count+.1
  # we can, but normalization is actually done towards minimum count instead of median count

# Just look at example value that has a value
x[10,1]
xprime[10,1]
xprime_min[10,1]



# Now let's try to calculate rescaling factor manually ===============================================
no_grun <- median(apply(y[,part == i],2,sum))/median(apply(x[,part == i],2,sum))

# grun calculation
no            <- (median(apply(y[,part == 1],2,sum))) / (median(apply(x[,part == 1],2,sum)))
x0 <- x-.1
no_corrected  <- (median(apply(y[,part == 1],2,sum))) / (median(apply(x0[,part == 1],2,sum)))

# manual calculation by me
no_prime          <- (median(apply(y[,part == 1],2,sum))) / (median(apply(y,2,sum)))
no_prime_min      <- (median(apply(y[,part == 1],2,sum))) / (min(apply(y,2,sum)))

no_prime_try      <- (median(apply(y[,part == 1],2,sum))) / (min(apply(y+.1,2,sum)))







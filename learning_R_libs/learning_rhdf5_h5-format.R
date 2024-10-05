
# Working with the rhdf5 library

################################################################################
# Other options

# This is a library I installed earlier, but seems perhaps somewhat older
# Note there are also some h5 libraries installed because I used h5seurat
# library(hdf5r)
# https://cran.r-project.org/web/packages/hdf5r/vignettes/hdf5r.html

################################################################################

# Working with rhdf5
# https://www.bioconductor.org/packages/devel/bioc/vignettes/rhdf5/inst/doc/rhdf5.html
# 
# For more documentation, see also:
# https://www.bioconductor.org/packages/devel/bioc/manuals/rhdf5/man/rhdf5.pdf
# https://portal.hdfgroup.org/display/HDF5/Files (Through link in help; e.g. ?H5Fflush)


library(rhdf5)

my_h5_filepath = "/Users/m.wehrens/Data/__misc_data/rhdf5-testing/myhdf5file.h5"
# my_h5_filepath = "/hpc/hub_oudenaarden/mwehrens/test/hdf5/testfile.h5"

h5createFile(my_h5_filepath)

################################################################################
# Writing data

# One can create a direcotry structure ("group hierarchy") in the h5 file
h5createGroup(my_h5_filepath,"foo")
h5createGroup(my_h5_filepath,"baa")
h5createGroup(my_h5_filepath,"foo/foobaa")

# Show the directory structure
h5ls(my_h5_filepath)

# Create some objects that we're going to store as example
A = matrix(1:10,nr=5,nc=2)
#
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B, "scale") <- "liter"
#
C = matrix(paste(LETTERS[1:10],LETTERS[11:20], collapse=""),
  nr=2,nc=5)
df = data.frame(1L:5L,seq(0,1,length.out=5),
  c("ab","cde","fghi","a","s"), stringsAsFactors=FALSE)

# now store all these objects in a specific "directory"
h5write(A, my_h5_filepath,"foo/A")
h5write(B, my_h5_filepath,"foo/B")
h5write(C, my_h5_filepath,"foo/foobaa/C")
h5write(df, my_h5_filepath,"df")


################################################################################
# Reading the data again

# now use the "ls" function to show what's stored
h5ls(my_h5_filepath)

# Now things can be easily read back, using 
D_ = h5read(my_h5_filepath,"foo/A")
E_ = h5read(my_h5_filepath,"foo/B")
F_ = h5read(my_h5_filepath,"foo/foobaa/C")
G_ = h5read(my_h5_filepath,"df")

################################################################################
# Further ways of reading/writing things

# The whole file can also be read at once

h5f = H5Fopen(my_h5_filepath)
h5f

# The first dollar operator used, reads in the whole file;
# so 
h5f$foo$foobaa$C
# will read in h5f$foo, and then the rest of the $ operators just access 
# the parameter in the way $ would normally access a list.
# The following way will only read in C, not the whole content of "foo":
h5f$"foo/foobaa/C"

# One can also use the "&" operator, which returns a group or dataset handle
# [I guess what this means is, a handle in h5 format]
h5f&'df'
h5f&"foo/foobaa/C"
# This is e.g. convenient when memory is limited, and e.g. an algorithm can
# be applied to part of the memory
h5d = h5f&"/foo/B"
h5d[3,,]

# This can also be used to write data
h5d[3,,] = 1:4
H5Fflush(h5f) # flushing a buffer means to transfer data from temp memory to permanent memory (google search "flush buffers meaning")
# Hence, this would simply store the data
# And this can be done because previously, the "&" operator was used to "open"
# this file; so we didn't simply load the data, but the informatoin about 
# the associated file was retained.

# The tutorial furthermore notes that 
# Though the code below gives an error
h5f$foo$B = 101:120     # changes data locally
h5f$"/foo/B" = 101:120  # changes data on disk

# Important to also close the files once done with them
H5Dclose(h5d)
H5Fclose(h5f)

# Or simply close all
h5closeAll()


################################################################################
# A test specifically for something i'd like to do
# Ie dump a list of matrices into a hdf5 file

h5createFile(my_h5_filepath)
h5createGroup(my_h5_filepath,paste0("countmatrix/"))
# or
# h5f = H5Fopen(my_h5_filepath)

for (sample_id in paste0('S',1:5)) {
    
    # sample_id = 'tralala'
    
    M_test = matrix(round(runif(15)*100),nr=5,nc=3)
    
    #h5f$paste0("countmatrix/", sample_id) = M_test
    
    h5write(M_test, my_h5_filepath,paste0("countmatrix/", sample_id))
       
    #H5Dclose(h5d)
}

h5closeAll()

####################

my_h5_filepath2 = "/hpc/hub_oudenaarden/mwehrens/test/hdf5/testfile2.h5"
M_list = list()

for (sample_id in paste0('S',1:5)) {
 
    M_list[[sample_id]] = matrix(round(runif(15)*100),nr=5,nc=3)    
    # For testing purposes, note that these names will be lost when saving
    rownames(M_list[[sample_id]]) = paste0('gene.',1:5)
    colnames(M_list[[sample_id]]) = paste0('cell.',1:3)
    
}

h5save(M_list, file=my_h5_filepath2)
h5dump(my_h5_filepath2)

####################
# Note that this can also be opened in python..
# PYTHON CODE BELOW

my_h5_filepath2 = "/hpc/hub_oudenaarden/mwehrens/test/hdf5/testfile2.h5"

f_h5 = h5py.File(my_h5_filepath2, 'r')
list(f_h5.keys())
dset = f_h5['mydataset']
# dset[1:3,1:3]
# dset.dtype
# dset.shape


################################################################################
# There was a load more info in the tutorial at 
# https://www.bioconductor.org/packages/devel/bioc/vignettes/rhdf5/inst/doc/rhdf5.html

# more options regarding
# - subsetting 
# - saving
# - content listing 
# - 64 bit integers
# - ..














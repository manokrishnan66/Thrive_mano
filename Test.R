# ---Importing a datafile from dslabs

p <- system.file("extdata", package="dslabs")
list.files(p)

# -- getting fullpath
filename <- "murders.csv"
fullpath <- file.path(p,filename)
fullpath

#-- copying to working directory
file.copy(fullpath,getwd())
file.exists(filename)
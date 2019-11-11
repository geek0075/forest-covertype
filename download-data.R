###########################################
# Download forest cover type data set from
# UCI ML Repository
###########################################

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz"
dest_file <- "data/covtype.data.gz"
download.file(url, destfile = dest_file)
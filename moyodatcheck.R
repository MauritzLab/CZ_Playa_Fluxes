momyo code

par2.names <- colnames(fread(path2, sep = ",", dec = ".", skip = 1, header = TRUE)[1,])

par3.names<- colnames(fread (path3, sep = ",", dec = ".", skip = 1, header = TRUE)[1,])

par4.names<- colnames(fread(path4, sep = ",", dec = ".", skip = 1, header = TRUE)[1,])

par5.names<- colnames(fread(path5, sep = ",", dec = ".", skip = 1, header = TRUE)[1,])

par6.names<- colnames(fread(path6, sep = ",", dec = ".", skip = 1, header = TRUE)[1,])

par7.names<- colnames(fread(path7, sep = ",", dec = ".", skip = 1, header = TRUE)[1,])

par8.names<- colnames(fread(bahada, sep = ",", dec = ".", skip = 1, header = TRUE)[1,])

# read in data using par.names as column names
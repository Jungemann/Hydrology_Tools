# This script is used to convert daily to monthly data
# By Harold Llauca

# Remove stored variables and clean console
rm(list=ls())
cat('\f')


# Enter input variables
wd        <- 'D:/GR2M_PERU/Caudales'
file.load <- 'Q_diario_titicaca.csv'
file.save <- 'Q_mensual_titicaca.csv'
threshold <- 5


# Read file with daily data
setwd(wd)
df.raw   <- read.table(file.load, sep=',', header=F)


# Extract metadata and data
Metadata <- df.raw[1:5,]
Qdaily   <- df.raw[-5:-1,]


# Convert daily to monthly data
source('convert_d2m.R')
Qmonthly <- convert_d2m(Qdaily, threshold, FUN='mean')


# Save monthly data as .csv
ans <- rbind(as.matrix(Metadata), as.matrix(Qmonthly))
write.table(ans, file.save, sep=',', row.names=F, col.names=F)

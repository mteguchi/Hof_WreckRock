#position of predation

# Tomo Eguchi
# 20 May 2015


rm(list=ls())
sysInfo <- Sys.info() 
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))
D00 <- dirSelector()

invlogit <- function(x){
  return(exp(x)/(1 + exp(x)))
}

infile1 <- 'Full Beach Scan Data Tomo FinalCORRECTED_TE20May2015.csv'
data1 <- read.csv(paste(D00$Dtomo, 
                        'turtles/Australia/WreckRock_experiment/',
                        infile1, sep = ""), 
                  header = TRUE)
data1_cc <- subset(data1, Sp == "L")
data1_cc$ID <- 1:dim(data1_cc)[1]

data1_cc$DOY <- as.numeric(strftime(strptime(data1_cc$Date, 
                                             "%m/%d/%Y"), 
                                    format = "%j"))

#summary(data1_cc)

# extract predation data:
pred_cc1 <- subset(data1_cc, 
                   Sample1 == "1" | Sample1 == "0" | 
                     Sample2 == "1" | Sample2 == "0" | 
                     Sample2 == "POSSIBLE")

summary(pred_cc1)

# find habitat:
dune <- pred_cc1[c(grep("DUNE", pred_cc1$Habitat, ignore.case = TRUE), 
                   grep("D", pred_cc1$Habitat)), ]
beach <- pred_cc1[grep("B", pred_cc1$Habitat), ]
# need to remove "Back of dune"
beach <- beach[-grep("BACK OF DUNE", beach$Habitat),]
slope <- pred_cc1[grep("S", pred_cc1$Habitat), ]

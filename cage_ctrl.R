# control vs. cage experiment

# Tomo Eguchi
# 1 Aug 2014

rm(list=ls())
sysInfo <- Sys.info() 
ifelse(sysInfo[1] == 'Linux',
       source('~/Documents/R/TomosFunctions.R'),
       source('~/R/TomosFunctions.R'))
D00 <- dirSelector()

invlogit <- function(x){
  return(exp(x)/(1 + exp(x)))
}

infile1 <- 'predation_v13Aug2014.csv'
# data1 <- read.csv(paste(D00$Dtomo, 
#                         'turtles/Australia/WreckRock_experiment/',
#                         infile1, sep = ""), 
#                   header = TRUE)

data1 <- read.csv(paste('data/',
                        infile1, sep = ""), 
                  header = TRUE)

# run a logistic regression on predation:
data1$fPred <- factor(data1$Predated)
data1$DOY <- as.numeric(strftime(strptime(data1$Date, 
                                          "%d/%m/%Y"), 
                                 format = "%j"))
data1$Days[data1$DOY>100] <- data1$DOY[data1$DOY>100] - 336 + 1   # first laid date is 1
data1$Days[data1$DOY<100] <- data1$DOY[data1$DOY<100] + (365 - 336 + 1)

#+ PredationDay
fit.1 <- glm (fPred ~ Device + peg1, 
              family=binomial(link="logit"),
              data = data1)
summary(fit.1)

fit.2 <- glm (fPred ~ Device * peg1, 
              family=binomial(link="logit"),
              data = data1)

summary(fit.2)

AIC(fit.1, fit.2)

# plot predation vs non-predation:
# Control and non-predated
par(bty = "l")
plot(data1$Days, data1$peg1, type = "n",
     xlab = "Days since start of the experiment",
     ylab = "Peg #")
# Circle pch = 1 and 16 - controls
points(data1$Days[data1$Device == "C" & data1$fPred == "0"], 
     data1$peg1[data1$Device == "C" & data1$fPred == "0"], 
     col = "green", pch = 1, cex = 2)
points(data1$Days[data1$Device == "C" & data1$fPred == "1"], 
     data1$peg1[data1$Device == "C" & data1$fPred == "1"], 
     col = "red", pch = 16, cex = 2)

# squares pch = 0 and 15 - devices
points(data1$Days[data1$Device == "D" & data1$fPred == "0"], 
       data1$peg1[data1$Device == "D" & data1$fPred == "0"], 
       col = "green", pch = 0, cex = 2)
points(data1$Days[data1$Device == "D" & data1$fPred == "1"], 
       data1$peg1[data1$Device == "D" & data1$fPred == "1"], 
       col = "red", pch = 15, cex = 2)

# plot for comparing device and control
ctrl_0 <- data1[data1$Device == "C" & data1$fPred == "0",]
ctrl_1 <- data1[data1$Device == "C" & data1$fPred == "1",]
ctrl_prop <- c(dim(ctrl_0)[1]/(dim(ctrl_0)[1] + dim(ctrl_1)[1]),
               dim(ctrl_1)[1]/(dim(ctrl_0)[1] + dim(ctrl_1)[1]))

dev_0 <- data1[data1$Device == "D" & data1$fPred == "0",]
dev_1 <- data1[data1$Device == "D" & data1$fPred == "1",]
dev_prop <- c(dim(dev_0)[1]/(dim(dev_0)[1] + dim(dev_1)[1]),
               dim(dev_1)[1]/(dim(dev_0)[1] + dim(dev_1)[1]))

par(mfrow = c(2, 1))
pie(ctrl_prop, c("not predated", "predated"),
    radius = 1.0, col = c("white", "red"),
    main = "Control")
pie(dev_prop, c("not predated", "predated"),
    radius = 1.0, col = c("white", "red"),
    main = "Device")



# fit.2 is tiny bit better so use it for predictions
infile2 <- 'FullBeachScanData_v06Aug2014b.csv'
data2_1 <- read.table(paste(D00$Dtomo, 
                            'turtles/Australia/WreckRock_experiment/',
                            infile2, sep = ""), 
                      header = TRUE, sep = ",",
                      strip.white = TRUE,
                      stringsAsFactors = FALSE,
                      na.strings = "NA")

data2 <- na.omit(data2_1[, c("Date", "peg1", "Species", "Activity")])
data2$ACTIVITY <- as.factor(toupper(data2$Activity))
data2$Device <- as.factor(rep("C", times = dim(data2)[1]))
data2_Cc_0 <- data2[data2$Species == "L",]
data2_Cc <- data2_Cc_0[data2_Cc_0$ACTIVITY != "WALKING" & 
                         data2_Cc_0$ACTIVITY != "PREDATED" & 
                         data2_Cc_0$ACTIVITY != "EMERGENCE",]

data2_Cc$DOY <- as.numeric(strftime(strptime(data2_Cc$Date, "%m/%d/%Y"), 
                                    format = "%j"))
data2_Cc$Days[data2_Cc$DOY>100] <- data2_Cc$DOY[data2_Cc$DOY>100] - 336 + 1   # first laid date is 1
data2_Cc$Days[data2_Cc$DOY<100] <- data2_Cc$DOY[data2_Cc$DOY<100] + (365 - 336 + 1)

summary(data2_Cc)

pred.2 <- predict(fit.2, newdata = data2_Cc)
pred.2_prob <- invlogit(pred.2)

par(bty = "l")
plot(data2_Cc$Days, data2_Cc$peg1, type = "n",
     xlab = "Days since start of the experiment",
     ylab = "Peg #",
     main = "Probability of predation")
points(data2_Cc$Days, data2_Cc$peg1, 
       col = "green", pch = 1, lwd = 5,
       cex = pred.2_prob*10)

# the expected number of hatchlings with predation is 
# the number eggs laid{1 - (predation pr) x (prop of predated eggs)}
# BUT... we don't have data to compute proportion of predated eggs per predated nest
# so, we will assume all eggs in predated nests are dead. 

# the mean number of eggs per nest and mean hatching success:
# they are quite identical to the data below: Ctrl... 
# data2_1$ACTIVITY <- as.factor(toupper(data2_1$Activity))
# data2_Cc <- data2_1[data2_1$Species == "L" & data2_1$ACTIVITY == "LAID" & data2_1$N_eggs_predated == 0,]
# 
# data2_Cc_emerg <- na.omit(data2_Cc[, c(grep(colnames(data2_Cc), pattern = "peg1"), 
#                                        grep(colnames(data2_Cc), pattern = "N_"))])
# hatchSuc <- data2_Cc_emerg$N_eggs_hatched/(rowSums(data2_Cc_emerg[2:7]))
# 
infile3 <- 'predation_v06Aug2014.csv'
# data3 <- read.csv(paste(D00$Dtomo, 'turtles/Australia/WreckRock_experiment/',
#                         infile3, sep = ""), header = TRUE)
data3 <- read.csv(paste0('data/', infile3), header = TRUE)
# data3$propPred <- (data3$PREDATED_EGGS)/(data3$PREDATED_EGGS + data3$UNHATCHED_EGGS + 
#                                            data3$UNDEVELOPED_EGGS + data3$DEAD_HATCHLINGS + 
#                                            data3$LIVE_HATCHLINGS + data3$EMPTY_SHELLS)
Ctrl <- data3[data3$C_D == "C",]
summary(Ctrl)
Ctrl$hatchSuc <- (Ctrl$EMPTY_SHELLS)/(Ctrl$UNHATCHED_EGGS + 
                                        Ctrl$UNDEVELOPED_EGGS +
                                        Ctrl$EMPTY_SHELLS)

mean.hatchSuc <- mean(Ctrl$hatchSuc, na.rm = TRUE)
median.hatchSuc <- median(Ctrl$hatchSuc, na.rm = TRUE)

Ctrl$nestSize <- Ctrl$UNHATCHED_EGGS + 
  Ctrl$UNDEVELOPED_EGGS + Ctrl$DEAD_HATCHLINGS + 
  Ctrl$LIVE_HATCHLINGS + Ctrl$EMPTY_SHELLS
mean.nestSize <- mean(Ctrl$nestSize, na.rm = TRUE)
median.nestSize <- median(Ctrl$nestSize, na.rm = TRUE)

# To incorporate uncertainty in the total estiamted, we run bootstrap:
# each nest gets predated with probability estimated by the logistic regression.
hatchlings <- vector(mode = "numeric", length = 1000)

for (k in 1:1000){
  tmp1 <- vector(mode = "numeric", length = length(pred.2_prob))
  for (i in 1:length(pred.2_prob)){
    tmp1[i] <- rbinom(n=1, size=1, prob = 1 - pred.2_prob[i])
  }
  hatchlings[k] <- sum(tmp1) * median.nestSize * median.hatchSuc
}

mean.total.possible <- dim(data2_Cc)[1] * mean.nestSize * mean.hatchSuc
mean.total.estimated <- sum(mean.nestSize * (1 - pred.2_prob)) * mean.hatchSuc

median.total.possible <- dim(data2_Cc)[1] * median.nestSize * median.hatchSuc
median.total.estimated <- sum(median.nestSize * (1 - pred.2_prob)) * median.hatchSuc

quantile(hatchlings, probs = c(0.025,0.5,0.975))

# beta parameters for hatching success:
# m <- mean(Ctrl$hatchSuc, na.rm = TRUE)
# v <- var(Ctrl$hatchSuc, na.rm = TRUE)
# a <- ((1 - m) * ((m) ^ 2)) / v - m
# b <- a * (1 - m) / m;




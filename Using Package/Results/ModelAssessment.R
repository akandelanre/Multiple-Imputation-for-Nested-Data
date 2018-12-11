
###########################################################################
###########################################################################
################################# Model Assessment ########################
###########################################################################
###########################################################################

################################# START ###################################
#rm(list = ls())
source("ModelAssessmentFunctions.R")

###### 1: Save imputed datasets from model run...Use immediately after using "RunModel"
writeFun <- function(LL){
  for(i in 1:length(LL)){
    if(HHhead_at_group_level){
      if(weight_option){
        write.table(LL[[i]],paste0("impData_newFormat_weighted",i,".txt"),row.names = F,col.names = T)
      } else {
        write.table(LL[[i]],paste0("impData_newFormat",i,".txt"),row.names = F,col.names = T)
      }
    } else {
      if(weight_option){
        write.table(LL[[i]],paste0("impData_oldFormat_weighted",i,".txt"),row.names = F,col.names = T)
      } else {
        write.table(LL[[i]],paste0("impData_oldFormat",i,".txt"),row.names = F,col.names = T)
      }
    }
  }
}
if(WriteNewResults){
  writeFun(ModelResults$impData)
}


###### 2: Load original dataset with no missing values
TrueSample <- list();
#TrueSample$House <- read.table("Data_house_truth.txt",header=TRUE)
#TrueSample$Indiv <- read.table("Data_indiv_truth.txt",header=TRUE)
TrueData <- read.table("origdata_truth.txt",header=TRUE)
TrueSample$House <- GetHouseData(TrueData)
TrueSample$Indiv <- GetIndivData(TrueData)
remove(TrueData)


###### 3: Load imputed datasets
GlobalPara <- list()
GlobalPara$mm <- mm
Model <- list();
Model$House <- NULL; Model$Indiv <- NULL
#weight_option <- weight_option
weight_option <- FALSE
if(weight_option){
  for(i in 1:GlobalPara$mm){
    ImputedData <- read.table(paste0("impData_newFormat_weighted",i,".txt"),header = T)
    Model$House <- rbind(Model$House, GetHouseData(ImputedData))
    Model$Indiv <- rbind(Model$Indiv, GetIndivData(ImputedData))
  }
} else {
  for(i in 1:GlobalPara$mm){
    ImputedData <- read.table(paste0("impData_newFormat",i,".txt"),header = T)
    Model$House <- rbind(Model$House, GetHouseData(ImputedData))
    Model$Indiv <- rbind(Model$Indiv, GetIndivData(ImputedData))
  }
}
remove(ImputedData)


###### 4: Define other parameters
GlobalPara$N <- nrow(Model$Indiv)/GlobalPara$mm
GlobalPara$n <- nrow(Model$House)/GlobalPara$mm
GlobalPara$n_i <- as.numeric(as.character((Model$House[1:GlobalPara$n,])$HHSize))
GlobalPara$p <- ncol(Model$Indiv)
GlobalPara$q <- ncol(Model$House)
GlobalPara$house_index <- rep(c(1:GlobalPara$n),GlobalPara$n_i)
GlobalPara$H <- sort(unique(GlobalPara$n_i))
GlobalPara$level_house <- list(c(min((Model$House)$HHSize):max((Model$House)$HHSize)),
                               c(1:2),c(1:2),c(1:9),c(1:5),c(16:96),c(1))
GlobalPara$level_indiv <- list(c(1:2),c(1:9),c(1:5),c(1:96),c(2:13))
GlobalPara$Estimands <-
  c(apply(as.matrix(GlobalPara$H),1,function(x) paste0("Same race household, $n_i = ",as.character(x+1),"$")),
    "Spouse present","Black HH, own",
    "Spouse present, HH is White","Spouse present, HH is Black",
    "White couple", "Non-White couple, own","Same race couple",
    "White-nonwhite couple", "Only one parent",
    "At least one biological child present","One grandchild present",
    "At least three generations present",
    "Couples with age difference less than 5",
    "HH older than spouse, White HH","White HH with Hispanic origin",
    "Only one adult female in house with at least one child less than 5",
    "Only one adult Hispanic male in house with at least one child less than 10",
    "Only one adult Black female in house with at least one child less than 18",
    "HH over 35, no children present","Male HH, own",
    "Black HH younger than 40, own","White HH younger than 25, own",
    "Hispanic HH older than 50, own",
    "2 generations present, Black HH",
    "3 generations present, White couple",
    "At least 2 generations present, Hispanic couple",
    "At least one stepchild",
    "At least one adopted child, White couple",
    "At least one biological child, Hispanic couple",
    "At least two biological children, Black couple")
GlobalPara$samp_size <- matrix(0,nrow=length(GlobalPara$Estimands))
GlobalPara$samp_size[1:length(GlobalPara$H)] <- table(GlobalPara$n_i)
GlobalPara$samp_size[GlobalPara$samp_size==0] <- GlobalPara$n


###### 5: Calculate the probabilities of interest for original dataset
TrueSampleResults <- GetAllProbs(TrueSample$House,TrueSample$Indiv,GlobalPara$level_house,GlobalPara$level_indiv)
TrueSampleResults$OtherProb <-
  GetOtherProbs(TrueSample$House,TrueSample$Indiv,GlobalPara$n,GlobalPara$p,GlobalPara$q,
                GlobalPara$house_index,GlobalPara$H,length(GlobalPara$Estimands))
TrueSampleResults$OtherProb <- TrueSampleResults$OtherProb/GlobalPara$samp_size
TrueSampleResults$OtherVar <- (TrueSampleResults$OtherProb*(1-TrueSampleResults$OtherProb))/GlobalPara$samp_size
TrueSampleResults$OtherCINT <- CalculateCI(TrueSampleResults$OtherProb,TrueSampleResults$OtherVar,imp_ind=F)


###### 6: Calculate the probabilities of interest for imputed datasets
ModelResults <- GetOtherProbsMI(Model$House,Model$Indiv,GlobalPara,GlobalPara$samp_size)


###### 7: Make plot and table
#png("Results/AllProbabilities_ModelResults.png",
#    pointsize=11,width = 13, height = 7,bg="white",units="in",res=150)
par(mfrow=c(1,3),oma=c(2,0,0,0))
plot(TrueSampleResults$MarginalProb,ModelResults$MarginalProb,
     pch = 2,col="red", las=2, main ="Marginal",
     xlab = "Sample Estimate", ylab = paste0("Average From ",GlobalPara$mm," Imputed Datasets"),xlim = c(0,1.0),ylim = c(0,1.0))
abline(a=0, b=1,lty = 1, lwd = 1);
plot(TrueSampleResults$BivariateProb,ModelResults$BivariateProb,
     pch = 2,col="darkblue", las=2, main ="Bivariate",
     xlab = "Sample Estimate", ylab = paste0("Average From ",GlobalPara$mm," Imputed Datasets"),xlim = c(0,1.0),ylim = c(0,1.0))
abline(a=0, b=1,lty = 1, lwd = 1);
plot(TrueSampleResults$TrivariateProb,ModelResults$TrivariateProb,
     pch = 2,col="darkgreen", las=2, main ="Trivariate",
     xlab = "Sample Estimate", ylab = paste0("Average From ",GlobalPara$mm," Imputed Datasets"),xlim = c(0,1.0),ylim = c(0,1.0))
abline(a=0, b=1,lty = 1, lwd = 1)
#dev.off()

CompareProbs <- cbind(TrueSampleResults$OtherCINT,ModelResults$OtherCINT)
colnames(CompareProbs) <- c("Orig. Data L","Orig. Data U","Model L","Model U")
rownames(CompareProbs) <- GlobalPara$Estimands
round(CompareProbs[order(TrueSampleResults$OtherProb,decreasing = TRUE),],3)



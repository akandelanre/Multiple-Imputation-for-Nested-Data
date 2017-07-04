


###########################################################################
###########################################################################
####### MI using NDPMPM: Use rejection sampler to sample missing data #####
####### Add weighting option for sampling impossibles #####################
####### Add the Rejection sampler/sampling proposals hybrid option ########
####### Also make household head into a household level variable ##########
###########################################################################
###########################################################################

###################################### START ######################################
########################## Step 1: One Time Data Preparation ##########################
rm(list = ls())
set.seed(1010)
###### 1: Import Data
House <- read.csv("House.csv",header=T)
Indiv <- read.csv("Indiv.csv",header=T)


###### 2: Remove Households with size < 2 and > 4
House <- House[which(House$NP >= 2 & House$NP <= 4),]


###### 3: Keep only Households with TEN == 1,2,or 3 and recode 1,2 as 1 and 3 as 2
House <- House[which(House$TEN == 1 | House$TEN == 2 | House$TEN == 3),]
House$TEN[which(House$TEN == 2)] <- 1
House$TEN[which(House$TEN == 3)] <- 2


###### 4: Take a sample of size 5,000 Households
sample_size <- 5000
samp_index <- sort(sample(1:nrow(House),sample_size,replace=F))
House <- House[samp_index,]


###### 5: Pick the same households in the indiv data
pick_index <- is.element(Indiv$SERIALNO,House$SERIALNO)
Indiv <- Indiv[pick_index,]


###### 6: Recode within-household variables
###### 6a: First, the relationship variable
Indiv$RELP[which(Indiv$RELP == 11 | Indiv$RELP == 12 | Indiv$RELP == 13)] <- 12 #Boarder, roommate or partner
Indiv$RELP[which(Indiv$RELP == 14 | Indiv$RELP == 15)] <- 13 #Other non-relative or foster child
Indiv$RELP[which(Indiv$RELP == 10)] <- 11 #Other relative
Indiv$RELP[which(Indiv$RELP == 9)] <- 10 #Child-in-law
Indiv$RELP[which(Indiv$RELP == 8)] <- 9 #Parent-in-law
Indiv$RELP[which(Indiv$RELP == 7)] <- 8 #Grandchild
Indiv$RELP[which(Indiv$RELP == 6)] <- 7 #Parent
Indiv$RELP[which(Indiv$RELP == 5)] <- 6 #Sibling
Indiv$RELP[which(Indiv$RELP == 4)] <- 5 #Stepchild
Indiv$RELP[which(Indiv$RELP == 3)] <- 4 #Adopted child
Indiv$RELP[which(Indiv$RELP == 2)] <- 3 #Biological child
Indiv$RELP[which(Indiv$RELP == 1)] <- 2 #Spouse
Indiv$RELP[which(Indiv$RELP == 0)] <- 1 #Household head
###### 6b: Next, the race variable
Indiv$RAC3P[which(Indiv$RAC3P == 4 | Indiv$RAC3P == 8| Indiv$RAC3P == 9 | Indiv$RAC3P == 10)] <- 6
Indiv$RAC3P[which(Indiv$RAC3P == 5)] <- 4
Indiv$RAC3P[which(Indiv$RAC3P == 7)] <- 5
Indiv$RAC3P[which(Indiv$RAC3P >= 11 & Indiv$RAC3P <= 15)] <- 7
Indiv$RAC3P[which(Indiv$RAC3P >= 16 & Indiv$RAC3P <= 59)] <- 8
Indiv$RAC3P[which(Indiv$RAC3P >= 60 & Indiv$RAC3P <= 100)] <- 9
###### 6c: Next, the hisp variable
Indiv$HISP[which(Indiv$HISP >= 5 & Indiv$HISP <= 24)] <- 5
###### 6d: Lastly, age
Indiv$AGEP <- Indiv$AGEP + 1L


###### 7: Make household head into household level data
HHhead_data <- Indiv[which(Indiv$SPORDER==1),]
Indiv_minHH <- Indiv[-which(Indiv$SPORDER==1),]


###### 8: Combine Household and within-household data using the following ordering:
origdata <- data.frame(Hhindex = rep(c(1:sample_size),(House$NP-1L)),
                       pernum = Indiv_minHH$SPORDER,
                       sex = Indiv_minHH$SEX, race = Indiv_minHH$RAC3P, hisp = Indiv_minHH$HISP,
                       age = Indiv_minHH$AGEP, relate = Indiv_minHH$RELP,
                       ownership = rep(House$TEN,(House$NP-1L)),
                       headsex = rep(HHhead_data$SEX,(House$NP-1L)),
                       headrace = rep(HHhead_data$RAC3P,(House$NP-1L)),
                       headhisp = rep(HHhead_data$HISP,(House$NP-1L)),
                       headage = rep(HHhead_data$AGEP,(House$NP-1L)))
                       #headrelate = rep(HHhead_data$RELP,(House$NP-1L)))
origdata_truth <- origdata


###### 10: Poke holes in Data:: Individual-level variables for now
O_matrix <- matrix(1,ncol=ncol(origdata),nrow=nrow(origdata))
colnames(O_matrix) <- colnames(origdata)
quick_miss_index <- c("sex","race","hisp","age")
O_matrix[,quick_miss_index] <- rbinom((nrow(origdata)*length(quick_miss_index)),1,0.70)
O_matrix[which(origdata$relate==2),"age"] <- rbinom(length(which(origdata$relate == 2)),1,0.50)
O_matrix[which(origdata$relate==3 | origdata$relate==4 | origdata$relate==5 | origdata$relate==10),"age"] <-
  rbinom(length(which(origdata$relate==3 | origdata$relate==4 | origdata$relate==5 | origdata$relate==10)),1,0.80)
O_matrix[which(origdata$relate==7 | origdata$relate==9),"age"] <-
  rbinom(length(which(origdata$relate==7 | origdata$relate==9)),1,0.60)
O_matrix[which(origdata$age <= 20),"relate"] <- rbinom(length(which(origdata$age <= 20)),1,0.60)
O_matrix[which(origdata$age > 70),"relate"] <- rbinom(length(which(origdata$age > 70)),1,0.45)
O_matrix[which(origdata$age > 20 & origdata$age <= 50),"relate"] <-
  rbinom(length(which(origdata$age > 20 & origdata$age <= 50)),1,0.75)
O_matrix[which(origdata$age > 50 & origdata$age <= 70),"relate"] <-
  rbinom(length(which(origdata$age > 50 & origdata$age <= 70)),1,0.90)
origdata[O_matrix==0] <- NA



###### 11: Poke holes in household-level data; Separate household and individual data
n_all <- length(unique(origdata$Hhindex))
X_house <- NULL; X_house_truth <- NULL;
X_indiv <- NULL; X_indiv_truth <- NULL;
for(i in 1:n_all){
  which_indiv <- which(origdata$Hhindex==i)
  quick_HHmiss_index <- c("ownership","headsex","headrace","headhisp")
  O_miss <- rbinom(length(quick_HHmiss_index),1,0.70)
  O_miss <- matrix(O_miss,byrow=T,nrow=length(which_indiv),ncol=length(quick_HHmiss_index))
  origHHdata_i <- origdata[which_indiv,quick_HHmiss_index]
  origHHdata_i[O_miss==0] <- NA
  origdata[which_indiv,quick_HHmiss_index] <- origHHdata_i

  X_indiv <- rbind(X_indiv,origdata[which_indiv,c("sex","race","hisp","age","relate")])
  X_house <-
    rbind(X_house,cbind(length(which_indiv),origdata[which_indiv[length(which_indiv)],
                                                     c("ownership","headsex","headrace","headhisp","headage")]))
  X_indiv_truth <- rbind(X_indiv_truth,origdata_truth[which_indiv,c("sex","race","hisp","age","relate")])
  X_house_truth <-
    rbind(X_house_truth,cbind(length(which_indiv),origdata_truth[which_indiv[length(which_indiv)],
                                                           c("ownership","headsex","headrace","headhisp","headage")]))
}
colnames(X_house) <- c("Hhindex","ownership","headsex","headrace","headhisp","headage")
X_house <- as.data.frame(X_house)
X_house_truth <- as.data.frame(X_house_truth)


###### 12: Save!!!
write.table(origdata,"origdata.txt",row.names = FALSE)
write.table(origdata_truth,"origdata_truth.txt",row.names = FALSE)
#write.table(X_house, file = "Data_house.txt",row.names = FALSE)
#write.table(X_indiv, file = "Data_indiv.txt",row.names = FALSE)
write.table(X_house_truth, file = "Data_house_truth.txt",row.names = FALSE)
write.table(X_indiv_truth, file = "Data_indiv_truth.txt",row.names = FALSE)
########################## End of Step 1 ##########################


###########################################################################
###########################################################################
#############################***Blank Space***#############################
###########################################################################
###########################################################################
###########################################################################



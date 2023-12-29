library(readxl)
HDBdata <- read_excel("Downloads/HDB_resale_prices (1)/HDB_data_2021_sample.xlsx")

#loading necessary files
library(kknn)
library(tree)
library(rpart)

#creating individual dataframes 
towndata <- HDBdata[, c(1,66:91)] 

#####################################################
##Training and testing data
#####################################################

totalobs = 6000

set.seed(393) 

ntrain=4000 #set size of the training sample

tr = sample(1:totalobs,ntrain)  # draw ntrain observations from original data
train = HDBdata[tr,]   # Training sample
test = HDBdata[-tr,]   # Testing sample

#########
#unsupervised learning
#########

########################
#kernel density estimate
########################

plot(kde(HDBdata$resale_price, h = hlscv(HDBdata$resale_price)), main = "Kernel Density Estimate - min MISE", xlab = "Resale Prices")
abline(v=470000)
#average resale price around $470000

#########
#supervised learning
#########

big.tree = rpart(I(resale_price/1000)~.,method="anova",data=train, minsplit=5,cp=.0005)

bestcp=big.tree$cptable[which.min(big.tree$cptable[,"xerror"]),"CP"] #extract best cp value

#Locate the best tree on CV:
best.tree = prune(big.tree,cp=bestcp) #get tree for best cp on CV

plot(best.tree,uniform=TRUE)
text(best.tree,digits=4,use.n=TRUE,fancy=FALSE,bg='lightblue') 

#######################################
####Linear model for insights
#######################################

linearall = lm(I(resale_price/1000) ~ ., data = HDBdata) #/1000 for readibility of values
summary(linearall)
#most significant variables based on largest t value include remaining lease, floor area, max floor level, dist to CBD, dist to nearest CC

lineartown = lm(I(resale_price/1000) ~ ., data = towndata)
summary(lineartown)
#Bishan, Bukit Timah, Central Area, Queenstown have the greatest effect on HDB resale prices

linearrl = lm(I(resale_price/1000) ~ Remaining_lease, data = train)
summary(linearrl)

lineararea = lm(I(resale_price/1000) ~ floor_area_sqm, data = train)
summary(lineararea)

lineardist = lm(I(resale_price/1000) ~ Dist_CBD, data = train)
summary(lineardist)

linearmaxlvl = lm(I(resale_price/1000) ~ max_floor_lvl, data = train)
summary(linearmaxlvl)

#####
#for prediction
#####

####
#linear regression
####

linearpred = lm(I(resale_price/1000)~Remaining_lease + floor_area_sqm + Dist_CBD + max_floor_lvl + mature + Dist_nearest_CC, train)
summary(linearpred)
predlm=predict.lm(linearpred, test)
mean((test$resale_price/1000-predlm)^2)

######
#KNN
######

HDBcv=train.kknn(I(resale_price/1000)~Remaining_lease + floor_area_sqm + Dist_CBD + max_floor_lvl + mature + Dist_nearest_CC,data=train,kmax=100, kernel = "rectangular")

#Find the best K:
kbest=HDBcv$best.parameters$k

#We find K=3 works best according to LOOCV

#Fit for the selected K:
knnreg = kknn(I(resale_price/1000)~Remaining_lease + floor_area_sqm + Dist_CBD + max_floor_lvl + mature,train,test,k=kbest,kernel = "rectangular")

mean((test$resale_price/1000-knnreg$fitted.values)^2) #test set MSE

##############
#decision tree
##############

treefit=predict(best.tree,newdata=test,type="vector") #prediction on test data
mean((test$resale_price/1000-treefit)^2)


###
#predictions
###

#creating new data frames to predict a HDB price
kallangwhampoaHDB = data.frame(Remaining_lease = 94, floor_area_sqm = 93, Dist_CBD = 4.99, max_floor_lvl = 30, mature = 1, Dist_nearest_CC = 0.54)
#actual listed resale price of this flat is $840000

clementiHDB = data.frame(Remaining_lease = 95, floor_area_sqm = 87, Dist_CBD = 10.33, max_floor_lvl = 30, mature = 1, Dist_nearest_CC = 0.54)
#actual listed resale price of this flat is $750000

#prediction for HDB flat located in Kallang/Whampoa
predict(linearpred, kallangwhampoaHDB)
#linear regression predicted $743000 (3s.f.)
predict(HDBcv, kallangwhampoaHDB)
#knn predicted $837000

#prediction for HDB flat located in Clementi
predict(linearpred, clementiHDB)
#linear regression predicted $661000 (3s.f.)
predict(HDBcv, clementiHDB)
#knn predicted $777000

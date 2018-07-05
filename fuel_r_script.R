library (caret)
library(rpart)
library(rpart.plot)

fuel = read.table('fueldata.csv',sep=',',header=T)


View(fuel)

str(fuel)
fuel$GEARS=as.integer(fuel$GEARS)
fuel$consume=as.factor(fuel$consume)

set.seed(5) #Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(fuel), size = floor(.75*nrow(fuel)), replace = F)
train <- fuel[sample, ]
test  <- fuel[-sample, ]


fueltree1 = rpart(consume~ENG+CLASS+FUEL+CYLINDERS+TRANSMISSION+GEARS+AuthomaticOrManual,data=train,method="class")
predFuel=predict(fueltree1,newdata=test[,c(4:11)],type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree1)

fueltree2 = rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(minsplit = 1))
predFuel=predict(fueltree2,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree2)



fueltree3= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(minsplit =10))
predFuel=predict(fueltree3,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree3)


fueltree4= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(minsplit =30))
predFuel=predict(fueltree4,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree4)


fueltree5= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(minsplit = 50))
predFuel=predict(fueltree5,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree5)

fueltree6= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(minsplit = 80))
predFuel=predict(fueltree6,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree6)


fueltree7= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(minsplit =200))
predFuel=predict(fueltree7,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree7)





fueltree8= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(minbucket =25,minsplit = 40))
predFuel=predict(fueltree8,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree8)

fueltree9= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(cp=0.001))
predFuel=predict(fueltree9  ,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree9)




fueltree10= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(cp=0.005))
predFuel=predict(fueltree10  ,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree10)


fueltree11= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(cp=0.010))
predFuel=predict(fueltree11  ,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree11)



fueltree12= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(cp=0.050))
predFuel=predict(fueltree12  ,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree12)


fueltree13= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(cp=0.100))
predFuel=predict(fueltree13  ,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree13)




fueltree14= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(cp=0.02))
predFuel=predict(fueltree14  ,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree14)



fueltree15= rpart(consume~ENG+CLASS+FUEL+CYLINDERS+GEARS,data=train,method="class",control=rpart.control(cp=0.001,minsplit = 50))
predFuel=predict(fueltree15  ,newdata=test[,c(4:6,8:11)],   type="class")
fuelCM=table(test$consume,predFuel)
fuelCM 
confusionMatrix(predFuel,test$consume)
rpart.plot(fueltree15)


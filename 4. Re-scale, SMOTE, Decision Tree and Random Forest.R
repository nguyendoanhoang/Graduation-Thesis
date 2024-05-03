#rescale dữ liệu để chạy DT và RF, áp dụng rescale cho tập dữ liệu gốc, cluster 1234
#Re-scale tập gốc
data_rescaled <- data1
for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,14)) {
  colname <- names(data_scaled)[i]
  data_rescaled[[colname]] <- round(data_scaled[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]))}
for (i in c(13,15,16)) {
  colname <- names(data_scaled)[i]
  data_rescaled[[colname]] <- round(data_scaled[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]),3)}

##Re-scale tập cluster1
cluster1_rescaled <- cluster1
for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,14)) {
  colname <- names(cluster1)[i]
  cluster1_rescaled[[colname]] <- round(cluster1[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]))}
for (i in c(13,15,16)) {
  colname <- names(cluster1)[i]
  cluster1_rescaled[[colname]] <- round(cluster1[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]),3)}

##Re-scale tập cluster2
cluster2_rescaled <- cluster2
for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,14)) {
  colname <- names(cluster2)[i]
  cluster2_rescaled[[colname]] <- round(cluster2[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]))}
for (i in c(13,15,16)) {
  colname <- names(cluster2)[i]
  cluster2_rescaled[[colname]] <- round(cluster2[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]),3)}

##Re-scale tập cluster3
cluster3_rescaled <- cluster3
for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,14)) {
  colname <- names(cluster3)[i]
  cluster3_rescaled[[colname]] <- round(cluster3[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]))}
for (i in c(13,15,16)) {
  colname <- names(cluster3)[i]
  cluster3_rescaled[[colname]] <- round(cluster3[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]),3)}

##Re-scale tập cluster4
cluster4_rescaled <- cluster4
for (i in c(1,2,3,4,5,6,7,8,9,10,11,12,14)) {
  colname <- names(cluster4)[i]
  cluster4_rescaled[[colname]] <- round(cluster4[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]))}
for (i in c(13,15,16)) {
  colname <- names(cluster4)[i]
  cluster4_rescaled[[colname]] <- round(cluster4[[colname]] * sd(data1[[colname]]) + mean(data1[[colname]]),3)}



#########################
#Chia các tập dữ liệu thành 2 tập train và test cho tập dữ liệu gốc
set.seed(42)
train0 <- createDataPartition(data_rescaled$Attrition_Flag, p = 0.8, list = FALSE)
trainset0 <-  slice(data_rescaled, train0)
testset0 <- slice(data_rescaled, - train0)

#chia tập cho cluster1
set.seed(42)
train1 <- createDataPartition(cluster1_rescaled$Attrition_Flag, p = 0.8, list = FALSE)
trainset1 <-  slice(cluster1_rescaled, train1)
testset1 <- slice(cluster1_rescaled, - train1)

#chia tập cho cluster2
set.seed(42)
train2 <- createDataPartition(cluster2_rescaled$Attrition_Flag, p = 0.8, list = FALSE)
trainset2 <-  slice(cluster2_rescaled, train2)
testset2 <- slice(cluster2_rescaled, - train2)

#chia tập cho cluster3
set.seed(42)
train3 <- createDataPartition(cluster3_rescaled$Attrition_Flag, p = 0.8, list = FALSE)
trainset3 <-  slice(cluster3_rescaled, train3)
testset3 <- slice(cluster3_rescaled, - train3)

#chia tập cho cluster4
set.seed(42)
train4 <- createDataPartition(cluster4_rescaled$Attrition_Flag, p = 0.8, list = FALSE)
trainset4 <-  slice(cluster4_rescaled, train4)
testset4 <- slice(cluster4_rescaled, - train4)

#tỷ trọng rời bỏ của từng tập dữ liệu
freq_table0 <- table(trainset0$Attrition_Flag)
print(freq_table0)
freq_table1 <- table(trainset1$Attrition_Flag)
print(freq_table1)
freq_table2 <- table(trainset2$Attrition_Flag)
print(freq_table2)
freq_table3 <- table(trainset3$Attrition_Flag)
print(freq_table3)
freq_table4 <- table(trainset4$Attrition_Flag)
print(freq_table4)


############################áp dụng smote cho tập trainset0 dữ liệu gốc
set.seed(42)
smotedtrain0 <- SMOTE(X = trainset0[,-1], target = trainset0[,1], dup_size = 4)
smotedtrain0 <- smotedtrain0$data #chỉ lấy ra dữ liệu từ cái vừa chạy smote xong
str(smotedtrain0)
#bảng tuần số và tần suất 2 lớp
prop.table(table(smotedtrain0$class))
table(smotedtrain0$class)
#kiểm tra lại xem có phải smote real không:))))))0
get_dupes(smotedtrain0)
#check  structure
str(smotedtrain0)

#áp dụng smote cho tập trainset1
set.seed(42)
smotedtrain1 <- SMOTE(X = trainset1[,-1], target = trainset1[,1], dup_size = 4)
#chỉ lấy ra dữ liệu từ cái vừa chạy smote xong
smotedtrain1 <- smotedtrain1$data
#bảng tuần số và tần suất 2 lớp
prop.table(table(smotedtrain1$class))
table(smotedtrain1$class)

#áp dụng smote cho tập trainset2
set.seed(42)
smotedtrain2 <- SMOTE(X = trainset2[,-1], target = trainset2[,1], dup_size = 3)
#chỉ lấy ra dữ liệu từ cái vừa chạy smote xong
smotedtrain2 <- smotedtrain2$data
#bảng tuần số và tần suất 2 lớp
prop.table(table(smotedtrain2$class))
table(smotedtrain2$class)

#áp dụng smote cho tập trainset3
set.seed(42)
smotedtrain3 <- SMOTE(X = trainset3[,-1], target = trainset3[,1], dup_size = 16)
#chỉ lấy ra dữ liệu từ cái vừa chạy smote xong
smotedtrain3 <- smotedtrain3$data
#bảng tuần số và tần suất 2 lớp
prop.table(table(smotedtrain3$class))
table(smotedtrain3$class)

#áp dụng smote cho tập trainset4
set.seed(42)
smotedtrain4 <- SMOTE(X = trainset4[,-1], target = trainset4[,1], dup_size = 4)
#chỉ lấy ra dữ liệu từ cái vừa chạy smote xong
smotedtrain4 <- smotedtrain4$data
#bảng tuần số và tần suất 2 lớp
prop.table(table(smotedtrain4$class))
table(smotedtrain4$class)

#chuyển biến mục tiêu sang dạng factor
smotedtrain0$class <-  as.factor(smotedtrain0$class)
smotedtrain1$class <-  as.factor(smotedtrain1$class)
smotedtrain2$class <-  as.factor(smotedtrain2$class)
smotedtrain3$class <-  as.factor(smotedtrain3$class)
smotedtrain4$class <-  as.factor(smotedtrain4$class)

testset0$Attrition_Flag <-  as.factor(testset0$Attrition_Flag)
testset1$Attrition_Flag <-  as.factor(testset1$Attrition_Flag)
testset2$Attrition_Flag <-  as.factor(testset2$Attrition_Flag)
testset3$Attrition_Flag <-  as.factor(testset3$Attrition_Flag)
testset4$Attrition_Flag <-  as.factor(testset4$Attrition_Flag)


############################DECISION TREEEEEEEEEEE
#Tập dữ liệu gốc
#vẽ cây : Mo hinh cay voi minbucket = 50 (Toi thieu 50 quan sat trong nhanh la)
tree1 = rpart(class ~ ., method="class", data = smotedtrain0, control=rpart.control(minbucket=50))
rpart.plot(tree1)
# Xac dinh cac tham so cua pp cross-validation
# 10-fold cross-validation, voi gia tri tham so cp trong khoang 0.001-0.1
fitControl = trainControl( method = "cv", number = 10)
cartGrid = expand.grid( .cp=c(1:1000)*0.001) 
train(class ~ ., data = smotedtrain0, na.action  = na.pass, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
# Tao mo hinh cay voi gia tri cp tao ra sai so nho nhat theo cross-validation
tree2 = rpart(class ~ ., data = smotedtrain0, method="class", control=rpart.control(cp = 0.001))
# Du bao va tinh toan sai so tren mau kiem dinh - mo hinh tree0
dt_pred = predict(tree2, newdata = testset0, type = "class")
table(testset0$Attrition_Flag, dt_pred)
matrix = table(testset0$Attrition_Flag, dt_pred)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset0))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore


#DECISION TREE cho cluster 1
#vẽ cây : Mo hinh cay voi minbucket = 50 (Toi thieu 50 quan sat trong nhanh la)
tree1 = rpart(class ~ ., method="class", data = smotedtrain1, control=rpart.control(minbucket=50))
rpart.plot(tree1)
# Xac dinh cac tham so cua pp cross-validation
# 10-fold cross-validation, voi gia tri tham so cp trong khoang 0.001-0.1
fitControl = trainControl( method = "cv", number = 10)
cartGrid = expand.grid( .cp=c(1:1000)*0.001) 
train(class ~ ., data = smotedtrain1, na.action  = na.pass, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)

# Tao mo hinh cay voi gia tri cp tao ra sai so nho nhat theo cross-validation
tree2 = rpart(class ~ ., data = smotedtrain1, method="class", control=rpart.control(cp = 0.001))
# Du bao va tinh toan sai so tren mau kiem dinh - mo hinh tree0
dt_pred = predict(tree2, newdata = testset1, type = "class")
table(testset1$Attrition_Flag, dt_pred)
matrix = table(testset1$Attrition_Flag, dt_pred)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset1))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore


#DECISION TREE cho cluster 2
#vẽ cây : Mo hinh cay voi minbucket = 50 (Toi thieu 50 quan sat trong nhanh la)
tree1 = rpart(class ~ ., method="class", data = smotedtrain2, control=rpart.control(minbucket=50))
rpart.plot(tree1)
# Xac dinh cac tham so cua pp cross-validation
# 10-fold cross-validation, voi gia tri tham so cp trong khoang 0.001-0.1
fitControl = trainControl( method = "cv", number = 10)
cartGrid = expand.grid( .cp=c(1:1000)*0.001) 
train(class ~ ., data = smotedtrain2, na.action  = na.pass, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)

# Tao mo hinh cay voi gia tri cp tao ra sai so nho nhat theo cross-validation
tree2 = rpart(class ~ ., data = smotedtrain2, method="class", control=rpart.control(cp = 0.001))
# Du bao va tinh toan sai so tren mau kiem dinh - mo hinh tree0
dt_pred = predict(tree2, newdata = testset2, type = "class")
table(testset2$Attrition_Flag, dt_pred)
matrix = table(testset2$Attrition_Flag, dt_pred)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset2))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore


#DECISION TREE cho cluster 3
#vẽ cây : Mo hinh cay voi minbucket = 50 (Toi thieu 50 quan sat trong nhanh la)
tree1 = rpart(class ~ ., method="class", data = smotedtrain3, control=rpart.control(minbucket=50))
rpart.plot(tree1)
# Xac dinh cac tham so cua pp cross-validation
# 10-fold cross-validation, voi gia tri tham so cp trong khoang 0.001-0.1
fitControl = trainControl( method = "cv", number = 10)
cartGrid = expand.grid( .cp=c(1:1000)*0.001) 
train(class ~ ., data = smotedtrain3, na.action  = na.pass, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)

# Tao mo hinh cay voi gia tri cp tao ra sai so nho nhat theo cross-validation
tree2 = rpart(class ~ ., data = smotedtrain3, method="class", control=rpart.control(cp = 0.007))
# Du bao va tinh toan sai so tren mau kiem dinh - mo hinh tree0
dt_pred = predict(tree2, newdata = testset3, type = "class")
table(testset3$Attrition_Flag, dt_pred)
matrix = table(testset3$Attrition_Flag, dt_pred)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset3))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore


#DECISION TREE cho cluster 4
#vẽ cây : Mo hinh cay voi minbucket = 50 (Toi thieu 50 quan sat trong nhanh la)
tree1 = rpart(class ~ ., method="class", data = smotedtrain4, control=rpart.control(minbucket=50))
rpart.plot(tree1)
# Xac dinh cac tham so cua pp cross-validation
# 10-fold cross-validation, voi gia tri tham so cp trong khoang 0.001-0.1
fitControl = trainControl( method = "cv", number = 10)
cartGrid = expand.grid( .cp=c(1:1000)*0.001) 
train(class ~ ., data = smotedtrain4, na.action  = na.pass, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)

# Tao mo hinh cay voi gia tri cp tao ra sai so nho nhat theo cross-validation
tree2 = rpart(class ~ ., data = smotedtrain4, method="class", control=rpart.control(cp = 0.007))
# Du bao va tinh toan sai so tren mau kiem dinh - mo hinh tree0
dt_pred = predict(tree2, newdata = testset4, type = "class")
table(testset4$Attrition_Flag, dt_pred)
matrix = table(testset4$Attrition_Flag, dt_pred)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset4))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore


###############################RANDOM FOREST cho tập dữ liệu smotedtran0
set.seed(42)
forest = randomForest(class ~ ., data = smotedtrain0)
forest
PredictForest = predict(forest, newdata = testset0)
table(testset0$Attrition_Flag, PredictForest)
matrix = table(testset0$Attrition_Flag, PredictForest)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset0))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
NPV <- (matrix[2,2])/(matrix[2,2]+matrix[1,2])
NPV
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore

#các biến quan trọng nhất theo accuracy và Gini
set.seed(42)
forest = randomForest(class ~ ., data = smotedtrain0, importance=TRUE)
varImpPlot(forest, main = "Important Predictors in Random Forest")
forest

#RANDOM FOREST cho tập dữ liệu smotedtran1
set.seed(42)
forest = randomForest(class ~ ., data = smotedtrain1)
forest
PredictForest = predict(forest, newdata = testset1)
table(testset1$Attrition_Flag, PredictForest)
matrix = table(testset1$Attrition_Flag, PredictForest)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset1))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
NPV <- (matrix[2,2])/(matrix[2,2]+matrix[1,2])
NPV
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore


#RANDOM FOREST cho tập dữ liệu smotedtran2
set.seed(42)
forest = randomForest(class ~ ., data = smotedtrain2)
forest
PredictForest = predict(forest, newdata = testset2)
table(testset2$Attrition_Flag, PredictForest)
matrix = table(testset2$Attrition_Flag, PredictForest)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset2))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
NPV <- (matrix[2,2])/(matrix[2,2]+matrix[1,2])
NPV
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore


#RANDOM FOREST cho tập dữ liệu smotedtran3
set.seed(42)
forest = randomForest(class ~ ., data = smotedtrain3)
forest
PredictForest = predict(forest, newdata = testset3)
table(testset3$Attrition_Flag, PredictForest)
matrix = table(testset3$Attrition_Flag, PredictForest)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset3))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
NPV <- (matrix[2,2])/(matrix[2,2]+matrix[1,2])
NPV
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore


#RANDOM FOREST cho tập dữ liệu smotedtran4
set.seed(42)
forest = randomForest(class ~ ., data = smotedtrain4)
forest
PredictForest = predict(forest, newdata = testset4)
table(testset4$Attrition_Flag, PredictForest)
matrix = table(testset4$Attrition_Flag, PredictForest)
#hiệu suất
Accuracy <-  (matrix[1,1]+matrix[2,2])/(nrow(testset4))
Accuracy
Precision  <- (matrix[1,1])/(matrix[1,1]+matrix[2,1])
Precision
Sensitivity <- (matrix[1,1])/(matrix[1,1]+matrix[1,2])
Sensitivity
Specificity <- (matrix[2,2])/(matrix[2,2]+matrix[2,1])
Specificity
NPV <- (matrix[2,2])/(matrix[2,2]+matrix[1,2])
NPV
Fscore <- 2*Sensitivity*Precision/(Sensitivity+Precision)
Fscore
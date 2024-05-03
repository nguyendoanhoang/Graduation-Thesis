########################Chia các tập dữ liệu thành 2 tập train và test cho tập dữ liệu gốc
set.seed(42)
train0 <- createDataPartition(data_scaled$Attrition_Flag, p = 0.8, list = FALSE)
trainset0 <-  slice(data_scaled, train0)
testset0 <- slice(data_scaled, - train0)

#chia tập cho cluster1
set.seed(42)
train1 <- createDataPartition(cluster1$Attrition_Flag, p = 0.8, list = FALSE)
trainset1 <-  slice(cluster1, train1)
testset1 <- slice(cluster1, - train1)

#chia tập cho cluster2
set.seed(42)
train2 <- createDataPartition(cluster2$Attrition_Flag, p = 0.8, list = FALSE)
trainset2 <-  slice(cluster2, train2)
testset2 <- slice(cluster2, - train2)

#chia tập cho cluster3
set.seed(42)
train3 <- createDataPartition(cluster3$Attrition_Flag, p = 0.8, list = FALSE)
trainset3 <-  slice(cluster3, train3)
testset3 <- slice(cluster3, - train3)

#chia tập cho cluster4
set.seed(42)
train4 <- createDataPartition(cluster4$Attrition_Flag, p = 0.8, list = FALSE)
trainset4 <-  slice(cluster4, train4)
testset4 <- slice(cluster4, - train4)

#######################tỷ trọng rời bỏ của từng tập dữ liệu
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

#############Chuyển biến mục tiêu class trong tập smotedtrain về dạng 0 1
smotedtrain0$class <- ifelse(smotedtrain0$class == "-0.43748470095969", 0, 1)
smotedtrain1$class <- ifelse(smotedtrain1$class == "-0.43748470095969", 0, 1)
smotedtrain2$class <- ifelse(smotedtrain2$class == "-0.43748470095969", 0, 1)
smotedtrain3$class <- ifelse(smotedtrain3$class == "-0.43748470095969", 0, 1)
smotedtrain4$class <- ifelse(smotedtrain4$class == "-0.43748470095969", 0, 1)

###############Chuyển biến mục tiêu Attrition_Flag trong tập testset về dạng 0 1 và chuyển sang kiểu factor để lập confusionmatrix
testset0$Attrition_Flag <- ifelse(testset0$Attrition_Flag == "-0.43748470095969", 0, 1)
testset0$Attrition_Flag <- as.factor(testset0$Attrition_Flag)
testset1$Attrition_Flag <- ifelse(testset1$Attrition_Flag == "-0.43748470095969", 0, 1)
testset1$Attrition_Flag <- as.factor(testset1$Attrition_Flag)
testset2$Attrition_Flag <- ifelse(testset2$Attrition_Flag == "-0.43748470095969", 0, 1)
testset2$Attrition_Flag <- as.factor(testset2$Attrition_Flag)
testset3$Attrition_Flag <- ifelse(testset3$Attrition_Flag == "-0.43748470095969", 0, 1)
testset3$Attrition_Flag <- as.factor(testset3$Attrition_Flag)
testset4$Attrition_Flag <- ifelse(testset4$Attrition_Flag == "-0.43748470095969", 0, 1)
testset4$Attrition_Flag <- as.factor(testset4$Attrition_Flag)

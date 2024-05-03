###############################Mô hình phân loại Maive Bayes
set.seed(42)
nb0 <- naiveBayes(class ~ ., data =smotedtrain0)
nb0
#dự đoán dựa trên tập thử nghiệm
nb_pred0 <- predict(nb0, newdata = testset0)
summary(nb_pred0)
str(nb_pred0)
# Confusion Matrix
cm_nb0 <- confusionMatrix(nb_pred0, testset0$Attrition_Flag, positive = "0")
cm_nb0
#tính Fscore
F_score_nb0 <- 2*cm_nb0$byClass[1]*cm_nb0$byClass[3]/(cm_nb0$byClass[1]+cm_nb0$byClass[3])
F_score_nb0

#Mô hình phân loại Maive Bayes cho CỤM 1
set.seed(42)
nb1 <- naiveBayes(class ~ ., data =smotedtrain1)
nb1
#dự đoán dựa trên tập thử nghiệm
nb_pred1 <- predict(nb1, newdata = testset1)
summary(nb_pred1)
str(nb_pred1)
# Confusion Matrix
cm_nb1 <- confusionMatrix(nb_pred1, testset1$Attrition_Flag, positive = '0')
cm_nb1
#tính Fscore
F_score_nb1 <- 2*cm_nb1$byClass[1]*cm_nb1$byClass[3]/(cm_nb1$byClass[1]+cm_nb1$byClass[3])
F_score_nb1
#Mô hình phân loại Maive Bayes cho CỤM 2
set.seed(42)
nb2 <- naiveBayes(class ~ ., data =smotedtrain2)
nb2
#dự đoán dựa trên tập thử nghiệm
nb_pred2 <- predict(nb2, newdata = testset2)
summary(nb_pred2)
str(nb_pred2)
# Confusion Matrix
cm_nb2 <- confusionMatrix(nb_pred2, testset2$Attrition_Flag, positive = '0')
cm_nb2
#tính Fscore
F_score_nb2 <- 2*cm_nb2$byClass[1]*cm_nb2$byClass[3]/(cm_nb2$byClass[1]+cm_nb2$byClass[3])
F_score_nb2
#Mô hình phân loại Maive Bayes cho CỤM 3
set.seed(42)
nb3 <- naiveBayes(class ~ ., data =smotedtrain3)
nb3
#dự đoán dựa trên tập thử nghiệm
nb_pred3 <- predict(nb3, newdata = testset3)
summary(nb_pred3)
str(nb_pred3)
# Confusion Matrix
cm_nb3 <- confusionMatrix(nb_pred3, testset3$Attrition_Flag, positive = '0')
cm_nb3
#tính Fscore
F_score_nb3 <- 2*cm_nb3$byClass[1]*cm_nb3$byClass[3]/(cm_nb3$byClass[1]+cm_nb3$byClass[3])
F_score_nb3
#Mô hình phân loại Maive Bayes cho CỤM 4
set.seed(42)
nb4 <- naiveBayes(class ~ ., data =smotedtrain3)
nb4
#dự đoán dựa trên tập thử nghiệm
nb_pred4 <- predict(nb4, newdata = testset4)
summary(nb_pred4)
str(nb_pred4)
# Confusion Matrix
cm_nb4 <- confusionMatrix(nb_pred4, testset4$Attrition_Flag, positive = '0')
cm_nb4
#tính Fscore
F_score_nb4 <- 2*cm_nb4$byClass[1]*cm_nb4$byClass[3]/(cm_nb4$byClass[1]+cm_nb4$byClass[3])
F_score_nb4


#########################Hồi quy LOGISTIC cho dữ liệu smotedtrain0
lr0 <- glm(class~., data= smotedtrain0, family = binomial(link="logit"))
lr_pred0 <-  predict(lr0, newdata = testset0, type = "response")
summary(lr0)
cm_lr0 <- confusionMatrix(data = as.factor(as.numeric(lr_pred0>0.5)),
                          reference = testset0$Attrition_Flag, positive = '0')
cm_lr0
F_score_lr0 <- 2*cm_lr0$byClass[1]*cm_lr0$byClass[3]/(cm_lr0$byClass[1]+cm_lr0$byClass[3])
F_score_lr0

#Hồi quy LOGISTIC cho CỤM 1
lr1 <- glm(class~., data= smotedtrain1, family = binomial(link="logit"))
summary(lr1)
lr_pred1 <-  predict(lr1, newdata = testset1, type = "response")
cm_lr1 <- confusionMatrix(data = as.factor(as.numeric(lr_pred1>0.5)),
                          reference = testset1$Attrition_Flag, positive = '0')
cm_lr1
F_score_lr1 <- 2*cm_lr1$byClass[1]*cm_lr1$byClass[3]/(cm_lr1$byClass[1]+cm_lr1$byClass[3])
F_score_lr1

#Hồi quy LOGISTIC cho CỤM 2
lr2 <- glm(class~., data= smotedtrain2, family = binomial(link="logit"))
summary(lr2)
lr_pred2 <-  predict(lr2, newdata = testset2, type = "response")
cm_lr2 <- confusionMatrix(data = as.factor(as.numeric(lr_pred2>0.5)),
                          reference = testset2$Attrition_Flag, positive = '0')
cm_lr2
F_score_lr2 <- 2*cm_lr2$byClass[1]*cm_lr2$byClass[3]/(cm_lr2$byClass[1]+cm_lr2$byClass[3])
F_score_lr2

#Hồi quy LOGISTIC cho CỤM 3
lr3 <- glm(class~., data= smotedtrain3, family = binomial(link="logit"))
summary(lr3)
lr_pred3 <-  predict(lr3, newdata = testset3, type = "response")
cm_lr3 <- confusionMatrix(data = as.factor(as.numeric(lr_pred3>0.5)),
                          reference = testset3$Attrition_Flag, positive = '0')
cm_lr3
F_score_lr3 <- 2*cm_lr3$byClass[1]*cm_lr3$byClass[3]/(cm_lr3$byClass[1]+cm_lr3$byClass[3])
F_score_lr3

#Hồi quy LOGISTIC cho CỤM 4
lr4 <- glm(class~., data= smotedtrain4, family = binomial(link="logit"))
summary(lr4)
lr_pred4 <-  predict(lr4, newdata = testset4, type = "response")
cm_lr4 <- confusionMatrix(data = as.factor(as.numeric(lr_pred4>0.5)),
                          reference = testset4$Attrition_Flag, positive = '0')
cm_lr4
F_score_lr4 <- 2*cm_lr4$byClass[1]*cm_lr4$byClass[3]/(cm_lr4$byClass[1]+cm_lr4$byClass[3])
F_score_lr4



#Mức độ quan trọng của các thuộc tính
importances <- varImp(lr0)
importances %>% arrange(desc(Overall))



###############################Máy Vector hỗ trợ
svm0 <-  svm(formula = class ~., data = smotedtrain0,
             type = 'C-classification', kernel= 'radial')
svm0
svm_pred0 <-  predict(svm0, newdata = testset0)
cm_svm0 <- confusionMatrix(svm_pred0, testset0$Attrition_Flag, positive = '0')
cm_svm0
F_score_svm0 <- 2*cm_svm0$byClass[1]*cm_svm0$byClass[3]/(cm_svm0$byClass[1]+cm_svm0$byClass[3])
F_score_svm0


#Máy Vector hỗ trợ cho cụm 1
svm1 <-  svm(formula = class ~., data = smotedtrain1,
             type = 'C-classification', kernel= 'radial')
svm1
svm_pred1 <-  predict(svm1, newdata = testset1)
cm_svm1 <- confusionMatrix(svm_pred1, testset1$Attrition_Flag, positive = '0')
cm_svm1
F_score_svm1 <- 2*cm_svm1$byClass[1]*cm_svm1$byClass[3]/(cm_svm1$byClass[1]+cm_svm1$byClass[3])
F_score_svm1

#Máy Vector hỗ trợ cho cụm 2
svm2 <-  svm(formula = class ~., data = smotedtrain2,
             type = 'C-classification', kernel= 'radial')
svm2
svm_pred2 <-  predict(svm2, newdata = testset2)
cm_svm2 <- confusionMatrix(svm_pred2, testset2$Attrition_Flag, positive = '0')
cm_svm2
F_score_svm2 <- 2*cm_svm2$byClass[1]*cm_svm2$byClass[3]/(cm_svm2$byClass[1]+cm_svm2$byClass[3])
F_score_svm2

#Máy Vector hỗ trợ cho cụm 3
svm3 <-  svm(formula = class ~., data = smotedtrain3,
             type = 'C-classification', kernel= 'radial')
svm3
svm_pred3 <-  predict(svm3, newdata = testset3)
cm_svm3 <- confusionMatrix(svm_pred3, testset3$Attrition_Flag, positive = '0')
cm_svm3
F_score_svm3 <- 2*cm_svm3$byClass[1]*cm_svm3$byClass[3]/(cm_svm3$byClass[1]+cm_svm3$byClass[3])
F_score_svm3

#Máy Vector hỗ trợ cho cụm 4
svm4 <-  svm(formula = class ~., data = smotedtrain4,
             type = 'C-classification', kernel= 'radial')
svm4
svm_pred4 <-  predict(svm4, newdata = testset4)
cm_svm4 <- confusionMatrix(svm_pred4, testset4$Attrition_Flag, positive = '0')
cm_svm4
F_score_svm4 <- 2*cm_svm4$byClass[1]*cm_svm4$byClass[3]/(cm_svm4$byClass[1]+cm_svm4$byClass[3])
F_score_svm4
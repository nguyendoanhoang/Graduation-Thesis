library(tidyr)
library(tidyverse)
library(dplyr)
library(janitor) #kiểm tra dữ liệu trùng lặp
library(grid) #gói base
library(lattice) #gói yêu cầu của DMwR
# Cài đặt devtools nếu bạn chưa có
#install.packages("devtools")
# Sử dụng devtools để cài đặt gói DMwR từ GitHub
#devtools::install_github("cran/DMwR")
library(DMwR) #dùng cho smote
library(ROSE) #dùng cho smote
library(smotefamily)
library(tables) #lập bảng
library(ggplot2)
library(caret) #confusion matrix, chia dữ liệu
library(MASS)
library(modelr) # hồi quy logistic
library(broom)
library(ISLR)
library(e1071) # naive bayes + svm
library(psych) # naive bayes
library(rpart) # decision tree
library(randomForest)
library(rpart.plot)
library(caTools)# KNN
library(class)#KNN
library(pROC) # đánh giá mô hình ROC AUV
library(highcharter) #heatmap
library(factoextra)
library(PreProcess)
library(DAAG)   # decision tree
library(party) # decision tree
library(mlbench) # decision tree
library(tree) # decision tree

#đọc dữ liệu
data_original <- read.csv("G:/My Drive/4 Senior/Chuyên đề thực tập/BankChurners/BankChurners.csv")

#bỏ biến ID và 2 biến suy ra từ Naive Bayes
data <- data_original[,-c(1,22,23)]

#sao chép dataset để đg tương quan
data_cor <- data

#Kiểm tra missing
colSums(is.na(data))

#chuyển biến mục tiêuL: khách hàng rời bỏ: 1, khách hàng không rời bỏ: 0
sub_target <- function(x){
  if(x == "Existing Customer"){
    return(0)
  } else {
    return(1)
  }
}
data_cor$Attrition_Flag <- sapply(data_cor$Attrition_Flag, sub_target)

#Kiểm tra dữ kiểu dữ liệu
str(data_cor)

#Tính toán hệ số tương quan
cor_spearman <- cor(data_cor[, sapply(data_cor, is.numeric)], method = 'spearman')
#biểu diễn ma trận hệ số tương quan bằng heatmap
as.matrix(data.frame(cor_spearman)) %>%
  round(3) %>% #round
  hchart() %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_title(text = "Spearman's correlation coefficients", align = "center") %>%
  hc_legend(align = "center") %>%
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10))) %>%
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)))

#Loại các biến Total_Revolving_Bal, Total_Trans_Ct, Avg_Open_To_Buy, và Customer_Age ra khỏi mô hình do có hệ số tương quan cao
data1 <- data_cor[, !(names(data) %in% c("Total_Revolving_Bal", "Total_Trans_Ct", "Avg_Open_To_Buy", "Customer_Age"))]

str(data1)

#chuyển các biến độc lập (phân loại) về dạng factor theo quy ước
data1$Gender <- ifelse(data1$Gender == "F", 0, 1)
data1$Education_Level <- ifelse(data1$Education_Level == "Unknown", 0,
                                ifelse(data1$Education_Level == "Uneducated", 1,
                                       ifelse(data1$Education_Level == "High School", 2,
                                              ifelse(data1$Education_Level == "College", 3,
                                                     ifelse(data1$Education_Level == "Graduate", 4,
                                                            ifelse(data1$Education_Level == "Post-Graduate", 5,
                                                                   ifelse(data1$Education_Level == "Doctorate", 6, NA)))))))
data1$Marital_Status <- ifelse(data1$Marital_Status == "Unknown", 0,
                               ifelse(data1$Marital_Status == "Divorced", 1,
                                      ifelse(data1$Marital_Status == "Single", 2,
                                             ifelse(data1$Marital_Status == "Married", 3, NA))))
data1$Income_Category <- ifelse(data1$Income_Category == "Unknown", 0,
                                ifelse(data1$Income_Category =="Less than $40K", 1,
                                       ifelse(data1$Income_Category =="$40K - $60K", 2,
                                              ifelse(data1$Income_Category =="$60K - $80K", 3,
                                                     ifelse(data1$Income_Category =="$80K - $120K", 4,
                                                            ifelse(data1$Income_Category =="$120K +", 5, NA))))))
data1$Card_Category <- ifelse(data1$Card_Category == "Blue", 0,
                              ifelse(data1$Card_Category == "Silver", 1,
                                     ifelse(data1$Card_Category == "Gold", 2,
                                            ifelse(data1$Card_Category == "Platinum", 3, NA))))
str(data1)

#chuẩn hóa dữ liệu: scale(): Hàm này chuẩn hóa dữ liệu bằng cách trừ đi trung bình và chia cho độ lệch chuẩn.
#biến mục tiêu k chuẩn hóa
data_scaled <- scale(data1)
data_scaled <- as.data.frame(data_scaled)
str(data_scaled)

#Phân cụm K-Means
set.seed(42)
for (k in 1:10){
  assign(paste("KMC",k), kmeans(data_scaled, centers = k, iter.max = 10000))
}

#xác định số cụm tối ưu => chọn k=4
set.seed(42)
fviz_nbclust(
  data_scaled,
  kmeans,
  method = "wss") +
  labs(subtite="Elbow method") +
  geom_vline(xintercept = 4, linetype = 2)

#biểu đồ
Num_of_Clusters = seq(1,10,1)
Total_Withiniss = c(`KMC 1`$tot.withinss, `KMC 2`$tot.withinss, `KMC 3`$tot.withinss,
                    `KMC 4`$tot.withinss, `KMC 5`$tot.withinss, `KMC 6`$tot.withinss,
                    `KMC 7`$tot.withinss, `KMC 8`$tot.withinss, `KMC 9`$tot.withinss,
                    `KMC 10`$tot.withinss)
plot(Num_of_Clusters, Total_Withiniss, type="b")

#Dựa vào biểu đồ, 4 cụm là đủ để nghiên cứu về khách hàng ngân hàng
str(`KMC 4`)
Cluster_Size = `KMC 4`$size/10127 * 100
Cluster_Size

setwd("G:/My Drive/4 Senior/Chuyên đề thực tập/Bản thảo")
#Lưu các cụm vào file csv
set.seed(42)
cluster_labels <- kmeans(data_scaled, centers = 4)$cluster
filenames <- paste0("cluster", c(1, 2, 3, 4), ".csv")
for (i in 1:4) {
  data1_clustered <- data_scaled[cluster_labels == i, ]
  write.csv(data1_clustered, filenames[i], row.names = FALSE)
}

######################Đưa 4 cụm vừa phân được vào môi trường
cluster1 <- read.csv("G:/My Drive/4 Senior/Chuyên đề thực tập/Bản thảo/cluster1.csv")
cluster2 <- read.csv("G:/My Drive/4 Senior/Chuyên đề thực tập/Bản thảo/cluster2.csv")
cluster3 <- read.csv("G:/My Drive/4 Senior/Chuyên đề thực tập/Bản thảo/cluster3.csv")
cluster4 <- read.csv("G:/My Drive/4 Senior/Chuyên đề thực tập/Bản thảo/cluster4.csv")
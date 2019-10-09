list_z<-c()
file_n<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\data\\combn\\aac\\Merged_aac_with non_AR_S_6", ".csv")

data <- read.csv(file_n, header = TRUE)

z<-length(data)

col_n<-colnames(data)

#col_n
#col_n[6]
#d<-0
file_name1<-paste0("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Histon acetyltransferase\\Merged AAC_essngeneplushistone\\pseudovibenterononarwith histone\\aac_merged",".csv")
data1 <- read.csv(file_name1, header = TRUE)
#data1[[1]]

#data1[[2]]
d<-length(data1)
#z1
col_n1<-colnames(data1)




for(i in 1:z){
  for(j in 2:d){
    if(col_n[i]==col_n1[j]){
      list_z<-c(list_z, data1[j])
    }
  }
  
}

file_over<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\Pseudovibentero\\aac_merged_feature_under6",".csv")

write.csv(list_z, file_over, row.names = FALSE)



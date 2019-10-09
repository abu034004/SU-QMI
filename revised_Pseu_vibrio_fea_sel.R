library(data.table)


file_n<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\data\\combn\\bl_after_del_row_no_15\\Merged_bl_with non_AR_S_2", ".csv")
data <- read.csv(file_n, header = TRUE)

#z<-length(data)
z<-ncol(data)
z
col_n<-colnames(data)

#col_n
#col_n[6]
#d<-0
file_name1<-paste0("E:\\Research\\All ML works\\Each AMR gene\\split\\Sequence\\Test1\\joined\\separate\\Filtered\\Pseudomonas Vibrio Enterobacter\\bl\\merged_bl\\bl_merged",".csv")
data1 <- read.csv(file_name1, header = TRUE)
#data1[[1]]

#data1[[2]]
ncol(data1)
d<-ncol(data1)
data1[,623]

col_n1<-colnames(data1)
col_n1[2]

common_cols <- intersect(col_n, col_n1)

list_z<-subset(data1, select = common_cols)

file_over<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\Pseudovibentero\\bl_after_del_row_15\\bl_merged_feature_under2",".csv")

fwrite(list_z, file_over)

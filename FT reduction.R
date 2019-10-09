#install.packages("stringr")
#install.packages("HapEstXXR")
#install.packages("sets")

library(sets)
require('HapEstXXR')
library(stringr)
#install.packages("stringi")
library(stringi)
#install.packages("dplyr")
#library(dplyr)
library(infotheo)
#install.packages("proxy")
library(proxy)
#pr_DB$get_entry("eJaccard")
library(FSelector)
library(randomForest)
#data(iris)
#summary(iris)

#df <- read.csv("E:\\Research\\All ML works\\Graph Theory approach\\data\\Updated_merged_aac_with non_AR.csv", header=TRUE)
df <- read.csv("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_BL\\Merged_BL_with non_AR_Final_exclude1.csv", header=TRUE)
#df1 <- read.csv("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_AAC\\Merged_aac_with non_AR_normalized.csv", header=TRUE)
df<-df[,-1]
df_actual<-df
#df$Output<-as.factor(df$Output)

df$Output[df$Output==-1] <- "No"
df$Output[df$Output==1] <- "Yes"


weights <- symmetrical.uncertainty(Output~., df)
weights
#sort(weights)
lf<-weights$attr_importance
lf
#print(weights)
#print(lf[621])



#df$Output <- as.character(df$Output)
df$Output <- as.factor(df$Output)


fit_rf = randomForest(Output~., data=df)
gindex<-importance(fit_rf)
#str(df)
#gindex[619]

ri<-matrix(0,621,621)

df<-discretize(df)

for (i in 1:621){
  print(i)
  
  for (j in 1:621){
    if(i!=j){
      ri[i,j]<-2*(condinformation(df[,i], df[,622], S=df[,j], method="emp")- gindex[i]* mutinformation(df[,i], df[,622], method="emp"))/(entropy(df[,i], method="emp")+entropy(df[,622], method="emp"))
      #eucl
      
    }
  }
}

list_z<-c()
col_added<-c()
flag<-matrix(0,1,621)
mul<-matrix(0,1,621)
wt<-matrix(0,1,621)

maximum<-0
index<-0

for(i in 1:621){
  
    if(maximum<lf[i]){
      maximum =lf[i]
      index =i
    }
  
  
}

flag[index]<-1

list_z<-c(list_z, df[index])
#list_z[[1]]
col_added<-c(col_added,colnames(df[index]))
#col_added[1]
#index
#lf
posit<-grep(col_added[1], colnames(df))
#posit
for(i in 2:30){
  print(i)
  len_sel<-length(col_added)
  for(k in 1:len_sel){
    sm<-0
    for(b in k:len_sel){
      sm<- sm+ 1/b
    }
  wt[k]<-sm/len_sel
  }
  
  for(j in 1:621){
    if(flag[j]!=1){
    sm<-0
    
    for(k in 1:len_sel){
      posit<-grep(col_added[k], colnames(df))
     sm<-sm + wt[k]*ri[j,posit] 
    }
    
    mul[j] = lf[j]*sm
   }
  }
  
  maximum<-0
  #index<-0
  
  for(i in 1:621){
    if(flag[i]!=1){
    if(maximum<mul[i]){
      maximum =mul[i]
      index =i
    }
    
    }
  } 
  
  flag[index]<-1
  list_z<-c(list_z, df[index])
  #list_z
  col_added<-c(col_added,colnames(df[index]))
  
}
length(col_added)


list_z<-c(list_z, df[622])
col_added<-c(col_added, colnames(df[622]))
col_added
#df_actual <- read.csv("E:\\Research\\All ML works\\Extracted Feature_revised\\CD-HIT Resistance Genes\\Merged_aac\\Merged_aac_with non_AR_Final.csv", header=TRUE)
file_actual<-df_actual[,col_added]

file_name<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\data\\graph_bl_YN_actual_latest_del_row_no_15",".csv")
write.csv(file_actual, file_name, row.names = FALSE)







file_n<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\data\\graph_bl_YN_actual_latest_del_row_no_15",".csv")
data <- read.csv(file_n, header = TRUE)
z<-length(data)
lp<-z-1
list_dat<-c()
for(i in 1: lp){
  list_add<-c()
  list_dat<-c(list_dat, data[i])
  list_add<-c(list_add, list_dat, data[z])
  file_name<-paste0("E:\\Research\\All ML works\\Graph Theory approach\\data\\combn\\bl_after_del_row_no_15\\Merged_bl_with non_AR_S_", i, ".csv")
  write.csv(list_add, file_name, row.names = FALSE)
  
}



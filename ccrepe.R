library(ccrepe)
library(readr)
library(tidyverse)
library(zoo)
abudata<- read_csv("C:/Users/Administrator/Desktop/CI/basic source/skin/groupO.csv")%>%column_to_rownames(var='score_subjectid')
rown<-rownames(abudata)
abudata<-abudata[, -which(colMeans(is.na(abudata)) > 0.5)]%>%na.fill(fill = 0)
rownames(abudata)<-rown
ccrepe_matr<-ccrepe(as.matrix(abudata), iterations=100, min.subj=7)

ccrepe_matr$p.values[!upper.tri(ccrepe_matr$p.values, diag=F)]<-NA
ccrepe_matr$sim.score[!upper.tri(ccrepe_matr$sim.score, diag=F)]<-NA
merged_value <- data.frame(i=rep(row.names(ccrepe_matr$sim.score),ncol(ccrepe_matr$sim.score)),j=rep(colnames(ccrepe_matr$sim.score),each=nrow(ccrepe_matr$sim.score)), corvalue=as.vector(ccrepe_matr$sim.score),Pvalue=as.vector(ccrepe_matr$p.values))
filter_value<-merged_value%>%filter(Pvalue<0.05)
write.csv(filter_value,'groupOccrepe.csv')


wharton_agg_orig <- read.csv("WhartonFinal.csv")


colnames(wharton_agg_orig)

##Selectivity 0 s dont mean anything Imputed to 1
for(i in 1:nrow(wharton_agg_orig))
{  
  if(wharton_agg_orig[i,"Selectivity"]==0)
  {
    wharton_agg_orig[i,"Selectivity"] <- 1
  }
}

# aggregating the data per university,per year,per disposition,per selectivity,per
aggdata <-aggregate(wharton_agg_orig$UniversityId, 
                    by=list(wharton_agg_orig$UniversityId,wharton_agg_orig$App.Year,wharton_agg_orig$Disposition,
                            wharton_agg_orig$Selectivity,wharton_agg_orig$Type,
                            wharton_agg_orig$Size,wharton_agg_orig$Region), 
                    FUN=count)

colnames(aggdata) <- c("UniversityId","App.Year","Disposition","Selectivity","Type","Size","Region","count")

write.csv(aggdata, file = "Wharton_agg_data.csv",row.names=FALSE)
wharton_dataset <- read.csv("Wharton_agg_data.csv")

wharton_agg <- wharton_dataset
rm(wharton_dataset)


distxy <- dist(wharton_agg[,-1])
hClustering <- hclust(distxy)
plot(hClustering)
rm(distxy)

library(ade4)
library(data.table)

ohe_feats = c("App.Year","Disposition","Selectivity","Type","Size","Region" )
for (f in ohe_feats){
  wharton_dummy = acm.disjonctif(wharton_agg[f])
  wharton_dummy[f] = NULL
  wharton_agg = cbind(wharton_agg, wharton_dummy)
}

write.csv(wharton_agg,file = "Wharton_agg_data_ohe.csv",row.names=FALSE)
wharton_agg <-wharton_agg[,-(2:8)]


kmeansObj2 <- kmeans(wharton_agg[,-1],centers=2)
kmeansObj3 <- kmeans(wharton_agg[,-1],centers=3)
kmeansObj4 <- kmeans(wharton_agg[,-1],centers=4)
kmeansObj5 <- kmeans(wharton_agg[,-1],centers=5)
kmeansObj6 <- kmeans(wharton_agg[,-1],centers=6)
kmeansObj7 <- kmeans(wharton_agg[,-1],centers=7)
wharton_agg$cluster_two<-kmeansObj2$cluster
wharton_agg$cluster_three<-kmeansObj3$cluster
wharton_agg$cluster_four<-kmeansObj4$cluster
wharton_agg$cluster_five<-kmeansObj5$cluster
wharton_agg$cluster_six<-kmeansObj6$cluster
wharton_agg$cluster_seven<-kmeansObj7$cluster
plot(kmeansObj4$cluster)
library(cluster)
library(HSAUR)
dissE <- daisy(wharton_agg[,-1]) 
dE2   <- dissE^2
sk2   <- silhouette(kmeansObj3$cl, dE2)
pdf("plot_k3.pdf")
plot(sk2)
dev.off()

dE2   <- dissE^2
sk2   <- silhouette(kmeansObj2$cl, dE2)
pdf("plots_k2.pdf")
plot(sk2)
dev.off()

dE2   <- dissE^2
sk2   <- silhouette(kmeansObj4$cl, dE2)
pdf("plots_k4.pdf")
plot(sk2)
dev.off()

dE2   <- dissE^2
sk2   <- silhouette(kmeansObj5$cl, dE2)
pdf("plots_k5.pdf")
plot(sk2)
dev.off()

dE2   <- dissE^2
sk2   <- silhouette(kmeansObj5$cl, dE2)
pdf("plots_k5.pdf")
plot(sk2)
dev.off()

dE2   <- dissE^2
sk2   <- silhouette(kmeansObj6$cl, dE2)
pdf("plots_k6.pdf")
plot(sk2)
dev.off()

dE2   <- dissE^2
sk2   <- silhouette(kmeansObj7$cl, dE2)
pdf("plots_k7.pdf")
plot(sk2)
dev.off()


subset(wharton_agg_orig,App.Year=='2016')
wharton_clustered_data <- cbind("Tier2016")

write.csv(wharton_agg, file = "Wharton_clustered_data.csv",row.names=FALSE)

names(kmeansObj3)

wharton_agg = wharton_agg[,c(-3)]

names(kmeansObj)

wharton_agg = wharton_agg[,c(-3)]
kmeansObj <- kmeans(wharton_agg[,-1],centers=5)
names(kmeansObj)


wharton_agg$cluster_four<-kmeansObj4$cluster
#wharton_agg$cluster_four

colnames(wharton_agg)
wharton_Final_clustered <- wharton_agg[,-(2:24)]
wharton_Final_clustered$App.Year <- aggdata$App.Year
wharton_Final_clustered$Disposition <- aggdata$Disposition
wharton_Final_clustered$Selectivity <- aggdata$Selectivity 
wharton_Final_clustered$Type <- aggdata$Type
wharton_Final_clustered$Size <- aggdata$Size
wharton_Final_clustered$Region <- aggdata$Region

a<-subset(aggdata,App.Year==2016)
b<-subset(aggdata,App.Year==2017)

a$tier <- 

write.csv(wharton_Final_clustered, file = "Wharton_clustered_Final.csv",row.names=FALSE)



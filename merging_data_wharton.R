getwd()
setwd("C:/Independent Projects/Wharton_teach_for_america/Final_cleaned")
data2016 <- read.csv("2016.csv")
data2017<- read.csv("2017.csv")
univData<- read.csv("UniversityData.csv")
library(sqldf)
merged_2016 <- sqldf("Select data2016.*,univData.* from data2016 left join univData
                        on data2016.'UniversityId'=univData.'UniversityId'")
#rm(merged_dataset)
merged_2017 <- sqldf("Select data2017.*,univData.* from data2017 left join univData
                        on data2017.'UniversityId'=univData.'UniversityId'")
#total_merged <- sqldf("Select * from merged_2016 union all Select * from merged_2017")
total_merged<-rbind(merged_2016,merged_2017)
write.csv(total_merged, file = "single_merged_dataset_wharton.csv",row.names=TRUE)
colnames(merged_2016)<-colnames(merged_2017)
#rm(data2016,data2017,merged_2016,merged_2017)
#rm(univData)

#Data Preprocessing
wharton_dataset <- read.csv("single_merged_dataset_wharton.csv")
#unique(wharton_dataset[,"App.Year"])
#rm(total_merged)
wharton_dataset_bkp <- wharton_dataset
for(i in 1:nrow(wharton_dataset))
{  
if(wharton_dataset[i,"GPA.Above.3.6"]=="No")
{
  wharton_dataset[i,"GPA.Above.3.6"] <- 'N'
}
else if(wharton_dataset[i,"GPA.Above.3.6"]=="Yes")
{
  wharton_dataset[i,"GPA.Above.3.6"] <- 'Y'
}  
}
sqldf("select  count(wharton_dataset.'GPA.Above.3.6') from wharton_dataset")
colnames(wharton_dataset)
# Acceptance Percentage
colnames_wharton <- c( "X","ID"
               , "CalculatedProspectType","DispositionStep"
               , "AppYear","ApplicationDeadline"
               , "UndergradUniversityID","USWorldNewsReportSelectivity"
               , "2016AdjustedSchoolList","Major1"
               , "GPAAbove36","Met"
               , "SourcedbyRTvsSignupForm","UniversityID"
               , "X2017RecruitmentTier","X2016RecruitmentTier"
               , "PublicPrivate","Size"
               , "USWorldNewsSelectivity","Region"
               , "XofAlumniwhoattendedschoolinundergrad","XofCurrentCMswhoattendedschoolinundergrad"
               , "XofCurrentTFAStaffwhoattendedschoolinundergrad","AwarenessLevel")
colnames(wharton_dataset)<-colnames_wharton
wharton_subset <- wharton_dataset[,c(2,4,5,8,9,10,11,12,14)]

wharton_dataset.Acceptance_percentage <- 
  
sqldf("Select UndergradUniversityID,count(DispositionStep) from wharton_dataset group by UndergradUniversityID ")  
xtabs(wharton_dataset$DispositionStep~wharton_dataset$UndergradUniversityID+wharton_dataset$AppYear)

distxy <- dist(wharton_subset)
hClustering <- hclust(distxy)
plot(hClustering)

aggdata <-aggregate(wharton_dataset$UniversityId, 
                    by=list(wharton_dataset$UniversityId,wharton_dataset$App.Year,wharton_dataset$Disposition,
                            wharton_dataset$Size,wharton_dataset$Region), 
                    FUN=count)
length(wharton_dataset$Disposition)

wharton_subset<-subset(wharton_dataset,drop=c("X","Id","MonthDeadline","DateDeadline",
                                              "DateDeadline","YearDeadline","Major.2","Minor","Tier"
                                              ))



wharton_agg_orig <- read.csv("WhartonFinal.csv")
distxy <- dist(wharton_agg[,-1])
hClustering <- hclust(distxy)
plot(hClustering)


library(ade4)
library(data.table)
wharton_dummy = acm.disjonctif(wharton_agg["Year"])
wharton_dummy["Year"] = NULL
wharton_agg = cbind(wharton_agg[,-2], wharton_dummy)

wharton_agg = wharton_agg[,c(-3)]
kmeansObj2 <- kmeans(wharton_agg[,-1],centers=2)
names(kmeansObj)

wharton_agg = wharton_agg[,c(-3)]
kmeansObj3 <- kmeans(wharton_agg[,-1],centers=3)
names(kmeansObj)

wharton_agg = wharton_agg[,c(-3)]
kmeansObj4 <- kmeans(wharton_agg[,-1],centers=4)
names(kmeansObj)

wharton_agg = wharton_agg[,c(-3)]
kmeansObj5 <- kmeans(wharton_agg[,-1],centers=5)
names(kmeansObj)

wharton_agg = wharton_agg[,c(-3)]
kmeansObj6 <- kmeans(wharton_agg[,-1],centers=6)
names(kmeansObj)

wharton_agg = wharton_agg[,c(-3)]
kmeansObj7 <- kmeans(wharton_agg[,-1],centers=7)
names(kmeansObj)


wharton_agg$cluster_four<-kmeansObj$cluster
#wharton_agg$cluster_four

#Silhoutte Plots

dE2 <- daisy(wharton_agg[,-1])
pdf("k2.pdf", width=480, height=480) #pdf graphics device
plot(silhouette(x=kmeansObj2$cl,dist=dE2))
dev.off()
#continue the above for other clusters
plot(silhouette(x=kmeansObj3$cl,dist=dE2))
plot(silhouette(x=kmeansObj4$cl,dist=dE2))
plot(silhouette(x=kmeansObj5$cl,dist=dE2))
plot(silhouette(x=kmeansObj6$cl,dist=dE2))
plot(silhouette(x=kmeansObj7$cl,dist=dE2))


require("gplots")
require("ggmap")
require("plyr")

# setwd("f:./././informatica/")
if (!exists("map")){
  map <- get_map(location = 'San Francisco', zoom = 12)
}

message("Welcome to analisys program for crime in SF")
message("function list:\n
        data_load_prep - loading to csv file and pre-process\n
        create_all_figs - generates interesting figures accroding to things i saw\n
        generate_fig - generates figure according to diffrent paramaters from user\n
        show_on_geo_net - generates map figure with amount of diffrent catergories\n
        kmeans_on_map - runs kmeans on all points on map\n
        for start to can load first the data : load(\"s_data\")\n
        ")


#This function will load and pre-process the csv file
#If the file is loaded, the function assumes all fields are correct
#The function will return the veriable that contain all the data after the pre-process
#explenation about the pre-process will be in the PPT
data_load_prep <- function(){
  s_data=tryCatch(read.csv("sfpd_incident_2003-2011.csv",comment.char = ""),error=function(x){message("file not found")})
  if (is.null(s_data)){
    return(NULL);
  }
  colnames(s_data)[1]="IncidntNum"
  s_data=as.data.frame(s_data);
  #removing offer-offenses data, as from menully going over them this is not relevent to our case.
  #removing non-criminal data, as this is not relevent to our case
  NON_CRIMINAL=c("NON-CRIMINAL","OTHER OFFENSES","RUNAWAY","RECOVERED VEHICLE","MISSING PERSON","SUICIDE","PORNOGRAPHY/OBSCENE MAT","SUSPICIOUS OCC","LOITERING")
  s_data$Category=plyr::mapvalues(x =s_data$Category,from = NON_CRIMINAL,to = rep("NON-CRIMINAL",length(NON_CRIMINAL)) )
  s_data$Category=plyr::mapvalues(x =s_data$Category,from = c("FORGERY/COUNTERFEITING", "FRAUD", "BAD CHECKS"),to=rep("FRAUD",3))
  s_data$Category=plyr::mapvalues(x =s_data$Category,from = c("BURGLARY", "ROBBERY", "STOLEN PROPERTY", "EXTORTION"),to=rep("ROBBERY",4))
  s_data$Category=plyr::mapvalues(x =s_data$Category,from = c("LARCENY/THEFT", "VEHICLE THEFT", "EMBEZZLEMENT"), to= rep("THEFT",3))
  s_data$Category=plyr::mapvalues(x =s_data$Category,from = c("VANDALISM", "ARSON"), to = rep("VANDALISM",2))
  s_data$Category=plyr::mapvalues(x =s_data$Category,from = c("DRIVING UNDER THE INFLUENCE", "DRUNKENNESS", "LIQUOR LAWS","DISORDERLY CONDUCT") , to = rep("ALCOHOL RELATED",4))
  s_data$Category=plyr::mapvalues(x =s_data$Category,from = c("SEX OFFENSES, FORCIBLE", "PROSTITUTION", "SEX OFFENSES, NON FORCIBLE" ), to = rep("SEX",3))
  s_data$Category=plyr::mapvalues(x =s_data$Category,from = c("KIDNAPPING","FAMILY OFFENSES") , to =  rep("FAMILY RELATED",2))
  s_data$Category=plyr::mapvalues(x =s_data$Category,from = c("BRIBERY","FRAUD","GAMBLING"), to = rep("WHITE COLLAR",3))
  s_data$Category=factor(s_data$Category);
  #by looking on the raw data, we can see that there are duplicate IncidntNum, from the look of it, it's the unfolding event, but for now we dont need it.
  s_data <- subset(s_data, !duplicated(s_data$IncidntNum))
  # seconds and minutes are not relevent for most statistics, we keep only hours
  s_data$Time=strptime((s_data$Time),"%I:%M:%S %p")
  s_data$Time=as.numeric(format(round(s_data$Time,units="hours"),"%H"))
  #i wanted that the night will be continues, from looking at the data, crime day start and 5, so i shifted the time accordenly
  #change time to (0:5) -> (24:29)
  s_data$Time=plyr::mapvalues(x =s_data$Time,from = 0:5,to = 24:29 )
  s_data$Time=factor(s_data$Time, levels = 6:29)
  s_data$Date=strptime((s_data$Date),"%F");
  s_data$Year=format(s_data$Date,"%y");
  s_data$Month=format(s_data$Date,"%m");
  s_data$DayOfWeek = factor(s_data$DayOfWeek, levels= c("Monday", 
                              "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"))
  s_data$DayOfWeek = plyr::mapvalues(x=s_data$DayOfWeek, from = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday","Sunday"),
                                     to=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))
  #change names of X,Y to Longitude,Latitude
  names(s_data)[names(s_data)=="X"] <- "Longitude";
  names(s_data)[names(s_data)=="Y"] <- "Latitude";
  s_data=s_data[!(s_data$PdDistrict==""),];
  s_data$PdDistrict=factor(s_data$PdDistrict);
  s_data$Resolution[(s_data$Resolution == "NONE")|(s_data$Resolution == "NOT PROSECUTED")|(s_data$Resolution == "UNFOUNDED")]="NONE"
  s_data$Resolution=factor(s_data$Resolution)
  save(s_data,file="s_data");
  return(s_data);
}
attr(data_load_prep, "help") <-"this function will load and pre-process this csv file"

#col_filter =  columns to be selected (like "Category" or c("Category","Time"))
#scale_change = (0 = no change);(1 = log space) (2 = Proportion)
#clust_dim_two = if you selected two columns do you want to cluster the second one?
#sortPlot = if you selected only one column do you want the data to be sorted?
#filename = name of the figure file
generate_fig <- function(s_data,col_filter,scale_change=0,clust_dim_two=T,sortPlot=F,file_name=NULL){
  if (length(col_filter)>2 || length(col_filter)==0){
    message("error in col_filter");
    return(NULL);
  }
  fig_data= tryCatch(expr=(fig_data<-table(s_data[col_filter])),error = function(e) {
    message("error in col_filter-filter is not correct");
    return(NULL);
  });
  if (is.null(fig_data)){
    return (NULL);
  }
  #end of input error handeling
  # chiStatus=c("Category are Associated(non Uniform)","Category are not Associated(Uniform)")
  chiStatus="";
  # pValue=chisq.test(fig_data)$p.value;
  pValue=0;
  ylab="count";
  figTitle= paste(paste(col_filter,collapse = ","),chiStatus[(pValue>0.05)+1],sep="\n",collapse = ",");
  
  if ((scale_change)==1){
    fig_data=log10(1+fig_data);
    ylab=paste(ylab,"(in log space)")
  }else if ((scale_change)==2){
    fig_data=prop.table(fig_data,1);
    figTitle = paste(figTitle,"Proportion of",col_filter[1]);
  }
  
  #checking if it's one dim or two (barplot or hist)
  if (length(fig_data)==dim(fig_data)[1]){
    fig_data=as.data.frame(fig_data,stringsAsFactors=T);
    if (sortPlot) {
      fig_data=arrange(fig_data,Freq)
      fig_data$Var1=factor(fig_data$Var1, levels = fig_data$Var1[order(fig_data$Freq)])
    };
    ggplot(fig_data, aes(Var1, Freq,fill=Freq,group = 1)) + geom_bar(stat="identity", position="dodge") + 
      xlab(col_filter) + ggtitle(figTitle) + scale_fill_gradient(low="green",high="red") +
      geom_hline(aes(yintercept=median(Freq),color="blue")) + #geom_smooth(method=lm,se=FALSE,fullrange=T) + 
      theme(plot.title = element_text(lineheight=.8, face="bold"),axis.text.x = element_text(angle = 45, hjust = 1,face="bold",colour="black"),axis.text.y = element_text(face="bold",colour="black")) + 
      ggsave(paste0(file_name,".png"),width = 12,height = 8);
  }else{
    if (!is.null(file_name)){
      png(filename = paste0(file_name,".png"),width = 900,height = 750);
    }
    myColor<-colorRampPalette(c("green","yellow", "red"))(1000);
    dendrogram="both";
    Colv=T;
    if (!clust_dim_two){
      dendrogram="row";
      Colv=F;
    }
    heatmap.2(fig_data,trace = "none",col=myColor,dendrogram = dendrogram ,Colv = Colv,xlab=col_filter[2],ylab=col_filter[1],margins = c(4, 8),main=paste("HeatMap",figTitle,sep="\n"),srtCol=25)
    if (!is.null(file_name)){
      dev.off();
    }
  }
}

#This function generates all figures that are in the readme file
create_all_figs <-function(s_data){
  dir.create("fig",showWarnings = F);
  generate_fig(s_data,c("Time"),scale_change = 0,clust_dim_two=F,file_name = "fig/Time")
  generate_fig(s_data,c("PdDistrict"),scale_change = 0,clust_dim_two=F,file_name = "fig/PdDistrict",sortPlot = T)
  generate_fig(s_data,c("DayOfWeek"),scale_change = 0,clust_dim_two=F,file_name = "fig/Days")
  generate_fig(s_data,c("Category"),scale_change = 0,clust_dim_two=F,file_name = "fig/Cat",sortPlot = T)
  generate_fig(s_data,c("Category","Time"),scale_change = 1,clust_dim_two=F,file_name = "fig/Cat-Time")
  generate_fig(s_data,c("Category","Time"),scale_change = 2,clust_dim_two=F,file_name = "fig/Cat-TimeFreq")
  generate_fig(s_data,c("Time","Category"),scale_change = 2,clust_dim_two=T,file_name = "fig/Time-CatFreq")
  generate_fig(s_data,c("Category","DayOfWeek"),scale_change = 1,clust_dim_two=T,file_name = "fig/Cat-Days")
  generate_fig(s_data,c("Category","DayOfWeek"),scale_change = 2,clust_dim_two=F,file_name = "fig/Cat-DaysFreq")
  generate_fig(s_data,c("Category","Month"),scale_change = 1,clust_dim_two=T,file_name = "fig/Cat-Month")
  generate_fig(s_data[s_data$Year=="08",],c("Category","Month"),scale_change = 2,clust_dim_two=F,file_name = "fig/Cat-Month08")
  generate_fig(s_data[s_data$Year=="09",],c("Category","Month"),scale_change = 2,clust_dim_two=F,file_name = "fig/Cat-Month09")
  generate_fig(s_data,c("Category","Month"),scale_change = 2,clust_dim_two=F,file_name = "fig/Cat-MonthFreq")
  generate_fig(s_data,c("Category","Year"),scale_change = 2,clust_dim_two=F,file_name = "fig/Cat-YearFreq")
  generate_fig(s_data,c("Category","Year"),scale_change = 0,clust_dim_two=F,file_name = "fig/Cat-Year")
  generate_fig(s_data,c("Category","PdDistrict"),scale_change = 0,clust_dim_two=T,file_name = "fig/Cat-PdDistrict")
  generate_fig(s_data,c("Category","PdDistrict"),scale_change = 2,clust_dim_two=T,file_name = "fig/Cat-PdDistrictFreq")
  generate_fig(s_data,c("Resolution","Category"),scale_change = 2,clust_dim_two=T,file_name = "fig/Res-CatProp")
  generate_fig(s_data,c("Category","Resolution"),scale_change = 2,clust_dim_two=T,file_name = "fig/Cat-ResProp")
  generate_fig(s_data,c("PdDistrict","Category"),scale_change = 2,clust_dim_two=T,file_name = "fig/PdDistrict-CatFreq")
  generate_fig(s_data,c("Year","Category"),scale_change = 2,clust_dim_two=T,file_name = "fig/Year-CatFreq")
  generate_fig(s_data,c("Year","Category"),scale_change = 0,clust_dim_two=T,file_name = "fig/Year-Cat")
  generate_fig(s_data,c("Year"),scale_change = 0,clust_dim_two=T,file_name = "fig/Year")
  generate_fig(s_data,c("Month"),scale_change = 0,clust_dim_two=T,file_name = "fig/Month")
  generate_fig(s_data,c("Month","Year"),scale_change = 0,clust_dim_two=F,file_name = "fig/Month-Year")
  generate_fig(s_data,c("Month","Year"),scale_change = 2,clust_dim_two=F,file_name = "fig/Month-YearFreq")
  
  cat_time=prop.table(table(s_data$Category,s_data$Time),1);
  png("fig/multiCat_Time.png",800,600)
  par(mfrow=c(4,4))
  for (i in (1:dim(cat_time)[1])){
    barplot(cat_time[i,],main=rownames(cat_time)[i],xlab="hour")
  }
  dev.off()
  
  cat_time=prop.table(table(s_data$Category,s_data$Year),1);
  png("fig/multiCat_Year.png",800,600)
  par(mfrow=c(4,4))
  for (i in (1:dim(cat_time)[1])){
    barplot(cat_time[i,],main=rownames(cat_time)[i],xlab="year")
  }
  dev.off()
  
  kmeans_on_map(map,s_data,F,"kmeanRaw");
  kmeans_on_map(map,s_data,T,"kmeanProp");
  show_on_geo_net(map,s_data,"THEFT",T,"fig/theft");
  
  s_data=s_data[!(s_data$Year=="09"),]
  cat_time=prop.table(table(s_data$Category,s_data$Month),1);
  png("fig/multiCat_Month.png",800,600)
  par(mfrow=c(4,4))
  for (i in (1:dim(cat_time)[1])){
    barplot(cat_time[i,],main=rownames(cat_time)[i],xlab="month")
  }
  dev.off()
  

}

#this function will get all all crimes and assin each to a point in the map, next it will cluster them using k-means
#prop = is you want to count of each crime to be the raw count or the proportion in the point
kmeans_on_map <- function(map,s_data,prop=F,filename=""){
  s_data=s_data[s_data$Longitude!=0,];
  xmin = min(s_data$Longitude)
  xmax = max(s_data$Longitude)
  ymin = min(s_data$Latitude)
  ymax = max((s_data$Latitude))
  #table of all points or refrence in the grid of SF
  xtable=seq(xmin,xmax+10^-2,10^-2)
  ytable=seq(ymin,ymax+10^-2.5,10^-2.5)
  multiBox=cbind(.bincode(s_data$Longitude,xtable,include.lowest = T),.bincode(s_data$Latitude,ytable,include.lowest = T))
  s_data$Cell=paste(multiBox[,1],multiBox[,2],sep=",")
  if (prop){
    tableForKM=prop.table(table(s_data$Cell,s_data$Category),1)
  }else{
    tableForKM=(table(s_data$Cell,s_data$Category))
  }
  tableKM=kmeans(tableForKM,centers = 6)$cluster
  tableKM=cbind(t(matrix(as.numeric(unlist(strsplit(names(tableKM),","))),nrow = 2,ncol = length(tableKM))),cluster=as.numeric(tableKM))
  tableKM=data.frame(x=xtable[tableKM[,1]],y=ytable[tableKM[,2]],class=tableKM[,3])
  ggmap(map) + geom_point(data=tableKM,aes(x = x, y = y, colour=factor(class),size=20), alpha = .9) +
    ggtitle("kmeans freq in cell") + scale_colour_brewer(palette="Set1") + ggsave(file=paste0(filename,".png"),width = 7,height = 7)
  png(filename =paste0(filename,"BarPlot",".png"),1000,1000 )
  par(mfrow=c(2,3))
  for (i in 1:6){
    barplot(colMeans(tableForKM[which(tableKM$class==i),]),main=paste0("Class-",i),las=2)
  }
  dev.off()
}

#show_on_geo_net
#map = map object of SF (loaded each time your source the R file)
#cat_filter = category to be selected (like "THEFT")
#div_desc = if you want separate figures for each description (like T)
#filename = name of the figure file
show_on_geo_net <- function (map,s_data,cat_filter,div_desc=F,filename){
  get_map <- function(multiBox,filename,div_desc=F){
    if (div_desc){
      filename=paste(filename,make.names(multiBox[1,3]),sep="_");
      cat_filter=paste(cat_filter,multiBox[1,3]);
    }
    multiBox=multiBox[,1:2];
    multiBox=rbind(multiBox,(cbind(1:(length(xtable)),rep(1:(length(ytable)),each=(length(xtable))))))
    geoMatric=log10(as.numeric(table(multiBox[,1],multiBox[,2])));
    allBoxes=data.frame(x=xtable,y=rep(ytable,each=length(xtable)),count=geoMatric);
    ggmap(map) + geom_point(data=allBoxes,aes(x = x, y = y, size = count,colour=count), alpha = .9) + scale_colour_gradient(low = "green",high="red") +
      # geom_polygon(data=df.sf_neighborhoods,aes(x=long,y=lat,group=group) ,fill="#404040",colour= "#5A5A5A", lwd=0.05) +
      ggtitle(cat_filter) + theme(plot.title = element_text(lineheight=.8, face="bold")) + 
      ggsave(file=paste0(filename,".png"),width = 7,height = 7) ;
  }
  s_data=s_data[s_data$Longitude!=0,];
  xmin = min(s_data$Longitude)
  xmax = max(s_data$Longitude)
  ymin = min(s_data$Latitude)
  ymax = max((s_data$Latitude))
  xtable=seq(xmin,xmax+10^-2,10^-2)
  ytable=seq(ymin,ymax+10^-2.5,10^-2.5)
  
  s_data=subset(s_data,Category==cat_filter)
  multiBox=cbind(.bincode(s_data$Longitude,xtable,include.lowest = T),.bincode(s_data$Latitude,ytable,include.lowest = T))
  s_data$Cell=paste(multiBox[,1],multiBox[,2],sep=",")
    # browser();
  multiBox=as.data.frame(multiBox);
  multiBox=cbind(multiBox,Descript=s_data$Descript)
  if (div_desc){
    # browser()
     multiBox=multiBox[multiBox$Descript %in% names(which((table(multiBox$Descript))>3000)),]
     multiBox$Descript=factor(multiBox$Descript);
    toDelete=by(multiBox,multiBox$Descript,get_map,filename=filename,div_desc=T)
  }else{
    get_map(multiBox,filename)
  }
}

#calculating the euclidean distance from a vector x
eudli_dist <- function(x){
  return(sqrt((x[1]-x[4])^2+(x[2]-x[3])^2))
}

#getting a vector and returning the count of element according to the diffrent breaks
hist_dist <- function(x,breaks){
  hist(x,breaks =breaks,plot=F)$count
}

#mesuring the distance of crimes from their stations
radios_police <- function(s_data,cat_filter){
  police=read.csv("PoliceStations.csv");
  police=as.data.frame(police)
  police$PdDistrict=factor(police$PdDistrict,levels = levels(s_data$PdDistrict))
  police$Latitude=(format(police$Latitude,nsmall=12))
  police$Longitude=(format(police$Longitude,nsmall=12))
  police$Latitude=as.numeric(police$Latitude);
  police$Longitude=as.numeric(police$Longitude);
  # ggmap(map) + geom_point(data=police,aes(x = Longitude, y =Latitude ,colour=PdDistrict,size=16)) + ggsave(file="fig/policeOnMap.png",width = 7,height = 7)
  s_data=subset(s_data,Category==cat_filter)
  s_data=s_data[!(s_data$PdDistrict==""),]
  s_data=s_data[!(s_data$Longitude==0),]
  s_data$PdDistrict=factor(s_data$PdDistrict);
  tempData=(subset(s_data,select = c("PdDistrict","Longitude","Latitude")))
  tempData[,4:5] = police[match(as.character(tempData$PdDistrict),as.character(police$PdDistrict)),3:4]
  ret_dist=apply(tempData[,-1],1,eudli_dist)
  tempData$ret_dist=ret_dist;
  breaks=c(seq(0,0.04,10^-3),0.2);
  #counting the crimes in each distance
  geoList=tapply(tempData$ret_dist,tempData$PdDistrict,FUN = hist_dist,breaks=breaks );
  geoList=matrix(unlist(geoList),10,length(breaks)-1,dimnames = list(names(geoList),breaks[-1]))
  #calculating the cor
  return(apply(geoList,1,cor,y=41:1,method = "spearman"))
}

geograpical <- function(s_data){
  #geograpical
  ############
  s_data=s_data[s_data$Longitude!=0,];
  allCluster=NULL
  for (i in levels(s_data$Category)){
    temp=subset(s_data,subset = (Category==i),select = c("Longitude","Latitude"))
    kme=kmeans(temp,centers = 10);
    allCluster=rbind(cbind(i,kme$centers),allCluster)
  }
  ggmap(map) + geom_point(aes(x = as.numeric(allCluster[,2]), y = as.numeric(allCluster[,3]), size = 3,colour=allCluster[,1]), alpha = .9)
  
  s_data$clusterG=kme$cluster;
  map <- get_map(location = 'San Francisco', zoom = 12)
  mapPoints <- ggmap(map) + geom_point(aes(x = Longitude, y = Latitude, size = 3,colour=Category), data = s_data[1:1000,], alpha = .5)
  mapPoints <- ggmap(map) + geom_rect(aes(xmin = min(s_data$Longitude),xmax=max(s_data$Longitude) , ymin = min(s_data$Latitude),ymax=max((s_data$Latitude)), size = 3), alpha = .5)
  #Dangers neighborhood total and over time
  #connection between time and location 
  ggmap(map) + geom_point(aes(x = as.numeric(allCluster[,2]), y = as.numeric(allCluster[,3]), size = 3,colour=allCluster[,1]), alpha = .9)
}

holidays_analisys <- function(s_data){
  #Holidays
  ##########
  hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay","USNewYearsDay","USThanksgivingDay")
  myholidays  <- dates(as.character(holiday(2003:2009,hlist)),format="Y-M-D")
  onHoliday=table(s_data$Category,is.holiday(s_data$Date,myholidays))
  heatmap.2(round(t(t(onHoliday)/colSums(onHoliday)),3));
  chisq.test(cbind(as.numeric(onHoliday[, 1]) ,as.numeric(onHoliday[, 2]) ),simulate.p.value = 200);
}

### things that didnt work:
dub_menage <- function(){
  #dont remove dup, and see crime solving in diffrent places , not working
  #Compare time of happened and solved
  #not time differ or location dif, or any.
  dub=unique(s_data$IncidntNum[duplicated(s_data$IncidntNum)])
  s_data_dup = s_data[s_data$IncidntNum%in%dub,]
  s_data_dup= s_data_dup[order(s_data_dup$IncidntNum),]
  length(unique(s_data_dup$IncidntNum));
  dim(unique(cbind(s_data_dup$IncidntNum,s_data_dup$Descript)));
  dim(unique(cbind(s_data_dup$IncidntNum,s_data_dup$DayOfWeek)));
}

knn_analsys <- function (s_data){
  attach(s_data)
  s_data <- s_data[-dim(s_data)[1],]
  
  #Look at bar plot
  tres_crimes <- table(Category)[table(Category)>100]
  barplot(tres_crimes,main = "Crimes Distribution",cex.names=0.6,las=2)
  
  ### KNN ###
  library(class)
  
  #Remove missing values without location for KNN
  valid_locations<-(s_data[,10]>(-123) & s_data[,10]<(-121)) & !is.na(s_data[,10])
  d<-s_data[valid_locations,]
  
  #Split train/test
  n = dim(d)[1]
  train_prop <- 0.8
  
  set.seed(0)
  train_ind <- sample(1:n,n*train_prop,replace=F)
  test_ind <- !(1:n %in% train_ind)
  
  tr <- d[train_ind,]
  tst <- d[test_ind,]
  
  knn_size = rep(0,50)
  for (k in 1:length(knn_size)) {
    res <- knn(tr[,10:11],tst[,10:11],tr[,2],k=k)
    knn_size[k] = 1 - sum(res == tst[,2])/dim(tst)[1] 
  }
  ind = 1:length(knn_size)
  plot(knn_size ~ ind,main="Optimal Neigbhoor Size",xlab="# of Neighboors",ylab="Misclassification rate",type="b")
}

log_reg <- function(s_data){
  library(nnet)
  attach(s_data)
  head(s_data)
  
  log1<-multinom(Category~DayOfWeek + Time + PdDistrict + Resolution,data=s_data,MaxNWts = 1100)
  
  summary(log1)
  table(factor(DayOfWeek))
  
  fitt = log1$fitted
  get_max <- function(v){
    return(which(max(v) == v))
  }
  fit <- apply(log1$fitted,1,get_max)
  predictions <- colnames(log1$fitted)[fit]
  sum(Category == predictions)/dim(s_data)[1]
}

resolution_learn <- function(s_data){
  attach(s_data)
  #Prepare data
  notSolved <- (Resolution == "NONE")|(Resolution == "NOT PROSECUTED")|(Resolution == "UNFOUNDED")
  d<- data.frame(factor(notSolved),s_data[,c(3,4,7)],factor(s_data[,6]),factor(s_data[,12]),factor(s_data[,13]))
  names(d)[1] <- "Resolu"
  n = dim(d)[1]
  
  #Train/Test
  set.seed(0)
  train_ind <- sample(1:n,50000,replace=F)
  test_ind <- sample(which(!(1:n %in% train_ind)==T),10000,replace=F)
  tr <- d[train_ind,]
  tst <- d[test_ind,]
  
  #Do bagging
  library(adabag)
  bag1<-bagging(Resolu~Category+DayOfWeek+Descript+Location+PdDistrict+Time,data=d)
  predBag <- predict(bag1,newdata=subset(tst,select=(c("Category","DayOfWeek","Descript","Location","PdDistrict","Time"))))
  return(sum(predBag==tst[,1])/dim(tst)[1])
#   #Research crimes proportions
#   calc_efficiency <- function(crime,d){
#     return(table(d[d[,2]==crime,1])[2]/sum(table(d[d[,2]==crime,1])))
#   }
#   eff<-sapply(names(which(table(d[,2])>100)),calc_efficiency,d)
#   eff_most_popular<-sapply(names(which(table(d[,2])>10000)),calc_efficiency,d)
#   browser();
}

check_white <- function(s_data){
  white=subset(s_data,Category=="WHITE COLLAR")
  white$Time=as.numeric(white$Time)
  white$Time[!(white$Time==24 | white$Time==12)]=0
  white$Time[(white$Time==24 | white$Time==12)]=1
  selected=sample(1:dim(white)[1],10000);
  sampl=white[selected,]
  nsampl=white[-selected,]
  lm1=lm(Time ~ DayOfWeek + Descript + Location + PdDistrict,data=sampl)
  res=predict(lm1,newdata=subset(nsampl,select = c("Time","DayOfWeek","Descript","Location","PdDistrict")))
  print(sqrt(avg((res-nsampl$Time)^2)))
  return(check);
}
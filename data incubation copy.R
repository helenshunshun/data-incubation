#################################################Q1
## make a function to contain M for each trial
maxsubchain<-function(N,trial){
  bag<-seq(1:N)
  m<-c(1,rep(0,N-1))## m contail possible subchain after each draw
  max.m<-rep(0,trial)## max.m contain maxinum subchian during each trial process
  for(t in 1:trial){
    draw<-sample(bag,N,F)## randomly take a link from bag
    for(i in 2:N){
      differ<-rep(0,(i-1))
      ## it contains each difference between new link and links that draw before
      for(j in 1:(i-1)){
        differ[j]<-abs(draw[i]-draw[j])
        if(length(which(differ==1))==0) {m[i]<-m[i-1]+1}
        ## if new link cannot be connected to any existing links,it will be a new subchain
        else{m[i]<-m[i-1]}
      }  
    }
    max.m[t]<-max(m)## figure out maximum subchain M
  }
  max.m
}
result<-function(M){
  rr<-c(mean(M),sd(M))## claculate mean and sd
  names(rr)<-c("mean","stad.deviation")
  print(rr)
}
## take 10000 trials to get result
M<-maxsubchain(8,10000)
result(M)
result(maxsubchain(16,10000))
result(maxsubchain(32,10000))

##################################################################





install.packages("reshape2")
install.packages("ggplot2")

######################################### music project start

##read.csv
attach(song)
attach(similarity)
attach(amb)
attach(aterm)
#################################### codes for picture 1
require(reshape2)
require(ggplot2)
sinpopular<-function(singername){
  sigsong<-song[which(artist_name==singername),]## find out related song info
  id<-sigsong[1,"artist_id"]## find out id of chosen singer
  simiterm<-as.character(subset(aterm,aterm[,1] %in% id)[,2]) ##find out all related terms of this singer
  ### make a funciton to generate boxplot to show the distribution of popularity of 
  ##selected singer based on each feature term that this singer has
  pic<-function(termname){
    others<-subset(aterm,aterm[,2] %in% simiterm) ## find out other singer who have same terms included
    termsingid<-subset(others,others[,2] %in% termname)[,1]## get ids of singer who have same terms    
    popu<-subset(song[,c("artist_id","artist_familiarity","artist_hotttnesss")],song[,"artist_id"] %in% termsingid)
    ## found out how popularity of those singers
    undupli<-popu[!duplicated(popu[,1]),]
    bb<-as.data.frame(cbind(as.numeric(as.character(undupli[,2])),as.numeric(as.character(undupli[,3]))))
    colnames(bb)<-c("familiarity","hottness")##get numeric value of familiarity and hottness
    bb$highlight <- c(1:nrow(undupli) %in% which(undupli[,"artist_id"]==id) ) ## highlight selected singer popularity index 
    df.2 <- melt(bb)  # draw plot with ggplot2
    ggplot(subset(df.2, !highlight),aes(x=variable, y=value))+geom_boxplot()+scale_y_log10()+
    geom_point(data=subset(df.2, highlight), aes(x=variable, y=value), color="red", size=3)+ ggtitle(termname)
  }
  plots <- list()  # new empty list
  for (i in 1:length(simiterm)) {
    p1 = pic(simiterm[i])
    plots[[i]] <- p1 
  }
  do.call(grid.arrange,plots) ## draw multiple plots together
}

sinpopular("Archie Roach")  ## take singer Archie Roach as an example, to show his familiarity and hottness
## in all singers who have the same feature.


#################################### codes for picture 2
terminfo<-function(TERM){
  feature<-aterm[which(term==TERM),]## get all artist id who has this term
  length(feature[,1][duplicated(feature[,1])]) ## to test if there was dulicated id
  subsong<-subset(song,song[,"artist_id"] %in% feature[,1])
  y<-subsong[,"year"][!duplicated(subsong[,"year"])]## subset of song info based on year
  num<-round(suppressWarnings(as.numeric(as.character(y))),0) 
  abyear<-subset(num,num>1900 & !is.na(num))##clean and deal with raw year data
  yprofile<-function(YEAR){
    subyear<-subset(subsong, subsong$year %in% YEAR) 
    nsong<-nrow(subyear)
    nsinger<-length(subyear[,"artist_id"][!duplicated(subyear[,"artist_id"])])
    sumfami<-sum(round(as.numeric(as.character(subyear[,"artist_familiarity"])),10))
    sumhot<-sum(round(as.numeric(as.character(subyear[,"artist_hotttnesss"])),10))
    nalbum<-length(subyear[,"release"][!duplicated(subyear[,"release"])])
    info<-c(YEAR,nsong,nsinger,sumfami,sumhot,nalbum)
    return(info)
  } ### function yprofile return info about term for each year
  picinfo<-matrix(nrow=length(abyear),ncol=6)
  for(i in 1:length(abyear)){
    picinfo[i,]<-yprofile(abyear[i])
  } ## matrix picinfo contains term info for all existing years
  colnames(picinfo)<-c("year","#song","#singers","total.familiarity","total.hottness","total.album")
  picyear<-picinfo[,1]
  plot(x=picyear,y=picinfo[,"#song"],pch=20,col="blue",
       xlab="year",ylab="number",main="Term Information Through Years")
  color<-c("red","grey","brown","green")
  for(i in 3:6){
    points(x=picyear,y=picinfo[,i],pch=20,col=color[i-2])
  }
  legend("topleft",pch=rep(19,5),col=c("blue",color),
         legend=c("#song", "#singer","familiarity", "hottness", "#album"))
  print(picinfo)## print value in numbers
}
terminfo("rock")### use "rock" as an instance,it will return number change of singers,
#realeased albums,songs,familiarity and hottness though years.



#################################################################
### Q6 second part using R
xmldata<-read.csv()
colnames(xmldata)<-c("id","ATime","QTime","CTime")
unduid<-xmldata[!duplicated(xmldata[,1]),1]## find out all distinct id
t.condition<-matrix(nrow=length(unduid),ncol=9)## matrix t.condition will contain action sequence for each id
A<-0
Q<-0
C<-0
for (t in 1:length(unduid)){
behavior<-xmldata[which(xmldata[,"id"]==unduid[t]),]
atime<-abs(behavior[!duplicated(behavior[,2]),2])
qtime<-abs(behavior[!duplicated(behavior[,3]),3])
ctime<-abs(behavior[!duplicated(behavior[,4]),4])

timeorder<-as.character(sort(c(atime,qtime,ctime)))## order action time
chatime<-as.character(atime)
chqtime<-as.character(qtime)
chctime<-as.character(ctime)

for(j in 1:length(timeorder)){ 
  aftera<-replace(timeorder,timeorder %in% chatime,"A")
  afterq<-replace(aftera,aftera %in% chqtime,"Q")
  afterc<-replace(afterq,afterq %in% chctime,"C")
}## create action sequence, replace time with action
A<-A+length(which(afterc=="A"))
Q<-Q+length(which(afterc=="Q"))
C<-C+length(which(afterc=="C"))## get unconditional freq for each id
condition<-function(before,after){
  fcon<-0
  for (i in 1: (length(afterc)-1)){
    if(afterc[i]==before && afterc[i+1]==after ) fcon<-fcon+1
  }
  return(fcon)
}## function condition is used to calculate conditional frequency

conaction<-c(condition("A","A"),condition("A","Q"),condition("A","C"),condition("C","A"),condition("C","Q"),condition("C","C"),condition("Q","A"),condition("Q","Q"),condition("Q","C"))## get conditional freq for each id 
t.condition[t,]<-conaction
}
total<-as.data.frame(cbind(unduid,t.condition))
colnames(total)<-c( "id","A|A","A|Q","A|C","C|A","C|Q","C|C","Q|A","Q|Q","Q|C") 
cc<-colSums(total)[-1]
un.A<-A/sum(A,Q,C)
un.Q<-Q/sum(A,Q,C)
un.C<-C/sum(A,Q,C)
Acon.C<-cc[3]/sum(cc[c(3,6,9)])
Ccon.C<-cc[6]/sum(cc[c(3,6,9)])
Qcon.C<-cc[9]/sum(cc[c(3,6,9)])
Acon.A<-cc[1]/sum(cc[c(1,4,7)])
Ccon.A<-cc[4]/sum(cc[c(1,4,7)])
Qcon.A<-cc[7]/sum(cc[c(1,4,7)])
Acon.Q<-cc[2]/sum(cc[c(2,5,8)])
Ccon.Q<-cc[5]/sum(cc[c(2,5,8)])
Qcon.Q<-cc[8]/sum(cc[c(2,5,8)])
max(c(Acon.C,Acon.A,Acon.Q)/un.A,c(Ccon.C,Ccon.A,Ccon.Q)/un.C,c(Qcon.A,Qcon.C,Qcon.Q)/un.Q)##find out the largest


#############################################################################


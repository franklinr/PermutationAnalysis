require(stringr)
require(ggplot2)
require(lme4)
require(reshape2)

# to run the permutation test, set this to TRUE
# to reuse previously created test results, set to FALSE
dopermutation = FALSE
sname="PintPstr"
set.seed(102)

# set default font for ggplot
theme_set(theme_get() + theme(text = element_text(family = 'Arial')))

sessionInfo()

convertEyeTrackIntoDataFrame <- function(df){
  #melt changes the timebins from columns to rows
  #long.df is the imported data in long (rows) format
  long.df = melt(df , id=names(df )[0:6])
  head(long.df)
  nontar = subset(long.df , Object == "Non-target")
  tar.df = subset(long.df , Object == "Target")
  tar.df$Nontarget = nontar$value
  names(tar.df)[8] = "Target"
  head(tar.df)
  
  ## change time codes
  name = levels(tar.df$variable)
  name = str_replace(name, "X[.]", "-")
  #name
  name = str_replace(name, "X", "")
  #name
  tar.df$variable = factor(tar.df$variable, labels = name)
  #  print(levels(factor(name)))
  tar.df$timebin =  as.numeric(as.character(tar.df$variable))
  
  names(tar.df)[2] <- "Condition"
  # deal with conditions
  tar.df$Condition = factor(tar.df$Condition, labels=c("Active","Passive"))
  df = tar.df
  
  # compute targetbias agent bias
  df$targbias = df$Target - df$Nontarget
  df$agentbias = df$Target - df$Nontarget
  df$agentbias[df$Condition == "Passive"] =  df$Nontarget[df$Condition == "Passive"]-df$Target[df$Condition == "Passive"]
  df$agentbiasbinary = NA
  df$agentbiasbinary[df$agentbias == 1] = 1
  df$agentbiasbinary[df$agentbias == -1] = 0
  
  #coding item on basis of counterbalanced order
  df$item = NA
  df$item[df$Trial %in% c(1,2,11:18)] = 777  # put more specific things ifrst
  df$item[df$Trial %in% c(3,4)] = 888
  df$item[df$Counterbalanced.order == 1 & df$Trial == 5] = 1
  df$item[df$Counterbalanced.order == 1 & df$Trial == 6] = 2
  df$item[df$Counterbalanced.order == 1 & df$Trial == 7] = 3
  df$item[df$Counterbalanced.order == 1 & df$Trial == 8] = 4
  df$item[df$Counterbalanced.order == 1 & df$Trial == 9] = 5
  df$item[df$Counterbalanced.order == 1 & df$Trial == 10] = 6
  df$item[df$Counterbalanced.order == 2 & df$Trial == 5] = 5
  df$item[df$Counterbalanced.order == 2 & df$Trial == 6] = 6
  df$item[df$Counterbalanced.order == 2 & df$Trial == 7] = 1
  df$item[df$Counterbalanced.order == 2 & df$Trial == 8] = 2
  df$item[df$Counterbalanced.order == 2 & df$Trial == 9] = 3
  df$item[df$Counterbalanced.order == 2 & df$Trial == 10] = 4
  df$item[df$Counterbalanced.order == 3 & df$Trial == 5] = 2
  df$item[df$Counterbalanced.order == 3 & df$Trial == 6] = 3
  df$item[df$Counterbalanced.order == 3 & df$Trial == 7] = 4
  df$item[df$Counterbalanced.order == 3 & df$Trial == 8] = 5
  df$item[df$Counterbalanced.order == 3 & df$Trial == 9] = 6
  df$item[df$Counterbalanced.order == 3 & df$Trial == 10] = 1
  df$item[df$Counterbalanced.order == 4 & df$Trial == 5] = 4
  df$item[df$Counterbalanced.order == 4 & df$Trial == 6] = 5
  df$item[df$Counterbalanced.order == 4 & df$Trial == 7] = 6
  df$item[df$Counterbalanced.order == 4 & df$Trial == 8] = 1
  df$item[df$Counterbalanced.order == 4 & df$Trial == 9] = 2
  df$item[df$Counterbalanced.order == 4 & df$Trial == 10] = 3
  df$item[df$Counterbalanced.order == 5 & df$Trial == 5] = 6
  df$item[df$Counterbalanced.order == 5 & df$Trial == 6] = 1
  df$item[df$Counterbalanced.order == 5 & df$Trial == 7] = 2
  df$item[df$Counterbalanced.order == 5 & df$Trial == 8] = 3
  df$item[df$Counterbalanced.order == 5 & df$Trial == 9] = 4
  df$item[df$Counterbalanced.order == 5 & df$Trial == 10] = 5
  df$item[df$Counterbalanced.order == 6 & df$Trial == 5] = 2
  df$item[df$Counterbalanced.order == 6 & df$Trial == 6] = 3
  df$item[df$Counterbalanced.order == 6 & df$Trial == 7] = 4
  df$item[df$Counterbalanced.order == 6 & df$Trial == 8] = 5
  df$item[df$Counterbalanced.order == 6 & df$Trial == 9] = 6
  df$item[df$Counterbalanced.order == 6 & df$Trial == 10] = 1
  df$item[df$Counterbalanced.order == 7 & df$Trial == 5] = 4
  df$item[df$Counterbalanced.order == 7 & df$Trial == 6] = 5
  df$item[df$Counterbalanced.order == 7 & df$Trial == 7] = 6
  df$item[df$Counterbalanced.order == 7 & df$Trial == 8] = 1
  df$item[df$Counterbalanced.order == 7 & df$Trial == 9] = 2
  df$item[df$Counterbalanced.order == 7 & df$Trial == 10] = 3
  df$item[df$Counterbalanced.order == 8 & df$Trial == 5] = 3
  df$item[df$Counterbalanced.order == 8 & df$Trial == 6] = 4
  df$item[df$Counterbalanced.order == 8 & df$Trial == 7] = 5
  df$item[df$Counterbalanced.order == 8 & df$Trial == 8] = 6
  df$item[df$Counterbalanced.order == 8 & df$Trial == 9] = 1
  df$item[df$Counterbalanced.order == 8 & df$Trial == 10] = 2
  
  df$item = factor(df$item)
  print("items for each counterbalanced order")
  print(xtabs(~ item + Counterbalanced.order, df))
  
  # changing larger window size for analysis,floor changes 0.9 to 0
  window = 1
  df$timewindow = floor((df$timebin)*1.0/window)*window
  
  df$beforeafter0 = ifelse(df$timewindow > 0, "after0","before0")
  df$cafter0 = ifelse(df$beforeafter0 == "after0",0.5,-0.5)
  df$cactive = ifelse(df$Condition == "Active",0.5,-0.5)
  df$cage = df$Agegroup - mean(df$Agegroup)
  
  return(df)
}

computeTestStatisticOnRaw <- function(abias,meanabias){
  tbinlist = unique(abias$timebin)
  agelist = unique(abias$Agegroup)
  for (ag in agelist){
    for (tbin in tbinlist){
      # create data frame for ONE timebin for one age group
      dataTime = subset(abias,timebin == tbin & Agegroup == ag)
      # do regression model on agentbias using structure
      tarmodel = lm(logitagentbias ~ cactive, dataTime)
      # this is the t-value for structure
      coefmodel = coef(summary(tarmodel))
      targetT = abs(coefmodel[2,3])  # observed t-value
      targetP = abs(coefmodel[2,4])  # observed p-value
      meanabias$tstr[meanabias$timebin == tbin & meanabias$Agegroup == ag] = targetT
      meanabias$pstr[meanabias$timebin == tbin & meanabias$Agegroup == ag] = targetP
      # this is the t-value for intercept
      targetTint = abs(coefmodel[1,3])  # observed t-value
      targetPint = abs(coefmodel[1,4])  # observed p-value
      meanabias$tint[meanabias$timebin == tbin & meanabias$Agegroup == ag] = targetTint
      meanabias$pint[meanabias$timebin == tbin & meanabias$Agegroup == ag] = targetPint
    }
  }
  return(meanabias)
}

# create clusters for adjacent windows with p<0.05 for structure and intercept
findclusters <- function(df) {
  cnum = 1
  lastpval = 100
  cnumint = 1
  lastpvalint = 100
  df$cnum = 1
  df$cnumint = 1
  for (i in 1:length(df$pstr)) {
    if (df$Condition[i] == "Active") {
      time = df$timebin[i]
      pval = abs(df$pstr[i])
      if (pval < 0.05 & lastpval > 0.05) {
        cnum = cnum + 1
      }
      if (pval > 0.05) {
        cnum = cnum + 1
      }
      lastpval = pval
      # create clusters for intercept
      pvalint = abs(df$pint[i])
      if (pvalint < 0.05 & lastpvalint > 0.05) {
        cnumint = cnumint + 1
      }
      if (pvalint > 0.05) {
        cnumint = cnumint + 1
      }
      lastpvalint = pvalint
    }
    df$cnum[i] = cnum
    df$cnumint[i] = cnumint
    # same cnum for active and passive
  }
  head(df)
  return(df)
}

runExperimentsByPermutation <- function(sigcluster,abias,n=1000){
  # we run permutation test on any window that has a significant intercept or structure
  gc() # garbage collection
  # create the permutation test dist
  exptestvalue = data.frame()
  ptime <- system.time({
    for (s in 1:length(sigcluster$cnum)) {
      #  print(cl)
      cl = sigcluster$cnum[s]
      clint = sigcluster$cnumint[s]
      b = sigcluster$timebin[s]
      ag = sigcluster$Agegroup[s]
      
      if (b > 0) {
        print(paste("b",b,"ag",ag))
        dataTime = subset(abias,Agegroup == ag & timebin %in% b)
        randSet = dataTime
        for (i in 1:n){
          # randomly scramble labels in cactive
          randSet$cactive = sample(randSet$cactive,length(randSet$cactive))
          # test if agentbias is related to random scrambled cactive
          randmodel = coef(summary(lm(logitagentbias ~ cactive ,randSet)))
          # save t-values
          tint = randmodel[1,3]
          pint = randmodel[1,4]
          tstr = randmodel[2,3]
          pstr = randmodel[2,4]
          df = data.frame(simtstr = tstr,simtint = tint,simpstr=pstr,simpint=pint,cluster = cl,bin = b,age = ag,sim = i)
          exptestvalue = rbind(exptestvalue, df)
         }
      }
    }
  })
  print(ptime[3])

  return(exptestvalue)
}

findMaximumSumTforEachExp <- function(sumt.df){
  # this code extracts out the maximum sum t for each simulation at each age
  permstrdist = data.frame()
  permintdist = data.frame()
  for (a in unique(sumt.df$age)){
    for (s in unique(sumt.df$sim)) {
      onesim = subset(sumt.df,sim == s & age == a)
      onesimint = onesim
      
      onesim$abssimtstr = abs(onesim$simtstr)
      maxrow = onesim[order(onesim$abssimtstr,decreasing = T),] # order from largest to smallest
      permstrdist = rbind(permstrdist,maxrow[1,]) # max absolute value str t
      
      onesimint$abssimtint = abs(onesimint$simtint)
      maxrow2 = onesimint[order(onesimint$abssimtint,decreasing = T),]
      permintdist = rbind(permintdist,maxrow2[1,]) # max absolute value intercept t
    }
  }
  permstrdist$simtint=NULL
  permintdist$simtstr=NULL
  
  return(list(permstrdist,permintdist))
}

testClusterSumTagainstDistribution <- function(sumcluster,clust, permdist,tmeasure,meanabias.df,abias.df){
  # this identifies tvalues that are greater than dist t-values
  for (cl in unique(sumcluster[,clust])){
    bins = unique(meanabias.df[meanabias.df[,clust] == cl,]$timebin)
    ag = sumcluster$Agegroup[sumcluster[,clust] == cl][1]
    
    permdistage = subset(permdist, age == ag) # permutation distribution for particular age group
    tvalue = abs(sumcluster[sumcluster[,clust]==cl,tmeasure]) # real sum t-value for cluster
    
    pdist = permdistage[,paste("abssim",tmeasure,sep="")] # permutation distribution for structure or intercept
    numExp = length(pdist) # number of experiments
    tless = sum(tvalue < pdist, na.rm = TRUE) # how many perm dist values are higher than observed t value
    pvalue = tless/numExp # pvalue is proportions of values above observed sum t
    
    # save this info for figure
    trows = which(meanabias.df$timebin %in% bins & meanabias.df$Agegroup == ag)
    meanabias.df[trows,paste("permP",tmeasure,sep="")] = pvalue
    meanabias.df[trows,paste("permT",tmeasure,sep="")] = tvalue
    
    abias.df[abias.df$timebin %in% bins & abias.df$Agegroup == ag,clust] = cl
  }
  return(list(meanabias.df,abias.df))
}

formatfig <- function(p){
  # p = p + xlim(-100,8000)
  # p = p + ylim(-Inf,maxy+.1)
  p = p + theme_bw()
  #p = p + scale_fill_manual(values=c("blue"))
  p = p + theme(legend.position="top")
  p = p + ylab("Mean proportion active match")
  p = p + scale_fill_manual(values = c("grey90","blue"))
#  p = p + scale_fill_grey(start = 0.90, end = 0.0)
  p = p + guides(fill=FALSE)
  p = p + geom_hline(yintercept = 0.5,colour="grey",linetype="dashed")
  return(p)
}


####################### timelocked to 1st NP #######################
file = "St2_1stNP_new_templates_noprocess_ALL_noadults_minus600.csv"
onedata.df = read.csv(file)
data.df = convertEyeTrackIntoDataFrame(onedata.df)

alltrialsNP1.df = subset(data.df, Trial %in% c(5,6,7,8,9,10))
alltrialsNP1.df = subset(alltrialsNP1.df, timewindow < 6000)

means.df = aggregate(agentbiasbinary ~ timewindow + Condition + Agegroup, alltrialsNP1.df, mean)
means.df$Agegroup = factor(means.df$Agegroup, labels = c("2 year olds","3 year olds"))
p = ggplot(means.df , aes( x = timewindow, y = agentbiasbinary, colour = Condition,group=Condition))
p = p + geom_line() + scale_color_brewer(palette="Set1")
p = p +facet_wrap(~ Agegroup, ncol=1)
print(p)

windowsize = 20
alltrialsNP1.df$timebin = floor(alltrialsNP1.df$timewindow/windowsize)
length(unique(alltrialsNP1.df$timewindow))
length(unique(alltrialsNP1.df$timebin))

#aggregated over 6 test trials -> overall proportion active match -> empirically logit transformed
abias.df = aggregate(agentbiasbinary ~ cactive + Condition + timebin + Agegroup + Participant, alltrialsNP1.df, mean)
names(abias.df)[6] <- "agentbias"
head(abias.df)
#xtabs( ~ timebin, abias.df)
abias.df$logitagentbias = log((abias.df$agentbias+0.5)/(1-abias.df$agentbias+0.5))

# this data frame is used for the figures
meanabias.df = aggregate(agentbias ~ cactive + Condition + timebin + Agegroup, abias.df, mean)
meanabias.df$pstr = 1000
meanabias.df$timewindow = meanabias.df$timebin * windowsize

# Step 1) for each age and each time window, compute observed t-value
meanabias.df=computeTestStatisticOnRaw(abias.df,meanabias.df)
head(meanabias.df)

# create clusters for adjacent windows with p<0.05 for structure and intercept
meanabias.df = findclusters(meanabias.df)
head(meanabias.df)

# any window that has a significant intercept or structure
# we only use active, because we only one row for each lm model
sigcluster = subset(meanabias.df, timebin > 0 &  Condition == "Active" & pint < 0.05)
head(sigcluster)

length(unique(sigcluster$cnumint)) # total significant clusters found
length(unique(sigcluster$cnumint[sigcluster$Agegroup==2])) # cluster for 2 year olds
length(unique(sigcluster$cnumint[sigcluster$Agegroup==3])) # clusters for 3 year olds

# this figure shows lines for each sig intercept cluster.  
# They are all above 0.5, since that is what the intercept is testing
p = ggplot(sigcluster , aes( x = timewindow, y = agentbias,colour=factor(cnumint)))
p = p + geom_line()
p = p +facet_wrap(~ Agegroup, ncol=1)
print(p)

if (dopermutation){
# run permutation test
  exptestvalue=runExperimentsByPermutation(sigcluster,abias.df)
  write.csv(exptestvalue,paste("exptestvalue",sname,".csv",sep="")) # save values
}else{
  exptestvalue=read.csv(paste("exptestvalue",sname,".csv",sep=""))  # used saved values
}
print(head(exptestvalue))

# get the simulated sum t-value for structure and intercept each cluster
sumt.df =  aggregate(cbind(simtstr,simtint) ~ age + cluster + sim, exptestvalue, sum)
head(sumt.df)

# find maximum sum t for each experiment
permlist = findMaximumSumTforEachExp(sumt.df)
permintdist=permlist[[2]] # simulated max intercept t
head(permintdist)

# test observed data against permutation distribution for intercept
sumclusterint  = aggregate(tint ~ cnumint + Agegroup, subset(meanabias.df, Condition == "Active"), sum)
head(sumclusterint)
outlists = testClusterSumTagainstDistribution(sumclusterint,"cnumint", permintdist,"tint",meanabias.df,abias.df)
meanabias.df = outlists[[1]]
abias.df = outlists[[2]]
head(meanabias.df)

# perm distribution for intercepts
meansigInt = subset(meanabias.df,permPtint < 0.025 & Condition == "Active")
sigmeanclust = aggregate(cbind(permTtint,permPtint)~cnum + Agegroup, meansigInt,mean)
sigmeanclust$age= sigmeanclust$Agegroup
p = ggplot(permintdist,aes(x=simtint))
p = p +geom_histogram(bins = 100) + facet_wrap(~ age,ncol=1)
p = p + geom_vline(aes(xintercept=permTtint),sigmeanclust,colour="red")
p

bitmap("Figure1.tiff", height = 9, width = 13, units = 'cm', res=600)
# this figure shows the permutation analysis for first noun as agent bias
meanabias2.df = aggregate(cbind(permPtint,pint,agentbias,cnumint) ~ timewindow + Agegroup, meanabias.df, mean)
meanabias2.df$Agegroup2 = factor(meanabias2.df$Agegroup, labels = c("2-year olds","3-year olds")) 
meanabias2.df$tsig = ifelse(meanabias2.df$pint < 0.05,"sigf","notsig")
# create figure with p-values as bars
meansigInter = subset(meanabias2.df,permPtint < 0.025) 
pliney = 0.4  # position of p-value bar
pbarlen = 0.2 # length of p-value bars at bottom
maxy = 0.7
miny = 0.3
txtsize = 3
p = ggplot(meanabias2.df, aes( x = timewindow, y = agentbias))
if (length(meansigInter$timewindow) > 0){
  p = p + geom_rect(data=meansigInter,aes(xmin=timewindow, xmax=timewindow+windowsize, ymin = pliney+0.02, ymax= 0.7),fill="yellow",show.legend = FALSE)
}
p = p + geom_rect(aes(xmin=timewindow, xmax=timewindow+windowsize, ymin = pliney, ymax= pliney+pbarlen*(0.05-pint),fill= tsig),show.legend = FALSE)
p = p + geom_line()
p = p +facet_wrap(~ Agegroup2, ncol=1)
p = p + xlab("Time from first NP timelock (msec)")
p = formatfig(p)
p = p + geom_vline(xintercept = 551,colour="green")
p = p + annotate("text", x = 551, y = 0.65, label = "is",size=txtsize,hjust = 0)

p = p + annotate("text", x = 1370+50, y = 0.65, label = "NP2 passive",size=txtsize,hjust = 0)
p = p + geom_segment(aes(x= 1370,y = 0.5, xend=1370, yend=Inf),linetype = 2, colour = "purple")

p = p + annotate("text", x = 983+50, y =0.45 , label = "NP2 active",size=txtsize, hjust = 0)
p = p + geom_segment(aes(x=983,y = 0.5, xend=983, yend=pliney+0.02),linetype = 2, colour = "purple")

p = p + annotate("text", x = 0+50, y = 0.65, label = "NP1",size=txtsize,hjust = 0)
p = p + geom_segment(aes(x=0,y = -Inf, xend=0, yend=Inf),linetype = 3, colour = "black")
p
dev.off()
p

limitsdf = aggregate(timewindow ~ Agegroup + cnumint, meansigInter,min)
names(limitsdf)[3]<-"mintime"
# add windowsize to maxtime to get end of window
limitsdf$maxtime = aggregate(timewindow ~ Agegroup + cnumint, meansigInter,max)$timewindow+windowsize
print(limitsdf)




####################### STUDY 2 timelocked to 2nd NP #######################
file = "St2All_2ndNP_new_templates_balancedN_noadults.csv"
onedata2.df = read.csv(file)
NP2data.df = convertEyeTrackIntoDataFrame(onedata2.df)
xtabs(~ timebin, NP2data.df)

alltrialsNP2.df = subset(NP2data.df, Trial %in% c(5,6,7,8,9,10))
alltrialsNP2.df = subset(alltrialsNP2.df, timewindow < 6000)
xtabs(~ timebin, alltrialsNP2.df)

# NP2 figure Jan 2016
means.df = aggregate(agentbiasbinary ~ timewindow + Condition + Agegroup, alltrialsNP2.df, mean)
means.df$Agegroup = factor(means.df$Agegroup, labels = c("2-year olds","3-year olds"))
p = ggplot(means.df , aes( x = timewindow, y = agentbiasbinary, colour = Condition,group=Condition))
p = p + geom_line()+ scale_color_brewer(palette="Set1")
p = p +facet_wrap(~ Agegroup, ncol=1)
p

windowsize = 20
alltrialsNP2.df$timebin = floor(alltrialsNP2.df$timewindow/windowsize)
length(unique(alltrialsNP2.df$timewindow))
length(unique(alltrialsNP2.df$timebin))

alltrialsNP2.df$Participant = paste(alltrialsNP2.df$Participant,alltrialsNP2.df$Agegroup,sep="_")
max(alltrialsNP2.df$timebin)
#aggregated over 6 test trials -> overall proportion active match -> empirically logit transformed
abiasNP2.df = aggregate(agentbiasbinary ~ cactive + Condition + timebin + Agegroup + Participant, alltrialsNP2.df, mean)
names(abiasNP2.df)[6] <- "agentbias"
head(abiasNP2.df)
max(abiasNP2.df$timebin)
#xtabs( ~ timebin, abiasNP2.df)
abiasNP2.df$logitagentbias = log((abiasNP2.df$agentbias+0.5)/(1-abiasNP2.df$agentbias+0.5))
#xtabs(~  timebin, abiasNP2.df)
#aggregate(timebin ~ Participant,alltrialsNP2.df,mean)

# this data frame is used for the figures
meanabiasNP2.df = aggregate(agentbias ~ cactive + Condition + timebin + Agegroup, abiasNP2.df, mean)
meanabiasNP2.df$pstr = 1000
meanabiasNP2.df$timewindow = meanabiasNP2.df$timebin * windowsize
max(meanabiasNP2.df$timebin)

# Step 1) for each age and each time window, compute observed t-value
meanabiasNP2.df=computeTestStatisticOnRaw(abiasNP2.df,meanabiasNP2.df)
head(meanabiasNP2.df)
max(meanabiasNP2.df$timebin)

xtabs(~ timebin,meanabiasNP2.df)

# create clusters for adjacent windows with p<0.05 for structure and intercept
meanabiasNP2.df = findclusters(meanabiasNP2.df)
head(meanabiasNP2.df)
max(meanabiasNP2.df$timebin)

# any window that has a significant intercept or structure
# we only use active, because we only one row for each lm model
sigclusterNP2 = subset(meanabiasNP2.df, timebin > 0 &  Condition == "Active" & pstr < 0.05)
head(sigclusterNP2)

length(unique(sigclusterNP2$cnum)) # total significant clusters found
length(unique(sigclusterNP2$cnum[sigclusterNP2$Agegroup==2])) # cluster for 2 year olds
length(unique(sigclusterNP2$cnum[sigclusterNP2$Agegroup==3])) # clusters for 3 year olds
max(sigclusterNP2$timebin)

# this figure shows lines for each sig structure cluster.  
# They are all above 0.5, since that is what the intercept is testing
p = ggplot(sigclusterNP2 , aes( x = timewindow, y = agentbias,colour=factor(cnum)))
p = p + geom_line()
p = p +facet_wrap(~ Agegroup, ncol=1)
print(p)

if (dopermutation){
  # run permutation test
  exptestvalueNP2=runExperimentsByPermutation(sigclusterNP2,abiasNP2.df)
  write.csv(exptestvalueNP2,"exptestvalueNP2.csv") # save values
}else{
  exptestvalueNP2=read.csv("exptestvalueNP2.csv")  # used saved values
}
print(head(exptestvalueNP2))

# get the simulated sum t-value for structure and intercept each cluster
sumtNP2.df =  aggregate(cbind(simtstr,simtint) ~ age + cluster + sim, exptestvalueNP2, sum)
head(sumtNP2.df)

# find maximum sum t for each experiment
permlist = findMaximumSumTforEachExp(sumtNP2.df)
permstrdistNP2=permlist[[1]] # simulated max structure t
head(permstrdistNP2)

# here we test for significant structure clusters in the real data
# we only do one test for each time window, so using active only (but data includes passive)
sumclusterNP2  = aggregate(tstr ~ cnum + Agegroup, subset(meanabiasNP2.df, Condition == "Active"), sum)
head(sumclusterNP2)

# test observed data against permutation distribution for structure
outlists = testClusterSumTagainstDistribution(sumclusterNP2,"cnum", permstrdistNP2,"tstr",meanabiasNP2.df,abiasNP2.df)
meanabiasNP2.df = outlists[[1]] # for figure
abiasNP2.df = outlists[[2]]
head(meanabiasNP2.df)

# this shows a structure perm dist histogram with significant regions
meansigStr = subset(meanabiasNP2.df,permPtstr < 0.025 & Condition == "Active")
sigmeanclust = aggregate(cbind(permTtstr,permPtstr)~cnum + Agegroup, meansigStr,mean)
sigmeanclust$age= sigmeanclust$Agegroup
p = ggplot(permstrdistNP2,aes(x=simtstr))
p = p +geom_histogram(binwidth=1) + facet_wrap(~ age,ncol=1)
p = p + geom_vline(aes(xintercept=permTtstr),sigmeanclust,colour="red")
p

bitmap("Figure2.tiff", height = 11, width = 13, units = 'cm', res=600)
# this figure shows the permutation analysis for structure
meanabiasNP2.df = subset(meanabiasNP2.df, timebin > 0)
meanabiasNP2.df$tsig = ifelse(meanabiasNP2.df$pstr < 0.05,"sigf","notsig")
meanabiasNP2.df$Agegroup2 = factor(meanabiasNP2.df$Agegroup, labels = c("2-year olds","3-year olds")) 
# create figure with p-values as bars
meansigStrNP2 = subset(meanabiasNP2.df,permPtstr < 0.025) 
pliney = 0.35
pbarlen = 0.2
p = ggplot(meanabiasNP2.df, aes( x = timewindow, y = agentbias, group=Condition,  linetype = Condition))
if (length(meansigStrNP2$timewindow) > 0){
  p = p + geom_rect(data=meansigStrNP2,aes(xmin=timewindow, xmax=timewindow+windowsize, ymin = pliney, ymax= Inf),fill="orange",show.legend=FALSE)
}
p = p + geom_rect(aes(xmin=timewindow, xmax=timewindow+windowsize, ymin = pliney-0.05, ymax= -0.05+pliney+pbarlen*(0.05-pstr),fill= tsig),show.legend = FALSE)
p = p + geom_line()
p = p +facet_wrap(~ Agegroup2, ncol=1)
p = p + xlab("Time from second NP timelock (msec)")
p=formatfig(p)
p = p + annotate("text", x = 0+50, y = 0.35, label = "NP2",size=txtsize,hjust = 0)
p = p + geom_segment(aes(x=0,y = -Inf, xend=0, yend=Inf),linetype = 3, colour = "black")
p
dev.off()
p

limitsNP2df = aggregate(timewindow ~ Agegroup + cnum, meansigStrNP2,min)
names(limitsNP2df)[3]<-"mintime"
# add windowsize to maxtime to get end of window
limitsNP2df$maxtime = aggregate(timewindow ~ Agegroup + cnum, meansigStrNP2,max)$timewindow+windowsize
print(limitsNP2df)




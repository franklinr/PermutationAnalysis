## permutation script 2015
require(ggplot2)
require(lme4)

#Set your working directory
#  Go to Session in menu, go to Set Working Directory, 
#  click on To Source File Location



# this is a data set for an eye-tracking study looking at active and passive strucutures 
# in English and Japanese.  Data collected every 100ms for 2000ms.
data = read.csv("passive.csv")
# plot figure for dataset
means.df = aggregate(target ~ time + lang + cond, data, mean)
p = ggplot(means.df , aes( x = time, y = target, colour=cond))
p = p + geom_line()
p = p + scale_colour_brewer(palette="Set1")
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to passive match")
p = p + ylim(0,1)
p = p + facet_wrap(~ lang, ncol=1)
p = p + geom_vline(xintercept = 500,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 1000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 1500,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 2000,colour="black", linetype = 2) 
p

# here we do a standard mixed model analysis using 500 ms windows
data$win = as.integer(-0.001+data$time/500)
subjmeans.df = aggregate(target ~ win + lang + cond + subj, data, mean)
subjmeans.df$cwin = subjmeans.df$win - mean(subjmeans.df$win)
subjmeans.df$ceng = ifelse(subjmeans.df$lang == "English",0.5,-0.5)
subjmeans.df$cpassive = ifelse(subjmeans.df$cond == "passive",0.5,-0.5)
model = lmer(target ~ cwin*ceng*cpassive + (1 | subj), subjmeans.df)
print(summary(model))
# nothing is significant

# create copy of data frame
pdata = data
pdata$cpassive = ifelse(pdata$cond == "passive",0.5,-0.5)
# create data frame which averages over subjects.  
# This also stores the results of the permutation analysis
means.df = aggregate(target ~ cond + cpassive + time + lang, pdata, mean)
means.df$pstr = 1000

# permutation analysis uses simple tests to find significant regions.  
# Here is a regression on target based on structure for one time window (400 ms) in English
onetime = subset(pdata,time == 400 & lang == "English")
# do regression model on target using structure condition
onemodel = lm(target ~ cpassive, onetime)
print(summary(onemodel))  # 
print(abs(coef(summary(onemodel))[2,4]))  # get observed p-value

# We do this for each 100 ms window in the data
timelist = unique(pdata$time)
for (l in c("English","Japanese")){
  for (t in timelist){
    # create data frame for ONE timebin for each language
    onetime = subset(pdata,time == t & lang == l)
    # do regression model on target using structure condition
    onemodel = lm(target ~ cpassive, onetime)
    # print(summary(tarmodel))
    # this is the t-value for structure
    targetT = coef(summary(onemodel))[2,3]  # observed t-value
    targetP = abs(coef(summary(onemodel))[2,4])  # observed p-value
    means.df$tstr[means.df$time == t & means.df$lang == l] = targetT
    means.df$pstr[means.df$time == t & means.df$lang == l] = targetP
  }
}

# to see these p-values, we draw them arbitrarily on the graph at 0.2.
# when the p-value < 0.05, we draw a blue line above 0.2
# when the p-value > 0.05, we draw an orange line below 0.2
wsize = 40
pliney = 0.2
plinemax = 0.1
means.df$pline = pliney+plinemax*(0.05-means.df$pstr)
means.df$plinecol = ifelse(means.df$pstr < 0.05,"a","b")

p = ggplot(means.df , aes( x = time, y = target, colour=cond))
p = p + geom_line()
p = p + scale_colour_brewer(palette="Set1")
p = p + scale_fill_brewer(palette="Set2")
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to passive match")
p = p + ylim(0,1)
p = p + facet_wrap(~ lang, ncol=1)
p = p + geom_rect(aes(xmin=time-wsize, xmax=time+wsize, ymin = pliney, ymax= pline, colour=NA,fill=plinecol),show.legend=FALSE)
p
# we can't use these p-values, because we have done 20 comparisons, 
# so we have 20 times greater chance of finding significant effect (multiple comparisons problem)

# Also, each window is not independent, so we create clusters for adjacent windows with p<0.05
# cnum is the cluster number and we increment the number when the p value is > 0.05
# so clusters with the same cnum are part of the same cluster
cnum = 1
lastpval = 100
means.df$cnum = 1
for (l in c("English","Japanese")){
  for (t in timelist){
    onetime = subset(means.df,time == t & lang == l & cond == "active")
    pval = abs(onetime$pstr)
    tdir = onetime$tstr
    if (pval < 0.05 & lastpval > 0.05 ){
      cnum = cnum + 1  # increase cluster number when entering a significant cluster from a non-significant cluster
    }
    if (pval > 0.05 ){  
      cnum = cnum + 1 # increase cluster number when not significant
    }else{
      # if t value flips direction, even if both are signif, 
      # we should treat those as separate clusters
      if (lasttdir*tdir < 0){
        cnum = cnum + 1 
      }
    }
    lastpval = pval
    lasttdir = tdir
    means.df$cnum[means.df$time == t & means.df$lang == l] = cnum
  }
  cnum=cnum+1  # new cluster for different languages
}
head(means.df,10)

# this shows the clusters
p = ggplot(means.df , aes( x = time, y = target, colour=cond, label=cnum))
p = p + scale_colour_brewer(palette="Set1")
p = p + scale_fill_brewer(palette="Set2")
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to passive match")
p = p + ylim(0,1)
p = p + facet_wrap(~ lang, ncol=1)
p = p + geom_rect(aes(xmin=time-wsize, xmax=time+wsize, ymin = pliney, ymax= pline, colour=NA,fill=plinecol),show.legend=FALSE)
p + geom_text(size=4)

# we now want to identify the clusters that were significant
# p-values are same for active and passive, so we just used the active items.
meansonlyact.df = subset(means.df, cond == "active")
sigcluster = subset(meansonlyact.df, abs(pstr) < 0.05 )
print(sigcluster)
# this computes the sum of the t-values for each cluster
sumcluster  = aggregate(tstr ~ cnum + lang, meansonlyact.df, sum)
print(sumcluster)

# now we create a distribution of t-values (save in permdist)
# by randomly scrambling the active and passive labels for each time window 1000 times
n = 100
exptestvalue = data.frame()
for (s in 1:length(sigcluster$time)){
  #  print(cl)
  cl = sigcluster$cnum[s] # cluster number
  b = sigcluster$time[s] # time
  l = sigcluster$lang[s] # language
  print(paste("b ",b," lang",l))
  # one time point
  onetime = subset(pdata,lang == l & time %in% b)
  # randSet is a copy of onetime that is scrambled
  randSet = onetime
  
  for (i in 1:n){
    #  set.seed(i)
    # randomly scramble cpassive labels without replacement
    randSet$cpassive = sample(randSet$cpassive,length(randSet$cpassive))
    # test if target is related to random scrambled cpassive
    randmodel = lm(target ~ cpassive ,randSet)
    #  print(summary(randmodel))
    # extract and save t-values
    t = coef(summary(randmodel))[2,3]
    df = data.frame(t=t,cluster=cl,time=b,lang=l,sim=i)
    exptestvalue = rbind(exptestvalue, df )
  }
}
# we sum over clusters so that longer clusters have stronger t-values
sumt.df =  aggregate(t ~ lang + cluster + sim, exptestvalue, sum)
head(sumt.df)

# simulated sum cluster histogram
p = ggplot(sumt.df,aes(x = t))
p = p +geom_histogram(binwidth=0.5)
p = p + facet_wrap(~ lang)
p+theme_bw()

# this code extracts out the maximum sum t for each simulation at each age
maxclusterdist = data.frame()
for (l in unique(sumt.df$lang)){
  for (s in unique(sumt.df$sim)) {
    # get all results for one simulation in one language
    onesim = subset(sumt.df,sim == s & lang == l)
    onesim$astruct = abs(onesim$t)
    # find max t-value
    maxrow = onesim[order(onesim$astruct,decreasing = T),]
    maxclusterdist = rbind(maxclusterdist,maxrow[1,])
  }
}
head(maxclusterdist)

# Shows the simulated distribution with maximum cluster t values
maxclusterdist2 = maxclusterdist[order(maxclusterdist$lang,maxclusterdist$t),]
end = data.frame(lang=c("English","English","Japanese","Japanese"),xint = maxclusterdist2[c(25,975,1025,1975),]$t)
p = ggplot(maxclusterdist,aes(x = t))
p = p +geom_histogram(binwidth=0.5)
p = p +geom_vline(end,mapping=aes(xintercept=xint))
p = p + facet_wrap(~ lang)
p+theme_bw()

# maxclusterdist is sorted by language, 
# so first 1000 are English and second 1000 are Japanese
emaxclusterdist = maxclusterdist$t[1:1000]
jmaxclusterdist = maxclusterdist$t[1001:2000]

# this identifies tvalues that are greater than dist t-values
for (cl in unique(sumcluster$cnum)){
  bins = unique(means.df[means.df$cnum == cl,]$time)
  lan = sumcluster$lang[sumcluster$cnum == cl][1]
  
  tstr = abs(sumcluster$tstr[sumcluster$cnum==cl])
  # permutation t distribution depends on language
  if (lan == "English"){
    permtdist = emaxclusterdist
  }else{
    permtdist = jmaxclusterdist
  }
  # permtdist is the dist t-values, 
  # so p value is proportion of values greater than observed t-value.
  pstr = sum(abs(permtdist) > tstr, na.rm = TRUE)/length(permtdist)
  if (tstr > 2){
  print(paste("Cluster ",cl," Observed sum t ",round(tstr,3)," Proportion dist > observed (p-value) ",pstr))
  }
  means.df$permtestp[means.df$time %in% bins & means.df$lang == lan] = pstr
}

# now we update our plot
p = ggplot(means.df , aes( x = time, y = target, colour=cond))
# this pulls out the clusters which are significant by the permutation test
meansigStr = subset(means.df,permtestp < 0.025) 
# color them grey
if (length(meansigStr$time) > 0){
  p = p + geom_rect(data=meansigStr,aes(xmin=time-50, xmax=time+50, ymin = pliney+0.1, ymax= 1.0),,colour=NA,fill="grey90",show.legend=FALSE)
}
# same as before
p = p + geom_line()
p = p + scale_colour_brewer(palette="Set1")
p = p + scale_fill_brewer(palette="Set2")
p = p + theme_bw()   # change background to white
p = p + ylab("Looks to passive match")
p = p + ylim(0,1)
p = p + facet_wrap(~ lang, ncol=1)
p = p + geom_vline(xintercept = 500,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 1000,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 1500,colour="black", linetype = 2) 
p = p + geom_vline(xintercept = 2000,colour="black", linetype = 2) 
p = p + geom_rect(aes(xmin=time-wsize, xmax=time+wsize, ymin = pliney, ymax= pline, colour=NA,fill=plinecol),show.legend=FALSE)
p
ggsave("perm.png")

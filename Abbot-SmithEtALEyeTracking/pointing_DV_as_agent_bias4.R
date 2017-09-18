require(ggplot2)
require(lme4)

# set default font for ggplot
theme_set(theme_get() + theme(text = element_text(family = 'Arial')))

sessionInfo()

dataOrig <- read.csv ("pointing_item_as_item.csv")
head(dataOrig)    # show file
# remove DV which are NA
data1 = subset(dataOrig, ! is.na(DV))

# check number of participants in each cell
xtabs(~ Condition + Agegroup, subset(data1, Item == 1))
# count the number of items per condition/age
xtabs(~ Item + Condition + Agegroup, data1)

data1$DV[data1$Condition == "passive" & data1$DV == 1] = 7
data1$DV[data1$Condition == "passive" & data1$DV == 0] = 8
data1$DV[data1$Condition == "passive" & data1$DV == 7] = 0
data1$DV[data1$Condition == "passive" & data1$DV == 8] = 1

#use update function for p values here
modelComparison <- function(model){
  #  print(model)
  terms = attr(terms(model),"term.labels")
  #  print(terms)
  if (length(terms) > 0){
    print(paste("########## Remove ",terms[length(terms)]))
    newformula = paste(". ~ . - ",terms[length(terms)],"")
    model2.glmer = update(model, as.formula(newformula))
    print(summary(model2.glmer))
    print(anova(model2.glmer, model))
    terms = attr(terms(model),"term.labels")
    modelComparison(model2.glmer)
  }
}


# since Participant and Item are numbers, we need to use factor to make them into categories
data1$Participant = factor(data1$Participant)
data1$Item = factor(data1$Item)
# condition and age should be factors because they are strings, but we do this just to be safe
data1$Condition = factor(data1$Condition)
data1$Agegroup = factor(data1$Agegroup)
# centered variables
data1$cThreeAge =  ifelse(data1$Agegroup == "3_years", 0.5, -0.5)
data1$cPassive =  ifelse(data1$Condition == "passive", 0.5, -0.5)
data1$DV = as.numeric(data1$DV)

# model for all participants (maximal model given the data )
fullmodel.glmer = glmer(DV ~ cPassive * cThreeAge  + (1 +cPassive  | Item) + (1  | Participant), data1, family="binomial", control=glmerControl(optimizer = "bobyqa"))
print(summary(fullmodel.glmer))
modelComparison(fullmodel.glmer)

# 3-year-olds
threedata = subset(data1, Agegroup == "3_years")
threedata$Participant = factor(threedata$Participant)
threedata$Item = factor(threedata$Item)
threedata$Condition = factor(threedata$Condition)
threedata$cPassive =  ifelse(threedata$Condition == "passive", 0.5, -0.5)
threedata$DV = as.numeric(threedata$DV)


# model for three-year-olds
three.glmer = glmer(DV ~ cPassive  + (1 + cPassive | Item) + (1 | Participant), threedata, family="binomial", control=glmerControl(optimizer = "bobyqa"))
print(summary(three.glmer))
modelComparison(three.glmer)

# 2-year-olds
twosdata = subset(data1, Agegroup == "2_years")
twosdata$Participant = factor(twosdata$Participant)
twosdata$Item = factor(twosdata$Item)
twosdata$Condition = factor(twosdata$Condition)
twosdata$cPassive =  ifelse(twosdata$Condition == "passive", 0.5, -0.5)
twosdata$DV = as.numeric(twosdata$DV)

# model for two-year-olds
two.glmer = glmer(DV ~ cPassive  + (1  | Item) + (1 | Participant), twosdata, family="binomial", control=glmerControl(optimizer = "bobyqa"))
print(summary(two.glmer))
modelComparison(two.glmer)

# to create error bars, we use remef to get predicted values for our data 
# without random effects and then estimate varianced based on the predicted data
# install.packages("devtools")
#devtools::install_github("hohenstein/remef")
library(remef)

# now we predict values based on the fixed factors in our model
data1$logitpred = remef(fullmodel.glmer, ran = "all")
# since our model results logit terms, we convert back to proportions
data1$proppred = 100*exp(data1$logitpred)/(1 + exp(data1$logitpred))
head(data1)
# first we get our means for our bar chart
means.df = aggregate(DV ~  Agegroup + Condition, data1, mean)
# now we get sd for each age/cond using our predicted data without random effects
sd.df = aggregate(proppred ~  Agegroup + Condition, data1, sd)
# now we copy sd into means.df
means.df$sd = sd.df$proppred
# standard error is relative to sample size, using number of participants
ssize = nlevels(data1$Participant)
means.df$se = means.df$sd/sqrt(ssize)
# make DV percentage
means.df$perc = means.df$DV * 100
means.df$upper = means.df$perc + means.df$se
means.df$lower = means.df$perc - means.df$se
print(means.df)

bitmap("Figure3.tiff", height = 11, width = 13, units = 'cm', res=600)

means.df$Structure = factor(means.df$Condition, labels=c("Active","Passive"))
means.df$Agegroup = factor(means.df$Agegroup, labels=c("2 year olds","3 year olds"))
## lower and upper in mapping aes
p = ggplot(means.df, aes( x = Agegroup, y = perc, fill = Structure, ymin = lower, ymax= upper))
p = p + geom_bar(stat="identity",position="dodge")
p = p + geom_errorbar(width=0.25, position=position_dodge(.9) )
p = p + geom_hline(aes(yintercept = 50),colour="black")
p = p + theme_bw()
p = p + ylim(0,100)
p = p + scale_fill_manual(values = c("grey40","grey"))
p=p+ylab("Percentage Active Match Points")
p=p+xlab("Age group")
p
dev.off()
p


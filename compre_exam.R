# read data
setwd('/Users/zihanxiao/Documents/UCDavis/19spring/comprehensive_exam')
alz_data = read.csv('zihan_msexam_data.csv', stringsAsFactors = FALSE)

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#-----------------------------Select 3 summary measures------------------------------#
# delete rows with NAs in cognitive function measures
alz_data1 = alz_data[complete.cases(alz_data[, colnames(alz_data)[c(8:14, 20:22)]]), ]
dim(alz_data1)
summary(alz_data1)

# check outliers
sapply(alz_data1[c('AGE', 'Ventricles_bl', 'Hippocampus_bl', 'Entorhinal_bl', 'FDG_bl')], boxplot)

# replace missing values in other features with median
replace_na = function(x){
  x[is.na(x)] = mean(x, na.rm = T)
  return(x)
}

alz_data1[c('AGE', 'Ventricles_bl', 'Hippocampus_bl', 'Entorhinal_bl', 'FDG_bl')] = sapply(alz_data1[c('AGE', 'Ventricles_bl', 'Hippocampus_bl', 'Entorhinal_bl', 'FDG_bl')], replace_na)

summary(alz_data1)

# cognitions measures 
cog = alz_data1[, colnames(alz_data1)[c(9:14, 20:22)]]
dim(cog)

# pca
cog.pca = prcomp(cog)
summary(cog.pca)
# select the top 3 principle components which explain 99% variance
cog_pc = cog.pca$x[, 1:3]

#---------------------------Compare EMCI and LMCI----------------------------------#
emci = alz_data1[which(alz_data1$DX_bl == 'EMCI'), ]
lmci = alz_data1[which(alz_data1$DX_bl != 'EMCI'), ]

# compare predictors distribution for EMCI and LMCI

#------age------#
ggplot(alz_data1, aes(x=DX_bl, y=AGE, fill = DX_bl)) + 
  labs(title="Boxplot of AGE vs MCI status")+
  theme_minimal() +
  geom_boxplot() +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())

#------sex------#
library(ggplot2)
# summary the dataset of sex vs MCI status
df = table(alz_data1[c('PTGENDER', 'DX_bl')])
PTGENDER = rep(c('Female', 'Male'), 2)
MCI_status =c(rep('EMCI',2),  rep('LMCI',2))
Numbers = c(df[, 1], df[, 2])
df = data.frame(PTGENDER, MCI_status, Numbers)
# plot
p1 = ggplot(df, aes(fill=PTGENDER, y=Numbers, x=MCI_status)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Blues") +
  ggtitle('Distributions of Sex in EMCI and LMCI group') +
  theme_minimal() +
  theme(legend.title = element_blank())

#------year of education-------#
ggplot(alz_data1, aes(x=PTEDUCAT, color=DX_bl, fill=DX_bl)) +
  geom_histogram(alpha=0.5, position="identity", binwidth=1) +
  labs(title="Year of Educations",x="Years", y = "Count")+
  theme_minimal() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

#---genetics---#
# summary the dataset of genetics vs MCI status
df = table(alz_data1[c('APOE4', 'DX_bl')])
APOE4 = factor(rep(c(0,1,2), 2))
MCI_status =c(rep('EMCI',3),  rep('LMCI',3))
Numbers = c(df[, 1], df[, 2])
df = data.frame(APOE4, MCI_status, Numbers)
# plot
p2 = ggplot(df, aes(fill=APOE4, y=Numbers, x=MCI_status)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Blues") +
  ggtitle('Distributions of APOE4 in EMCI and LMCI group') +
  theme_minimal()

multiplot(p1, p2, cols=2)

#-----imaging measures------#
# Ventricles_bl
p3 = ggplot(alz_data1, aes(x=Ventricles_bl, color=DX_bl, fill=DX_bl)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="Volume of the ventricles histogram plot",x="Volume of the ventricles", y = "Count")+
  theme_minimal() +
  theme(legend.title = element_blank())

# Hippocampus_bl
p4 = ggplot(alz_data1, aes(x=Hippocampus_bl, color=DX_bl, fill=DX_bl)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="Volume of the hippocampus histogram plot",x="Volume of the hippocampus", y = "Count")+
  theme_minimal() +
  theme(legend.title = element_blank())

# Entorhinal_bl
p5= ggplot(alz_data1, aes(x=Entorhinal_bl, color=DX_bl, fill=DX_bl)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="Volume of the entorhinal cortex histogram plot",x="Volume of the entorhinal cortex", y = "Count")+
  theme_minimal() +
  theme(legend.title = element_blank())

# FDG_bl
p6 = ggplot(alz_data1, aes(x=FDG_bl, color=DX_bl, fill=DX_bl)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="Summary FDG measure histogram plot",x="PET", y = "Count")+
  theme_minimal() +
  theme(legend.title = element_blank())

multiplot(p3, p4, p5, p6, cols=2)

#-------cognitions-------#
alz_data2 = cbind(alz_data1, cog_pc)
# PC1
p7 = ggplot(alz_data2, aes(x=PC1, color=DX_bl, fill=DX_bl)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="1st pricipal component of cognitions histogram",x="PC1", y = "Count")+
  theme_minimal() +
  theme(legend.title = element_blank())

# PC2
p8 = ggplot(alz_data2, aes(x=PC2, color=DX_bl, fill=DX_bl)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="2nd pricipal component of cognitions histogram",x="PC2", y = "Count")+
  theme_minimal() +
  theme(legend.title = element_blank())

# PC3
p9 = ggplot(alz_data2, aes(x=PC3, color=DX_bl, fill=DX_bl)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="3rd pricipal component of cognitions histogram",x="PC3", y = "Count")+
  theme_minimal() +
  theme(legend.title = element_blank())

multiplot(p7, p8, p9, cols=2)

#--------------------------event status of progress to dementia-------------------------#
alz_data1$Dementia = factor(alz_data1$Dementia)

#------age-----#
ggplot(alz_data1, aes(x=Dementia, y=AGE, fill = Dementia)) + 
  labs(title="Boxplot of AGE vs Dementia Status")+
  theme_minimal() +
  geom_boxplot() +
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
  labs(color = "Dementia")

#------year of education-------#
ggplot(alz_data1, aes(x=PTEDUCAT, color=Dementia, fill=Dementia)) +
  geom_histogram(alpha=0.5, position="identity", binwidth=1) +
  labs(title="Year of Educations vs Dementia Status",x="Years", y = "Count")+
  theme_minimal() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

#------sex------#
# summary the dataset of sex vs MCI status
df = table(alz_data1[c('PTGENDER', 'Dementia')])
PTGENDER = rep(c('Female', 'Male'), 2)
event_status =c(rep('Cencored',2),  rep('Progessed to Dementia',2))
Numbers = c(df[, 1], df[, 2])
df = data.frame(PTGENDER, event_status, Numbers)
# plot
p10 = ggplot(df, aes(fill=PTGENDER, y=Numbers, x=event_status)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distributions of Sex in Cencored group and Dementia group",x="Patient Status", y = "Count")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())

#---genetics---#
# summary the dataset of genetics vs event status
df = table(alz_data1[c('APOE4', 'Dementia')])
APOE4 = factor(rep(c(0,1,2), 2))
event_status =c(rep('Cencored',3),  rep('Progessed to Dementia',3))
Numbers = c(df[, 1], df[, 2])
df = data.frame(APOE4, event_status, Numbers)
# plot
p11 = ggplot(df, aes(fill=APOE4, y=Numbers, x=event_status)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distributions of APOE4 in Cencored group and Dementia group",x="Patient Status", y = "Count")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p10, p11, cols=2)

#-----imaging measures------#
# Ventricles_bl
p12 = ggplot(alz_data1, aes(x=Ventricles_bl, color=Dementia, fill=Dementia)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="Volume of the ventricles histogram plot",x="Volume of the ventricles", y = "Count")+
  theme_minimal()

# Hippocampus_bl
p13 = ggplot(alz_data1, aes(x=Hippocampus_bl, color=Dementia, fill=Dementia)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="Volume of the hippocampus histogram plot",x="Volume of the hippocampus", y = "Count")+
  theme_minimal()

# Entorhinal_bl
p14 = ggplot(alz_data1, aes(x=Entorhinal_bl, color=Dementia, fill=Dementia)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="Volume of the entorhinal cortex histogram plot",x="Volume of the entorhinal cortex", y = "Count")+
  theme_minimal()

# FDG_bl
p15 = ggplot(alz_data1, aes(x=FDG_bl, color=Dementia, fill=Dementia)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="Summary FDG measure histogram plot",x="PET", y = "Count")+
  theme_minimal()

multiplot(p12, p13, p14, p15, cols=2)

#-------cognitions-------#
alz_data2 = cbind(alz_data1, cog_pc)
# PC1
p16 = ggplot(alz_data2, aes(x=PC1, color=Dementia, fill=Dementia)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="1st pricipal component of cognitions histogram",x="PC1", y = "Count")+
  theme_minimal()

# PC2
p17 = ggplot(alz_data2, aes(x=PC2, color=Dementia, fill=Dementia)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="2nd pricipal component of cognitions histogram",x="PC2", y = "Count")+
  theme_minimal()

# PC3
p18 = ggplot(alz_data2, aes(x=PC3, color=Dementia, fill=Dementia)) +
  geom_histogram(alpha=0.5, position="identity") +
  labs(title="3rd pricipal component of cognitions histogram",x="PC3", y = "Count")+
  theme_minimal()

multiplot(p16, p17, p18, cols=2)


#---------Assess difference of progression to Dementia between EMCI and LMCI------------#
#-------survival functions in EMCI and LMCI---------#
alz_data1$Dementia = as.numeric(as.character(alz_data1$Dementia))
alz_data2$Dementia = as.numeric(as.character(alz_data2$Dementia))
# KM plot
library(survival)
library(ggfortify)
fit <- survfit(Surv(time, Dementia) ~ DX_bl, data = alz_data1)
autoplot(fit, main = 'Kaplan-Meier Curve in EMCI group and LMCI group')
# log-rank test
survdiff(Surv(time,Dementia)~DX_bl, data=alz_data1)#log-rank
# p-value < 2e-16, indicating survival functions are significantly different

#-------------Assess the association between genetics, imaging markers, 
# cognitions and progrssion to Dementia ------------------------------#
library(survminer)
colnames(alz_data2)
# transform gender, MCI_status and APOE4 to categorical variable
alz_data2$PTGENDER = factor(alz_data2$PTGENDER)
alz_data2$DX_bl = factor(alz_data2$DX_bl)
alz_data2$APOE4 = factor(alz_data2$APOE4)
# try cox model with all variables
fit.coxph <- coxph(Surv(time, Dementia) ~ AGE + PTGENDER + PTEDUCAT + DX_bl + APOE4 + 
                     Ventricles_bl + Hippocampus_bl + Entorhinal_bl + FDG_bl + PC1 + PC2 + PC3, 
                   data = alz_data2)
summary(fit.coxph)
# plot the summary
ggforest(fit.coxph, data = alz_data2)

# select significant variables in cox model and refit
fit.coxph1 = coxph(Surv(time, Dementia) ~ DX_bl + APOE4 + Entorhinal_bl + FDG_bl + PC1 + PC2 + PC3, 
                   data = alz_data2)
summary(fit.coxph1)
# plot the summary
ggforest(fit.coxph1, data = alz_data2)

#------------------------Interaction Terms------------------------------#
fit.coxph3 = coxph(Surv(time, Dementia) ~ DX_bl + APOE4 + Entorhinal_bl + FDG_bl + PC1 + PC2 + PC3
                   + DX_bl*APOE4 + DX_bl*Entorhinal_bl + DX_bl*FDG_bl + DX_bl*PC1 + DX_bl*PC2 + DX_bl*PC3, 
                   data = alz_data2)
summary(fit.coxph3)
# plot the summary
ggforest(fit.coxph1, data = alz_data2)

fit.coxph4 = coxph(Surv(time, Dementia) ~ APOE4 + Entorhinal_bl + FDG_bl + PC1 + PC2
                   + DX_bl*FDG_bl + DX_bl*PC3, 
                   data = alz_data2)
summary(fit.coxph4)
# plot the summary
ggforest(fit.coxph4, data = alz_data2)



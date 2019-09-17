#Measure Inter-rater reliability
library(readxl)
library(psych)
library(irr)

icc_dat <- read.csv('Y:/Main/Research/Research_share/Gulce Askin/RAD Contract/Katherine Simon/CView/icc_roc.csv')
icc_dat[is.na(icc_dat)] <- 0
View(icc_dat)

########## I. ICC ##########
#icc(ratings, model = c("oneway", "twoway"),
#type = c("consistency", "agreement"),
#unit = c("single", "average"), r0 = 0, conf.level = 0.95)

mat1<-data.matrix(icc_dat[3:5])  

icc_obj<-icc(mat1, model = c("twoway"), type = c("consistency"), unit = c("single"), r0 = 0, conf.level = 0.95) #from irr 

str(icc_obj)

tab1<-round(cbind(icc_obj$value, icc_obj$p.value, icc_obj$lbound, icc_obj$ubound), 3) 
colnames(tab1) <- c("ICC", "p-value", "95% CI Lower", "95% CI Upper")

tab1 %>%
  kable() %>%
  kable_styling(full_width = F)

#ES ifelse for p-val

########## II. Kappa ##########
kappa_dat <- read.csv('Y:/Main/Research/Research_share/Gulce Askin/RAD Contract/Dodelzon/demo.csv')

### Cohen's kappa for two raters 
mat2<-data.matrix(kappa_dat[c("AL_CVIEW", "AM_CVIEW")])
View(mat2)
kappa_obj_cohen<-cohen.kappa(mat2, w=NULL,n.obs=NULL,alpha=.05,levels=NULL)
str(kappa_obj_cohen)

tab2<-round(cbind(kappa_obj_cohen$value, kappa_obj_cohen$p.value), 3) 
colnames(tab2) <- c("Cohen's Kappa coefficient", "p-value")

kappa_obj_cohen$confid
#index the CIs' 
#[1,1]
#[1,3]

tab2 %>%
  kable() %>%
  kable_styling(full_width = F)



### Fleiss Kappa for 3 or more raters 
#kappam.fleiss(ratings, exact = FALSE, detail = FALSE)
#View(kappa_dat)
mat3<-data.matrix(kappa_dat[c("AL_CVIEW", "AM_CVIEW", "JK_CVIEW")]) #from irr

kappa_obj_fleiss<-kappam.fleiss(mat3, exact = FALSE, detail = FALSE) # Kappa = 0.509 

str(kappa_obj_fleiss)

tab3<-round(cbind(kappa_obj$value, kappa_obj$p.value), 3) 
colnames(tab2) <- c("Fleiss Kappa coefficient", "p-value")

tab3 %>%
  kable() %>%
  kable_styling(full_width = F)



### Weighted kappa for 2 raters 
#kappa2(ratings, weight = c("unweighted", "equal", "squared"), sort.levels = FALSE)
kappa_dat<-read.csv('Y:/Main/Research/Research_share/Gulce Askin/RAD Contract/Chazen/irr_combined.csv')
#View(kappa_dat)
mat4 <- kappa_dat[,c(2,7)]
wkappa_obj<-kappa2(mat4,"squared")

str(wkappa_obj)
tab4<-round(cbind(wkappa_obj$value, wkappa_obj$p.value), 3) 
colnames(tab4) <- c("Weighted Kappa coefficient", "p-value")

tab4 %>%
  kable() %>%
  kable_styling(full_width = F)



#Notes: 
#Would like confidence interval on ALL estimates, how to get CI from cohens? 
#show full p-value 
#alternative packages that seem to provide CI: confIntKappa function in *biostatUZH* https://rdrr.io/rforge/biostatUZH/man/confIntKappa.html







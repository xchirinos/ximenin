library(car)
library(xlsx)
library(agricolae)
library(ggplot2)

#Seleccionar directorio de trabajo
dir<-file.choose()

#Leyendo nuestros datitos
root <- read.csv(choose.files())#csv
root <- read.table(choose.files(),header=TRUE)#Notepad

H20 <- read.xlsx("D:/Google Drive/Roots/Harvest_ANOVA.xlsx", sheetIndex=1)#xlsx

# Normalidad
modelo <- aov(SPN ~ Genotype, data = H20)
resid <- resid(modelo)
shapiro.test(resid) # no tiene sentido

# Homogeneidad de varianzas
bartlett.test(SPN ~ Genotype, data = H20)#Bartlett(normalidad de los residuales)

#QQ-PLOT

modelo <- aov(SPN ~ Genotype, data = H20)
plot(modelo)

y<-H20$SPN
qqnorm(y); qqline(y, col = 2)
qqplot(y, rt(300, df = 5))

#Factorial con repeticiones
lm <- aov( SPN ~ R+Genotype*H, H20)
anova(lm)

fish <- LSD.test(lm, c("Genotype", "Harvest")) #Fisher
fish

turkey <- HSD.test(lm, c("Genotype", "Harvest")) #Tukey
turkey

dunquito <- duncan.test(lm,c("Genotype","H"),alpha=0.01,console=TRUE)
dunquito

# No parametrico Kruskal
x<-H20$SPN
g<-H20$Genotype
kruskal.test(x,g)


#H20

#SPN
lm <- aov( SPN ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SN
lm <- aov( SN ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LN
lm <- aov( LN ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LNSP
lm <- aov( LNSP ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#HG
lm <- aov( HG ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#STD
lm <- aov( STD ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#APS
lm <- aov( APS ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#AAVR
lm <- aov( AAVR ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LLPS
lm <- aov( LLPS ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LLAVR
lm <- aov( LLAVR ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RLPS
lm <- aov( RLPS ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RLAVR
lm <- aov( RLAVR ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RR
lm <- aov( RR ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RRAVR
lm <- aov( RRAVR ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RNT
lm <- aov( RNT ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RNAVR
lm <- aov( RNAVR ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SRN
lm <- aov( SRN ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#PRD
lm <- aov( PRD ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RFW
lm <- aov( RFW ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RDW
lm <- aov( RDW ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RDM
lm <- aov( RDM ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SFW
lm <- aov( SFW ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SDW
lm <- aov( SDW ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SDM
lm <- aov( SDM ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LFW
lm <- aov( LFW ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LDW
lm <- aov( LDW ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LDM
lm <- aov( LDM ~ R+Genotype, H20)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#_______________________________________________________________________________________________________________________
#H40
H40 <- read.xlsx("D:/Google Drive/Roots/Harvest_ANOVA.xlsx", sheetIndex=2)#xlsx

#SPN
lm <- aov( SPN ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SN
lm <- aov( SN ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LN
lm <- aov( LN ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LNSP
lm <- aov( LNSP ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#HG
lm <- aov( HG ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#STD
lm <- aov( STD ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#APS
lm <- aov( APS ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#AAVR
lm <- aov( AAVR ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LLPS
lm <- aov( LLPS ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LLAVR
lm <- aov( LLAVR ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RLPS
lm <- aov( RLPS ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RLAVR
lm <- aov( RLAVR ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SNT  
lm <- aov( SNT ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SNSP
lm <- aov( SNSP ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RR
lm <- aov( RR ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RRAVR
lm <- aov( RRAVR ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RNT
lm <- aov( RNT ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RNAVR
lm <- aov( RNAVR ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SRN
lm <- aov( SRN ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#PRD
lm <- aov( PRD ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RFW
lm <- aov( RFW ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RDW
lm <- aov( RDW ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RDM
lm <- aov( RDM ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SFW
lm <- aov( SFW ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SDW
lm <- aov( SDW ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SDM
lm <- aov( SDM ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LFW
lm <- aov( LFW ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LDW
lm <- aov( LDW ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LDM
lm <- aov( LDM ~ R+Genotype, H40)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#_______________________________________________________

#H60
H60 <- read.xlsx("D:/Google Drive/Roots/Harvest_ANOVA.xlsx", sheetIndex=3)#xlsx

#SPN
lm <- aov( SPN ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SN
lm <- aov( SN ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LN
lm <- aov( LN ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LNSP
lm <- aov( LNSP ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#HG
lm <- aov( HG ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#STD
lm <- aov( STD ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#APS
lm <- aov( APS ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#AAVR
lm <- aov( AAVR ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LLPS
lm <- aov( LLPS ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LLAVR
lm <- aov( LLAVR ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RLPS
lm <- aov( RLPS ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RLAVR
lm <- aov( RLAVR ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SNT  
lm <- aov( SNT ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SNSP
lm <- aov( SNSP ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SKN
lm <- aov( SKN ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SKSP
lm <- aov( SKSP ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#StRN
lm <- aov( StRN ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SRSP
lm <- aov( SRSP ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RR
lm <- aov( RR ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RRAVR
lm <- aov( RRAVR ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RNT
lm <- aov( RNT ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RNAVR
lm <- aov( RNAVR ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SRN
lm <- aov( SRN ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#PRD
lm <- aov( PRD ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RFW
lm <- aov( RFW ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RDW
lm <- aov( RDW ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#RDM
lm <- aov( RDM ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SFW
lm <- aov( SFW ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SDW
lm <- aov( SDW ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#SDM
lm <- aov( SDM ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LFW
lm <- aov( LFW ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LDW
lm <- aov( LDW ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LDM
lm <- aov( LDM ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#LDM
lm <- aov( LDM ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#StFW
lm <- aov( StFW ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#StDW
lm <- aov( StDW ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

#StDM
lm <- aov( StDM ~ R+Genotype, H60)
anova(lm)

turkey <- HSD.test(lm, c("Genotype")) #Tukey
turkey

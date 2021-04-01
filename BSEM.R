#merge dataframes together 


IowaSEMdataCC<-merge(Iowa_SOP_Scores.df, Iowa_GF_Scores.df, by.x = "IowaData.Mseqnum", by.y = "IowaData.Mseqnum")
IowaSEMdataCC<-merge(IowaSEMdataCC, Iowa_SOR_Scores.df, by.x = "IowaData.Mseqnum", by.y = "IowaData.Mseqnum")
IowaSEMdataCC<-merge(IowaSEMdataCC, CoverCrop, by.x = "IowaData.Mseqnum", by.y = "IowaData.Mseqnum")

View(IowaSEMdataCC)



##model


modelCC<- '
           # measurement model 
            conservationist1 =~ GoodFarmerMinErosion + GoodFarmerStreamHealth + GoodFarmerMinNutrientRunoff + GoodFarmerOrganicMatter + GoodFarmerLongTermConservation + GoodFarmerScoutSpray
            preservationist1 =~ GoodFarmerYield + GoodFarmerPlantFirst + GoodFarmerHighProfitYield + GoodFarmerSeedTech + GoodFarmerUpdateEquip
            Attachment_Identity1 =~ SOPHomeLandFarm + SOPHappiestOnLand + SOPFavoritePlace
            Dependence1 =~ SOPFarmAnywhere + SOPRatherFarmHere
            network1 =~ SOPBeliefsValuesSimilar + SOPTrustedNetwork
            self1 =~ ResponsibleMyself + ResponsibleImmediateFamily
            community1 =~ ResponsibleNeighborhood + ResponsiblePeopleAreaFarm + ResponsibleWatershed
            globe1 =~ ResponsibleEarth + ResponsibleFutureGenerations
            CoverCrop1 =~ BinaryColumn
            
           # regressions 
            Attachment_Identity1 ~ self1 + globe1 
            self1 ~ CoverCrop1
            conservationist1 ~ CoverCrop1
            globe1 ~ CoverCrop1
            network1~community1
            
            #residual correlations
            
            globe1 ~~ community1
            
           
            
            #direct effect 
CoverCrop1 ~ c*Attachment_Identity1
Dependence1 ~ i*self1 


#mediator
preservationist1 ~ a*Attachment_Identity1
conservationist1 ~ d*Attachment_Identity1
CoverCrop1 ~ b*preservationist1
CoverCrop1 ~ e*conservationist1
preservationist1 ~ g*self1
Dependence1 ~ h*preservationist1




#indirect effect 
ab := a*b
de :=d*e
gh :=g*h




#total effect 
total := c + (a*b)
total := c + (d*e)
total := i + (g*h)

            
             
'
##BSEM fit and summary 
fitCCBsem<- bsem(modelCC, data = IowaSEMdataCC)
summary(fitCCBsem)




##Setting priors

dpriors(fitCCBsem)
mydp<-dpriors(alpha = "normal(0, 1)", lambda = "normal(0,1)")




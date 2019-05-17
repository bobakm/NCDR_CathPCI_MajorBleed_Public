#5-fold cross-validation
library(pROC)
library(xgboost)
library(foreach)
library(doParallel)
registerDoParallel()
MODE <- 'clinical'#'site'#'clinical'# 'all'
load(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V8_7JUN2016.RData'))
source('./fMeasure.R')
#Existing Model in CV
folds <- c(1,2,3,4,5)
full.data <- rbind(test1.data, test2.data, test3.data, test4.data, test5.data)
full.data.labels <- c(test1.labels, test2.labels, test3.labels, test4.labels, test5.labels)
#f <- 5
vars.clinical <- c(  "Smoker"              ,          "Hypertension"              ,    "Dyslipidemia"         ,        
                     "FamilyHxCAD"        ,           "PriorMI"                   ,    "PriorHF"              ,        
                     "ValveSurgery"       ,           "PriorPCI"                  ,    "PriorCABG"            ,        
                     "Height"             ,           "Weight"                    ,    "CurrentDialysis"      ,        
                     "PriorCVD"           ,           "PriorPAD"                  ,    "ChronicLungDisease"   ,        
                     "Diabetes"           ,           "AdmtSource"                ,    "DiabetesControl"      ,        
                     "InsPrivate"         ,           "InsMedicare"               ,    "InsMedicaid"          ,        
                     "InsMilitary"        ,           "InsState"                  ,    "InsIHS"               ,        
                     "InsNonUS"           ,           "InsNone"                   ,    "Age"                  ,        
                     "Sex"                ,           "RaceWhite"                 ,    "RaceBlack"            ,        
                     "RaceAsian"          ,           "RaceAmIndian"              ,    "RaceNatHaw"           ,        
                     "HispOrig"           ,           "ThromTherapy"              ,    "AntiAnginalMed"       ,        
                     "AA_BetaBlockers"    ,           "AA_CaChannel"              ,    "AA_LongActingNitrates" ,       
                     "AA_Ranolazine"      ,           "AA_OtherAgent"             ,    "Prior2weeksHF"        ,        
                     "CardioLVSD"         ,           "PeriopEval"                ,    "PriorCardioShock"     ,        
                     "PriorCardiacArrest" ,           "StressImaging"             ,    "ExerciseStressTest"   ,        
                     "ExerciseTestResult" ,           "ExerciseTestRisk"          ,    "StressEcho"           ,        
                     "StressEchoResult"   ,           "StressEchoRisk"            ,    "SPECTStressTest"      ,        
                     "SPECTStressResult"  ,           "SPECTStressRisk"           ,    "CMRStressTest"        ,        
                     "CMRStressResults"   ,           "CMRStressRisk"             ,    "CardiacCTA"           ,        
                     "CardCTAResults"     ,           "CorCalciumScore"           ,    "CalciumScore"         ,        
                     "LMStenosis"        ,            "ProxLADStenosis"           ,    "MidDistalLADStenosis" ,        
                     "CIRCStenosis"     ,             "RCAStenosis"               ,    "RamusStenosis"        ,        
                     "ProxLADGtStenosis"  ,           "MidDistalLADGStenosis"     ,    "CIRCGStenosis"        ,        
                     "RCAGStenosis"       ,           "RamusGStenosis"            ,    "CADPresentation"      ,        
                     "AnginalClass"       ,           "Dominance"                 ,    "DiagnosticCath"       ,        
                     "OtherProcedure"     ,           "FluroTime"                 ,    "ContrastVol"          ,        
                     "IABP"               ,           "IABPTiming"                ,    "MVSupport"            ,        
                     "MVSupportTiming"    ,           "DiagCorAngio"         ,        
                     "LeftHeartCath"      ,           "CardiacTransplant"         ,    "CardiacTransType"     ,        
                     "DCathStatus"        ,           "DCathTreatment"            ,    "PCIStatus"            ,        
                     "PrePCILVEF"         ,           "PCICardioShock"            ,    "PatientTransPCI"      ,        
                     "DissectionSeg"      ,           "PerfSeg"                   ,    "PCIndication"         ,        
                     "STEMIFirstNoted"    ,           "PCIDelayReason"            ,    "PreProcCKMB"          ,        
                     "PreProcCKMBNM"      ,           "PreProcTnl"                ,    "PreProcTnT"           ,        
                     "PreProcCreat"       ,           "PreProcHgb"                ,    "CulpritArtery"        ,        
                     "StenosisPriorTreat" ,           "ChronicOcclusion"          ,    "IVUS"                 ,        
                     "FFR"                ,           "FFRatio"                   ,    "PreProcTIMI"          ,        
                     "PrevTreatedLesion"  ,           "PreviousStent"             ,    "InRestenosis"         ,        
                     "InThrombosis"       ,           "LesionLength"              ,    "Thrombus"             ,        
                     "BifurcationLesion"  ,           "GuidewireLesion"           ,    "DeviceDeployed"       ,        
                     "StentType"          ,           "LesionGraft"               ,    "LesonComplexty"       ,        
                     "LocGraft"           ,           "Culprit"                   ,    "CTO"                  ,        
                     "THROM"              ,           "PreTIMI"                   ,    "LESSCAI"              ,        
                     "SegmentID"          ,           "NEWSEQ"                    ,    "SOMECA"               ,        
                     "PreOpMed6"            ,        
                     "PreOpMed7"                  ,   "PreOpMed3"                 ,    "PreOpMed4"            ,        
                     "PreOpMed5"                  ,   "PreOpMed1"                 ,    "PreOpMed8"            ,        
                     "PreOpMed2"                  ,   "PreOpMed9"                  ,   "PreOpMed18"           ,        
                     "PreOpMed20"                 ,   "ICDEV_Drug Eluting Stent"  ,    "ICDEV_Balloon"        ,        
                     "ICDEV_Bare Metal Stent"     ,   "ICDEV_Cutting Balloon"      ,   "ICDEV_Thrombectomy"   ,        
                     "ICDEV_Extraction Catheter"  ,   "ICDEV_Embolic Protection"  ,    "ICDEV_Atherectomy"    ,        
                     "ICDEV_Laser"                ,   "ICDEV_Other"                ,   "ICDEV_Coated Stent"   ,        
                     "ICDEV_Chronic Total Occlusion" ,"ICDEV_Brachy Therapy"       ,   "ICDEV_Covered Stent"  ,        
                     "LesionCounter.y"        ,       "LSDEV_Drug Eluting Stent"  ,    "LSDEV_Balloon"        ,        
                     "LSDEV_Bare Metal Stent" ,       "LSDEV_Cutting Balloon"    ,     "LSDEV_Thrombectomy"   ,        
                     "LSDEV_Extraction Catheter"  ,   "LSDEV_Embolic Protection",      "LSDEV_Atherectomy"    ,        
                     "LSDEV_Laser"                ,   "LSDEV_Other"          ,         "LSDEV_Coated Stent"   ,        
                     "LSDEV_Chronic Total Occlusion" ,"LSDEV_Brachy Therapy",          "LSDEV_Covered Stent"  ,        
                     "STEMI"                      ,   "NYHA"                ,                  
                     "AGELE70"                    ,   "AGEGT70"            ,           "BMI"                  ,        
                     "BMILE30"                    ,   "NEWDIAB"           ,            "NEWDIAB1"             ,        
                     "NEWDIAB2"                   ,   "HDEF"              ,            "FEMALE"               ,        
                     "GENDMULT"                   ,   "RACEMULT"          ,            "RENFAIL"              ,        
                     "CKD"                        ,   "CKD1"              ,            "CKD2"                 ,        
                     "CKD3"                       ,   "CKD4"              ,            "GFR"                  ,        
                     "LYTICS"                     ,   "CARSHOCK"          ,            "DCARSHOCK"            ,        
                     "SHOCKPCIS"                  ,   "SHOCKPCIS1"        ,            "SHOCKPCIS2"           ,        
                     "SHOCKPCIS3"                  ,  "SHOCKPCIS4"         ,           "SHOCKPCIS5"            ,       
                     "SHOCKPCIS6"                 ,   "LESSCAI23"         ,            "LESSCAI4"             ,        
                     "NEWSEQ2"                    ,   "NEWSEQ3"            ,           "NYHA123"              ,        
                     "NYHA4"                      ,   "PRETIMINO"         ,            "NVD"                  ,        
                     "NVD23"                      ,   "PREHGBLE13"       ,             "PREHGBGT13"           ,        
                     "Femoral_Clos_Patch",           
                     "Femoral_Clos_Manual com",       "Femoral_Clos_Sealant",          "Femoral_Clos_Other"         ,   "Femoral_Clos_Mechanical"   ,    "Femoral_Clos_Suture"    ,      
                     "Femoral_Clos_Staple"     ,      "Brachial_Clos_Patch" ,          "Brachial_Clos_Manual com"   ,   "Brachial_Clos_Sealant"     ,    "Brachial_Clos_Other"    ,      
                     "Brachial_Clos_Mechanical" ,     "Brachial_Clos_Suture",          "Brachial_Clos_Staple"       ,   "Radial_Clos_Patch"         ,    "Radial_Clos_Manual com" ,      
                     "Radial_Clos_Sealant"      ,     "Radial_Clos_Other"   ,          "Radial_Clos_Mechanical"     ,   "Radial_Clos_Suture"        ,    "Radial_Clos_Staple"     ,      
                     "Other_Clos_Patch"         ,     "Other_Clos_Manual com",         "Other_Clos_Sealant"         ,   "Other_Clos_Other"          ,    "Other_Clos_Mechanical"  ,      
                     "Other_Clos_Suture"        ,     "Other_Clos_Staple", 'Femoral_Clos_None', 'Brachial_Clos_None', 'Radial_Clos_None', 'Other_Clos_None', 'femoral', 'radial', 'brachial', 'other' )
vars.site <- c("CommunityDesc"      ,           "ProfitTypeDesc"            ,       
               
               "IsTeaching"             ,       "IsPublic"                  ,    "DoesMonitorVolume"    ,        
               "HasSurgicalBackup"      ,       "HasSurgicalTransport"      ,    "IsClinicalTrialSite"  ,        
               "IsFutureClinicalTrialSite"  ,   "PatientBeds"               ,    "ClientPercentMedicare",        
               "ClientPercentManagedCare"   ,   "EPSCount"                  ,    "PacemakerCount"       ,        
               "ICDCount"                   ,   "DxCathCount"               ,    "PCICount"             ,        
               "CASCount"                   ,   "CEACount"                  ,    "FTLabStaffAdminSuper" ,        
               "FTLabStaffNurse"            ,   "FTLabStaffCVT"             ,    "FTLabStaffRT"         ,        
               "FTLabStaffClerical"         ,   "FTLabStaffAnalytics"       ,    "FTLabOther"           ,        
               "LabDocCount"                ,   "LabInvasiveDocCount"       ,    "CardiologistGroupCount",       
               "LabCountStatic"             ,   "LabCountMobile"            ,    "LabCountFreeStand"    ,        
               "DoesCKMB"                   ,   "DoesCKMBPostProc"          ,    "UpperNormCKMBFemale"  ,        
               "UpperNormCKMBMale"          ,   "DoesTnL"                   ,    "DoesTnPostProc"       ,        
               "DoesTnT"                    ,   "TnDecisionLimit"           )

list.importance <- list()
list.colnames <- list()
list.roc <- list()

idx.select <- sort(sample(dim(full.data)[1], 0.9 * dim(full.data)[1], replace=FALSE))
train.data <- full.data[idx.select,]
test.data <- full.data[-idx.select,]
train.labels <- full.data.labels[idx.select]
test.labels <- full.data.labels[-idx.select]
  train.data <- train.data[,-which(grepl('HemoDynamicEquipment', colnames(train.data)))]
  test.data <- test.data[,-which(grepl('HemoDynamicEquipment', colnames(test.data)))]
  
  if(MODE == 'all') {
    #do nothing
    
  } else if(MODE == 'clinical') {
    #Remove site variables
    train.data <- train.data[,-which(colnames(train.data) %in% vars.site)]
    test.data <- test.data[, -which(colnames(test.data) %in% vars.site)]
  } else if(MODE == 'site') {
    #remove clinical variables
    train.data <- train.data[,-which(colnames(train.data) %in% vars.clinical)]
    test.data <- test.data[, -which(colnames(test.data) %in% vars.clinical)]
  }
  
  
  
  #cat(paste('f: ', f,' model building\n'))
  model.bleed <- xgboost(data=as.matrix(train.data), label=as.numeric(train.labels), verbose=0,
                       nrounds=1000, eta=0.1, max.depth=6, objective='binary:logistic') 
  #cat(paste('f: ', f,' importance\n'))
  model.importance <- xgb.importance(feature_names=colnames(train.data), model=model.bleed)
  
  stepwise.auc <- vector()
  
  for(feat.idx in 1:20) {
    cat(paste('features:', feat.idx, '\n'))
    train.feat.data <- train.data[,model.importance$Feature[1:feat.idx]]
    test.feat.data <- test.data[,model.importance$Feature[1:feat.idx]]
    model.bleed <- xgboost(data=as.matrix(train.feat.data), label=as.numeric(train.labels), verbose=0,
                           nrounds=1000, eta=0.1, max.depth=6, objective='binary:logistic') 
    model.predict <- predict(model.bleed, as.matrix(test.feat.data))
    roc.bleed <- roc(as.numeric(test.labels), as.numeric(model.predict))
    stepwise.auc[feat.idx] <- roc.bleed$auc    
  }
  

save.image(file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/9010_feature_ranking_and_results_',MODE,'_five_fold_data_V8_10JAN2017.RData', sep=''))
#save('list.colnames', 'list.importance', 'list.roc', file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/folded_feature_ranking_and_train_results_',MODE,'_five_fold_data_V8_5DEC2016.RData', sep=''))
save('stepwise.auc', 'model.importance', file=paste('Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/resultsgather/nohemo/9010_feature_ranking_results_only_',MODE,'_five_fold_data_V8_10JAN2017.RData', sep=''))

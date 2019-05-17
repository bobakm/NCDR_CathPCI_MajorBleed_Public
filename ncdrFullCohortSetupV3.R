load(file='Y:/NCDR/BJMDATA/CPBLEED/FullCohort_v3_07MAR2016.RData')

#Remove Certain Columns
numeric.IDs <- c('HospitalID','VisitKey', 'EpisodeKey', 'ZipCode')
numeric.removes <- c('SubmissionKey', 'SubmissionPatientKey',
                     'DeathCause', 'DeathLab','ZipCodeNA', 'HospStatus', 'ArrivalDate',
                     'PriorPCIDate', 'PriorCABGDate', 'CABGDate', 'DCDate', 'ArrivalTime', 'CABGTime',
                     'NCDRPatientID', 'DOB', 'TimeframeCode', 'PopulationCode', 'BenchmarkCode', 
                     'STATE','MPN', 'ClientName.x', 'H_NPI', 'H_AHA', 'Address1.x',
                     'Address2.x', 'City.x', 'Zip.x', 'CPMPN', 'ICDMPN', 'ACTMPN', 'mpn2', 
                     'PROV_NUM', 'MDNDUP', 'PCI', 'OnsetDate', 'ThromDate', 'ProcedureDate', 
                     'SubECGDate', 'FirstDevActiDate', 'EDPresentDate', 'OnsetTime', 'ThromTime',
                     'ProcedureTime', 'SubECGTime','FirstDevActiTime',
                     'EDPresentTime', 'PrevLesionTime','HID', 'PID', 'EID', 'ClientName.y', 
                     'ParticipantClassificationDesc', 'ParticipantClassOtherDesc', 'Address1.y', 
                     'Address2.y', 'City.y', 'Zip.y', 'CountryCode', 'CountryName', 'ISOCode2', 
                     'ISONumber','StateProvinceCode', 'StateProvinceName', 'MasterContractDate', 
                     'MasterContractEffectiveDate', 'ContractDate', 'ContractEffectiveDate',
                     'PaidThroughDate', 'DateLastModified', 'iMISID', 'Hosp_ClientName', 'Hosp_ H_AHA',
                     'Hosp_ H_NPI', 'Hops_ CITY', 'Hosp_ STATE', 'Hosp_ MPN')
numeric.exc <- c('exc1', 'exc2','exc3', 'exc4', 'exc5', 'exc6')
numeric.other <- c('DCathOperatorKey' ,'PCIOperatorKey')
numeric.insurance <- c('InsPrivate', 'InsMedicare' ,'InsMedicaid', 'InsMilitary','InsState', 
                       'InsIHS', 'InsNonUS', 'InsNone')

STEMI <- rep(0, dim(final.data)[1])
STEMI[which(final.data$CADPresentation == 6)] <- 1

#Fix Insurance - 
#All Insurance Fields with NA == When InsNone == 1 so -> Impute 0 for those field
idx <- which(colnames(final.data) %in% numeric.insurance)
for(i in idx) {
  final.data[,i][which(is.na(final.data[,i]))] <- 0
}
final.data$STEMI <- STEMI
final.input.data <- final.data[,which(!(colnames(final.data) %in% c(numeric.removes, numeric.exc, numeric.other)))]

#Fix Blanks in Columns
#numeric.blanks <- c('CABGLocation', 'CABGIndication', 'CABGStatus')
#Blanks should -> 0
#Blanks should correlate with CABG == 0
final.input.data$CABGLocation <- as.numeric(final.input.data$CABGLocation)
final.input.data$CABGIndication <- as.numeric(final.input.data$CABGIndication)
final.input.data$CABGStatus <- as.numeric(final.input.data$CABGStatus)

#DiabetesControl NA-> 0 because it means"None" -> had to have diabetes to have an answer
final.input.data$DiabetesControl[which(is.na(final.input.data$DiabetesControl))] <- 0
#ThromTherapy only if STEMI <- NA = 0
final.input.data$ThromTherapy[which(is.na(final.input.data$ThromTherapy))] <- 0
#ExerciseStressTest <- only if StressImaging <- NA = 0
final.input.data$ExerciseStressTest[which(is.na(final.input.data$ExerciseStressTest))] <- 0
#CMRStressTest
final.input.data$CMRStressTest[which(is.na(final.input.data$CMRStressTest))] <- 0
#CardiaCTA
final.input.data$CardiacCTA[which(is.na(final.input.data$CardiacCTA))] <- 0
#CorCalciumScore
final.input.data$CorCalciumScore[which(is.na(final.input.data$CorCalciumScore))] <- 0
#Prior2WeekNYHA
final.input.data$NYHA <- final.input.data$Prior2weekNYHA
final.input.data$NYHA[which(is.na(final.input.data$NYHA))] <- 0
final.input.data$Prior2weekNYHA <- NULL
#IABPTiming
final.input.data$IABPTiming[which(is.na(final.input.data$IABPTiming))] <- 0
#MVSupportTiming
final.input.data$MVSupportTiming[which(is.na(final.input.data$MVSupportTiming))] <- 0
#CardiacTransType
final.input.data$CardiacTransType[which(is.na(final.input.data$CardiacTransType))] <- 0

#ExerciseTestResult == 4 == Unavailable <- NA = 4 / Make NA 0 and then shift everything else up 1
final.input.data$ExerciseTestResult[which(final.input.data$ExerciseTestResult == 4)] <- 0
final.input.data$ExerciseTestResult[which(is.na(final.input.data$ExerciseTestResult))] <- 0

#ExerciseTestRisk NA -> 4 unvailable / Make NA 0 and then shift everything else up 1
final.input.data$ExerciseTestRisk[which(final.input.data$ExerciseTestRisk == 4)] <- 0
final.input.data$ExerciseTestRisk[which(is.na(final.input.data$ExerciseTestRisk))] <- 0

#Same with StressEcho
final.input.data$StressEcho[which(is.na(final.input.data$StressEcho))] <- 0
#Same with StressEchoResults
final.input.data$StressEchoResult[which(final.input.data$StressEchoResult == 4)] <- 0
final.input.data$StressEchoResult[which(is.na(final.input.data$StressEchoResult))] <- 0
#Same with StressEchoRISK
final.input.data$StressEchoRisk[which(final.input.data$StressEchoRisk == 4)] <- 0
final.input.data$StressEchoRisk[which(is.na(final.input.data$StressEchoRisk))] <- 0

#SPECTStressTest
final.input.data$SPECTStressTest[which(is.na(final.input.data$SPECTStressTest))] <- 0
#SPECTStressResult
final.input.data$SPECTStressResult[which(final.input.data$SPECTStressResult == 4)] <- 0
final.input.data$SPECTStressResult[which(is.na(final.input.data$SPECTStressResult))] <- 0
#SPECTStressRisk
final.input.data$SPECTStressRisk[which(final.input.data$SPECTStressRisk == 4)] <- 0
final.input.data$SPECTStressRisk[which(is.na(final.input.data$SPECTStressRisk))] <- 0

#CMRStressResults
final.input.data$CMRStressResults[which(final.input.data$CMRStressResults == 4)] <- 0
final.input.data$CMRStressResults[which(is.na(final.input.data$CMRStressResults))] <- 0
#CMRStressRisk
final.input.data$CMRStressRisk[which(final.input.data$CMRStressRisk == 4)] <- 0
final.input.data$CMRStressRisk[which(is.na(final.input.data$CMRStressRisk))] <- 0

#CardCTAResults
final.input.data$CardCTAResults[which(is.na(final.input.data$CardCTAResults))] <- 5
final.input.data$CardCTAResults[which(final.input.data$CardCTAResults == 5)] <- -1
final.input.data$CardCTAResults <- final.input.data$CardCTAResults + 1

#CalciumScore #Tricky - Value but has 0s
final.input.data$CalciumScore[which(is.na(final.input.data$CalciumScore))] <- 0
#final.input.data$CorCalciumScore[which(is.na(final.input.data$CorCalciumScore))] <- 0

# numeric.large.NA.keep <- c('DiabetesControl', 'ThromTherapy', 
#                            'ExerciseStressTest', 'ExerciseTestResult', 'ExerciseTestRisk', 
#                            'StressEcho',' StressEchoResult', 'StressEchoRisk', 'SPECTStressTest',
#                            'SPECTStressResult', 'SPECTStressRisk', 'CMRStressTest',
#                            'CMRStressResults', 'CMRStressRisk', 'CardiacCTA', 'CardCTAResults', 
#                            'CorCalciumScore', 'CalciumScore', 
#                            


#                            'RamusGStenosis', 'RamusGNA',
final.input.data$RamusStenosis[which(is.na(final.input.data$RamusStenosis))] <- 0
#final.input.data$RamusNA[which(is.na(final.input.data$RamusNA))] <- 0

final.input.data$RamusGStenosis[which(is.na(final.input.data$RamusGStenosis))] <- 0
final.input.data$RamusGNA[which(is.na(final.input.data$RamusGNA))] <- 1
#All the RamusGNA Missing = All the RamusGStensosis Missing

#                            #Any NA should be 0, and RamusGNA 1 for all 0s
#                            
#                             
#                            'IABPTiming', 'MVSupportTiming', 
#final.input.data$IABPTiming[which(is.na(final.input.data$IABPTiming))] <- 0
#final.input.data$MVSupportTiming[which(is.na(final.input.data$MVSupportTiming))] <- 0

#                            'CardiacTransType',
#                            'PatientTransPCI', 'STEMIFirstNoted', 'PCIDelayReason', 
final.input.data$PatientTransPCI[which(is.na(final.input.data$PatientTransPCI))] <- -1
final.input.data$PatientTransPCI <- final.input.data$PatientTransPCI + 1

final.input.data$STEMIFirstNoted[which(is.na(final.input.data$STEMIFirstNoted))] <- -1
final.input.data$STEMIFirstNoted <- final.input.data$STEMIFirstNoted + 1

final.input.data$PCIDelayReason[which(is.na(final.input.data$PCIDelayReason))] <- -1
final.input.data$PCIDelayReason <- final.input.data$PCIDelayReason + 1
#                            
#                            'PreProcCKMB', 
#                            'PreProcCKMBNM',
final.input.data$PreProcCKMB[which(is.na(final.input.data$PreProcCKMB))] <- 0
final.input.data$PreProcCKMBNM[which(is.na(final.input.data$PreProcCKMBNM))] <- 0
#                            #PreProcCKND
#                            
#                            'PreProcTnl', 'PreProcTnT',
final.input.data$PreProcTnl[which(is.na(final.input.data$PreProcTnl))] <- 0
final.input.data$PreProcTnT[which(is.na(final.input.data$PreProcTnT))] <- 0

#                            #PreProcTnlND, #preProcTnTND 1 if NA on these
#                            'ChronicOcclusion','IVUS', 'FFR', 'FFRatio',
final.input.data$ChronicOcclusion[which(is.na(final.input.data$ChronicOcclusion))] <- 0
final.input.data$IVUS[which(is.na(final.input.data$IVUS))] <- 0
final.input.data$FFR[which(is.na(final.input.data$FFR))] <- 0
final.input.data$FFRatio[which(is.na(final.input.data$FFRatio))] <- 0
#                            
#                            'PreviousStent', 'InRestenosis', 'InThrombosis', 
final.input.data$PreviousStent[which(is.na(final.input.data$PreviousStent))] <- -1
final.input.data$PreviousStent <- final.input.data$PreviousStent + 1

final.input.data$InRestenosis[which(is.na(final.input.data$InRestenosis))] <- -1
final.input.data$InRestenosis <- final.input.data$InRestenosis + 1

final.input.data$InThrombosis[which(is.na(final.input.data$InThrombosis))] <- -1
final.input.data$InThrombosis <- final.input.data$InThrombosis + 1
#                            'StentType', #3 <- Type Unknown
final.input.data$StentType[which(is.na(final.input.data$StentType))] <- 3
#                            
#                            'LocGraft' 
final.input.data$LocGraft[which(is.na(final.input.data$LocGraft))] <- 0
#                            #0 Not in Graft
# )
# 'StressEchoResult', 
final.input.data$StressEchoResult[which(is.na(final.input.data$StressEchoResult))] <- 0
#'ProxLADGtStenosis', 'ProxLADGNA', 
final.input.data$ProxLADGtStenosis[which(is.na(final.input.data$ProxLADGtStenosis))] <- 0
final.input.data$ProxLADGNA[which(is.na(final.input.data$ProxLADGNA))] <- 1

# 'CIRCGStenosis', 'CIRCGNA', 
final.input.data$CIRCGStenosis[which(is.na(final.input.data$CIRCGStenosis))] <- 0
final.input.data$CIRCGNA[which(is.na(final.input.data$CIRCGNA))] <- 1


#'RCAGStenosis', 'RCAGNA',
final.input.data$RCAGStenosis[which(is.na(final.input.data$RCAGStenosis))] <- 0
final.input.data$RCAGNA[which(is.na(final.input.data$RCAGNA))] <- 1

#'MidDistalLADGStenosis', 'MidDistalLADGNA',
final.input.data$MidDistalLADGStenosis[which(is.na(final.input.data$MidDistalLADGStenosis))] <- 0
final.input.data$MidDistalLADGNA[which(is.na(final.input.data$MidDistalLADGNA))] <- 1


numeric.inf <- c('CTO', 'PreTIMI', 'NEWSEQ')
catagorical.var <- c('SegmentID', 'NPINumber' ,'AHANumber', 'CommunityDesc', 'ProfitTypeDesc', 
                     'HemoDynamicEquipment1','HemoDynamicEquipment2',
                     'HemoDynamicEquipment3', 'HemoDynamicEquipment4')

#Fix NA in columns
numeric.NA <- c('DCStatus', 'Smoker', 'Hypertension', 'Dyslipidemia', 'FamilyHxCAD', 'PriorMI', 
                'PriorHF', 'ValveSurgery', 'PriorPCI', 'PriorCABG', 'Height', 'Weight', 
                'CurrentDialysis', 'PriorCVD', 'PriorPAD', 'ChronicLungDisease', 'Diabetes', 
                'AdmtSource', 'DCLocation', 'DC_CardRehab', 'CABG', 'OtherMajorSurgery' ,
                'DCLVEF', 'HispOrig', 'AntiAnginalMed', 'AA_BetaBlockers' ,'AA_CaChannel', 
                'AA_LongActingNitrates', 'AA_Ranolazine', 'AA_OtherAgent', 'Prior2weeksHF', 
                'CardioLVSD', 'PeriopEval', 'PriorCardioShock', 'PriorCardiacArrest', 'StressImaging', 
                'LMStenosis', 'LMNA', 'ProxLADStenosis', 'MidDistalLADStenosis', 'CIRCStenosis', 
                'RCAStenosis', 'CADPresentation', 'AnginalClass', 'Dominance',
                'OtherProcedure', 'FluroTime', 'ContrastVol', 'IABP', 'MVSupport', 'AcessSite',
                'PostMI', 'PostCardiogenicShock','PostHF', 'PostCVA', 'PostTamponade', 'PostDialysis',
                'PostOtherVasComp', 'PostTransfusion', 'PostBleed', 'DiagCorAngio', 'LeftHeartCath', 
                'CardiacTransplant', 'DCathStatus' ,'DCathTreatment' ,'PCIStatus', 'PrePCILVEF', 
                'PCICardioShock', 'DissectionSeg', 'PerfSeg', 'PCIndication',
                'PreProcCreat', 'PreProcHgb', 'PostProcCreat', 'PostProcHgb', 'CulpritArtery', 
                'StenosisPriorTreat', 'PreProcTIMI', 'PrevTreatedLesion',
                'LesionLength', 'Thrombus', 'BifurcationLesion', 'GuidewireLesion', 'StensosiPostProc',
                'PostProcTIMI', 'DeviceDeployed', 'LesionGraft','LesonComplexty', 'DoesMonitorVolume', 
                'CEACount', 'FTLabOther', 'UpperNormCKMBFemale', 'UpperNormCKMBMale',
                'DoesTnT', 'TnDecisionLimit', 'DischargeMed10', 'DischargeMed11', 'DischargeMed12', 
                'DischargeMed13', 'DischargeMed14', 'DischargeMed15', 'DischargeMed15', 'DischargeMed17',
                'DischargeMed19', 'DischargeMed21', 'PreOpMed6', 'PreOpMed7', 'PreOpMed3', 'PreOpMed4', 
                'PreOpMed5', 'PreOpMed1', 'PreOpMed8', 'PreOpMed2', 'PreOpMed9', 'PreOpMed18', 'PreOpMed20',
                'ICDEV_Drug Eluting Stent', 'ICDEV_Balloon', 'ICDEV_Bare Metal Stent', 'ICDEV_Cutting Balloon', 'ICDEV_Thrombectomy', 
                'ICDEV_Extraction Catheter' ,'ICDEV_Embolic Protection', 'ICDEV_Atherectomy', 'ICDEV_Laser', 'ICDEV_Other', 'ICDEV_Coated Stent',
                'ICDEV_Chronic Total Occlusion','ICDEV_Brachy Therapy', 'ICDEV_Covered Stent', 'LesionCounter.y', 
                'LSDEV_Drug Eluting Stent', 'LSDEV_Balloon', 'LSDEV_Bare Metal Stent', 'LSDEV_Cutting Balloon',
                'LSDEV_Thrombectomy', 'LSDEV_Extraction Catheter', 'LSDEV_Embolic Protection', 
                'LSDEV_Atherectomy', 'LSDEV_Laser', 'LSDEV_Other', 'LSDEV_Coated Stent', 
                'LSDEV_Chronic Total Occlusion', 'LSDEV_Brachy Therapy', 'LSDEV_Covered Stent',
                "ClosID1"      ,                 "ClosID10"       ,               "ClosID11"       ,               "ClosID12"    ,                 
                "ClosID13"      ,                "ClosID14"       ,               "ClosID15"      ,                "ClosID16"   ,                   "ClosID17",                     
                "ClosID18"       ,               "ClosID19"       ,               "ClosID2"       ,                "ClosID20"   ,                   "ClosID21" ,                    
               "ClosID22"         ,             "ClosID23"        ,              "ClosID24"       ,               "ClosID25"    ,                  "ClosID26" ,                    
                "ClosID27"         ,             "ClosID28"       ,               "ClosID29"      ,                "ClosID3"    ,                   "ClosID30",                     
                "ClosID31"          ,            "ClosID32"       ,               "ClosID33"      ,                "ClosID34"   ,                   "ClosID35",                     
                "ClosID36"            ,          "ClosID37"       ,               "ClosID38"      ,                "ClosID39"   ,                   "ClosID4" ,                     
                "ClosID40"          ,            "ClosID41"       ,               "ClosID42"      ,                "ClosID43"    ,                  "ClosID44",                     
                "ClosID45"           ,           "ClosID46"       ,               "ClosID47"      ,                "ClosID48"   ,                   "ClosID49",                     
                "ClosID5"           ,            "ClosID50"       ,               "ClosID51"      ,                "ClosID52"   ,                   "ClosID53",                     
                "ClosID54"          ,            "ClosID55"       ,               "ClosID56"      ,                "ClosID57"   ,                   "ClosID58",                     
                "ClosID59"          ,            "ClosID6"        ,               "ClosID60"      ,                "ClosID61"   ,                   "ClosID62",                     
                "ClosID63"          ,            "ClosID64"       ,               "ClosID65"      ,                "ClosID66"   ,                   "ClosID68",                     
                "ClosID69"          ,            "ClosID7"        ,               "ClosID70"      ,                "ClosID71"   ,                   "ClosID72",                     
                "ClosID8"           ,            "ClosID9"        ,               "ClosID952"     ,                "ClosID953"  ,                   "ClosID954",                    
                "ClosID955")

#Can I keep 'PostTransfusion' <- or would that be highly indicative of a bleed??
numeric.large.NA <- c('OnsetTimeEst', 'OnsetTimeNA', 'FluroDose',  
                      'PostHemStroke', 'HgbPriorTransfusion', 'PostBleedAccessSite', 
                      'PostBleedHmatoma', 'PostBleedHemaSize', 'PostRetroBleed', 'PostGIBleed',
                      'PostGUBleed', 'PostOtherBleed',
                      'Prior2weekNYHA',
                      'PostProcCKMB', 'PostprocCKMBNM', 'PostProcTnl', 'PostProcTnT', 'ClosureCounter',
                      'LesionCounter.x')


#Remove Post Proc, Discharge, and Death Information?
outcome <- c('PostTransfusion' ,'PostBleed', 'PostBleedAccessSite', 'PostHemStroke', 
             'HgbPriorTransfusion', 'PostBleedAccessSite',
             'PostBleedHmatoma', 'PostBleedHemaSize', 'PostRetroBleed', 'PostGIBleed', 
             'PostGUBleed', 'PostOtherBleed', 'StensosiPostProc', 'PostProcTIMI',
             'Bleed', 'DischargeMed10', 'DischargeMed11', 'DischargeMed12', 
             'DischargeMed13', 'DischargeMed14', 
             'DischargeMed15', 'DischargeMed15', 'DischargeMed17', 'DischargeMed19', 'DischargeMed21',
             'PostMI', 'PostCardiogenicShock','PostHF', 'PostCVA', 'PostTamponade', 'PostDialysis',
             'PostOtherVasComp', 'PostTransfusion', 'PostBleed','PostProcTIMI')

removes.extra <- c('AHANumber', 'DCLocation', 'DCLVEF', 'DCLVEFNA', 'DCStatus', 'DischargeMed16',
                   'Hosp_ CITY','NPINumber',
                   "PostBleedHematoma" ,"PostProcCKMBND","PostProcCreat",                
                   "PostProcCreatND","PostProcHgb", "PostProcHgbND" ,
                   "PostProcTnlND","PostProcTnTND", 'MPNDUP',
                   'DC_CardRehab')
# 
# "FTLabOther","FTLabStaffAdminSuper","FTLabStaffAnalytics","FTLabStaffClerical", "FTLabStaffCVT",                
# "FTLabStaffNurse","FTLabStaffRT", 
# sorted <- final.data[,sort(colnames(final.data))]
# test.sorted <- summary(sorted)
# print(test.sorted)


#Remove Large NAs
final.complete.input <- final.input.data[,which(!(colnames(final.input.data)) %in% c(numeric.large.NA, outcome,numeric.other, removes.extra))]

# sorted <- final.complete.input[,sort(colnames(final.complete.input))]
# test.sorted <- summary(sorted)
# print(test.sorted)                                                                       


#Fix Numeric.blanks
#Segment ID - None are 0 so make NA's 0 leave rest as integers
final.complete.input$SegmentID <- as.numeric(final.complete.input$SegmentID)
final.complete.input$SegmentID[which(is.na(final.complete.input$SegmentID))] <- 0

#final.complete.input$NPINumber <- as.numeric(final.complete.input$NPINumber)
#final.complete.input$AHANumber <- as.numeric(final.complete.input$AHANumber)

#final.complete.input$CommunityDesc <- as.numeric(levels(final.complete.input$CommunityDesc))
lvls <- levels(final.complete.input$CommunityDesc)
cmtdesc <- rep(0, length(final.complete.input$CommunityDesc))
for(i in 1:length(cmtdesc)) {
  cmtdesc[i] <- which(lvls == final.complete.input$CommunityDesc[i])
}

lvls.profit <- levels(final.complete.input$ProfitTypeDesc)
desc.profit <- rep(0, length(final.complete.input$ProfitTypeDesc))
lvls.hemo1 <- levels(final.complete.input$HemoDynamicEquipment1)
lvls.hemo2 <- levels(final.complete.input$HemoDynamicEquipment2)
lvls.hemo3 <- levels(final.complete.input$HemoDynamicEquipment3)
lvls.hemo4 <- levels(final.complete.input$HemoDynamicEquipment4)
desc.hemo1 <- rep(0, length(desc.profit))
desc.hemo2 <- rep(0, length(desc.profit))
desc.hemo3 <- rep(0, length(desc.profit))
desc.hemo4 <- rep(0, length(desc.profit))

for(i in 1:length(desc.profit)) {
  desc.profit[i] <- which(lvls.profit == final.complete.input$ProfitTypeDesc[i])
  desc.hemo1[i] <- which(lvls.hemo1 == final.complete.input$HemoDynamicEquipment1[i])
  desc.hemo2[i] <- which(lvls.hemo2 == final.complete.input$HemoDynamicEquipment2[i])
  desc.hemo3[i] <- which(lvls.hemo3 == final.complete.input$HemoDynamicEquipment3[i])
  desc.hemo4[i] <- which(lvls.hemo4 == final.complete.input$HemoDynamicEquipment4[i])
}

final.numeric.input <- final.complete.input
final.numeric.input$CommunityDesc <- cmtdesc
final.numeric.input$ProfitTypeDesc <- desc.profit
final.numeric.input$HemoDynamicEquipment1 <- desc.hemo1
final.numeric.input$HemoDynamicEquipment2 <- desc.hemo2
final.numeric.input$HemoDynamicEquipment3 <- desc.hemo3
final.numeric.input$HemoDynamicEquipment4 <- desc.hemo4


#numeric.inf <- c('CTO', 'PreTIMI', 'NEWSEQ')
final.numeric.input$CTO[which(final.numeric.input$CTO < 0)] <- NA
final.numeric.input$PreTIMI[which(final.numeric.input$PreTIMI == Inf)] <- NA
final.numeric.input$NEWSEQ[which(final.numeric.input$NEWSEQ < 0)] <- 0

#final.numeric.input <- final.numeric.input[,which(!(colnames(final.numeric.input)) %in% c(numeric.blanks))]
#Impute or Leave as own Category and create a catagorical Variable? and an NA variable?
#numeric.NA
numeric.missing <- numeric.NA[which(!(numeric.NA %in% outcome))]
# [1] "DCStatus"                      "Smoker"                        "Hypertension"                  "Dyslipidemia"                  "FamilyHxCAD"                  
# [6] "PriorMI"                       "PriorHF"                       "ValveSurgery"                  "PriorPCI"                      "PriorCABG"                    
# [11] "Height"                        "Weight"                        "CurrentDialysis"               "PriorCVD"                      "PriorPAD"                     
# [16] "ChronicLungDisease"            "Diabetes"                      "AdmtSource"                    "DCLocation"                    "DC_CardRehab"                 
# [21] "CABG"                          "OtherMajorSurgery"             "DCLVEF"                        "HispOrig"                      "AntiAnginalMed"               
# [26] "AA_BetaBlockers"               "AA_CaChannel"                  "AA_LongActingNitrates"         "AA_Ranolazine"                 "AA_OtherAgent"                
# [31] "Prior2weeksHF"                 "CardioLVSD"                    "PeriopEval"                    "PriorCardioShock"              "PriorCardiacArrest"           
# [36] "StressImaging"                 "LMStenosis"                    "LMNA"                          "ProxLADStenosis"               "MidDistalLADStenosis"         
# [41] "CIRCStenosis"                  "RCAStenosis"                   "CADPresentation"               "AnginalClass"                  "Dominance"                    
# [46] "OtherProcedure"                "FluroTime"                     "ContrastVol"                   "IABP"                          "MVSupport"                    
# [51] "AcessSite"                     "PostMI"                        "PostCardiogenicShock"          "PostHF"                        "PostCVA"                      
# [56] "PostTamponade"                 "PostDialysis"                  "PostOtherVasComp"              "DiagCorAngio"                  "LeftHeartCath"                
# [61] "CardiacTransplant"             "DCathStatus"                   "DCathTreatment"                "PCIStatus"                     "PrePCILVEF"                   
# [66] "PCICardioShock"                "DissectionSeg"                 "PerfSeg"                       "PCIndication"                  "PreProcCreat"                 
# [71] "PreProcHgb"                    "PostProcCreat"                 "PostProcHgb"                   "CulpritArtery"                 "StenosisPriorTreat"           
# [76] "PreProcTIMI"                   "PrevTreatedLesion"             "LesionLength"                  "Thrombus"                      "BifurcationLesion"            
# [81] "GuidewireLesion"               "DeviceDeployed"                "LesionGraft"                   "LesonComplexty"                "DoesMonitorVolume"            
# [86] "CEACount"                      "FTLabOther"                    "UpperNormCKMBFemale"           "UpperNormCKMBMale"             "DoesTnT"                      
# [91] "TnDecisionLimit"               "PreOpMed6"                     "PreOpMed7"                     "PreOpMed3"                     "PreOpMed4"                    
# [96] "PreOpMed5"                     "PreOpMed1"                     "PreOpMed8"                     "PreOpMed2"                     "PreOpMed9"                    
# [101] "PreOpMed18"                    "PreOpMed20"                    "ICDEV_Drug Eluting Stent"      "ICDEV_Balloon"                 "ICDEV_Bare Metal Stent"       
# [106] "ICDEV_Cutting Balloon"         "ICDEV_Thrombectomy"            "ICDEV_Extraction Catheter"     "ICDEV_Embolic Protection"      "ICDEV_Atherectomy"            
# [111] "ICDEV_Laser"                   "ICDEV_Other"                   "ICDEV_Coated Stent"            "ICDEV_Chronic Total Occlusion" "ICDEV_Brachy Therapy"         
# [116] "ICDEV_Covered Stent"           "LesionCounter.y"               "LSDEV_Drug Eluting Stent"      "LSDEV_Balloon"                 "LSDEV_Bare Metal Stent"       
# [121] "LSDEV_Cutting Balloon"         "LSDEV_Thrombectomy"            "LSDEV_Extraction Catheter"     "LSDEV_Embolic Protection"      "LSDEV_Atherectomy"            
# [126] "LSDEV_Laser"                   "LSDEV_Other"                   "LSDEV_Coated Stent"            "LSDEV_Chronic Total Occlusion" "LSDEV_Brachy Therapy"         
# [131] "LSDEV_Covered Stent"           "ClosID45"                      "ClosID1"                       "ClosID36"                      "ClosID36"                     
# [136] "ClosID35"                      "ClosID23"                      "ClosID19"                      "ClosID4"                       "ClosID2"                      
# [141] "ClosID31"                      "ClosID15"                      "ClosID27"                      "ClosID12"                      "ClosID5"                      
# [146] "ClosID44"                      "ClosID"                        "ClosID.1"                      "ClosID.2"                      "ClosID.3"                     
# [151] "ClosID.4"                      "ClosID.5"                      "ClosID.6"                      "ClosID.7"                      "ClosID.8"                     
# [156] "ClosID.9"                      "ClosID.10"                     "ClosID.11"                     "ClosID.12"                     "ClosID.13"                    
# [161] "ClosID.14"                     "ClosID.15"                     "ClosID.16"                     "ClosID.17"                     "ClosID.18"                    
# [166] "ClosID.19"                     "ClosID.20"                     "ClosID.21"                     "ClosID.22"                     "ClosID.23"                    
# [171] "ClosID.24"                     "ClosID.25"                     "ClosID.26"                     "ClosID.27"                     "ClosID.28"                    
# [176] "ClosID.29"                     "ClosID.30"                     "ClosID.31"                     "ClosID.32"                     "ClosID.33"                    
# [181] "ClosID.34"                     "ClosID.35"                     "ClosID.36"                     "ClosID.37"                     "ClosID.38"                    
# [186] "ClosID.39"                     "ClosID.40"                     "ClosID.41"                     "ClosID.42"                     "ClosID.43"                    
# [191] "ClosID.44"                     "ClosID.45"                     "ClosID.46"                     "ClosID.47"                     "ClosID.48"                    
# [196] "ClosID.49"                     "ClosID.50"                     "ClosID.51"                     "ClosID.52"                     "ClosID.53"                    
# [201] "ClosID.54"                     "ClosID.55"                     "ClosID.56"                     "ClosID.57"                     "ClosID.58"                    
# [206] "ClosID.59"                     "ClosID.60"  

#Closure, LSDEV, ICDEV, and PreOpMed NA => 0
med.na <- c("PreOpMed6"  ,                   "PreOpMed7"     ,                "PreOpMed3"                 ,    "PreOpMed4"                   , 
            "PreOpMed5"   ,                  "PreOpMed1"    ,                 "PreOpMed8"                 ,    "PreOpMed2"                    , "PreOpMed9"      ,              
            "PreOpMed18"   ,                 "PreOpMed20"   ,                 "ICDEV_Drug Eluting Stent"  ,    "ICDEV_Balloon"                , "ICDEV_Bare Metal Stent"  ,     
            "ICDEV_Cutting Balloon" ,        "ICDEV_Thrombectomy" ,           "ICDEV_Extraction Catheter" ,    "ICDEV_Embolic Protection"     , "ICDEV_Atherectomy"        ,    
            "ICDEV_Laser"            ,       "ICDEV_Other"         ,          "ICDEV_Coated Stent"        ,    "ICDEV_Chronic Total Occlusion", "ICDEV_Brachy Therapy"      ,   
            "ICDEV_Covered Stent"     ,      "LesionCounter.y"      ,         "LSDEV_Drug Eluting Stent"  ,    "LSDEV_Balloon"                , "LSDEV_Bare Metal Stent"    ,   
            "LSDEV_Cutting Balloon"    ,     "LSDEV_Thrombectomy"    ,        "LSDEV_Extraction Catheter" ,    "LSDEV_Embolic Protection"     , "LSDEV_Atherectomy"         ,   
            "LSDEV_Laser"               ,    "LSDEV_Other"            ,       "LSDEV_Coated Stent"        ,    "LSDEV_Chronic Total Occlusion", "LSDEV_Brachy Therapy"      ,   
            "LSDEV_Covered Stent"       ,    
            "ClosID1"      ,                 "ClosID10"       ,               "ClosID11"       ,               "ClosID12"    ,                 
            "ClosID13"      ,                "ClosID14"       ,               "ClosID15"      ,                "ClosID16"   ,                   "ClosID17",                     
            "ClosID18"       ,               "ClosID19"       ,               "ClosID2"       ,                "ClosID20"   ,                   "ClosID21" ,                    
            "ClosID22"         ,             "ClosID23"        ,              "ClosID24"       ,               "ClosID25"    ,                  "ClosID26" ,                    
            "ClosID27"         ,             "ClosID28"       ,               "ClosID29"      ,                "ClosID3"    ,                   "ClosID30",                     
            "ClosID31"          ,            "ClosID32"       ,               "ClosID33"      ,                "ClosID34"   ,                   "ClosID35",                     
            "ClosID36"            ,          "ClosID37"       ,               "ClosID38"      ,                "ClosID39"   ,                   "ClosID4" ,                     
            "ClosID40"          ,            "ClosID41"       ,               "ClosID42"      ,                "ClosID43"    ,                  "ClosID44",                     
            "ClosID45"           ,           "ClosID46"       ,               "ClosID47"      ,                "ClosID48"   ,                   "ClosID49",                     
            "ClosID5"           ,            "ClosID50"       ,               "ClosID51"      ,                "ClosID52"   ,                   "ClosID53",                     
            "ClosID54"          ,            "ClosID55"       ,               "ClosID56"      ,                "ClosID57"   ,                   "ClosID58",                     
            "ClosID59"          ,            "ClosID6"        ,               "ClosID60"      ,                "ClosID61"   ,                   "ClosID62",                     
            "ClosID63"          ,            "ClosID64"       ,               "ClosID65"      ,                "ClosID66"   ,                   "ClosID68",                     
            "ClosID69"          ,            "ClosID7"        ,               "ClosID70"      ,                "ClosID71"   ,                   "ClosID72",                     
            "ClosID8"           ,            "ClosID9"        ,               "ClosID952"     ,                "ClosID953"  ,                   "ClosID954",                    
            "ClosID955")

for(j in 1:dim(final.numeric.input)[2]) {
  col <- colnames(final.numeric.input)[j]
  if(col %in% numeric.missing) {
    if(col %in% med.na) {
      final.numeric.input[,j][which(is.na(final.numeric.input[,j]))] <- 0
    } else {
      #Use Median
      final.numeric.input[,j][which(is.na(final.numeric.input[,j]))] <- median(final.numeric.input[,j], na.rm=TRUE)
      #library(matrixStats)
      #Use Mean
      #final.numeric.input[,j][which(is.na(final.numeric.input[,j]))] <- colMeans(final.numeric.input[,j], na.rm=TRUE)
    }
  }
}



misc <- c('Hosp_ CITY', 'MPNDUP', 'PostMI', 'PostCardiogenicShock', 'PostHF','PostCVA', 'PostTamponade', 'PostDialysis', 'PostOtherVasComp', 'PostBleedHematoma',
          'PostProcCKMBND', 'PostProcTnlND', 'PostProcTnTND', 'PostProcCreat', 'PostProcCreatND', 'PostProcHgb', 'PostProcHgbND', 'StenosisPostProc',
          'DischargeMed16')

final.data.imputed <- final.numeric.input[,which(!(colnames(final.numeric.input) %in% misc))]
#imput CTO, PreTIMI
final.data.imputed$CTO[which(is.na(final.data.imputed$CTO))] <- median(final.data.imputed$CTO, na.rm=TRUE)
final.data.imputed$PreTIMI[which(is.na(final.data.imputed$PreTIMI))] <- median(final.data.imputed$PreTIMI, na.rm=TRUE)
ncdr.data.full <- final.data.imputed
ncdr.data.full$Bleed <- final.data$Bleed


ncdr.data.full$DQRULD <- NULL
ncdr.data.full$DQRULT <- NULL

#Create RAO Variables
#Have STEMI
AGELE70 <- rep(0, dim(ncdr.data.full)[1])
AGELE70[which(ncdr.data.full$Age <= 70)] <- 1

ncdr.data.full$AGELE70 <- AGELE70

AGEGT70 <- rep(0, dim(ncdr.data.full)[1])
AGEGT70[which(ncdr.data.full$Age > 70)] <- 1

ncdr.data.full$AGEGT70 <- AGEGT70

BMI <- (ncdr.data.full$Weight * 10000)/(ncdr.data.full$Height*ncdr.data.full$Height) #Direct from Yongfei's code
BMI[which(BMI < 5)] <- NA
BMI[which(BMI > 100)] <- NA
BMI[which(is.na(BMI))] <- median(BMI, na.rm=TRUE)

ncdr.data.full$BMI <- BMI

BMILE30 <- as.numeric((BMI <= 30))

ncdr.data.full$BMILE30 <- BMILE30

NEWDIAB <- rep(0, dim(ncdr.data.full)[1])
NEWDIAB[which((ncdr.data.full$Diabetes == 1) & (ncdr.data.full$DiabetesControl > 1))] <- 1
NEWDIAB[which((ncdr.data.full$Diabetes == 1) & (ncdr.data.full$DiabetesControl == 4))] <- 2

ncdr.data.full$NEWDIAB <- NEWDIAB

NEWDIAB1 <- as.numeric(NEWDIAB == 1)
NEWDIAB2 <- as.numeric(NEWDIAB == 2)

ncdr.data.full$NEWDIAB1 <- NEWDIAB1
ncdr.data.full$NEWDIAB2 <- NEWDIAB2

#From Yongfei's Code
HDEF <- ncdr.data.full$PrePCILVEF
#Removed the imputation aspects for consistency
HDEF[which(HDEF > 60)] <- 60
HDEF <- HDEF/5

ncdr.data.full$HDEF <- HDEF

FEMALE <- as.numeric(ncdr.data.full$Sex == 2)

ncdr.data.full$FEMALE <- FEMALE

GENDMULT <- rep(1, dim(ncdr.data.full)[1])
GENDMULT[which(FEMALE == 1)] <- 0.742

ncdr.data.full$GENDMULT <- GENDMULT

RACEMULT <- rep(1, dim(ncdr.data.full)[1])
RACEMULT[which(ncdr.data.full$RaceBlack == 1)] <- 1.21

ncdr.data.full$RACEMULT <- RACEMULT

GFR <- rep(NA, dim(ncdr.data.full)[1])
GFR[which(ncdr.data.full$PreProcCreat > 0)] <- (186 * (ncdr.data.full$PreProcCreat ^ (-1.154))) * (ncdr.data.full$Age ^ (-0.203)) * GENDMULT * RACEMULT

RENFAIL <- as.numeric((GFR < 30) | (ncdr.data.full$CurrentDialysis == 1))

ncdr.data.full$RENFAIL <- RENFAIL

CKD <- rep(3, dim(ncdr.data.full)[1])
CKD[which((ncdr.data.full$CurrentDialysis == 1) | (GFR < 30))] <- 4
CKD[which(GFR >= 30)] <- 2
CKD[which(GFR >= 45)] <- 1
CKD[which(GFR >= 60)] <- 0

CKD1 <- as.numeric(CKD == 1)
CKD2 <- as.numeric(CKD == 2)
CKD3 <- as.numeric(CKD == 3)
CKD4 <- as.numeric(CKD == 4)

ncdr.data.full$CKD <- CKD
ncdr.data.full$CKD1 <- CKD1
ncdr.data.full$CKD2 <- CKD2
ncdr.data.full$CKD3 <- CKD3
ncdr.data.full$CKD4 <- CKD4

GFR[which(GFR > 90)] <- 90
GFR <- GFR/5

ncdr.data.full$GFR <- GFR

LYTICS <- as.numeric((ncdr.data.full$CADPresentation == 6) & (ncdr.data.full$ThromTherapy == 1))

ncdr.data.full$LYTICS <- LYTICS

CARSHOCK <- as.numeric((ncdr.data.full$PriorCardioShock == 1) | (ncdr.data.full$PCICardioShock == 1))

ncdr.data.full$CARSHOCK <- CARSHOCK

DCARSHOCK <- rep(0, dim(ncdr.data.full)[1])
DCARSHOCK[which(ncdr.data.full$PriorCardioShock == 1)] <- (1 + as.numeric(ncdr.data.full$PCICardioShock == 0))[which(ncdr.data.full$PriorCardioShock == 1)]
DCARSHOCK[which(ncdr.data.full$PriorCardioShock == 0)] <- (3 + as.numeric(ncdr.data.full$PCICardioShock == 0))[which(ncdr.data.full$PriorCardioShock == 0)]

ncdr.data.full$DCARSHOCK <- DCARSHOCK

SHOCKPCIS <- rep(6, dim(ncdr.data.full)[1])
SHOCKPCIS[which(ncdr.data.full$PCIStatus == 2)] <- 5
SHOCKPCIS[which(ncdr.data.full$PCIStatus == 3)] <- 4
SHOCKPCIS[which((DCARSHOCK == 2) | (DCARSHOCK == 3))] <- 3
SHOCKPCIS[which((DCARSHOCK == 1) | (ncdr.data.full$PCIStatus == 4))] <- 2
SHOCKPCIS[which((DCARSHOCK == 1) & (ncdr.data.full$PCIStatus == 4))] <- 1

SHOCKPCIS1 <- as.numeric(SHOCKPCIS == 1)
SHOCKPCIS2 <- as.numeric(SHOCKPCIS == 2)
SHOCKPCIS3 <- as.numeric(SHOCKPCIS == 3)
SHOCKPCIS4 <- as.numeric(SHOCKPCIS == 4)
SHOCKPCIS5 <- as.numeric(SHOCKPCIS == 5)
SHOCKPCIS6 <- as.numeric(SHOCKPCIS == 6)

ncdr.data.full$SHOCKPCIS <- SHOCKPCIS
ncdr.data.full$SHOCKPCIS1 <- SHOCKPCIS1
ncdr.data.full$SHOCKPCIS2 <- SHOCKPCIS2
ncdr.data.full$SHOCKPCIS3 <- SHOCKPCIS3
ncdr.data.full$SHOCKPCIS4 <- SHOCKPCIS4
ncdr.data.full$SHOCKPCIS5 <- SHOCKPCIS5
ncdr.data.full$SHOCKPCIS6 <- SHOCKPCIS6

ncdr.data.full$SHOCKPRIORANDSTART <- as.numeric(DCARSHOCK == 1)


LESSCAI23 <- as.numeric((ncdr.data.full$LESSCAI == 2) | (ncdr.data.full$LESSCAI == 3))
LESSCAI4 <- as.numeric(ncdr.data.full$LESSCAI == 4)

ncdr.data.full$LESSCAI23 <- LESSCAI23
ncdr.data.full$LESSCAI4 <- LESSCAI4

NEWSEQ2 <- as.numeric(ncdr.data.full$NEWSEQ == 2)
NEWSEQ3 <- as.numeric(ncdr.data.full$NEWSEQ == 3)

ncdr.data.full$NEWSEQ2 <- NEWSEQ2
ncdr.data.full$NEWSEQ3 <- NEWSEQ3

NYHA123 <- as.numeric((ncdr.data.full$NYHA == 1) | (ncdr.data.full$NYHA == 2) | (ncdr.data.full$NYHA == 3))
NYHA4 <- as.numeric(ncdr.data.full$NYHA == 4)

ncdr.data.full$NYHA123 <- NYHA123
ncdr.data.full$NYHA4 <- NYHA4

PRETIMINO <- as.numeric(ncdr.data.full$PreTIMI == 0)

ncdr.data.full$PRETIMINO <- PRETIMINO

NVD <- rep(0, dim(ncdr.data.full)[1])
NVD[which(ncdr.data.full$LMStenosis <= 50)] <- (as.numeric(ncdr.data.full$RCAStenosis > 70) + 
                                                  as.numeric((ncdr.data.full$ProxLADStenosis > 70) | (ncdr.data.full$MidDistalLADStenosis > 70)) +
                                                  as.numeric((ncdr.data.full$CIRCStenosis > 70) | (ncdr.data.full$RamusStenosis > 70)))[which(ncdr.data.full$LMStenosis <= 50)]
NVD[which((ncdr.data.full$LMStenosis > 50) & ((ncdr.data.full$Dominance == 1) | (ncdr.data.full$RCAStenosis > 70)))] <- 3
NVD[which((ncdr.data.full$LMStenosis > 50) & !((ncdr.data.full$Dominance == 1) | (ncdr.data.full$RCAStenosis > 70)))] <- 2

NVD23 <- as.numeric((NVD ==2) | (NVD == 3))

ncdr.data.full$NVD <- NVD
ncdr.data.full$NVD23 <- NVD23

PREHGBLE13 <- as.numeric(ncdr.data.full$PreProcHgb <= 13)
PREHGBGT13 <- as.numeric(ncdr.data.full$PreProcHgb > 13)

ncdr.data.full$PREHGBLE13 <- PREHGBLE13
ncdr.data.full$PREHGBGT13 <- PREHGBGT13




closure.IDs <- colnames(ncdr.data.full)[which(grepl('ClosID',colnames(ncdr.data.full)))]
closure.types <- rep(NA, length(closure.IDs))

for(i in 1:length(closure.IDs)) {
  clos.id <- closure.IDs[i]
  id.num <- as.numeric(strsplit(clos.id, 'ID')[[1]][2])
  id.type <- as.character(data.sas[which(data.sas$ClosureDevID == id.num),]$DeviceTypeName[1])
  closure.types[i] <- id.type
}

clos.info <- data.frame(closure.IDs, closure.types)

closure.data <- ncdr.data.full[,which(colnames(ncdr.data.full) %in% closure.IDs)]
clos.vars <- matrix(0, dim(ncdr.data.full)[1], length(unique(closure.types)))


library(doParallel)
library(foreach)
registerDoParallel()
clos.data <- foreach(i=1:dim(ncdr.data.full)[1], .combine='rbind') %dopar% {
#for(i in 1:dim(ncdr.data.full)[1]) {
  #patch
  row <- ncdr.data.full[i,]
  clos.vec <- vector()
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Patch'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Patch[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Manual com
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Manual com'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$`Clos_Manual com`[1] <- sum(row[,which(colnames(row) %in% IDs)])
  #Sealant
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Sealant'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Sealant[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Other
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Other'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Other[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Mechanical
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Mechanical'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Mechanical[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Suture
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Suture'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Suture[i] <- sum(row[,which(colnames(row) %in% IDs)])
  #Staple
  IDs <- unique(clos.info[which(clos.info$closure.types == 'Staple'),][,1])
  clos.vec <- c(clos.vec,sum(row[,which(colnames(row) %in% IDs)]))
  #clos.vars$Clos_Staple[i] <- sum(row[,which(colnames(row) %in% IDs)])
  
  as.numeric(clos.vec)
}
stopImplicitCluster()

clos.vars <- data.frame(clos.data)
colnames(clos.vars) <- paste('Clos_',unique(closure.types), sep='')

ncdr.data.full <- ncdr.data.full[,-which(colnames(ncdr.data.full) %in% closure.IDs)]
ncdr.data.full <- cbind(ncdr.data.full, clos.vars)

#Load IDX Rao
#save('rao.variables', 'remainder.variables', 'idx', file='Y:/NCDR/BJMDATA/CPBLEED/RaoVariables_v2_13Nov2015.RData')
load(file='Y:/NCDR/BJMDATA/CPBLEED/RaoVariables_v3_07MAR2016.RData')

ncdr.rao.var <- ncdr.data.full[idx,]
ncdr.rem.var <- ncdr.data.full[-idx,]

save('ncdr.data.full', 'ncdr.rao.var', 'ncdr.rem.var', file='Y:/NCDR/BJMDATA/CPBLEED/NCDR_Full_v5_20APR2016.RData')
load(file='Y:/NCDR/BJMDATA/CPBLEED/NCDR_Full_v5_20APR2016.RData')

#Remove NA/ND columns
idx.na <- which((grepl('ND', colnames(ncdr.data.full)) | grepl('NA', colnames(ncdr.data.full))) & !(grepl('GENDMULT', colnames(ncdr.data.full))))
ncdr.data.full <- ncdr.data.full[, -idx.na]
ncdr.rao.var <- ncdr.rao.var[, -idx.na]
ncdr.rem.var <- ncdr.rem.var[, -idx.na]
#Remove CABG Patients
cabg.pats <- which(ncdr.data.full$CABG == 1)
ncdr.data.full <- ncdr.data.full[-cabg.pats,]
cabg.pats.rao <- which(ncdr.rao.var$CABG == 1)
ncdr.rao.var <- ncdr.rao.var[-cabg.pats.rao,]
cabg.pats.rem <- which(ncdr.rem.var$CABG == 1)
ncdr.rem.var <- ncdr.rem.var[-cabg.pats.rem,]
#Remove CABG Columns
idx.cabg <- which((grepl('CABG', colnames(ncdr.data.full))) & !(grepl('PriorCABG', colnames(ncdr.data.full))))
ncdr.data.full <- ncdr.data.full[, -idx.cabg]
ncdr.rao.var <- ncdr.rao.var[, -idx.cabg]
ncdr.rem.var <- ncdr.rem.var[, -idx.cabg]
#Remove other major surgery patients
surg.pats <- which(ncdr.data.full$OtherMajorSurgery == 1)
ncdr.data.full <- ncdr.data.full[-surg.pats,]
surg.pats.rao <- which(ncdr.rao.var$OtherMajorSurgery == 1)
ncdr.rao.var <- ncdr.rao.var[-surg.pats.rao,]
surg.pats.rem <- which(ncdr.rem.var$OtherMajorSurgery == 1)
ncdr.rem.var <- ncdr.rem.var[-surg.pats,]
#Remove other major surgery columns
ncdr.data.full$OtherMajorSurgery <- NULL
ncdr.rao.var$OtherMajorSurgery <- NULL
ncdr.rem.var$OtherMajorSurgery <- NULL
save('ncdr.data.full', 'ncdr.rao.var', 'ncdr.rem.var', file='Y:/NCDR/BJMDATA/CPBLEED/NCDR_Full_v6_22APR2016.RData')
load(file='Y:/NCDR/BJMDATA/CPBLEED/NCDR_Full_v6_22APR2016.RData')
#Fix Closure + Access Site variables

clos.cols <- which(grepl('Clos', colnames(ncdr.data.full)))
clos.names <- colnames(ncdr.data.full)[clos.cols]
access.names <- c('Femoral', 'Brachial','Radial', 'Other')
femoral <- rep(0, dim(ncdr.data.full)[1])
brachial <- rep(0, dim(ncdr.data.full)[1])
radial <- rep(0, dim(ncdr.data.full)[1])
other <- rep(0, dim(ncdr.data.full)[1])

femoral[which(ncdr.data.full$AcessSite == 1)] <- 1
brachial[which(ncdr.data.full$AcessSite == 2)] <- 1
radial[which(ncdr.data.full$AcessSite == 3)] <- 1
other[which(ncdr.data.full$AcessSite == 4)] <- 1

access.closure.names <- vector()
for(i in 1:length(access.names)) {
  for(j in 1:length(clos.names)) {
    access.closure.names <- c(access.closure.names, paste(access.names[i], '_', clos.names[j], sep=''))
  }
}

access.closure <- matrix(0, dim(ncdr.data.full)[1], length(access.closure.names))
for(i in 1:length(access.closure.names)) {
  closure.name <- access.closure.names[i]
  print(i)
  if(grepl('Femoral_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Femoral_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- clos.index

    access.closure[,mat.idx][which(femoral == 1)] <- ncdr.data.full[,clos.idx][which(femoral == 1)]
    
  } else if(grepl('Brachial_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Brachial_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 7 + clos.index
    
    access.closure[,mat.idx][which(brachial == 1)] <- ncdr.data.full[,clos.idx][which(brachial == 1)]
  } else if(grepl('Radial_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Radial_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 14 + clos.index
    
    access.closure[,mat.idx][which(radial == 1)] <- ncdr.data.full[,clos.idx][which(radial == 1)]
  } else {
    closure.val <- strsplit(closure.name, 'Other_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 21 + clos.index
    
    access.closure[,mat.idx][which(other == 1)] <- ncdr.data.full[,clos.idx][which(other == 1)]
  }
}
access.closure <- data.frame(access.closure)
colnames(access.closure) <- access.closure.names

clos.na <- which(rowSums(access.closure) == 0)

fem.noclos <- rep(0, dim(ncdr.data.full)[1])
brachial.noclos <- rep(0, dim(ncdr.data.full)[1])
radial.noclos <- rep(0, dim(ncdr.data.full)[1])
other.noclos <- rep(0, dim(ncdr.data.full)[1])

fem.idx <- which(femoral == 1)
brachial.idx <- which(brachial == 1)
radial.idx <- which(radial == 1)
other.idx <- which(other == 1)

fem.noclos[fem.idx[which(fem.idx %in% clos.na)]] <- 1
brachial.noclos[brachial.idx[which(brachial.idx %in% clos.na)]] <- 1
radial.noclos[radial.idx[which(radial.idx %in% clos.na)]] <- 1
other.noclos[other.idx[which(other.idx %in% clos.na)]] <- 1


noclos.df <- data.frame(fem.noclos, brachial.noclos, radial.noclos, other.noclos)
colnames(noclos.df) <- c('Femoral_Clos_None', 'Brachial_Clos_None', 'Radial_Clos_None', 'Other_Clos_None')

access.closure <- cbind(access.closure, noclos.df)

site.df <- data.frame(femoral, brachial, radial, other)
colnames(site.df) <- c('Femoral_site', 'Brachial_site', 'Radial_site', 'Other_site')

access.closure <- cbind(access.closure, site.df)

ncdr.data.full <- cbind(ncdr.data.full, access.closure)
remove.cols <- c(clos.names, 'AcessSite')

ncdr.data.full <- ncdr.data.full[,-which(colnames(ncdr.data.full) %in% remove.cols)]
#Recreated for the other two
femoral <- rep(0, dim(ncdr.rao.var)[1])
brachial <- rep(0, dim(ncdr.rao.var)[1])
radial <- rep(0, dim(ncdr.rao.var)[1])
other <- rep(0, dim(ncdr.rao.var)[1])

femoral[which(ncdr.rao.var$AcessSite == 1)] <- 1
brachial[which(ncdr.rao.var$AcessSite == 2)] <- 1
radial[which(ncdr.rao.var$AcessSite == 3)] <- 1
other[which(ncdr.rao.var$AcessSite == 4)] <- 1
access.closure <- matrix(0, dim(ncdr.rao.var)[1], length(access.closure.names))
for(i in 1:length(access.closure.names)) {
  closure.name <- access.closure.names[i]
  print(i)
  if(grepl('Femoral_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Femoral_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- clos.index
    
    access.closure[,mat.idx][which(femoral == 1)] <- ncdr.rao.var[,clos.idx][which(femoral == 1)]
    
  } else if(grepl('Brachial_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Brachial_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 7 + clos.index
    
    access.closure[,mat.idx][which(brachial == 1)] <- ncdr.rao.var[,clos.idx][which(brachial == 1)]
  } else if(grepl('Radial_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Radial_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 14 + clos.index
    
    access.closure[,mat.idx][which(radial == 1)] <- ncdr.rao.var[,clos.idx][which(radial == 1)]
  } else {
    closure.val <- strsplit(closure.name, 'Other_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 21 + clos.index
    
    access.closure[,mat.idx][which(other == 1)] <- ncdr.rao.var[,clos.idx][which(other == 1)]
  }
}
access.closure <- data.frame(access.closure)
colnames(access.closure) <- access.closure.names

clos.na <- which(rowSums(access.closure) == 0)

fem.noclos <- rep(0, dim(ncdr.rao.var)[1])
brachial.noclos <- rep(0, dim(ncdr.rao.var)[1])
radial.noclos <- rep(0, dim(ncdr.rao.var)[1])
other.noclos <- rep(0, dim(ncdr.rao.var)[1])

fem.idx <- which(femoral == 1)
brachial.idx <- which(brachial == 1)
radial.idx <- which(radial == 1)
other.idx <- which(other == 1)

fem.noclos[fem.idx[which(fem.idx %in% clos.na)]] <- 1
brachial.noclos[brachial.idx[which(brachial.idx %in% clos.na)]] <- 1
radial.noclos[radial.idx[which(radial.idx %in% clos.na)]] <- 1
other.noclos[other.idx[which(other.idx %in% clos.na)]] <- 1


noclos.df <- data.frame(fem.noclos, brachial.noclos, radial.noclos, other.noclos)
colnames(noclos.df) <- c('Femoral_Clos_None', 'Brachial_Clos_None', 'Radial_Clos_None', 'Other_Clos_None')

access.closure <- cbind(access.closure, noclos.df)

site.df <- data.frame(femoral, brachial, radial, other)
colnames(site.df) <- c('Femoral_site', 'Brachial_site', 'Radial_site', 'Other_site')

access.closure <- cbind(access.closure, site.df)



ncdr.rao.var <- cbind(ncdr.rao.var, access.closure)
remove.cols <- c(clos.names, 'AcessSite')

ncdr.rao.var <- ncdr.rao.var[,-which(colnames(ncdr.rao.var) %in% remove.cols)]

femoral <- rep(0, dim(ncdr.rem.var)[1])
brachial <- rep(0, dim(ncdr.rem.var)[1])
radial <- rep(0, dim(ncdr.rem.var)[1])
other <- rep(0, dim(ncdr.rem.var)[1])

femoral[which(ncdr.rem.var$AcessSite == 1)] <- 1
brachial[which(ncdr.rem.var$AcessSite == 2)] <- 1
radial[which(ncdr.rem.var$AcessSite == 3)] <- 1
other[which(ncdr.rem.var$AcessSite == 4)] <- 1
access.closure <- matrix(0, dim(ncdr.rem.var)[1], length(access.closure.names))
for(i in 1:length(access.closure.names)) {
  closure.name <- access.closure.names[i]
  print(i)
  if(grepl('Femoral_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Femoral_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- clos.index
    
    access.closure[,mat.idx][which(femoral == 1)] <- ncdr.rem.var[,clos.idx][which(femoral == 1)]
    
  } else if(grepl('Brachial_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Brachial_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 7 + clos.index
    
    access.closure[,mat.idx][which(brachial == 1)] <- ncdr.rem.var[,clos.idx][which(brachial == 1)]
  } else if(grepl('Radial_', closure.name)) {
    closure.val <- strsplit(closure.name, 'Radial_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 14 + clos.index
    
    access.closure[,mat.idx][which(radial == 1)] <- ncdr.rem.var[,clos.idx][which(radial == 1)]
  } else {
    closure.val <- strsplit(closure.name, 'Other_')[[1]][2]
    clos.index <- which(clos.names == closure.val)
    clos.idx <- clos.cols[which(clos.names == closure.val)]
    mat.idx <- 21 + clos.index
    
    access.closure[,mat.idx][which(other == 1)] <- ncdr.rem.var[,clos.idx][which(other == 1)]
  }
}
access.closure <- data.frame(access.closure)
colnames(access.closure) <- access.closure.names

clos.na <- which(rowSums(access.closure) == 0)

fem.noclos <- rep(0, dim(ncdr.rem.var)[1])
brachial.noclos <- rep(0, dim(ncdr.rem.var)[1])
radial.noclos <- rep(0, dim(ncdr.rem.var)[1])
other.noclos <- rep(0, dim(ncdr.rem.var)[1])

fem.idx <- which(femoral == 1)
brachial.idx <- which(brachial == 1)
radial.idx <- which(radial == 1)
other.idx <- which(other == 1)

fem.noclos[fem.idx[which(fem.idx %in% clos.na)]] <- 1
brachial.noclos[brachial.idx[which(brachial.idx %in% clos.na)]] <- 1
radial.noclos[radial.idx[which(radial.idx %in% clos.na)]] <- 1
other.noclos[other.idx[which(other.idx %in% clos.na)]] <- 1


noclos.df <- data.frame(fem.noclos, brachial.noclos, radial.noclos, other.noclos)
colnames(noclos.df) <- c('Femoral_Clos_None', 'Brachial_Clos_None', 'Radial_Clos_None', 'Other_Clos_None')

access.closure <- cbind(access.closure, noclos.df)

site.df <- data.frame(femoral, brachial, radial, other)
colnames(site.df) <- c('Femoral_site', 'Brachial_site', 'Radial_site', 'Other_site')

access.closure <- cbind(access.closure, site.df)

ncdr.rem.var <- cbind(ncdr.rem.var, access.closure)
remove.cols <- c(clos.names, 'AcessSite')

ncdr.rem.var <- ncdr.rem.var[,-which(colnames(ncdr.rem.var) %in% remove.cols)]
save('ncdr.data.full', 'ncdr.rao.var', 'ncdr.rem.var', file='Y:/NCDR/BJMDATA/CPBLEED/NCDR_Full_v8_7JUN2016.RData')
for(ACCESS in c('femoral', 'brachial', 'radial', 'other')) {
load(file='Y:/NCDR/BJMDATA/CPBLEED/NCDR_Full_v8_7JUN2016.RData')
#ACCESS <- 'all'
if(ACCESS == 'femoral') {
  ncdr.data.full <- ncdr.data.full[which(ncdr.data.full$Femoral_site == 1),]
} else if(ACCESS == 'brachial') {
  ncdr.data.full <- ncdr.data.full[which(ncdr.data.full$Brachial_site == 1),]
} else if(ACCESS == 'radial') {
  ncdr.data.full <- ncdr.data.full[which(ncdr.data.full$Radial_site == 1),]
} else if(ACCESS == 'other') {
  ncdr.data.full <- ncdr.data.full[which(ncdr.data.full$Other_site == 1),]
} else {
  #All
}
#Create 5-fold Cross Validation
positives <- which(ncdr.data.full$Bleed == 1)
negatives <- which(ncdr.data.full$Bleed == 0)

data.positives <- ncdr.data.full[positives,]
data.negatives <- ncdr.data.full[negatives,]

numpos <- round(0.2*length(positives))
numneg <- round(0.2*length(negatives))

cv.fold1.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
cv.fold1.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))

fold1.pos <- data.positives[cv.fold1.idx.pos,]
fold1.neg <- data.negatives[cv.fold1.idx.neg,]

data.positives <- data.positives[-cv.fold1.idx.pos,]
data.negatives <- data.negatives[-cv.fold1.idx.neg,]

cv.fold2.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
cv.fold2.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))

fold2.pos <- data.positives[cv.fold2.idx.pos,]
fold2.neg <- data.negatives[cv.fold2.idx.neg,]

data.positives <- data.positives[-cv.fold2.idx.pos,]
data.negatives <- data.negatives[-cv.fold2.idx.neg,]

cv.fold3.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
cv.fold3.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))

fold3.pos <- data.positives[cv.fold3.idx.pos,]
fold3.neg <- data.negatives[cv.fold3.idx.neg,]

data.positives <- data.positives[-cv.fold3.idx.pos,]
data.negatives <- data.negatives[-cv.fold3.idx.neg,]

cv.fold4.idx.pos <- sort(sample(dim(data.positives)[1], numpos, replace=FALSE))
cv.fold4.idx.neg <- sort(sample(dim(data.negatives)[1], numneg, replace=FALSE))

fold4.pos <- data.positives[cv.fold4.idx.pos,]
fold4.neg <- data.negatives[cv.fold4.idx.neg,]

data.positives <- data.positives[-cv.fold4.idx.pos,]
data.negatives <- data.negatives[-cv.fold4.idx.neg,]

fold5.pos <- data.positives
fold5.neg <- data.negatives

model.outcomes <- c('Bleed')

#fold1
train1 <- rbind(fold2.pos, fold2.neg,
                fold3.pos, fold3.neg,
                fold4.pos, fold4.neg,
                fold5.pos, fold5.neg)

train1.data <- train1[,which(!(colnames(train1) %in% model.outcomes))]
train1.labels <- train1[,which(colnames(train1) %in% model.outcomes)]

test1 <- rbind(fold1.pos, fold1.neg)
test1.data <- test1[,which(!(colnames(test1) %in% model.outcomes))]
test1.labels <- test1[,which(colnames(test1) %in% model.outcomes)]

#fold2
train2 <- rbind(fold1.pos, fold1.neg,
                fold3.pos, fold3.neg,
                fold4.pos, fold4.neg,
                fold5.pos, fold5.neg)

train2.data <- train2[,which(!(colnames(train2) %in% model.outcomes))]
train2.labels <- train2[,which(colnames(train2) %in% model.outcomes)]

test2 <- rbind(fold2.pos, fold2.neg)
test2.data <- test2[,which(!(colnames(test2) %in% model.outcomes))]
test2.labels <- test2[,which(colnames(test2) %in% model.outcomes)]

#fold3
train3 <- rbind(fold2.pos, fold2.neg,
                fold1.pos, fold1.neg,
                fold4.pos, fold4.neg,
                fold5.pos, fold5.neg)

train3.data <- train3[,which(!(colnames(train3) %in% model.outcomes))]
train3.labels <- train3[,which(colnames(train3) %in% model.outcomes)]

test3 <- rbind(fold3.pos, fold3.neg)
test3.data <- test3[,which(!(colnames(test3) %in% model.outcomes))]
test3.labels <- test3[,which(colnames(test3) %in% model.outcomes)]

#fold4
train4 <- rbind(fold2.pos, fold2.neg,
                fold3.pos, fold3.neg,
                fold1.pos, fold1.neg,
                fold5.pos, fold5.neg)

train4.data <- train4[,which(!(colnames(train4) %in% model.outcomes))]
train4.labels <- train4[,which(colnames(train4) %in% model.outcomes)]

test4 <- rbind(fold4.pos, fold4.neg)
test4.data <- test4[,which(!(colnames(test4) %in% model.outcomes))]
test4.labels <- test4[,which(colnames(test4) %in% model.outcomes)]

#fold5
train5 <- rbind(fold2.pos, fold2.neg,
                fold3.pos, fold3.neg,
                fold4.pos, fold4.neg,
                fold1.pos, fold1.neg)

train5.data <- train5[,which(!(colnames(train5) %in% model.outcomes))]
train5.labels <- train5[,which(colnames(train5) %in% model.outcomes)]

test5 <- rbind(fold5.pos, fold5.neg)
test5.data <- test5[,which(!(colnames(test5) %in% model.outcomes))]
test5.labels <- test5[,which(colnames(test5) %in% model.outcomes)]

if(ACCESS == 'femoral') {
  filename <- 'Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/femoral_five_fold_data_V8_7JUN2016.RData'
} else if(ACCESS == 'brachial') {
  filename <- 'Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/brachial_five_fold_data_V8_7JUN2016.RData'
} else if(ACCESS == 'radial') {
  filename <- 'Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/radial_five_fold_data_V8_7JUN2016.RData'
} else if(ACCESS == 'other') {
  filename <- 'Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/other_five_fold_data_V8_7JUN2016.RData'
} else {
  #All
  filename <- 'Y:/NCDR/BJMDATA/CPBLEED/CrossVal/FullModel/five_fold_data_V8_7JUN2016.RData'
}


save('train1.data', 'train1.labels', 'test1.data', 'test1.labels',
     'train2.data', 'train2.labels', 'test2.data', 'test2.labels',
     'train3.data', 'train3.labels', 'test3.data', 'test3.labels',
     'train4.data', 'train4.labels', 'test4.data', 'test4.labels',
     'train5.data', 'train5.labels', 'test5.data', 'test5.labels',
    'model.outcomes', 'ncdr.data.full', 'ncdr.rao.var', 'ncdr.rem.var',
    'cv.fold1.idx.pos', 'cv.fold1.idx.neg',
    'cv.fold2.idx.pos', 'cv.fold2.idx.neg',
    'cv.fold3.idx.pos', 'cv.fold3.idx.neg',
    'cv.fold4.idx.pos', 'cv.fold4.idx.neg',
    file=filename)

rm(list=ls())
}
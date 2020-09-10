library(devtools)
library(roxygen2)
library(clarite)
library(moments)
library(ggplot2)
library(gridExtra)

#Load database
load("../Data/nh_99-06.Rdata")

# Load variable descriptions for annotation when saving files
descriptions_info <- unique(VarDescription[,colnames(VarDescription) %in% c("tab_desc","module","var","var_desc")])

#Upload all files
# data_main_table <- paste("/home/tomas/Documents/RepPheEWAS/Data/MainTable.csv", sep="/")
# data_var_description <- paste("/home/tomas/Documents/RepPheEWAS/Data/VarDescription.csv", sep="/")
# data_var_phenotypes_dis <- paste("/home/tomas/Documents/RepPheEWAS/Data/Pheno_List_dis.txt", sep="/")
# read_over_18 <- read.delim("/home/tomas/Documents/RepPheEWAS/Data/MainTable_keepvar_over18.tsv")
# preg_main_table <- read.csv("/home/tomas/Documents/RepPheEWAS/Data/Main_table_up.csv")

# Load variable descriptions for annotation when saving files
# descriptions <- read.csv(data_var_description)
# descriptions_info <- unique(descriptions[,colnames(descriptions) %in% c("tab_desc","module","var","var_desc")])

# Add survey year and combine LBDHDD and LBDHDL columns (they are the same variable)
# main_table <- read.csv(data_main_table) 
colnames(MainTable)[1] <- "ID" 
survey_year <- colfilter(MainTable, c("ID")) 
MainTable[MainTable$SDDSRVYR==3 | MainTable$SDDSRVYR==4, "LBDHDL"] = MainTable[MainTable$SDDSRVYR==3 | MainTable$SDDSRVYR==4, "LBDHDD"] 
nhanes <- merge_data(survey_year, MainTable, union = FALSE)

# The values 7 and/or 9 in some variables need to be defined as 'NA'
nhanes$SMQ077[nhanes$SMQ077==7] <- NA
nhanes$DBD100[nhanes$DBD100==9] <- NA

#Get survey info
survey_design_discovery <- read.csv(paste("/home/tomas/Documents/RepPheEWAS/Data/weights_discovery.txt", sep="/"), sep="\t")
colnames(survey_design_discovery)[1] <- "ID"
survey_design_discovery <- colfilter(survey_design_discovery, c("SDDSRVYR"), exclude=TRUE)

#Get survey info
survey_design_replication <- read.csv(paste("/home/tomas/Documents/RepPheEWAS/Data/weights_replication.txt", sep="/"), sep="\t")
colnames(survey_design_replication)[1] <- "ID"
survey_design_replication <- colfilter(survey_design_replication, c("SDDSRVYR"), exclude=TRUE)

# Get weight mapping data
var_weights <- read.csv(paste("/home/tomas/Documents/RepPheEWAS/Data/VarWeights.csv", sep="/"), fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
var_weights <- var_weights[complete.cases(var_weights), ]
weights_discovery <- setNames(as.list(var_weights$discovery), as.list(var_weights$variable_name))
weights_replication <- setNames(as.list(var_weights$replication), as.list(var_weights$variable_name))

# Divide replication weights by 2 to get 4 year weights
survey_design_replication[,-(1:4)] <- survey_design_replication[,-(1:4)] / 2

# Remove physical fitness vars and unknown pesticide variable
no_pf_vars_URXP08 <- c("CVDVOMAX","CVDESVO2","CVDS1HR","CVDS1SY","CVDS1DI","CVDS2HR","CVDS2SY", "CVDS2DI","CVDR1HR","CVDR1SY","CVDR1DI","CVDR2HR","CVDR2SY","CVDR2DI","physical_activity", "URXP08") 
nhanes <- colfilter(d = nhanes, cols = c(no_pf_vars_URXP08), exclude = TRUE)

# Remove other variables (inconclusive meaning)
others <- c("house_type","hepa","hepb", "house_age", "current_past_smoking","house_age", "DUQ130", "DMDMARTL", "income")
nhanes <- colfilter(nhanes, cols=others, exclude = TRUE)

#Set covariates and phenotypes 
covariates <- c("female", "SES_LEVEL", "SDDSRVYR", "RIDAGEYR", "other_eth", "other_hispanic", "black", "mexican", "BMXBMI")
phenotype <- c("LBXSCRINV", "URXUCR",	"LBXSCR", "LBXSATSI",	"LBXSAL",	"URXUMASI",	"URXUMA",	"LBXSAPSI",	"LBXSASSI",	"LBXSC3SI",	"LBXSBU",	"LBXBAP",	"LBXCPSI",	"LBXCRP",	"LBXSCLSI",	"LBXSCH",	"LBDHDL",	"LBDLDL",	"LBXFER",	"LBXSGTSI",	"LBXSGB",	"LBXGLU",	"LBXGH",	"LBXHCY",	"LBXSIR",	"LBXSLDSI",	"LBXMMA",	"LBXSOSSI",	"LBXSPH",	"LBXSKSI",	"LBXEPP",	"LBXSNASI",	"LBXTIB",	"LBXSTB",	"LBXSCA",	"LBXSTP",	"LBDPCT",	"LBXSTR",	"LBXSUA",	"LBDBANO",	"LBXBAPCT",	"LBDEONO",	"LBXEOPCT",	"LBXHCT",	"LBXHGB",	"LBDLYMNO",	"LBXMCHSI",	"LBXLYPCT",	"LBXMCVSI",	"LBXMPSI",	"LBDMONO",	"LBXMOPCT",	"LBXPLTSI",	"LBXRBCSI",	"LBXRDW",	"LBDNENO",	"LBXNEPCT", "LBXIRN")
phenotype_rep <- c("LBXSCRINV", "URXUCR",	"LBXSCR", "LBXSATSI",	"LBXSAL",	"URXUMASI",	"URXUMA",	"LBXSAPSI",	"LBXSASSI",	"LBXSC3SI",	"LBXSBU",	"LBXBAP",	"LBXCPSI",	"LBXCRP",	"LBXSCLSI",	"LBXSCH",	"LBDHDL",	"LBDLDL",	"LBXFER",	"LBXSGTSI",	"LBXSGB",	"LBXGLU",	"LBXGH",	"LBXHCY",	"LBXSIR",	"LBXSLDSI",	"LBXMMA",	"LBXSOSSI",	"LBXSPH", "LBXSKSI",	"LBXEPP",	"LBXSNASI",	"LBXTIB",	"LBXSTB",	"LBXSCA",	"LBXSTP",	"LBDPCT",	"LBXSTR",	"LBXSUA",	"LBDBANO",	"LBXBAPCT",	"LBDEONO",	"LBXEOPCT",	"LBXHCT",	"LBXHGB",	"LBDLYMNO",	"LBXMCHSI",	"LBXLYPCT",	"LBXMCVSI",	"LBXMPSI",	"LBDMONO",	"LBXMOPCT",	"LBXPLTSI",	"LBXRBCSI",	"LBXRDW",	"LBDNENO",	"LBXNEPCT", "LBXIRN")

#Remove variables that do not have 4 year weights 
remove_variables_lbxtc <- c("LBXTC")
nhanes <- colfilter(nhanes, remove_variables_lbxtc, exclude=TRUE)
nhanes <- colfilter(nhanes, remove_variables_lbxtc, exclude=TRUE)

# Split MainTable variables by type (1)
###########
bin_phe <- get_binary(nhanes)
cat_phe <- get_categorical(nhanes)
cont_phe <- get_continuous(nhanes)
check_phe <- get_check(nhanes)

# Some columns need to be moved from categorical to continuous
cat_to_cont_phe <- c("L_ASPARTIC_ACID_mg", "L_GLUTAMINE_gm", "DBQ095", "DRDDRSTZ", "BETA_CAROTENE_mcg", "CALCIUM_PPM", "GLYCINE_gm", "LBXPFDO", "LBDBANO", "LBXTO3", "DRD350AQ",	"DRD350BQ",	"DRD350CQ",	"DRD350DQ",	"DRD350EQ",	"DRD350FQ",	"DRD350GQ",	"DRD350IQ",	"DRD350JQ",	"DRD370AQ",	"DRD370CQ",	"DRD370DQ",	"DRD370EQ",	"DRD370FQ",	"DRD370GQ",	"DRD370HQ",	"DRD370IQ",	"DRD370KQ",	"DRD370NQ",	"DRD370RQ",	"DRD370SQ")
cont_phe <- merge_data(cont_phe, colfilter(cat_phe, cols = cat_to_cont_phe, exclude = FALSE))
cat_phe <- colfilter(cat_phe, cols = cat_to_cont_phe, exclude = TRUE)

# All "check" columns are continuous
print(paste("Checked: ", paste(names(check_phe), collapse=", "), sep=" "))
cont_phe <- merge_data(cont_phe, check_phe, union = TRUE)

# Get covariate types
covariates_cat_phe = intersect(names(cat_phe), covariates)
covariates_bin_phe = intersect(names(bin_phe), covariates)
covariates_catbin_phe <- union(covariates_cat_phe, covariates_bin_phe)
covariates_cont_phe = intersect(names(cont_phe), covariates)

#Adjust for statins (LDL=LDL/0.7, TC=TC/0.8) (2)
statins <- c("ATORVASTATIN_CALCIUM", "SIMVASTATIN", "PRAVASTATIN_SODIUM", "FLUVASTATIN_SODIUM")
phestatin <- colfilter(bin_phe, cols = statins)
phestatin$total <- rowSums(phestatin[2:5], na.rm = TRUE)
#1059 out of 41474 on statin

#Extract TC and LDL from continuous and merge with statin
ldl_statin_df <- merge(phestatin[, c(1,6)], colfilter(cont_phe, cols="LBDLDL", exclude=FALSE), by="ID", all.y=TRUE)
ldl_statin_df$LBDLDL <- ifelse(ldl_statin_df$total > 0, ldl_statin_df$LBDLDL/0.7, ldl_statin_df$LBDLDL)
#Come back to this step later since we do not know if we log transforming yet

#Merge back with df 
ldl_df_adj <- merge(ldl_statin_df[, c(1,3)], colfilter(cont_phe, cols=c(statins, "LBDLDL"), exclude=TRUE), by="ID")

#Remove statins from categorical
df_ldl_cat_no_statin <- colfilter(cat_phe, cols=c(statins, "T2D", "CAD"), exclude=TRUE)

#Merge these data frames so you can split up peds and adults
cat_cont_phe <- merge_data(ldl_df_adj, df_ldl_cat_no_statin, union=TRUE)
bin_cat_cont_phe <- merge_data(bin_phe, cat_cont_phe, union=TRUE)
# 487 variables, 41474 obs

#Split by age (3)
##List of variables to keep
keepvar <- read.delim("/home/tomas/Documents/RepPheEWAS/Data/VarDescription_keepcategories_varnames.txt", header=FALSE)
keepvar <- keepvar[keepvar$V1!="male" & keepvar$V1!="white", , drop=FALSE]

##Filter variables
nhanes_cols <- colfilter(bin_cat_cont_phe, cols=keepvar)
##920
sy <- bin_cat_cont_phe[, colnames(bin_cat_cont_phe) %in% c("ID", "SDDSRVYR", "LBXSCRINV", "URXUCR", "LBXSCR", "LBXSATSI", "LBXSAL", "URXUMASI", "URXUMA", "LBXSAPSI", "LBXSASSI", "LBXSC3SI", "LBXSBU", "LBXBAP", "LBXCPSI", "LBXCRP", "LBXSCLSI", "LBXFER", "LBXSGTSI", "LBXSGB", "LBXGLU", "LBXGH", "LBXHCY", "LBXSIR", "LBXSLDSI", "LBXMMA", "LBXSOSSI", "LBXSPH", "LBXGLUSI", "LBXEPP", "LBXSNASI", "LBXTIB", "LBXSTB", "LBXSCA", "LBXSTP", "LBDPCT", "LBXSUA", "LBDBANO", "LBXBAPCT", "LBDEONO", "LBXEOPCT", "LBXHCT", "LBXHGB", "LBDLYMNO", "LBXMCHSI", "LBXLYPCT", "LBXMCVSI", "LBXMPSI", "LBDMONO", "LBXMOPCT", "LBXPLTSI", "LBXRBCSI", "LBXRDW", "LBDNENO", "LBXNEPCT")]
nhanes_cols <- merge_data(sy, nhanes_cols, union=FALSE)
#981
nhanes_cols18 <- nhanes_cols[nhanes_cols$RIDAGEYR>=18, ]
##22624
nhanes_colslt18 <- nhanes_cols[nhanes_cols$RIDAGEYR<18, ]
##18850

#Split by series (4)
phe_discovery_series <- nhanes_cols18[nhanes_cols18$SDDSRVYR==1|nhanes_cols18$SDDSRVYR==2,]
phe_replication_series <- nhanes_cols18[nhanes_cols18$SDDSRVYR==3|nhanes_cols18$SDDSRVYR==4,]

#Only take complete cases (5)
phe_discovery_filter <- phe_discovery_series[!is.na('phenotype'), ]
phe_discovery_complete <- remove_incomplete_obs(phe_discovery_filter, c(covariates))
phe_replication_filter <- phe_replication_series[!is.na('phenotype_rep'), ]
phe_replication_complete <- remove_incomplete_obs(phe_replication_filter, c(covariates))

#Separate phenotypes and expsosure data to better follow the QC process with each type of variable (6)
phenotype_discovery_variables <- colfilter(d=phe_discovery_complete, cols=phenotype, exclude=FALSE)
exposure_discovery_variables <- colfilter(d=phe_discovery_complete, cols=phenotype, exclude=TRUE)
phenotype_replication_variables <- colfilter(d=phe_replication_complete, cols=phenotype_rep, exclude=FALSE)
exposure_replication_variables <- colfilter(d=phe_replication_complete, cols=phenotype_rep, exclude=TRUE)

####DISCOVERY AND REPLICATION ANALYSIS OF PHENOTYPES####
# Calculate percent zero (7)
get0 <- function(x){
  x <- as.numeric(as.character(x))
  N <- length(!is.na(x))
  NNZ <- length(x[x>0])
  PZ <- (N-NNZ)/N
  out <- rbind(N=N, NNZ=NNZ, PZ=PZ)
  return(out)
}

# Discovery Percent Zero
zeros_disc <- as.data.frame(t(sapply(phenotype_discovery_variables[,-1], get0)))
zeros_disc <- cbind(rownames(zeros_disc), data.frame(zeros_disc, row.names = NULL))
names(zeros_disc) <- c("var","N","NNZ","PZ")
# Drop those with percent zero > 90%
before <- length(phenotype_discovery_variables) -1 # minus 1 for ID
zeros_disc_filtered <- zeros_disc[zeros_disc$PZ >= 0.9, 1, drop=FALSE]
discovery_cont_zero_phe <- colfilter(phenotype_discovery_variables, cols=zeros_disc_filtered, exclude=TRUE)
# Log
sprintf("Removed %i of %i continuous variables due to the percent zero filter",
        length(zeros_disc_filtered), before)

# Replication Percent Zero
zeros_repl <- as.data.frame(t(sapply(phenotype_replication_variables[,-1], get0)))
zeros_repl <- cbind(rownames(zeros_repl), data.frame(zeros_repl, row.names = NULL))
names(zeros_repl) <- c("var","N","NNZ","PZ")
# Drop those with percent zero > 90%
before <- length(phenotype_replication_variables) -1 # minus 1 for ID
zeros_repl_filtered <- zeros_repl[zeros_repl$PZ >= 0.9, 1, drop=FALSE]
replication_cont_zero_phe <- colfilter(phenotype_replication_variables, cols=zeros_repl_filtered, exclude=TRUE)
# Log
sprintf("Removed %i of %i continuous variables due to the percent zero filter",
        length(zeros_repl_filtered), before)

#Get skewness of phenotypes (made histograms and saved them to ACI-ICS)
# write.table(discovery_cont_zero_phe, "/storage/home/n/nep63/group_mah546/personal/nikki/PheWAS_NHANES/phe_disocvery_zero_phe.txt",  sep="\t", quote=FALSE, row.names=FALSE)
# write.table(replication_cont_zero_phe, "/storage/home/n/nep63/group_mah546/personal/nikki/PheWAS_NHANES/phe_replication_zero_phe.txt",  sep="\t", quote=FALSE, row.names=FALSE)
#Read those previous and compare with the ones made here
rep_disc_zero_phe <- read.csv("/home/tomas/Documents/RepPheEWAS/Results/phe_disocvery_zero_phe.txt", sep="\t")

skewness(discovery_cont_zero_phe, na.rm=TRUE)
skewness(replication_cont_zero_phe, na.rm=TRUE)

#Reflect any negative skewness values that are less that -0.5 or greater than 0.5. 
reflect_phenotypes <- c("LBXMCVSI", "LBXMCHSI")
reflect_phe <- colfilter(discovery_cont_zero_phe, reflect_phenotypes, exclude=FALSE)
reflect_phenotypes_rep <- c("LBXMCVSI", "LBXMCHSI", "LBXSAL", "LBXSCLSI")
reflect_phe_rep <- colfilter(replication_cont_zero_phe, reflect_phenotypes_rep, exclude=FALSE)

reflect_phe_max <- max(reflect_phe, na.rm =TRUE)+1
reflect_phe_max_data <- reflect_phe_max-reflect_phe[2:3]
reflect_phe_rep_max <- max(reflect_phe_rep, na.rm =TRUE)+1
reflect_phe_max_rep_data <- reflect_phe_rep_max-reflect_phe_rep[2:5]
ID_name <- c("ID")
ID_dis <- colfilter(discovery_cont_zero_phe, ID_name, exclude = F)
ID_reflect_dis <-cbind(ID_dis, reflect_phe_max_data)
ID_rep <- colfilter(replication_cont_zero_phe, ID_name, exclude = F)
ID_reflect_rep <-cbind(ID_rep, reflect_phe_max_rep_data)

#Merge reflected phenotypes back with non-transformed data
no_reflect_dis <- colfilter(discovery_cont_zero_phe, reflect_phenotypes, exclude=TRUE)
no_reflect_rep <- colfilter(replication_cont_zero_phe, reflect_phenotypes_rep, exclude=TRUE)
reflect_log_dis <- merge_data(no_reflect_dis, ID_reflect_dis, union=TRUE)
reflect_log_rep <- merge_data(no_reflect_rep, ID_reflect_rep, union=TRUE)

phe_dis_not_transformed <- hist_plot(discovery_cont_zero_phe, n = 12, 
                                     file = "/storage/home/tug156/work/RepPheEWAS/Results/MyPlots/phe_dis_not_transformed", 
                                     nrow = 4, ncol = 3, wi = 13.5, hgt = 12, res = 300)
phe_rep_not_transformed <- hist_plot(replication_cont_zero_phe, n = 12, 
                                     file = "/storage/home/tug156/work/RepPheEWAS/Results/MyPlots/phe_rep_not_transformed", 
                                     nrow = 4, ncol = 3, wi = 13.5, hgt = 12, res = 300)

#Remove variables that are not being transformed
dis_no_transform <- c("LBXLYPCT",	"LBXNEPCT",	"LBXHGB",	"LBXHCT",	"LBXSAL", "LBXSCA",	"LBXSPH",	"LBXSTP",	"LBXSUA",	"LBXSCLSI",	"LBXSOSSI", "LBXSC3SI", "LBXSNASI")
res_no_transform <- c("LBXLYPCT",	"LBXNEPCT",	"LBXRBCSI",	"LBXHGB",	"LBXHCT",	"LBXSCA",	"LBXSPH",	"LBXSUA",	"LBXSOSSI",	"LBXSC3SI")

dis_transform <- colfilter(reflect_log_dis, dis_no_transform, exclude=TRUE)
res_transform <- colfilter(reflect_log_rep, res_no_transform, exclude = TRUE)

#Log transform variables and check skewness again
dis_log <- log(dis_transform)
rep_log <- log(res_transform)

skewness(dis_log, na.rm=TRUE)
skewness(rep_log, na.rm=TRUE)

log_one_dis_vars <- c("LBXEOPCT",	"LBXBAPCT",	"LBDEONO",	"LBXEPP",	"LBDBANO")
log_one_rep_vars <- c("LBXEOPCT", "LBXBAPCT",	"LBDEONO",	"LBDMONO",	"LBDBANO")

dis_one_transform <- colfilter(discovery_cont_zero_phe, log_one_dis_vars, exclude=FALSE)
res_one_transform <- colfilter(replication_cont_zero_phe, log_one_rep_vars, exclude = FALSE)

one_log_dis <- log(dis_one_transform[2:6]+1)
one_log_rep <- log(res_one_transform[2:6]+1)

#Put all transofrmed and non-transformed collumns back into one data frame
dis_remain <- colfilter(discovery_cont_zero_phe, dis_no_transform, exclude=FALSE)
res_remain <- colfilter(replication_cont_zero_phe, res_no_transform, exclude = FALSE)

log_dis_data <- cbind(dis_remain, one_log_dis)
log_rep_data <- cbind(res_remain, one_log_rep)

dis_no_log <- colfilter(dis_log, log_one_dis_vars, exclude=TRUE)
res_no_log <- colfilter(rep_log, log_one_rep_vars, exclude = TRUE)

dis_no_log <- dis_no_log[,-1]
res_no_log <- res_no_log[,-1]

all_dis_data <- cbind(log_dis_data, dis_no_log)
all_rep_data <- cbind(log_rep_data, res_no_log)

phe_dis_all <- hist_plot(all_dis_data, n = 12, file = "/storage/home/tug156/work/RepPheEWAS/Results/MyPlots/phe_dis_all", nrow = 4, ncol = 3, wi = 13.5, hgt = 12, res = 300)
phe_rep_all <- hist_plot(all_rep_data, n = 12, file = "/storage/home/tug156/work/RepPheEWAS/Results/MyPlots/phe_rep_all", nrow = 4, ncol = 3, wi = 13.5, hgt = 12, res = 300)

#Remove NA from phenotype rows
all_dis_data <- all_dis_data[complete.cases(all_dis_data), ]
all_rep_data <- all_rep_data[complete.cases(all_rep_data), ]

#Overall Sample Size Filter
phe_phenotype_min_dis <- min_n(all_dis_data)
phe_phenotype_min_rep <- min_n(all_rep_data)




####DISCOVERY AND REPLICATION ANALYSIS OF EXPOSURES####
###You are repeating from​ the spilt by variable step here for the exposures​###

# Split exposure variables by type (discovery) 
bin_phe_exp_dis <- get_binary(exposure_discovery_variables)
cat_phe_exp_dis <- get_categorical(exposure_discovery_variables)
cont_phe_exp_dis <- get_continuous(exposure_discovery_variables)
check_phe_exp_dis <- get_check(exposure_discovery_variables)

# Some columns need to be moved from categorical to continuous
discovery_cat_to_cont_phe_dis <- c("L_GLUTAMINE_gm")
discovery_cont_phe_exp <- merge_data(cont_phe_exp_dis, colfilter(cat_phe_exp_dis, cols = discovery_cat_to_cont_phe_dis, exclude = FALSE))
cat_phe_exp_dis <- colfilter(cat_phe_exp_dis, cols = discovery_cat_to_cont_phe_dis, exclude = TRUE)

# All "check" columns are continuous
print(paste("Checked: ", paste(names(check_phe_exp_dis), collapse=", "), sep=" "))
cont_phe_exp_dis <- merge_data(cont_phe_exp_dis, check_phe_exp_dis, union = TRUE)

# Get covariate types
discovery_covariates_cat_exp = intersect(names(cat_phe_exp_dis), covariates)
discovery_covariates_bin_exp = intersect(names(bin_phe_exp_dis), covariates)
discovery_covariates_catbin_exp <- union(discovery_covariates_cat_exp, discovery_covariates_bin_exp)
discovery_covariates_cont_exp = intersect(names(cont_phe_exp_dis), covariates)

#Replication
bin_phe_exp_rep <- get_binary(exposure_replication_variables)
cat_phe_exp_rep <- get_categorical(exposure_replication_variables)
cont_phe_exp_rep <- get_continuous(exposure_replication_variables)
check_phe_exp_rep <- get_check(exposure_replication_variables)

# Some columns need to be moved from categorical to continuous
replication_cat_to_cont_exp <- c("L_GLUTAMINE_gm")
cont_phe_exp_rep <- merge_data(cont_phe_exp_rep, colfilter(cat_phe_exp_rep, cols = replication_cat_to_cont_exp, exclude = FALSE))
cat_phe_exp_rep <- colfilter(cat_phe_exp_rep, cols = replication_cat_to_cont_exp, exclude = TRUE)

# All "check" columns are continuous
print(paste("Checked: ", paste(names(check_phe_exp_rep), collapse=", "), sep=" "))
cont_phe_exp_rep <- merge_data(cont_phe_exp_rep, check_phe_exp_rep, union = TRUE)

############################################
# Take note of differently-typed variables #
############################################
print(paste("Bin > Cat:", paste(intersect(names(bin_phe_exp_dis), names(cat_phe_exp_rep)), collapse=", ")))
print(paste("Bin > Cont:", paste(intersect(names(bin_phe_exp_dis), names(cont_phe_exp_rep)), collapse=", ")))
print(paste("Cat > Bin:", paste(intersect(names(cat_phe_exp_dis), names(bin_phe_exp_rep)), collapse=", ")))
print(paste("Cat > Cont:", paste(intersect(names(cat_phe_exp_dis), names(cont_phe_exp_rep)), collapse=", ")))
print(paste("Cont > Bin:", paste(intersect(names(cont_phe_exp_dis), names(bin_phe_exp_rep)), collapse=", ")))
print(paste("Cont > Cat:", paste(intersect(names(cont_phe_exp_dis), names(cat_phe_exp_rep)), collapse=", ")))

#Fix based on above findings 
# Binary in discovery that should be categorical
move_cols <- c("BETA_CAROTENE_mcg", "DRD350JQ", "DRD370PQ", "CALCIUM_Unknown", "MAGNESIUM_Unknown")
cat_phe_exp_dis <- merge_data(cat_phe_exp_dis, colfilter(bin_phe_exp_dis, cols = move_cols, exclude = FALSE))
bin_phe_exp_dis <- colfilter(bin_phe_exp_dis, cols = move_cols, exclude = TRUE)

#Binary in replication that should be categorical
move_cols <- c("VITAMIN_B_12_Unknown")
cat_phe_exp_rep <- merge_data(cat_phe_exp_rep, colfilter(bin_phe_exp_rep, cols = move_cols, exclude = FALSE))
bin_phe_exp_rep <- colfilter(bin_phe_exp_rep, cols = move_cols, exclude = TRUE)

#Categorical in discovery that should be continuous
move_cols <- c("RHQ598", "how_long_estrogen_progestin_patch", "DRD350AQ", "DRD350CQ", "DRD350DQ", "DRD350EQ", "DRD350GQ", "DRD370CQ", "DRD370GQ", "DRD370HQ", "DRD370RQ", "DRD370SQ")
cont_phe_exp_dis <- merge_data(cont_phe_exp_dis, colfilter(cat_phe_exp_dis, cols = move_cols, exclude = FALSE))
cat_phe_exp_dis <- colfilter(cat_phe_exp_dis, cols = move_cols, exclude = TRUE)

#Categorical in replication that should be continuous
move_cols <- c("LBDSY3")
cont_phe_exp_rep <- merge_data(cont_phe_exp_rep, colfilter(cat_phe_exp_rep, cols = move_cols, exclude = FALSE))
cat_phe_exp_rep <- colfilter(cat_phe_exp_rep, cols = move_cols, exclude = TRUE)


#Get Covariate Types #
#######################

# Discovery
discovery_covariates_cat_exp = intersect(names(cat_phe_exp_dis), covariates)
discovery_covariates_bin_exp = intersect(names(bin_phe_exp_dis), covariates)
discovery_covariates_catbin_exp <- union(discovery_covariates_cat_exp, discovery_covariates_bin_exp)
discovery_covariates_cont_exp = intersect(names(cont_phe_exp_dis), covariates)

# Replication
replication_covariates_cat_exp = intersect(names(cat_phe_exp_rep), covariates)
replication_covariates_bin_exp = intersect(names(bin_phe_exp_rep), covariates)
replication_covariates_catbin_exp <- union(replication_covariates_cat_exp, replication_covariates_bin_exp)
replication_covariates_cont_exp = intersect(names(cont_phe_exp_rep), covariates)

# Filtering Variables #
#######################

# Require a min of 200 observations after splitting
discovery_bin_min_exp <- min_n(bin_phe_exp_dis, skip=discovery_covariates_bin_exp)
discovery_cat_min_exp <- min_n(cat_phe_exp_dis, skip=discovery_covariates_cat_exp)
discovery_cont_min_exp <- min_n(cont_phe_exp_dis, skip=c(discovery_covariates_cont_exp))

replication_bin_min_exp <- min_n(bin_phe_exp_rep, skip=replication_covariates_bin_exp)
replication_cat_min_exp <- min_n(cat_phe_exp_rep, skip=replication_covariates_cat_exp)
replication_cont_min_exp <- min_n(cont_phe_exp_rep, skip=c(replication_covariates_cont_exp))

# Apply the min_cat filter to exclude categorical/binary with low samples of a category
discovery_bin_finish_exp <- min_cat_n(discovery_bin_min_exp, skip=discovery_covariates_bin_exp)
discovery_cat_finish_exp <- min_cat_n(discovery_cat_min_exp, skip=discovery_covariates_cat_exp)

replication_bin_finish_exp <- min_cat_n(replication_bin_min_exp, skip=replication_covariates_bin_exp)
replication_cat_finish_exp <- min_cat_n(replication_cat_min_exp, skip=replication_covariates_cat_exp)

# Calculate percent zero
get0 <- function(x){
  x <- as.numeric(as.character(x))
  N <- length(!is.na(x))
  NNZ <- length(x[x>0])
  PZ <- (N-NNZ)/N
  out <- rbind(N=N, NNZ=NNZ, PZ=PZ)
  return(out)
}

# Discovery Percent Zero
zeros_disc <- as.data.frame(t(sapply(discovery_cont_min_exp[,-1], get0)))
zeros_disc <- cbind(rownames(zeros_disc), data.frame(zeros_disc, row.names = NULL))
names(zeros_disc) <- c("var","N","NNZ","PZ")
# Drop those with percent zero > 90%
before <- length(discovery_cont_min_exp) -1 # minus 1 for ID
zeros_disc_filtered <- zeros_disc[zeros_disc$PZ >= 0.9, 1, drop=FALSE]
discovery_cont_zero_exp <- colfilter(discovery_cont_min_exp, cols=zeros_disc_filtered, exclude=TRUE)
# Log
sprintf("Removed %i of %i continuous variables due to the percent zero filter",
        length(zeros_disc_filtered), before)

# Replication Percent Zero
zeros_repl <- as.data.frame(t(sapply(replication_cont_min_exp[,-1], get0)))
zeros_repl <- cbind(rownames(zeros_repl), data.frame(zeros_repl, row.names = NULL))
names(zeros_repl) <- c("var","N","NNZ","PZ")
# Drop those with percent zero > 90%
before <- length(replication_cont_min_exp) -1 # minus 1 for ID
zeros_repl_filtered <- zeros_repl[zeros_repl$PZ >= 0.9, 1, drop=FALSE]
replication_cont_zero_exp <- colfilter(replication_cont_min_exp, cols=zeros_repl_filtered, exclude=TRUE)
# Log
sprintf("Removed %i of %i continuous variables due to the percent zero filter",
        length(zeros_repl_filtered), before)
# Only keep those variable present in both #
############################################

common_bin_exp <- intersect(names(discovery_bin_finish_exp), names(replication_bin_finish_exp))
discovery_bin_final_exp <- colfilter(discovery_bin_finish_exp, cols=common_bin_exp, exclude=FALSE)
replication_bin_final_exp <- colfilter(replication_bin_finish_exp, cols=common_bin_exp, exclude=FALSE)
sprintf("%i Binary Variables in Common", length(common_bin_exp))

common_cat_exp <- intersect(names(discovery_cat_finish_exp), names(replication_cat_finish_exp))
discovery_cat_final_exp <- colfilter(discovery_cat_finish_exp, cols=common_cat_exp, exclude=FALSE)
replication_cat_final_exp <- colfilter(replication_cat_finish_exp, cols=common_cat_exp, exclude=FALSE)
sprintf("%i Categorical Variables in Common", length(common_cat_exp))

common_cont_exp <- intersect(names(discovery_cont_zero_exp), names(replication_cont_zero_exp))
discovery_cont_final_exp <- colfilter(discovery_cont_zero_exp, cols=common_cont_exp, exclude=FALSE)
replication_cont_final_exp <- colfilter(replication_cont_zero_exp, cols=common_cont_exp, exclude=FALSE)
sprintf("%i Continuous Variables in Common", length(common_cont_exp))

# Merge Categorical and Binary #
################################

discovery_catbin_final_exp <- merge_data(discovery_cat_final_exp, discovery_bin_final_exp, union = TRUE)
replication_catbin_final_exp <- merge_data(replication_cat_final_exp, replication_bin_final_exp, union = TRUE)
#Merge phenotypes back with the continuous exposure variables
discovery_cont_phenotype_exp <- merge_data(phe_phenotype_min_dis, discovery_cont_final_exp, union = TRUE)
replication_cont_phenotype_exp <- merge_data(phe_phenotype_min_rep, replication_cont_final_exp, union = TRUE)

#Remove variables that do not have 4 year weights 
remove_variables <- c("LBXTC2", "LBXTR", "LBXPFOA", "LBXPFOS",	"LBXPFHS",	"LBXEPAH",	"LBXMPAH",	"LBXPFDE",	"LBXPFHP",	"LBXPFNA",	"LBXPFSA",	"LBXPFUA",	"URX14D",	"URX1TB",	"URX3TB",	"URXPCP",	"URXOPP",	"URXUIO",	"URXP17",	"URXP19",	"DR1TMOIS",	"LBXPFDO", "DRD370UQ")
discovery_cont_phenotype_exp <- colfilter(discovery_cont_phenotype_exp, remove_variables, exclude=TRUE)
replication_cont_phenotype_exp<- colfilter(replication_cont_phenotype_exp, remove_variables, exclude=TRUE)
# Get typed variables and then merge data #
###########################################
all_dis_data_list <- colnames(all_dis_data [2:57])
all_rep_data_list <- colnames(all_rep_data [2:57])
discovery_catbin_vars_exp = setdiff(names(discovery_catbin_final_exp),
                                    c(discovery_covariates_catbin_exp, all_dis_data_list, "ID", names(survey_design_discovery), names(survey_design_replication)))
discovery_cont_vars_exp = setdiff(names(discovery_cont_phenotype_exp),
                                  c(discovery_covariates_cont_exp, all_dis_data_list, "ID", names(survey_design_discovery), names(survey_design_replication)))


discovery_final_exp_phe <- merge_data(discovery_catbin_final_exp, discovery_cont_phenotype_exp, union = FALSE)
replication_final_exp_phe <- merge_data(replication_catbin_final_exp, replication_cont_phenotype_exp, union = FALSE)

discovery_final_exp_phe <- merge_data(discovery_final_exp_phe, survey_design_discovery, union=FALSE)
replication_final_exp_phe <- merge_data(replication_final_exp_phe, survey_design_replication, union=FALSE)

# Discovery Phe-EWAS - Weighted #Repeat 58 times with input of 
#############################

discovery_results_phe <- lapply(seq(from=1,to=length(all_dis_data_list)), function(i){
  # Get the phenotype value
  y <- all_dis_data_list[[i]]
  print(y)
  # Make sure it isn't included with the cont vars
  filtered_cont_vars <- discovery_cont_vars_exp[discovery_cont_vars_exp != y]
  # Run ewas using these two parameters
  ewas(d = discovery_final_exp_phe,
       cat_vars=discovery_catbin_vars_exp,
       cont_vars=filtered_cont_vars,
       y=y,
       cat_covars=discovery_covariates_catbin_exp,
       cont_covars=discovery_covariates_cont_exp,
       regression_family="gaussian",
       allowed_nonvarying= c('female', 'SDDSRVYR'),
       weights=weights_discovery,
       ids="SDMVPSU",
       strat="SDMVSTRA",
       nest=TRUE)
})
discovery_results_phe_ewas <- do.call(rbind,discovery_results_phe)
discovery_results_phe_ewas <- ewas_pval_adjust(discovery_results_phe_ewas)
discovery_results_phe_ewas <- discovery_results_phe_ewas[!(is.na(discovery_results_phe_ewas$Converged) | discovery_results_phe_ewas$Converged==""), ]

significant_discovery_vars_all <- discovery_results_phe_ewas[discovery_results_phe_ewas$pvalue_FDR < 0.1,]

# Save results
write.table(discovery_results_phe_ewas,
            file = paste("/storage/home/tug156/work/RepPheEWAS/Results/dis_all_results.txt", sep="/"),
            sep = "\t", quote = FALSE, row.names = FALSE)
write.table(significant_discovery_vars_all,
            file = paste("/storage/home/tug156/work/RepPheEWAS/Results/sig_all_results.txt", sep="/"),
            sep = "\t", quote = FALSE, row.names = FALSE)

# Keep significant variables for replication #
##############################################
sig_columns <- c("Variable", "phenotype", "LRT_pvalue", "pvalue_FDR")
sig_columns_get <- discovery_results_phe_ewas[sig_columns]
cat_rep_phe <- sig_columns_get[!is.na(sig_columns_get$LRT_pvalue), ]
cat_rep_FDR <- subset(cat_rep_phe, pvalue_FDR < 0.1, select=c("Variable", "phenotype"))

sig_columns_cont <- c("Variable", "phenotype", "Variable_pvalue", "pvalue_FDR")
sig_columns_get2 <- discovery_results_phe_ewas[sig_columns_cont]
cont_rep_phe <- sig_columns_get2[!is.na(sig_columns_get2$Variable_pvalue), ]
cont_rep_FDR <- subset(cont_rep_phe, pvalue_FDR < 0.1, select=c("Variable", "phenotype"))


# Replication EWAS - Weighted #
###############################
replication_results_phe_cat <- lapply(seq(from=1,to=length(cat_rep_FDR$phenotype)), function(i){
  # Get the phenotype value
  y <- cat_rep_FDR$phenotype[[i]]
  print(y)
  # Run ewas using these two parameters
  ewas(d =replication_final_exp_phe,
       cat_vars=cat_rep_FDR$Variable[[i]],
       cont_vars=NULL,
       y=y,
       cat_covars=replication_covariates_catbin_exp,
       cont_covars=replication_covariates_cont_exp,
       regression_family="gaussian",
       allowed_nonvarying= c('female', 'SDDSRVYR'),
       weights=weights_replication,
       id="SDMVPSU",
       strat="SDMVSTRA",
       nest=TRUE)
})

replication_results_phe_ewas_cat <- do.call(rbind,replication_results_phe_cat)

# Replication EWAS - Weighted #
###############################
replication_results_phe_cont <- lapply(seq(from=1,to=length(cont_rep_FDR$phenotype)), function(i){
  # Get the phenotype value
  y <- cont_rep_FDR$phenotype[[i]]
  print(y)
  # Run ewas using these two parameters
  ewas(d =replication_final_exp_phe,
       cat_vars=NULL,
       cont_vars=cont_rep_FDR$Variable[[i]],
       y=y,
       cat_covars=replication_covariates_catbin_exp,
       cont_covars=replication_covariates_cont_exp,
       regression_family="gaussian",
       allowed_nonvarying= c('female', 'SDDSRVYR'),
       weights=weights_replication,
       id="SDMVPSU",
       strat="SDMVSTRA",
       nest=TRUE)
})

replication_results_phe_ewas_cont <- do.call(rbind,replication_results_phe_cont)
replication_cat_cont_ewas <- rbind(replication_results_phe_ewas_cat, replication_results_phe_ewas_cont)
replication_results_phe_ewas <- ewas_pval_adjust(replication_cat_cont_ewas)
replication_results_phe_ewas <- replication_results_phe_ewas[!(is.na(replication_results_phe_ewas$Converged) | replication_results_phe_ewas$Converged==""), ]

#Save results
write.table(replication_results_phe_ewas,
            file = paste("/storage/home/n/nep63/group_mah546/personal/nikki/PheWAS_NHANES/New_PheEWAS/rep_results_all.txt", sep="/"),
            sep = "\t", quote = FALSE, row.names = FALSE)
#write.table(mpdata,
#            file = paste("/storage/home/n/nep63/group_mah546/personal/nikki/PheWAS_NHANES/New_PheEWAS/mpdata.txt", sep="/"),
#            sep = "\t", quote = FALSE, row.names = FALSE)
write.table(phenotype_discovery_variables,
            file = paste("/storage/home/n/nep63/group_mah546/personal/nikki/PheWAS_NHANES/New_PheEWAS/phenotype_vars.txt", sep="/"),
            sep = "\t", quote = FALSE, row.names = FALSE)

# Save combined report #
########################
results <- merge(discovery_results_phe_ewas, replication_results_phe_ewas, by=c("Variable", "phenotype"), suffixes = c("_Dis", "_Rep"))
results <- results[order(results$pvalue_FDR_Rep),order(names(results))]

not_NA <- !is.na(results$pval_Dis) & !is.na(results$pval_Rep)

sig_FDR_results <- not_NA & (results$pvalue_FDR_Dis < 0.1) & (results$pvalue_FDR_Rep < 0.1) 
write.table(results[sig_FDR_results, c("Variable", "phenotype", "pvalue_FDR_Dis", "pvalue_FDR_Rep")],
            file = paste("/storage/home/n/nep63/group_mah546/personal/nikki/PheWAS_NHANES/New_PheEWAS/results_phe_FDR_0.1.txt", sep="/"),
            sep = "\t", quote = FALSE, row.names = FALSE)

sig_Bonf_results <- not_NA & (results$pvalue_Bonf_Dis < 0.05) & (results$pvalue_Bonf_Rep < 0.05)
write.table(results[sig_Bonf_results, c("Variable", "phenotype", "pvalue_Bonf_Dis", "pvalue_Bonf_Rep")],
            file = paste("/storage/home/n/nep63/group_mah546/personal/nikki/PheWAS_NHANES/New_PheEWAS/results_phe_Bonf_0.05.txt", sep="/"),
            sep = "\t", quote = FALSE, row.names = FALSE)

sig_Bonf_results2 <- not_NA & (results$pvalue_Bonf_Dis < 0.01) & (results$pvalue_Bonf_Rep < 0.01)
write.table(results[sig_Bonf_results2, c("Variable", "phenotype", "pvalue_Bonf_Dis", "pvalue_Bonf_Rep")],
            file = paste("/storage/home/n/nep63/group_mah546/personal/nikki/PheWAS_NHANES/New_PheEWAS/results_phe_Bonf_0.01.txt", sep="/"),
            sep = "\t", quote = FALSE, row.names = FALSE)

# Record significant variables for testing with possible confounding variables
significant_in_both <- results[sig_FDR_results, "Variable"]

###################
# Manhattan Plots #
###################
# Keep only p-values for each variable
mpdata_discovery_phe <- discovery_results_phe_ewas[c("Variable", "pvalue_FDR")]
mpdata_replication_phe <- replication_results_phe_ewas[c("Variable", "pvalue_FDR")]

# Rename the pvalue columns
names(mpdata_discovery_phe)[2] <- "pvalue"
names(mpdata_replication_phe)[2] <- "pvalue"

# Add "Shape"
mpdata_discovery_phe$Shape <- "Discovery"
mpdata_replication_phe$Shape <- "Replication"

# Merge
mpdata <- rbind(mpdata_discovery_phe, mpdata_replication_phe)

# Load phenotype info
var_phenotypes_dis <- read.delim(data_var_phenotypes_dis)

# Plot
eman(mpdata_discovery_phe, ewas = FALSE,
     groups = var_phenotypes_dis,
     line = NULL,
     title = "PheEWAS Results: Discovery",
     morecolors = FALSE,
     file = paste("/storage/home/n/nep63/group_mah546/personal/nikki/PheWAS_NHANES/New_PheEWAS/PheEWAS_man_discovery", sep="/"),
     hgt = 7, wi = 12, res = 300)
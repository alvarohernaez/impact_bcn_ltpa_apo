rm(list=ls())

library(compareGroups)
library(dplyr)
library(gam)
library(ggplot2)
library(Hmisc)
library(lmtest)
library(miceadds)
library(paran)
library(splines)
library(readr)
library(readxl)

# OPEN PRE-GENERATED FUNCTIONS

RutinesLocals<- "D:/R/Rutinas"
source(file.path(RutinesLocals,"carrega.llibreria.r"))
source(file.path(RutinesLocals,"merge2.r"))
source(file.path(RutinesLocals,"fix2.r"))
source(file.path(RutinesLocals,"table2.r"))
source(file.path(RutinesLocals,"subset2.r"))
source(file.path(RutinesLocals,"format2.r"))
source(file.path(RutinesLocals,"order2.r"))
source(file.path(RutinesLocals,"intervals.r"))

# OPEN MY OWN FUNCTIONS TO PUT RESULTS IN NICE FORMAT ("guapa")

guapa<-function(x)
{
  redondeo<-ifelse(abs(x)<0.00001,signif(x,1),
                   ifelse(abs(x)<0.0001,signif(x,1),
                          ifelse(abs(x)<0.001,signif(x,1),
                                 ifelse(abs(x)<0.1,sprintf("%.3f",round(x,3)),
                                        ifelse(abs(x)<1,sprintf("%.2f",round(x,2)),
                                               ifelse(abs(x)<10,sprintf("%.2f",round(x,2)),
                                                      ifelse(abs(x)<100,sprintf("%.1f",round(x,1)),
                                                             ifelse(abs(x)>=100,round(x,0),round(x,0)))))))))
  return(redondeo)
}

ic_guapa<-function(x,y,z)
{
  ic<-paste(x," [",y,"; ",z,"]",sep="")
  return(ic)
}

ic_guapa2<-function(x,y,z)
{
  ic<-paste(x," (",y," to ",z,")",sep="")
  return(ic)
}

pval_guapa<-function(x)
{
  pval<-ifelse(x<0.00001,"<0.00001",
               ifelse(x<0.001,"<0.001",
                      ifelse(abs(x)<0.01,sprintf("%.3f",round(x,3)),
                             ifelse(abs(x)<0.1,sprintf("%.3f",round(x,3)),
                                    ifelse(abs(x)<1,sprintf("%.3f",round(x,3)),guapa(x))))))
  return(pval)
}

pval_guapa2<-function(x)
{
  pval<-ifelse(x<0.00001," < 0.00001",
               ifelse(x<0.001," < 0.001",
                      ifelse(abs(x)<0.01,paste(" = ",sprintf("%.3f",round(x,3)),sep=""),
                             ifelse(abs(x)<0.1,paste(" = ",sprintf("%.3f",round(x,3)),sep=""),
                                    ifelse(abs(x)<1,paste(" = ",sprintf("%.3f",round(x,3)),sep=""),guapa(x))))))
  return(pval)
}

mean_ic_guapa <- function(x, na.rm=FALSE) 
{
  if (na.rm) x <- na.omit(x)
  se<-sqrt(var(x)/length(x))
  z<-qnorm(1-0.05/2)
  media<-mean(x)
  ic95a<-guapa(media-(z*se))
  ic95b<-guapa(media+(z*se))
  media<-guapa(media)
  ic_ok<-ic_guapa(media,ic95a,ic95b)
  return(ic_ok)
}

mean_sd_guapa <- function(x) 
{
  media<-guapa(mean(x, na.rm=TRUE))
  sd<-guapa(sd(x, na.rm=TRUE))
  end<-paste(media," (",sd,")",sep="")
  return(end)
}

beta_se_ic_guapa <- function(x, y) 
{
  z<-qnorm(1-0.05/2)
  ic95a<-guapa(x-(z*y))
  ic95b<-guapa(x+(z*y))
  media<-guapa(x)
  ic_ok<-ic_guapa(media,ic95a,ic95b)
  return(ic_ok)
}

beta_se_ic_guapa2 <- function(x, y) 
{
  z<-qnorm(1-0.05/2)
  ic95a<-guapa(x-(z*y))
  ic95b<-guapa(x+(z*y))
  media<-guapa(x)
  ic_ok<-ic_guapa2(media,ic95a,ic95b)
  return(ic_ok)
}

risk_se_ic_guapa <- function(x,y) 
{
  z<-qnorm(1-0.05/2)
  hr<-guapa(exp(x))
  ic95a<-guapa(exp(x-(z*y)))
  ic95b<-guapa(exp(x+(z*y)))
  ic_ok<-ic_guapa(hr,ic95a,ic95b)
  return(ic_ok)
}

risk_se_ic_guapa2 <- function(x,y) 
{
  z<-qnorm(1-0.05/2)
  hr<-guapa(exp(x))
  ic95a<-guapa(exp(x-(z*y)))
  ic95b<-guapa(exp(x+(z*y)))
  ic_ok<-ic_guapa2(hr,ic95a,ic95b)
  return(ic_ok)
}

risk_se_ic_guapa3 <- function(x,y) 
{
  z<-qnorm(1-0.05/2)
  hr<-round(exp(x),3)
  ic95a<-round(exp(x-(z*y)),3)
  ic95b<-round(exp(x+(z*y)),3)
  ic_ok<-ic_guapa2(hr,ic95a,ic95b)
  return(ic_ok)
}

header.true <- function(df)
{
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

z<-qnorm(1-0.05/2)
se <- function(x) sqrt(var(x) / length(x))

closest<-function(xv,sv){
  xv[which(abs(xv-sv)==min(abs(xv-sv)))] }

options(scipen=999)


path <- "D:/Artículos/Sara Castro - IMPACT-BCN LTPA/Data"


#####################
### RECODING LTPA ###
#####################

# Merge, rename, sort and export Minnesota datasets
for (timepoint in c("inicio", "final")) {
  files <- file.path(path, paste0("Minesota ", timepoint, " ", 1:3, " fase.xlsx"))
  data <- lapply(files, \(f) read_excel(f) %>% mutate(Cod = trimws(as.character(Cod))) %>% filter(!is.na(Cod), Cod != ""))
  stopifnot(all(sapply(data, \(x) !anyDuplicated(x$Cod))))
  merged <- Reduce(\(x, y) full_join(x, y, by = "Cod"), data)
  
  for (i in 1:67) {
    old <- paste0("Min", timepoint, c("semdia", "semmin", "añodia", "añomin"), i)
    new <- paste0(c("pa_week_days", "pa_week_mins", "pa_year_days", "pa_year_mins"), sprintf("%02d", i))
    names(merged)[match(old, names(merged), nomatch = 0)] <- new[old %in% names(merged)]
  }
  
  merged <- merged %>% arrange(as.numeric(Cod))
  assign(paste0("Minnesota_", timepoint), merged)
  write_csv2(merged, file.path(path, paste0("Minnesota_", timepoint, ".csv")), na = "")
}


# 1. Convert numbers to decimal points; replace missing values with 0
num <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- "0"
  x <- gsub(",", ".", x, fixed = TRUE)
  if (any(!grepl("^[0-9]+(\\.[0-9]+)?$", x))) stop("Invalid numeric value found")
  as.numeric(x)
}

# 2. Import MET coefficients
mets <- read.csv(
  file.path(path, "Minnesota_67_activities_MET_coefficients_corrected.csv"),
  sep = ";", fileEncoding = "Windows-1252",
  check.names = FALSE, colClasses = "character"
)

mets$`No.` <- num(mets$`No.`)
mets$METs <- num(mets$METs)
mets <- mets[order(mets$`No.`), ]

stopifnot(
  nrow(mets) == 70,
  all(mets$`No.` == 1:70),
  all(mets$METs > 0)
)

# 3. Identify activities by intensity
intensity <- tolower(trimws(mets$Intensity))

light <- sprintf("%02d", mets$`No.`[intensity == "light"])
mv <- sprintf("%02d", mets$`No.`[intensity %in% c("moderate", "vigorous")])
all_ids <- sprintf("%02d", 1:70)

stopifnot(
  length(light) > 0,
  length(mv) > 0,
  length(c(light, mv)) == 70,
  !anyDuplicated(c(light, mv))
)

# 4. Define the 8 LTPA summary variables
variables <- c(
  "ltpa_t_week", "ltpa_t_year",
  "ltpa_l_week", "ltpa_l_year",
  "ltpa_mv_week", "ltpa_mv_year",
  "ltpa_l_week_daymins", "ltpa_l_year_daymins"
)

baseline_vars <- paste0(variables, "_baseline")
final_vars <- paste0(variables, "_final")

# 5. Calculate baseline and follow-up
for (tp in c("inicio", "final")) {
  
  data <- read.csv(
    file.path(path, paste0("Minnesota_", tp, "_CORRECTED.csv")),
    sep = ";", fileEncoding = "Windows-1252",
    check.names = FALSE, colClasses = "character",
    na.strings = ""
  )
  
  stopifnot(
    "Cod" %in% names(data),
    !anyNA(data$Cod),
    !anyDuplicated(data$Cod)
  )
  
  data <- data[order(num(data$Cod)), , drop = FALSE]
  
  suffix <- if (tp == "inicio") "baseline" else "final"
  summary_vars <- paste0(variables, "_", suffix)
  
  # 6. Identify participants with ALL original LTPA fields missing
  original_vars <- unlist(lapply(1:70, function(i) {
    id <- sprintf("%02d", i)
    paste0(
      c("pa_week_days", "pa_week_mins",
        "pa_year_days", "pa_year_mins"),
      id
    )
  }))
  
  stopifnot(all(original_vars %in% names(data)))
  
  original_values <- data[original_vars]
  
  all_missing <- rowSums(
    sapply(original_values, function(x)
      is.na(x) | trimws(x) == "")
  ) == length(original_vars)
  
  cat(tp, "- participants with all LTPA fields missing:",
      sum(all_missing), "\n")
  
  # 7. Calculate 140 activity-specific MET-min/week variables
  for (i in 1:70) {
    
    id <- sprintf("%02d", i)
    
    vars <- paste0(
      c("pa_week_days", "pa_week_mins",
        "pa_year_days", "pa_year_mins"),
      id
    )
    
    for (v in vars) data[[v]] <- num(data[[v]])
    
    data[[paste0("pa_week_act", id)]] <-
      round(
        data[[vars[1]]] *
          data[[vars[2]]] *
          mets$METs[i],
        3
      )
    
    data[[paste0("pa_year_act", id)]] <-
      round(
        data[[vars[3]]] *
          data[[vars[4]]] *
          mets$METs[i] / 52,
        3
      )
  }
  
  # 8. Calculate total, light and moderate-to-vigorous LTPA
  sum_activities <- function(prefix, ids) {
    rowSums(data[paste0(prefix, ids)])
  }
  
  data[[paste0("ltpa_t_week_", suffix)]] <-
    sum_activities("pa_week_act", all_ids)
  
  data[[paste0("ltpa_t_year_", suffix)]] <-
    sum_activities("pa_year_act", all_ids)
  
  data[[paste0("ltpa_l_week_", suffix)]] <-
    sum_activities("pa_week_act", light)
  
  data[[paste0("ltpa_l_year_", suffix)]] <-
    sum_activities("pa_year_act", light)
  
  data[[paste0("ltpa_mv_week_", suffix)]] <-
    sum_activities("pa_week_act", mv)
  
  data[[paste0("ltpa_mv_year_", suffix)]] <-
    sum_activities("pa_year_act", mv)
  
  # 9. Calculate light-intensity LTPA in minutes/day
  data[[paste0("ltpa_l_week_daymins_", suffix)]] <- rowSums(
    sapply(light, function(id)
      data[[paste0("pa_week_days", id)]] *
        data[[paste0("pa_week_mins", id)]] / 7)
  )
  
  data[[paste0("ltpa_l_year_daymins_", suffix)]] <- rowSums(
    sapply(light, function(id)
      data[[paste0("pa_year_days", id)]] *
        data[[paste0("pa_year_mins", id)]] / 365)
  )
  
  # 10. Replace summaries with NA when ALL original fields are missing
  data[all_missing, summary_vars] <- NA_real_
  
  # 11. Verify total = light + moderate-to-vigorous
  valid <- !all_missing
  
  stopifnot(
    isTRUE(all.equal(
      data[[paste0("ltpa_t_week_", suffix)]][valid],
      data[[paste0("ltpa_l_week_", suffix)]][valid] +
        data[[paste0("ltpa_mv_week_", suffix)]][valid],
      tolerance = 1e-8
    )),
    isTRUE(all.equal(
      data[[paste0("ltpa_t_year_", suffix)]][valid],
      data[[paste0("ltpa_l_year_", suffix)]][valid] +
        data[[paste0("ltpa_mv_year_", suffix)]][valid],
      tolerance = 1e-8
    ))
  )
  
  # Verify missingness and genuine zeros
  stopifnot(
    all(is.na(as.matrix(data[all_missing, summary_vars, drop = FALSE]))),
    !anyNA(data[valid, summary_vars, drop = FALSE])
  )
  
  # 12. Export complete calculated dataset
  outfile <- file.path(
    path, paste0("Minnesota_", tp, "_CALCULATED.csv")
  )
  
  write.table(
    data, outfile,
    sep = ";", dec = ".",
    row.names = FALSE, col.names = TRUE,
    quote = TRUE, na = "NA",
    fileEncoding = "Windows-1252"
  )
  
  assign(paste0("Minnesota_", tp), data)
  
  cat(
    "VERIFIED:", tp, "-",
    nrow(data), "participants;",
    sum(all_missing), "with missing LTPA;",
    sum(valid), "with calculated LTPA\n"
  )
}

# 13. Export definitive LTPA dataset
stopifnot(
  all(c("Cod", baseline_vars) %in% names(Minnesota_inicio)),
  all(c("Cod", final_vars) %in% names(Minnesota_final))
)

ltpa_export <- merge(
  Minnesota_inicio[, c("Cod", baseline_vars)],
  Minnesota_final[, c("Cod", final_vars)],
  by = "Cod",
  all = TRUE,
  sort = FALSE
)

ltpa_export <- ltpa_export[
  order(num(ltpa_export$Cod)),
  c("Cod", baseline_vars, final_vars)
]

# 14. Export with explicit NA values (no empty cells)
outfile <- file.path(path, "ltpa_export.csv")

write.table(
  ltpa_export, outfile,
  sep = ";", dec = ".",
  row.names = FALSE, col.names = TRUE,
  quote = TRUE, na = "NA",
  fileEncoding = "Windows-1252"
)

# 15. Final verification
check <- read.csv(
  outfile,
  sep = ";", dec = ".",
  check.names = FALSE,
  fileEncoding = "Windows-1252",
  na.strings = "NA",
  colClasses = "character"
)

stopifnot(
  ncol(check) == 17,
  identical(names(check), c("Cod", baseline_vars, final_vars)),
  nrow(check) == nrow(ltpa_export),
  !anyNA(check$Cod),
  !any(check == "", na.rm = TRUE)
)

# Verify numerical values and NA positions after export
for (v in c(baseline_vars, final_vars)) {
  exported <- as.numeric(check[[v]])
  original <- ltpa_export[[v]]
  
  stopifnot(
    identical(is.na(exported), is.na(original)),
    isTRUE(all.equal(exported, original))
  )
}

cat(
  "\nFINAL EXPORT VERIFIED\n",
  "File: ltpa_export.csv\n",
  "Participants:", nrow(check), "\n",
  "Columns:", ncol(check), "\n",
  "Missing baseline LTPA:",
  sum(is.na(ltpa_export$ltpa_t_week_baseline)), "\n",
  "Missing final LTPA:",
  sum(is.na(ltpa_export$ltpa_t_week_final)), "\n",
  "Empty cells: 0 (missing values exported as NA)\n"
)

# 16. Inspect first 10 participants
head(ltpa_export, 10)


###############################
### EXTRACTION FROM DATASET ###
###############################

library(readxl)

path <- "D:/Artículos/Sara Castro - IMPACT-BCN LTPA/Data"

# 1. Import original IMPACT BCN dataset
data <- read_excel(
  file.path(path, "IMPACT_Final_All_V18.xlsx"),
  .name_repair = "minimal"
)

data <- as.data.frame(data, check.names = FALSE)

# 2. Define original variable names and new names
variable_map <- c(
  "Cod" = "Cod",
  "Group_COD" = "group",
  "ITTm" = "ittm",
  "Dropout" = "dropout",
  "LostFollowUp" = "lostfollowup",
  "RetiraConsent" = "retira_consent",
  
  # Pregnancy outcomes
  "SGA_birth (<p10)" = "outc_sga_p10",
  "APO" = "outc_apo",
  "PE" = "outc_pe",
  "PElate" = "outc_pe_late",
  "PEearly" = "outc_pe_early",
  "Hypertensive Disorders" = "outc_hdp",
  "severeSGA (<p3)" = "outc_sga_severe_p3",
  "PTB" = "outc_ptb",
  "PEwithSGA" = "outc_sga_pe",
  "Neon_metab_acidosis" = "outc_neonatal_metab_acidosis",
  "Apgaryes_inf7" = "outc_apgar7",
  "MortalPERINAT" = "outc_mortalityper",
  "Complic_parto_NEON" = "outc_laborcomp",
  "NICU" = "outc_nicu",
  "MuerteNEON" = "outc_neonatal_death",
  "LGA_birth (>p90)" = "outc_lga_p90",
  "GDM" = "outc_gdm",
  "percentil_birth" = "outc_neonate_weight_percentile",
  "Weight_neonate" = "outc_neonate_weight",
  "Gender" = "neonate_sex",
  "Ga_delivery" = "mother_gestational_age_delivery",
  "Neon_acidosis_A" = "outc_neonatal_metab_acidosis_arterial",
  "Neon_acidosis_V" = "outc_neonatal_metab_acidosis_venous",
  
  # Maternal covariates
  "Age_cod" = "mother_age",
  "Nuliparity" = "mother_nulliparity",
  "ART" = "mother_art",
  "BMIpre" = "mother_bmi_pre",
  "Ethnicity_cod" = "mother_ethnicity",
  "Study_class" = "mother_education",
  "Class_socioec" = "mother_class_ses",
  "Smoking" = "mother_smoking",
  "Miscarriage previous" = "mother_ant_miscarriage",
  "Term_delivery_N" = "mother_ant_termdelivery",
  "Preterm_delivery_N" = "mother_ant_ptb",
  "Previous_PE" = "mother_ant_pe",
  "Previous_CIR" = "mother_ant_cir",
  "Weight pre-pregnancy" = "mother_weight_pre",
  "Height" = "mother_height",
  "GA_recruitment" = "mother_gestational_age_recruit",
  "INIC_VisNutri_GA" = "mother_gestational_age_initialvisit",
  "FIN_VisNutri_GA" = "mother_gestational_age_finalvisit",
  "MedDiet_inicio" = "mother_meddiet_baseline",
  "MedDiet_fin" = "mother_meddiet_final",
  "Weight_gained_TOT" = "mother_gestational_weight_gain",
  
  # Intervention adherence
  "ProtocolAA (adh >12p)" = "adherence_12plus",
  "ProtocolAA2 (Adh 3p)" = "adherence_3pgain",
  
  # Previous adverse obstetrical outcomes
  "Adverse obstetrical outcome (previous PE, SGA, Exitus, malform, PTB)" =
    "outc_adverse_comb",
  "Adverse obstetrical outcome placent (previous PE, SGA, Exitus)" =
    "outc_adverse_comb_placental",
  
  # Paternal covariates
  "PATERNAL Age_cod" = "father_age",
  "PATERNAL ethnicity cod" = "father_ethnicity",
  "Study_class (father)" = "father_education",
  "Class_socioec (father)" = "father_class_ses"
)

# 3. Verify that all selected variables exist
missing_vars <- setdiff(names(variable_map), names(data))

if (length(missing_vars) > 0) {
  stop(
    "The following variables were not found in the original dataset:\n",
    paste(missing_vars, collapse = "\n")
  )
}

stopifnot(
  !anyDuplicated(names(variable_map)),
  !anyDuplicated(unname(variable_map))
)

# 4. Extract and rename variables
impact <- data[, names(variable_map), drop = FALSE]
names(impact) <- unname(variable_map)

# 4A. Harmonize NICU admission and hypertensive disorders.
# Preserve true missing values. Unexpected NICU codes trigger an error.
normalize_binary_source <- function(x) {
  y <- tolower(trimws(as.character(x)))
  y[y %in% c("", "na", "n/a", "null")] <- NA_character_
  y
}

nicu_source <- normalize_binary_source(impact$outc_nicu)
nicu_unexpected <- setdiff(unique(nicu_source[!is.na(nicu_source)]),
                           c("0", "1", "no", "yes"))
if (length(nicu_unexpected)) {
  stop("Unexpected outc_nicu codes: ", paste(nicu_unexpected, collapse = ", "))
}
impact$outc_nicu <- ifelse(is.na(nicu_source), NA_character_,
                          ifelse(nicu_source %in% c("1", "yes"), "yes", "no"))

hdp_source <- normalize_binary_source(impact$outc_hdp)
impact$outc_hdp <- ifelse(is.na(hdp_source), NA_character_,
                         ifelse(hdp_source == "no", "no", "yes"))

# Harmonize the three newly extracted binary outcomes; preserve missingness.
for (v in c("outc_apgar7", "outc_mortalityper", "outc_laborcomp")) {
  source <- normalize_binary_source(impact[[v]])
  unexpected <- setdiff(unique(source[!is.na(source)]), c("0", "1", "no", "yes"))
  if (length(unexpected)) {
    stop("Unexpected ", v, " codes: ", paste(unexpected, collapse = ", "))
  }
  impact[[v]] <- ifelse(is.na(source), NA_character_,
                        ifelse(source %in% c("1", "yes"), "yes", "no"))
  cat(v, "recoded counts:\n")
  print(table(impact[[v]], useNA = "ifany"))
}

stopifnot(all(na.omit(impact$outc_nicu) %in% c("no", "yes")),
          all(na.omit(impact$outc_hdp) %in% c("no", "yes")),
          identical(is.na(impact$outc_nicu), is.na(nicu_source)),
          identical(is.na(impact$outc_hdp), is.na(hdp_source)))
cat("NICU recoded counts:\n")
print(table(impact$outc_nicu, useNA = "ifany"))
cat("HDP recoded counts:\n")
print(table(impact$outc_hdp, useNA = "ifany"))

# 5. Standardize participant identifier
impact$Cod <- trimws(as.character(impact$Cod))
impact$Cod <- sub("\\.0+$", "", impact$Cod)

stopifnot(
  !anyNA(impact$Cod),
  all(nzchar(impact$Cod)),
  !anyDuplicated(impact$Cod)
)

# 6. Export extracted variables
write.table(
  impact,
  file.path(path, "IMPACT_BCN_variables.csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  na = "NA",
  fileEncoding = "Windows-1252"
)

cat(
  "IMPACT BCN extraction completed:",
  nrow(impact), "participants,",
  ncol(impact), "variables\n"
)

# 7. Import definitive LTPA dataset
ltpa <- read.csv(
  file.path(path, "ltpa_export.csv"),
  sep = ";",
  dec = ".",
  check.names = FALSE,
  fileEncoding = "Windows-1252",
  colClasses = "character",
  na.strings = "NA"
)

ltpa$Cod <- trimws(as.character(ltpa$Cod))
ltpa$Cod <- sub("\\.0+$", "", ltpa$Cod)

stopifnot(
  !anyNA(ltpa$Cod),
  all(nzchar(ltpa$Cod)),
  !anyDuplicated(ltpa$Cod)
)

# 8. Check matching identifiers
cat("Participants in IMPACT:", nrow(impact), "\n")
cat("Participants in LTPA:", nrow(ltpa), "\n")
cat("Matched participants:", sum(impact$Cod %in% ltpa$Cod), "\n")
cat("IMPACT participants without LTPA:",
    sum(!impact$Cod %in% ltpa$Cod), "\n")
cat("LTPA participants without IMPACT:",
    sum(!ltpa$Cod %in% impact$Cod), "\n")

# 9. Merge datasets
# Keep all participants from the original IMPACT dataset
analysis_data <- merge(impact, ltpa, by = "Cod", all.x = TRUE, sort = FALSE)
analysis_data <- analysis_data[match(impact$Cod, analysis_data$Cod), ,drop = FALSE]
stopifnot(
  nrow(analysis_data) == nrow(impact),
  identical(analysis_data$Cod, impact$Cod))

# 10. Convert LTPA variables to numeric
ltpa_vars <- setdiff(names(ltpa), "Cod")

for (v in ltpa_vars) {
  analysis_data[[v]] <- as.numeric(
    gsub(",", ".", analysis_data[[v]], fixed = TRUE)
  )
}

# 11. Export definitive analytical dataset
write.table(
  analysis_data,
  file.path(path, "IMPACT_BCN_LTPA_raw.csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  col.names = TRUE,
  quote = TRUE,
  na = "NA",
  fileEncoding = "Windows-1252")

dat<-analysis_data
save(dat, file = file.path(path, "IMPACT_BCN_LTPA_raw.RData"))

# 12. Final verification
check <- read.csv(
  file.path(path, "IMPACT_BCN_LTPA_raw.csv"),
  sep = ";",
  dec = ".",
  check.names = FALSE,
  fileEncoding = "Windows-1252",
  na.strings = "NA"
)

stopifnot(
  nrow(check) == nrow(impact),
  ncol(check) == ncol(analysis_data),
  identical(names(check), names(analysis_data)),
  !anyDuplicated(check$Cod),
  file.exists(file.path(path, "IMPACT_BCN_LTPA_raw.RData"))
)

cat(
  "\nFINAL DATASET VERIFIED\n",
  "CSV: IMPACT_BCN_LTPA_raw.csv\n",
  "RData: IMPACT_BCN_LTPA_raw.RData\n",
  "Participants:", nrow(check), "\n",
  "Variables:", ncol(check), "\n"
)



#################################
### LTPA DESCRIPTIVE ANALYSES ###
### Distribution, outliers, eligibility and missingness ###
###########################################################

path <- "D:/Artículos/Sara Castro - IMPACT-BCN LTPA/Data"
output_path <- "D:/Artículos/Sara Castro - IMPACT-BCN LTPA/Outputs"

descriptive_path <- file.path(output_path, "Descriptive")
distribution_path <- file.path(descriptive_path, "Distribution")
results_path <- file.path(output_path, "Results")

dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
dir.create(descriptive_path, recursive = TRUE, showWarnings = FALSE)
dir.create(distribution_path, recursive = TRUE, showWarnings = FALSE)
dir.create(results_path, recursive = TRUE, showWarnings = FALSE)

load(file.path(path, "IMPACT_BCN_LTPA_raw.RData"))

stopifnot(exists("dat"), is.data.frame(dat))

cat("Participants:", nrow(dat), "\n")
cat("Variables:", ncol(dat), "\n")


# ------------------------------------------------------------
# 3. IDENTIFY ALL 16 LTPA VARIABLES
# ------------------------------------------------------------

ltpa_variables <- c(
  "ltpa_t_week_baseline",
  "ltpa_t_year_baseline",
  "ltpa_l_week_baseline",
  "ltpa_l_year_baseline",
  "ltpa_mv_week_baseline",
  "ltpa_mv_year_baseline",
  "ltpa_l_week_daymins_baseline",
  "ltpa_l_year_daymins_baseline",
  
  "ltpa_t_week_final",
  "ltpa_t_year_final",
  "ltpa_l_week_final",
  "ltpa_l_year_final",
  "ltpa_mv_week_final",
  "ltpa_mv_year_final",
  "ltpa_l_week_daymins_final",
  "ltpa_l_year_daymins_final"
)

stopifnot(all(ltpa_variables %in% names(dat)))

# Ensure numeric format
for (v in ltpa_variables) {
  dat[[v]] <- as.numeric(dat[[v]])
}



# ------------------------------------------------------------
# 4. PRIMARY OUTLIERS AND LTPA AVAILABILITY FILTERS
# ------------------------------------------------------------

# Identify baseline and final LTPA variables
baseline_vars <- grep(
  "^ltpa_.*_baseline$",
  names(dat),
  value = TRUE
)

final_vars <- grep(
  "^ltpa_.*_final$",
  names(dat),
  value = TRUE
)

stopifnot(
  length(baseline_vars) == 8,
  length(final_vars) == 8,
  all(c(baseline_vars, final_vars) %in% names(dat))
)

# Preserve original LTPA values
dat_ltpa_before_outliers <- dat[, c(
  "Cod", baseline_vars, final_vars
)]

# ------------------------------------------------------------
# 4.1. FLAG TECHNICAL OUTLIERS (ABSOLUTE Z-SCORE >= 6 SD)
# ------------------------------------------------------------
# Means and SDs are calculated using all available original
# observations, independently for each of the 16 variables.
# Preserve raw LTPA values; outliers are excluded using separate
# flags AFTER the availability filters in the sample flow.

outlier_threshold <- 6
primary_outliers_list <- list()

for (v in ltpa_variables) {
  x <- dat[[v]]
  mean_v <- mean(x, na.rm = TRUE)
  sd_v <- sd(x, na.rm = TRUE)
  if (!is.finite(sd_v) || sd_v == 0) next
  z <- (x - mean_v) / sd_v
  flag <- !is.na(z) & abs(z) >= outlier_threshold
  if (any(flag)) {
    primary_outliers_list[[v]] <- data.frame(
      Cod = dat$Cod[flag], variable = v,
      timepoint = if (grepl("_baseline$", v)) "baseline" else "final",
      original_value = x[flag], mean = mean_v, sd = sd_v,
      z_score = z[flag]
    )
  }
  cat(v, ":", sum(flag), "outlier values >=6 SD\n")
}

primary_outliers <- if (length(primary_outliers_list)) {
  do.call(rbind, primary_outliers_list)
} else {
  data.frame(Cod = character(), variable = character(),
             timepoint = character(), original_value = numeric(),
             mean = numeric(), sd = numeric(), z_score = numeric())
}
rownames(primary_outliers) <- NULL

baseline_outlier_ids <- unique(primary_outliers$Cod[
  primary_outliers$timepoint == "baseline"])
final_outlier_ids <- unique(primary_outliers$Cod[
  primary_outliers$timepoint == "final"])

baseline_excluded <- dat$Cod %in% baseline_outlier_ids
final_excluded <- dat$Cod %in% final_outlier_ids

# 1 = no >=6 SD technical outlier; 0 = at least one outlier
# The flags are independent for baseline and final assessments.
dat$ltpa_baseline_outlier_ok <- as.integer(!baseline_excluded)
dat$ltpa_final_outlier_ok <- as.integer(!final_excluded)

# ------------------------------------------------------------
# 4.2. AVAILABILITY FILTERS (INDEPENDENT OF OUTLIERS)
# ------------------------------------------------------------
# 1 = all eight time-point LTPA variables observed.
dat$ltpa_baseline_ok <- as.integer(
  rowSums(!is.na(dat[baseline_vars])) == length(baseline_vars))
dat$ltpa_final_ok <- as.integer(
  rowSums(!is.na(dat[final_vars])) == length(final_vars))

stopifnot(all(dat$ltpa_baseline_ok %in% 0:1),
          all(dat$ltpa_final_ok %in% 0:1),
          all(dat$ltpa_baseline_outlier_ok %in% 0:1),
          all(dat$ltpa_final_outlier_ok %in% 0:1))

cat("\nBASELINE LTPA AVAILABILITY\n")
print(table(dat$ltpa_baseline_ok))
cat("\nFINAL LTPA AVAILABILITY\n")
print(table(dat$ltpa_final_ok))
cat("\nBASELINE OUTLIERS >=6 SD:", sum(baseline_excluded), "\n")
cat("FINAL OUTLIERS >=6 SD:", sum(final_excluded), "\n")

# ------------------------------------------------------------
# 4.3. EXPORT OUTLIER DETAILS AND PARTICIPANT FLAGS
# ------------------------------------------------------------
write.table(primary_outliers,
            file.path(descriptive_path, "LTPA_technical_outliers_6SD.csv"),
            sep = ";", dec = ".", row.names = FALSE,
            quote = TRUE, na = "NA")

availability <- data.frame(
  Cod = dat$Cod,
  ltpa_baseline_ok = dat$ltpa_baseline_ok,
  ltpa_baseline_outlier_ok = dat$ltpa_baseline_outlier_ok,
  ltpa_final_ok = dat$ltpa_final_ok,
  ltpa_final_outlier_ok = dat$ltpa_final_outlier_ok,
  technical_outlier_baseline = as.integer(baseline_excluded),
  technical_outlier_final = as.integer(final_excluded)
)
write.table(availability,
            file.path(descriptive_path, "LTPA_availability.csv"),
            sep = ";", dec = ".", row.names = FALSE,
            quote = TRUE, na = "NA")

# ------------------------------------------------------------
# 5. DISTRIBUTION PLOTS FOR ALL LTPA VARIABLES
# ------------------------------------------------------------

# Generate figures excluding outliers from plotted values only.
# Use plot(compareGroups(~VARIABLE, data=dat)).
# Export one PNG per LTPA variable.

for (v in ltpa_variables) {
  
  cat("Plotting:", v, "\n")
  
  f <- as.formula(paste("~", v))
  
  plot_dat <- dat
  if (grepl("_baseline$", v)) {
    plot_dat[[v]][baseline_excluded] <- NA_real_
  } else {
    plot_dat[[v]][final_excluded] <- NA_real_
  }
  cg <- compareGroups(
    f,
    data = plot_dat
  )
  
  png(
    filename = file.path(
      distribution_path,
      paste0("Distribution_", v, ".png")
    ),
    width = 1800,
    height = 1200,
    res = 180
  )
  
  plot(cg)
  
  dev.off()
}

cat(
  "\n",
  length(ltpa_variables),
  "LTPA distribution figures exported.\n"
)


# ------------------------------------------------------------
# 6. LTPA DESCRIPTIVE STATISTICS
# ------------------------------------------------------------

describe_ltpa <- function(v) {
  
  x <- dat[[v]]
  if (grepl("_baseline$", v)) x[baseline_excluded] <- NA_real_
  else x[final_excluded] <- NA_real_
  n_available <- sum(!is.na(x))
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(data.frame(
      variable = v,
      n = 0,
      missing_n = nrow(dat),
      missing_pct = 100,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      p1 = NA_real_,
      p5 = NA_real_,
      p25 = NA_real_,
      median = NA_real_,
      p75 = NA_real_,
      p95 = NA_real_,
      p99 = NA_real_,
      max = NA_real_
    ))
  }
  
  data.frame(
    variable = v,
    n = length(x),
    missing_n = nrow(dat) - n_available,
    missing_pct = 100 * (nrow(dat) - n_available) / nrow(dat),
    mean = mean(x),
    sd = sd(x),
    min = min(x),
    p1 = unname(quantile(x, 0.01)),
    p5 = unname(quantile(x, 0.05)),
    p25 = unname(quantile(x, 0.25)),
    median = median(x),
    p75 = unname(quantile(x, 0.75)),
    p95 = unname(quantile(x, 0.95)),
    p99 = unname(quantile(x, 0.99)),
    max = max(x)
  )
}

ltpa_descriptives <- do.call(
  rbind,
  lapply(ltpa_variables, describe_ltpa)
)

write.table(
  ltpa_descriptives,
  file.path(descriptive_path, "LTPA_descriptive_statistics.csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  quote = TRUE,
  na = "NA"
)

print(ltpa_descriptives)


# ------------------------------------------------------------
# 8. FILTER PARTICIPANTS WITH BASELINE LTPA
# ------------------------------------------------------------

dat_baseline <- dat %>%
  filter(ltpa_baseline_ok == 1, ltpa_baseline_outlier_ok == 1)

cat("\nPARTICIPANTS WITH BASELINE LTPA:",
    nrow(dat_baseline), "\n")


# ------------------------------------------------------------
# 9. DEFINE KEY COVARIATES
# ------------------------------------------------------------

# Main covariates for statistical models
covariates_main <- c(
  "group",
  "mother_age",
  "mother_nulliparity",
  "mother_art",
  "mother_bmi_pre",
  "mother_ethnicity",
  "mother_smoking",
  "mother_education",
  "mother_class_ses"
)

# Additional maternal and paternal covariates
covariates_additional <- c(
  "mother_ant_miscarriage",
  "mother_ant_termdelivery",
  "mother_ant_ptb",
  "mother_ant_pe",
  "mother_ant_cir",
  "mother_weight_pre",
  "mother_height",
  "mother_gestational_age_recruit",
  "mother_gestational_age_initialvisit",
  "mother_gestational_age_finalvisit",
  "mother_meddiet_baseline",
  "mother_meddiet_final",
  "mother_gestational_weight_gain",
  "father_age",
  "father_ethnicity",
  "father_education",
  "father_class_ses"
)

# Study participation and adherence indicators
study_variables <- c(
  "ittm",
  "dropout",
  "lostfollowup",
  "retira_consent",
  "adherence_12plus",
  "adherence_3pgain"
)

# All outcomes identified by prefix
outcomes <- grep(
  "^outc_",
  names(dat_baseline),
  value = TRUE
)

cat("\nOUTCOMES IDENTIFIED:\n")
print(outcomes)

stopifnot(
  all(covariates_main %in% names(dat_baseline)),
  all(covariates_additional %in% names(dat_baseline)),
  all(study_variables %in% names(dat_baseline)),
  length(outcomes) > 0
)


# ------------------------------------------------------------
# 10. STANDARDIZE MISSING VALUES FOR ASSESSMENT
# ------------------------------------------------------------

# Empty strings and literal "NA" are missing.
# Do not recode yes/no or categorical values yet.

missing_vars <- unique(c(
  covariates_main,
  covariates_additional,
  study_variables,
  outcomes
))

for (v in missing_vars) {
  
  if (is.character(dat_baseline[[v]])) {
    
    x <- trimws(dat_baseline[[v]])
    
    x[toupper(x) %in% c("", "NA", "N/A", "NULL")] <- NA_character_
    
    dat_baseline[[v]] <- x
  }
}


# ------------------------------------------------------------
# 11. MISSINGNESS IN COVARIATES AND OUTCOMES
# ------------------------------------------------------------

missing_summary <- function(vars, category) {
  
  do.call(rbind, lapply(vars, function(v) {
    
    x <- dat_baseline[[v]]
    
    data.frame(
      category = category,
      variable = v,
      n_total = length(x),
      n_available = sum(!is.na(x)),
      n_missing = sum(is.na(x)),
      pct_missing = round(100 * mean(is.na(x)), 2),
      n_unique = length(unique(x[!is.na(x)])),
      stringsAsFactors = FALSE
    )
  }))
}

missingness <- rbind(
  missing_summary(covariates_main, "Main covariates"),
  missing_summary(covariates_additional, "Additional covariates"),
  missing_summary(study_variables, "Study variables"),
  missing_summary(outcomes, "Outcomes")
)

missingness <- missingness[
  order(missingness$category, -missingness$pct_missing),
]

rownames(missingness) <- NULL

write.table(
  missingness,
  file.path(descriptive_path, "Missingness_baseline_LTPA.csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  quote = TRUE,
  na = "NA"
)

cat("\nMISSINGNESS AMONG PARTICIPANTS WITH BASELINE LTPA\n")

print(missingness, row.names = FALSE)


# ------------------------------------------------------------
# 12. VARIABLES WITH >5% MISSINGNESS
# ------------------------------------------------------------

missingness_over5 <- missingness[
  missingness$pct_missing > 5,
]

write.table(
  missingness_over5,
  file.path(descriptive_path, "Missingness_over5percent.csv"),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  quote = TRUE,
  na = "NA"
)

cat("\nVARIABLES WITH >5% MISSINGNESS:\n")

print(missingness_over5, row.names = FALSE)


# ------------------------------------------------------------
# 13. SAVE DESCRIPTIVE DATASET AND R OBJECTS
# ------------------------------------------------------------

save(
  dat,
  dat_baseline,
  dat_ltpa_before_outliers,
  primary_outliers,
  availability,
  ltpa_descriptives,
  missingness,
  missingness_over5,
  file = file.path(
    descriptive_path,
    "IMPACT_BCN_LTPA_descriptive.RData"
  )
)

cat("\nDESCRIPTIVE ANALYSES COMPLETED\n")
cat("Figures:", distribution_path, "\n")
cat("Tables:", descriptive_path, "\n")
cat("RData: IMPACT_BCN_LTPA_descriptive.RData\n")


#######################
# MISSINGNESS RESULTS #
# No missingness in these covariates: group, ittm, mother_age, mother_nulliparity, mother_art, mother_bmi_pre, mother_ethnicity
# mother_smoking, mother_education, mother_class_ses, mother_gestational_age_recruit, mother_ant_XXX
# Missingness in outcomes n = 3: outc_sga_p10, outc_sga_severe_p3, outc_lga_p90, outc_neonate_weight_percentile, outc_neonate_weight
# Missingness in outcomes n = 7: outc_pe, outc_pe_late, outc_pe_early, outc_hdp, outc_ptb, outc_sga_pe, outc_nicu, outc_neonatal_death, outc_gdm
# Exclusion of NA in outcomes







# ============================================================
# 14. OUTCOME COMPLETENESS AND SEQUENTIAL SAMPLE SELECTION
# Integrated into revised IMPACT_BCN_LTPA_makingdata.R
# ============================================================

outcome_required <- c(
  "outc_sga_p10", "outc_sga_severe_p3", "outc_lga_p90",
  "outc_neonate_weight_percentile", "outc_neonate_weight",
  "outc_pe", "outc_pe_late", "outc_pe_early", "outc_hdp",
  "outc_ptb", "outc_sga_pe", "outc_nicu",
  "outc_neonatal_death", "outc_gdm"
)

stopifnot(all(outcome_required %in% names(dat)))

# Standardize missing codes in the complete dataset, not only dat_baseline.
# This is necessary for valid comparisons with excluded participants.
normalize_missing <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    x <- trimws(x)
    x[toupper(x) %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  }
  x
}

dat[outcome_required] <- lapply(dat[outcome_required], normalize_missing)
dat$ittm <- normalize_missing(dat$ittm)

ittm_clean <- tolower(trimws(as.character(dat$ittm)))
stopifnot(all(ittm_clean %in% c("yes", "no")))

dat$outc_ok <- as.integer(complete.cases(dat[, outcome_required, drop = FALSE]))

eligible_itt <- ittm_clean == "yes"
eligible_outc <- eligible_itt & dat$outc_ok == 1
eligible_baseline_available <- eligible_outc & dat$ltpa_baseline_ok == 1
eligible_baseline <- eligible_baseline_available & dat$ltpa_baseline_outlier_ok == 1
eligible_final_available <- eligible_baseline & dat$ltpa_final_ok == 1
eligible_final <- eligible_final_available & dat$ltpa_final_outlier_ok == 1

dat$data_ltpa_baseline_ok <- as.integer(eligible_baseline)
dat$data_ltpa_final_ok <- as.integer(eligible_final)

n_original <- nrow(dat)
n_excluded_itt <- sum(!eligible_itt)
n_after_itt <- sum(eligible_itt)
n_excluded_outc <- sum(eligible_itt & dat$outc_ok == 0)
n_after_outc <- sum(eligible_outc)
n_excluded_baseline <- sum(eligible_outc & dat$ltpa_baseline_ok == 0)
n_after_baseline_available <- sum(eligible_baseline_available)
n_excluded_baseline_outliers <- sum(eligible_baseline_available & dat$ltpa_baseline_outlier_ok == 0)
n_after_baseline <- sum(eligible_baseline)
n_excluded_final <- sum(eligible_baseline & dat$ltpa_final_ok == 0)
n_after_final_available <- sum(eligible_final_available)
n_excluded_final_outliers <- sum(eligible_final_available & dat$ltpa_final_outlier_ok == 0)
n_after_final <- sum(eligible_final)

selection_flow <- data.frame(
  step = c("Original IMPACT-BCN sample", "Excluded: ITTm = no",
           "Remaining: ITTm = yes", "Excluded: incomplete outcomes",
           "Remaining: complete outcomes", "Excluded: baseline LTPA unavailable",
           "Remaining: baseline LTPA available", "Excluded: baseline technical outliers >=6 SD",
           "Baseline analytical sample", "Excluded: final LTPA unavailable",
           "Remaining: final LTPA available", "Excluded: final technical outliers >=6 SD",
           "Final longitudinal analytical sample"),
  n = c(n_original, n_excluded_itt, n_after_itt,
        n_excluded_outc, n_after_outc, n_excluded_baseline,
        n_after_baseline_available, n_excluded_baseline_outliers,
        n_after_baseline, n_excluded_final,
        n_after_final_available, n_excluded_final_outliers, n_after_final)
)

stopifnot(
  n_original == n_excluded_itt + n_after_itt,
  n_after_itt == n_excluded_outc + n_after_outc,
  n_after_outc == n_excluded_baseline + n_after_baseline_available,
  n_after_baseline_available == n_excluded_baseline_outliers + n_after_baseline,
  n_after_baseline == n_excluded_final + n_after_final_available,
  n_after_final_available == n_excluded_final_outliers + n_after_final,
  sum(dat$data_ltpa_baseline_ok) == n_after_baseline,
  sum(dat$data_ltpa_final_ok) == n_after_final,
  all(dat$data_ltpa_final_ok <= dat$data_ltpa_baseline_ok)
)

cat("\nSEQUENTIAL SAMPLE SELECTION\n")
print(selection_flow, row.names = FALSE)
write.table(selection_flow,
            file.path(descriptive_path, "Sample_selection_flow.csv"),
            sep = ";", dec = ".", row.names = FALSE,
            quote = TRUE, na = "NA")

# ============================================================
# 15. VARIABLES AND COMPAREGROUPS METHODS
# ============================================================

selection_variables <- c(
  "group", "mother_age", "mother_nulliparity", "mother_art",
  "mother_bmi_pre", "mother_ethnicity", "mother_smoking",
  "mother_education", "mother_class_ses",
  "mother_gestational_age_recruit", "mother_ant_miscarriage",
  "mother_ant_termdelivery", "mother_ant_ptb", "mother_ant_pe",
  "mother_ant_cir", "father_age", "father_ethnicity",
  "father_education", "father_class_ses", 
  "outc_sga_p10", "outc_apo", "outc_pe", "outc_pe_late",
  "outc_hdp", "outc_sga_pe", "outc_sga_severe_p3", "outc_ptb",
  "outc_nicu", "outc_neonatal_metab_acidosis",
  "outc_apgar7", "outc_mortalityper", "outc_laborcomp",
  "outc_lga_p90", "outc_gdm"
)

categorical_variables <- c(
  "group", "mother_nulliparity", "mother_art",
  "mother_ethnicity", "mother_smoking", "mother_education",
  "mother_class_ses", "mother_ant_miscarriage",
  "mother_ant_termdelivery", "mother_ant_ptb", "mother_ant_pe",
  "mother_ant_cir", "father_ethnicity", "father_education",
  "father_class_ses", 
  "outc_sga_p10", "outc_apo", "outc_pe", "outc_pe_late",
  "outc_hdp", "outc_sga_pe", "outc_sga_severe_p3", "outc_ptb",
  "outc_nicu", "outc_neonatal_metab_acidosis",
  "outc_apgar7", "outc_mortalityper", "outc_laborcomp",
  "outc_lga_p90", "outc_gdm"
)

ltpa_nonnormal <- c(
  "ltpa_t_week_baseline", "ltpa_t_year_baseline",
  "ltpa_l_week_baseline", "ltpa_l_year_baseline",
  "ltpa_mv_week_baseline", "ltpa_mv_year_baseline",
  "ltpa_l_week_daymins_baseline", "ltpa_l_year_daymins_baseline",
  "ltpa_t_week_final", "ltpa_t_year_final",
  "ltpa_l_week_final", "ltpa_l_year_final",
  "ltpa_mv_week_final", "ltpa_mv_year_final",
  "ltpa_l_week_daymins_final", "ltpa_l_year_daymins_final"
)

descriptive_variables <- c(selection_variables, ltpa_nonnormal)
stopifnot(!anyDuplicated(descriptive_variables),
          all(descriptive_variables %in% names(dat)))

# Apply explicit categorical and continuous data types.
# Categorical codes are retained as labels; no numeric recoding.
for (v in descriptive_variables) {
  dat[[v]] <- normalize_missing(dat[[v]])
  if (v %in% categorical_variables) {
    dat[[v]] <- as.factor(dat[[v]])
  } else if (!is.numeric(dat[[v]])) {
    dat[[v]] <- as.numeric(gsub(",", ".", as.character(dat[[v]]), fixed = TRUE))
  }
}

# Named method vector: 1 = normal continuous, 2 = nonnormal,
# 3 = categorical. Neonatal weight and percentile are continuous.
method_selection <- setNames(rep(1, length(selection_variables)), selection_variables)
method_selection[categorical_variables] <- 3

method_descriptive <- setNames(rep(1, length(descriptive_variables)), descriptive_variables)
method_descriptive[categorical_variables] <- 3
method_descriptive[ltpa_nonnormal] <- 2

# CSV export preserving complete median [Q1; Q3] strings.
# export2csv() can split interval strings at semicolons in some versions.
# write.table(quote=TRUE) keeps semicolons inside a single quoted field.
export_complete_comparegroups <- function(tab, file) {
  description <- as.data.frame(tab$descr, stringsAsFactors = FALSE,
                               check.names = FALSE)
  if (nrow(description) == 0L || ncol(description) == 0L) {
    stop("Empty compareGroups descriptive table: ", file)
  }
  description <- data.frame(variable = rownames(description), description,
                            check.names = FALSE, stringsAsFactors = FALSE)
  write.table(description, file = file, sep = ";", dec = ".",
              quote = TRUE, row.names = FALSE, col.names = TRUE,
              na = "NA", fileEncoding = "UTF-8")
  # Round-trip check: no field may be split at an IQR separator.
  check <- read.csv(file, sep = ";", dec = ".", quote = "\"",
                    check.names = FALSE, fileEncoding = "UTF-8")
  stopifnot(nrow(check) == nrow(description),
            ncol(check) == ncol(description),
            identical(as.character(check[[1]]), as.character(description[[1]])))
  invisible(file)
}

# ============================================================
# 16. SELECTION BIAS: INCLUDED VS EXCLUDED
# ============================================================

selection_data <- dat[, c("data_ltpa_baseline_ok", selection_variables), drop = FALSE]
selection_data$data_ltpa_baseline_ok <- factor(
  selection_data$data_ltpa_baseline_ok,
  levels = c(0, 1), labels = c("Excluded", "Included")
)

selection_comparison <- compareGroups(
  data_ltpa_baseline_ok ~ ., data = selection_data,
  method = method_selection
)

selection_table <- createTable(selection_comparison,
                               show.n = TRUE, hide.no = 0)
print(selection_table)

export_complete_comparegroups(
  selection_table, file.path(descriptive_path, "Selection_bias_baseline.csv"))

# ============================================================
# 17. DESCRIPTIVE TABLE: BASELINE ANALYTICAL SAMPLE
# ============================================================

# Keep participants with ITTm=yes, complete outcomes and valid baseline LTPA.
# Follow-up LTPA is allowed to be missing and its available N is shown.
dat_baseline_analysis <- dat[dat$data_ltpa_baseline_ok == 1, , drop = FALSE]
dat_final_analysis <- dat[dat$data_ltpa_final_ok == 1, , drop = FALSE]

# One-column descriptive table for the complete baseline analytical sample.
# No artificial grouping variable is needed with the ~ . formula.
descriptive_data <- dat_baseline_analysis[, descriptive_variables, drop = FALSE]
# Baseline-eligible participants with >=6 SD final outliers remain
# in the baseline sample, but their final LTPA is set to missing
# for this descriptive table. Original dat remains unchanged.
descriptive_data[dat_baseline_analysis$ltpa_final_outlier_ok == 0,
                 ltpa_nonnormal[grepl("_final$", ltpa_nonnormal)]] <- NA_real_

descriptive_comparison <- compareGroups(
  ~ ., data = descriptive_data,
  method = method_descriptive
)

descriptive_table <- createTable(descriptive_comparison,
                                 show.n = TRUE, hide.no = 0)
print(descriptive_table)

export_complete_comparegroups(
  descriptive_table,
  file.path(descriptive_path, "Descriptive_baseline_analytical_sample.csv"))

# Verify that all 16 LTPA rows survive export with their complete intervals.
export_check <- read.csv(
  file.path(descriptive_path, "Descriptive_baseline_analytical_sample.csv"),
  sep = ";", check.names = FALSE, fileEncoding = "UTF-8")
ltpa_rows <- export_check[export_check$variable %in% ltpa_nonnormal, , drop = FALSE]
stopifnot(nrow(ltpa_rows) == length(ltpa_nonnormal))
cat("LTPA descriptive rows exported:", nrow(ltpa_rows), "\n")

# Save the filtered datasets and the tables for subsequent analyses.
save(dat, dat_baseline_analysis, dat_final_analysis,
     selection_flow, selection_comparison, selection_table,
     descriptive_comparison, descriptive_table,
     file = file.path(path, "IMPACT_BCN_LTPA_analysis_ready.RData"))

cat("\nANALYSES COMPLETED\n")
cat("Original participants:", n_original, "\n")
cat("Baseline analytical sample:", nrow(dat_baseline_analysis), "\n")
cat("Longitudinal analytical sample:", nrow(dat_final_analysis), "\n")
cat("Tables saved in:", descriptive_path, "\n")


# ============================================================
# 18. HORN'S PARALLEL ANALYSIS OF PREGNANCY OUTCOMES
# ============================================================

library(psych)

# 1. Select outcomes
outcomes_pa <- c("outc_sga_p10", "outc_apo", "outc_pe", "outc_pe_late",
                 "outc_hdp", "outc_sga_pe", "outc_sga_severe_p3", "outc_ptb",
                 "outc_nicu", "outc_neonatal_metab_acidosis",
  "outc_apgar7", "outc_laborcomp",
  "outc_lga_p90", "outc_gdm")


stopifnot(all(outcomes_pa %in% names(dat_baseline_analysis)))

# 2. Extract outcomes from baseline analytical sample
dat_pa <- dat_baseline_analysis[, outcomes_pa, drop = FALSE]

# 3. Convert binary outcomes to numeric 0/1
# Accepts both yes/no and 0/1 coding.
binary_to_numeric <- function(x) {
  
  x <- tolower(trimws(as.character(x)))
  
  x[x %in% c("", "na", "n/a", "null")] <- NA_character_
  
  unexpected <- unique(x[
    !is.na(x) & !x %in% c("yes", "no", "1", "0")
  ])
  
  if (length(unexpected) > 0) {
    stop(
      "Unexpected binary outcome values: ",
      paste(unexpected, collapse = ", ")
    )
  }
  
  ifelse(
    is.na(x),
    NA_real_,
    ifelse(x %in% c("yes", "1"), 1, 0)
  )
}

dat_pa[] <- lapply(dat_pa, binary_to_numeric)

# 4. Keep complete cases
dat_pa <- na.omit(dat_pa)

cat("\nParticipants included in parallel analysis:",
    nrow(dat_pa), "\n")

# Verify that every outcome has both categories
stopifnot(all(vapply(
  dat_pa,
  function(x) length(unique(x)) == 2,
  logical(1)
)))

# 5. Estimate tetrachoric correlation matrix
tetra <- psych::tetrachoric(dat_pa)

cor_outcomes <- tetra$rho

# Export correlation matrix
write.csv2(
  cor_outcomes,
  file.path(
    descriptive_path,
    "Outcome_tetrachoric_correlations.csv"
  )
)

# 6. Horn's parallel analysis
set.seed(12345)

png(
  filename = file.path(
    descriptive_path,
    "Horn_parallel_analysis_outcomes.png"
  ),
  width = 1800,
  height = 1200,
  res = 180
)

pa_results <- psych::fa.parallel(
  cor_outcomes,
  n.obs = nrow(dat_pa),
  fa = "pc",
  n.iter = 1000,
  quant = 0.95,
  error.bars = FALSE,
  main = "Horn's parallel analysis of pregnancy outcomes"
)

dev.off()

# 7. Number of retained principal components
n_components <- pa_results$ncomp

cat("\n====================================\n")
cat("HORN'S PARALLEL ANALYSIS RESULTS\n")
cat("====================================\n")
cat("Outcomes:", length(outcomes_pa), "\n")
cat("Participants:", nrow(dat_pa), "\n")
cat("Retained components:", n_components, "\n")

# 8. Bonferroni threshold based on retained components
alpha <- 0.05

bonferroni_alpha <- alpha / n_components

cat(
  "Exploratory component-based threshold:",
  bonferroni_alpha, "\n"
)

# 9. Export summary
pa_summary <- data.frame(
  n_participants = nrow(dat_pa),
  n_outcomes = length(outcomes_pa),
  retained_components = n_components,
  alpha = alpha,
  component_based_threshold = bonferroni_alpha,
  conventional_bonferroni_12 = alpha / length(outcomes_pa)
)

write.table(
  pa_summary,
  file.path(
    descriptive_path,
    "Horn_parallel_analysis_summary.csv"
  ),
  sep = ";",
  dec = ".",
  row.names = FALSE,
  quote = TRUE
)

# ============================================================
# 19. DEFINITIVE LTPA EXPOSURES AND UPDATED DESCRIPTIVE TABLE
# ============================================================
# Append-only section: original 16 LTPA columns are retained for
# traceability; new exposure names are added to all analysis datasets.
# MVLTPA / total LTPA: MET-min/week; LLTPA: min/week.
# Differences are pregnancy minus habitual (pre-pregnancy proxy).

exposure_sources <- c(
  mvltpa_prepreg = "ltpa_mv_year_baseline",
  mvltpa_gw20    = "ltpa_mv_week_baseline",
  mvltpa_gw35    = "ltpa_mv_week_final",
  lltpa_prepreg = "ltpa_l_year_daymins_baseline",
  lltpa_gw20    = "ltpa_l_week_daymins_baseline",
  lltpa_gw35    = "ltpa_l_week_daymins_final",
  tltpa_prepreg = "ltpa_t_year_baseline",
  tltpa_gw20    = "ltpa_t_week_baseline",
  tltpa_gw35    = "ltpa_t_week_final"
)
stopifnot(all(unname(exposure_sources) %in% names(dat)))

exposure_continuous <- unlist(lapply(c("mvltpa", "lltpa", "tltpa"), function(prefix)
  paste0(prefix, c("_prepreg", "_gw20", "_gw35",
                   "_dif_gw20", "_dif_gw35"))))
exposure_categorical <- unlist(lapply(c("mvltpa", "lltpa", "tltpa"), function(prefix)
  paste0(prefix, c("_prepreg_cat", "_gw20_cat", "_gw35_cat"))))
exposure_variables <- c(exposure_continuous, exposure_categorical)
stopifnot(!anyDuplicated(exposure_variables),
          !any(exposure_variables %in% names(dat)))

# Convert only expected numeric source variables; reject unexpected strings.
for (nm in names(exposure_sources)) {
  source_name <- unname(exposure_sources[[nm]])
  original <- dat[[source_name]]
  value <- suppressWarnings(as.numeric(gsub(",", ".", as.character(original), fixed = TRUE)))
  stopifnot(!any(is.na(value) & !is.na(original)))
  dat[[nm]] <- value * if (startsWith(nm, "lltpa_")) 7 else 1
}

for (prefix in c("mvltpa", "lltpa", "tltpa")) {
  prepreg <- paste0(prefix, "_prepreg")
  for (gw in c("gw20", "gw35")) {
    current <- paste0(prefix, "_", gw)
    dat[[paste0(prefix, "_dif_", gw)]] <- dat[[current]] - dat[[prepreg]]
  }
  cutoff <- switch(prefix, mvltpa = 750, lltpa = 500, tltpa = 2500)
  for (timepoint in c("prepreg", "gw20", "gw35")) {
    current <- paste0(prefix, "_", timepoint)
    categorical <- paste0(current, "_cat")
    # Factor levels 0 and 1 ensure compareGroups uses categorical method.
    dat[[categorical]] <- factor(
      ifelse(is.na(dat[[current]]), NA_integer_,
             as.integer(dat[[current]] >= cutoff)),
      levels = c(0, 1)
    )
    stopifnot(identical(is.na(dat[[current]]), is.na(dat[[categorical]])))
  }
}

# Update the two analysis subsets without changing their selection criteria.
dat_baseline_analysis <- dat[dat$data_ltpa_baseline_ok == 1, , drop = FALSE]
dat_final_analysis <- dat[dat$data_ltpa_final_ok == 1, , drop = FALSE]

# Retain all original non-LTPA descriptive variables in the same order.
# Replace the 16 original LTPA variables with 24 definitive exposures.
descriptive_variables_final <- c(selection_variables, exposure_variables)
stopifnot(!anyDuplicated(descriptive_variables_final),
          all(descriptive_variables_final %in% names(dat_baseline_analysis)))

# Update method vectors: categorical = 3; nonnormal continuous = 2.
categorical_variables_final <- c(categorical_variables, exposure_categorical)
ltpa_nonnormal_final <- exposure_continuous
method_descriptive_final <- setNames(rep(1, length(descriptive_variables_final)),
                                     descriptive_variables_final)
method_descriptive_final[categorical_variables_final] <- 3
method_descriptive_final[ltpa_nonnormal_final] <- 2
stopifnot(all(method_descriptive_final[exposure_categorical] == 3),
          all(method_descriptive_final[exposure_continuous] == 2))

# Use the baseline analytical sample, retaining participants with missing
# final assessments. Suppress GW35 exposures for final technical outliers,
# consistently with the earlier descriptive section.
descriptive_data_final <- dat_baseline_analysis[,
  descriptive_variables_final, drop = FALSE]
final_outlier_rows <- dat_baseline_analysis$ltpa_final_outlier_ok == 0
final_exposures <- grep("_gw35(_cat)?$", exposure_variables, value = TRUE)
for (v in final_exposures) {
  if (is.factor(descriptive_data_final[[v]])) {
    descriptive_data_final[[v]][final_outlier_rows] <- NA
  } else {
    descriptive_data_final[[v]][final_outlier_rows] <- NA_real_
  }
}
# Change from GW35 also requires a valid final assessment.
for (v in grep("_dif_gw35$", exposure_variables, value = TRUE)) {
  descriptive_data_final[[v]][final_outlier_rows] <- NA_real_
}

# compareGroups may drop constant binary factors or raise an error for
# variables with no observed data; stop explicitly if this occurs.
descriptive_comparison_final <- compareGroups(
  ~ ., data = descriptive_data_final,
  method = method_descriptive_final
)
descriptive_table_final <- createTable(
  descriptive_comparison_final, show.n = TRUE, hide.no = 0
)
print(descriptive_table_final)

# Overwrite the requested descriptive CSV with the updated 24 exposures.
final_descriptive_file <- file.path(
  descriptive_path, "Descriptive_baseline_analytical_sample.csv")
export_complete_comparegroups(descriptive_table_final, final_descriptive_file)

# Check the presence of all 24 exposure labels in exported row names.
# A categorical exposure can occupy multiple rows in the export.
export_check_final <- read.csv(final_descriptive_file, sep = ";",
                               check.names = FALSE, fileEncoding = "UTF-8")
exported_labels <- as.character(export_check_final$variable)
missing_labels <- exposure_variables[!vapply(exposure_variables, function(v)
  any(exported_labels == v | startsWith(exported_labels, paste0(v, ":")) |
        startsWith(exported_labels, paste0(v, " "))), logical(1))]
if (length(missing_labels)) {
  warning("Check compareGroups export labels for: ",
          paste(missing_labels, collapse = ", "))
}

# Update the existing analysis-ready file in the Data folder.
# Preserve previously saved objects and add the definitive tables.
save(dat, dat_baseline_analysis, dat_final_analysis,
     selection_flow, selection_comparison, selection_table,
     descriptive_comparison, descriptive_table,
     descriptive_comparison_final, descriptive_table_final,
     exposure_variables, exposure_continuous, exposure_categorical,
     file = file.path(path, "IMPACT_BCN_LTPA_analysis_ready.RData"))

cat("\nDEFINITIVE EXPOSURES CREATED: ", length(exposure_variables), "\n", sep = "")
cat("Baseline analytical sample: ", nrow(dat_baseline_analysis), "\n", sep = "")
cat("Final analytical sample: ", nrow(dat_final_analysis), "\n", sep = "")
cat("Updated descriptive: ", final_descriptive_file, "\n", sep = "")
cat("Updated RData: ", file.path(path, "IMPACT_BCN_LTPA_analysis_ready.RData"), "\n", sep = "")

# ============================================================
# 20. LOGISTIC REGRESSIONS: MVLTPA AND LLTPA ONLY
# ============================================================
# 16 exposures x 14 outcomes x 4 nested models = 896 fits.
# Ordered by exposure, then outcome, then Models 1-4.
# GW35 exposures require the final-visit analytical eligibility flag.
# GW35 exposures and earlier-diagnosed outcomes (e.g., GDM):
# temporal ordering must be considered when interpreting associations.

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Install the Excel export package first: install.packages('openxlsx')")
}

logistic_path <- file.path(results_path, "Logistic_regression")
dir.create(logistic_path, recursive = TRUE, showWarnings = FALSE)

logistic_outcomes <- c(
  "outc_sga_p10", "outc_apo", "outc_pe", "outc_pe_late",
  "outc_hdp", "outc_sga_pe", "outc_sga_severe_p3", "outc_ptb",
  "outc_nicu", "outc_neonatal_metab_acidosis",
  "outc_apgar7", "outc_laborcomp",
  "outc_lga_p90", "outc_gdm"
)

# Explicit exposure order: continuous exposures first, then categorical,
# within MVLTPA and LLTPA; outcomes remain grouped first.
logistic_exposures <- unlist(lapply(c("mvltpa", "lltpa"), function(prefix)
  paste0(prefix, c("_prepreg", "_gw20", "_dif_gw20", "_gw35",
                   "_dif_gw35", "_prepreg_cat", "_gw20_cat", "_gw35_cat"))))
logistic_continuous <- logistic_exposures[!grepl("_cat$", logistic_exposures)]
logistic_binary <- logistic_exposures[grepl("_cat$", logistic_exposures)]

logistic_adjustment <- list(
  Model1 = character(0),
  Model2 = c("group", "mother_age"),
  Model3 = c("group", "mother_age", "mother_nulliparity", "mother_art"),
  Model4 = c("group", "mother_age", "mother_nulliparity", "mother_art",
             "mother_bmi_pre", "mother_smoking", "mother_education")
)

stopifnot(exists("dat_baseline_analysis"),
          all(logistic_outcomes %in% names(dat_baseline_analysis)),
          all(logistic_exposures %in% names(dat_baseline_analysis)),
          all(unlist(logistic_adjustment) %in% names(dat_baseline_analysis)),
          "data_ltpa_final_ok" %in% names(dat_baseline_analysis),
          is.function(risk_se_ic_guapa2), is.function(pval_guapa))

# Preserve original database; perform all transformations on a working copy.
logistic_dat <- dat_baseline_analysis
stopifnot(!anyDuplicated(logistic_dat$Cod))

# Enforce explicit binary 0/1 outcome coding. Never allow the ordering of
# an as.factor() call to silently reverse the event definition.
logistic_binary01 <- function(x, name) {
  v <- tolower(trimws(as.character(x)))
  v[v %in% c("", "na", "n/a", "null")] <- NA_character_
  unexpected <- setdiff(unique(v[!is.na(v)]), c("0", "1", "no", "yes"))
  if (length(unexpected)) stop(name, ": unexpected outcome codes: ",
                               paste(unexpected, collapse = ", "))
  ifelse(is.na(v), NA_integer_, as.integer(v %in% c("1", "yes")))
}
for (v in logistic_outcomes) logistic_dat[[v]] <- logistic_binary01(logistic_dat[[v]], v)

# Covariates that are categories are modeled as factors; age and BMI numeric.
logistic_factors <- c("group", "mother_nulliparity", "mother_art",
                      "mother_smoking", "mother_education")
for (v in logistic_factors) {
  x <- trimws(as.character(logistic_dat[[v]]))
  x[tolower(x) %in% c("", "na", "n/a", "null")] <- NA_character_
  logistic_dat[[v]] <- factor(x)
}
for (v in c("mother_age", "mother_bmi_pre")) {
  original <- logistic_dat[[v]]
  converted <- suppressWarnings(as.numeric(gsub(",", ".", as.character(original), fixed = TRUE)))
  if (any(is.na(converted) & !is.na(original))) stop("Non-numeric covariate: ", v)
  logistic_dat[[v]] <- converted
}

logistic_scales <- setNames(ifelse(grepl("_dif_", logistic_continuous), 25, 100),
                            logistic_continuous)
for (v in logistic_continuous) {
  stopifnot(is.numeric(logistic_dat[[v]]))
  logistic_dat[[paste0(v, "_scaled")]] <- logistic_dat[[v]] / logistic_scales[[v]]
}
for (v in logistic_binary) {
  x <- suppressWarnings(as.integer(as.character(logistic_dat[[v]])))
  if (any(!is.na(logistic_dat[[v]]) & is.na(x)) ||
      any(!is.na(x) & !(x %in% 0:1))) stop("Invalid binary exposure: ", v)
  logistic_dat[[v]] <- x
}

# Use the existing longitudinal eligibility definition for GW35 exposures.
# All other exposures use the baseline analytical sample.
logistic_sample <- function(exposure) {
  if (grepl("_gw35", exposure, fixed = TRUE)) {
    logistic_dat[!is.na(logistic_dat$data_ltpa_final_ok) &
                   logistic_dat$data_ltpa_final_ok == 1, , drop = FALSE]
  } else logistic_dat
}

# Models are fitted on model-specific complete cases; the Excel file records
# N/events for each model so changes in denominators are transparent.
logistic_fit <- function(data, outcome, exposure, covariates) {
  variables <- c(outcome, exposure, covariates)
  d <- data[stats::complete.cases(data[, variables, drop = FALSE]),
            variables, drop = FALSE]
  for (v in intersect(logistic_factors, names(d))) d[[v]] <- droplevels(d[[v]])
  n <- nrow(d)
  events <- if (n) sum(d[[outcome]] == 1L) else 0L
  non_events <- n - events
  result <- list(fit = NULL, data = d, n = n, events = events,
                 non_events = non_events, status = "OK", warning = "")
  if (n < 10 || events < 2 || non_events < 2) {
    result$status <- "Insufficient observations/events"
    return(result)
  }
  if (length(unique(d[[exposure]])) < 2L) {
    result$status <- "Exposure has no variation"
    return(result)
  }
  constant <- covariates[vapply(covariates, function(v)
    length(unique(d[[v]])) < 2L, logical(1))]
  if (length(constant)) {
    result$status <- paste("Constant covariate:", paste(constant, collapse = ", "))
    return(result)
  }
  f <- stats::reformulate(c(exposure, covariates), response = outcome)
  warnings <- character()
  fit <- tryCatch(withCallingHandlers(
    stats::glm(f, data = d, family = stats::binomial()),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) e)
  if (inherits(fit, "error")) {
    result$status <- paste("Model error:", conditionMessage(fit))
    return(result)
  }
  result$warning <- paste(unique(warnings), collapse = " | ")
  co <- summary(fit)$coefficients
  if (!(exposure %in% rownames(co)) ||
      any(!is.finite(co[exposure, c(1, 2, 4)]))) {
    result$status <- "Exposure coefficient not estimable"
    return(result)
  }
  # Flag quasi/complete separation, nonconvergence and unstable estimates.
  if (!isTRUE(fit$converged) || any(grepl("fitted probabilities numerically", warnings))) {
    result$status <- "Nonconvergence / possible separation"
    return(result)
  }
  result$fit <- fit
  result
}

logistic_rows <- vector("list", length(logistic_outcomes) *
                        length(logistic_exposures) * 4L)
row_index <- 0L
for (outcome in logistic_outcomes) {
  for (exposure in logistic_exposures) {
    is_cont <- exposure %in% logistic_continuous
    model_exposure <- if (is_cont) paste0(exposure, "_scaled") else exposure
    base_data <- logistic_sample(exposure)
    for (model_name in names(logistic_adjustment)) {
      row_index <- row_index + 1L
      result <- logistic_fit(base_data, outcome, model_exposure,
                             logistic_adjustment[[model_name]])
      beta <- se_beta <- p <- odds <- lower <- upper <- NA_real_
      pretty <- NA_character_
      if (!is.null(result$fit)) {
        co <- summary(result$fit)$coefficients[model_exposure, ]
        beta <- unname(co[1]); se_beta <- unname(co[2]); p <- unname(co[4])
        odds <- exp(beta)
        lower <- exp(beta - qnorm(.975) * se_beta)
        upper <- exp(beta + qnorm(.975) * se_beta)
        pretty <- risk_se_ic_guapa2(beta, se_beta)
      }
      logistic_rows[[row_index]] <- data.frame(
        outcome = outcome, exposure = exposure,
        exposure_type = if (is_cont) "Continuous" else "Binary (1 vs 0)",
        contrast = if (is_cont) paste0("Per +", logistic_scales[[exposure]]) else "1 vs 0",
        model = model_name, n = result$n, events = result$events,
        non_events = result$non_events,
        OR_95CI = pretty, beta_logOR = beta, SE_logOR = se_beta,
        OR = odds, CI95_lower = lower, CI95_upper = upper,
        p_value = p, p_formatted = if (is.finite(p)) pval_guapa(p) else NA_character_,
        status = result$status, warning = result$warning,
        stringsAsFactors = FALSE
      )
    }
  }
  cat("Logistic models completed for outcome:", outcome, "\n")
}
logistic_results <- do.call(rbind, logistic_rows)
stopifnot(nrow(logistic_results) == length(logistic_outcomes) *
            length(logistic_exposures) * length(logistic_adjustment))

# Excel: same columns and format as LTPA_logistic_regressions.csv.
# Outcomes grouped first; within each outcome, exposures follow the requested order.
# Four consecutive rows for Models 1-4 within each outcome/exposure block.
workbook <- openxlsx::createWorkbook()
for (sheet in c("All_logistic", "Continuous", "Binary", "Model_definitions")) {
  openxlsx::addWorksheet(workbook, sheet)
}
openxlsx::writeData(workbook, "All_logistic", logistic_results)
openxlsx::writeData(workbook, "Continuous",
                    logistic_results[logistic_results$exposure_type == "Continuous", ])
openxlsx::writeData(workbook, "Binary",
                    logistic_results[logistic_results$exposure_type == "Binary (1 vs 0)", ])
model_definitions <- data.frame(
  model = names(logistic_adjustment),
  adjustment = vapply(logistic_adjustment,
                      function(v) if (!length(v)) "Unadjusted" else paste(v, collapse = " + "),
                      character(1)), stringsAsFactors = FALSE)
openxlsx::writeData(workbook, "Model_definitions", model_definitions)
for (sheet in names(workbook)) {
  openxlsx::freezePane(workbook, sheet, firstRow = TRUE)
  openxlsx::setColWidths(workbook, sheet, cols = 1:25, widths = "auto")
}
logistic_excel <- file.path(logistic_path, "LTPA_logistic_regressions.xlsx")
openxlsx::saveWorkbook(workbook, logistic_excel, overwrite = TRUE)

write.table(logistic_results, file.path(logistic_path, "LTPA_logistic_regressions.csv"),
            sep = ";", dec = ".", row.names = FALSE, quote = TRUE, na = "NA")
cat("\nLOGISTIC ANALYSES COMPLETED\n",
    "Regression rows:", nrow(logistic_results), "\n",
    "Excel:", logistic_excel, "\n")
cat("Models with estimation issues:\n")
print(table(logistic_results$status))


############################################################
### FOREST PLOTS: MVLTPA AND LLTPA (PANELS A-D) ###
############################################################
# Run after the logistic regressions above. Uses their numerical
# OR/CI columns and the original formatted OR_95CI text.
# Generates A-D and a combined 2x2 figure for EACH outcome.
# Required packages: ggplot2, cowplot, ragg (install if needed).

forest_path <- file.path(output_path, "Forest_plots")
dir.create(forest_path, recursive = TRUE, showWarnings = FALSE)

forest_colors <- c(Model1 = "#FF62BD", Model2 = "#9490FF",
                   Model3 = "#00BFC4", Model4 = "#39B600")
forest_models <- names(forest_colors)

# Labels are mapped by variable name, not by row number.
forest_labels <- c(
  mvltpa_prepreg = "MVLTPA,\nusual,\n+100 METs·min/w",
  mvltpa_gw20 = "MVLTPA,\nmid-pregnancy,\n+100 METs·min/w",
  mvltpa_dif_gw20 = "MVLTPA,\nmid-preg. − usual,\n+25 METs·min/w",
  mvltpa_gw35 = "MVLTPA,\nlate-pregnancy,\n+100 METs·min/w",
  mvltpa_dif_gw35 = "MVLTPA,\nlate-preg. − usual,\n+25 METs·min/w",
  mvltpa_prepreg_cat = "MVLTPA,\nusual,\nadequate vs. insuf.",
  mvltpa_gw20_cat = "MVLTPA,\nmid-pregnancy,\nadequate vs. insuf.",
  mvltpa_gw35_cat = "MVLTPA,\nlate-pregnancy,\nadequate vs. insuf.",
  lltpa_prepreg = "LLTPA,\nusual,\n+100 min/w",
  lltpa_gw20 = "LLTPA,\nmid-pregnancy,\n+100 min/w",
  lltpa_dif_gw20 = "LLTPA,\nmid-preg. − usual,\n+25 min/w",
  lltpa_gw35 = "LLTPA,\nlate-pregnancy,\n+100 min/w",
  lltpa_dif_gw35 = "LLTPA,\nlate-preg. − usual,\n+25 min/w",
  lltpa_prepreg_cat = "LLTPA,\nusual,\nadequate vs. insuf.",
  lltpa_gw20_cat = "LLTPA,\nmid-pregnancy,\nadequate vs. insuf.",
  lltpa_gw35_cat = "LLTPA,\nlate-pregnancy,\nadequate vs. insuf."
)

forest_panels <- list(
  A = c("mvltpa_prepreg", "mvltpa_gw20", "mvltpa_dif_gw20",
        "mvltpa_gw35", "mvltpa_dif_gw35"),
  B = c("mvltpa_prepreg_cat", "mvltpa_gw20_cat", "mvltpa_gw35_cat"),
  C = c("lltpa_prepreg", "lltpa_gw20", "lltpa_dif_gw20",
        "lltpa_gw35", "lltpa_dif_gw35"),
  D = c("lltpa_prepreg_cat", "lltpa_gw20_cat", "lltpa_gw35_cat")
)

stopifnot(all(unlist(forest_panels) %in% names(forest_labels)),
          all(c("outcome", "exposure", "model", "OR", "CI95_lower",
                "CI95_upper", "OR_95CI") %in% names(logistic_results)))


forest_panel_titles <- c(
  A = "Moderate-to-vigorous leisure-time physical activity, continuous",
  B = "Moderate-to-vigorous leisure-time physical activity, categorical",
  C = "Light-intensity leisure-time physical activity, continuous",
  D = "Light-intensity leisure-time physical activity, categorical"
)

forest_outcome_labels <- c(
  outc_sga_p10 = "Small for gestational age (<10th perc.), odds ratio (95% CI)",
  outc_apo = "All adverse pregnancy outcomes, odds ratio (95% CI)",
  outc_pe = "Pre-eclampsia/eclampsia, odds ratio (95% CI)",
  outc_pe_late = "Late pre-eclampsia/eclampsia, odds ratio (95% CI)",
  outc_hdp = "Hypertensive disorders of pregnancy, odds ratio (95% CI)",
  outc_sga_pe = "SGA (<10th perc.) and pre-eclampsia, odds ratio (95% CI)",
  outc_sga_severe_p3 = "Small for gestational age (<3rd perc.), odds ratio (95% CI)",
  outc_ptb = "Preterm birth, odds ratio (95% CI)",
  outc_nicu = "Neonatal Intensive Care Unit admission, odds ratio (95% CI)",
  outc_neonatal_metab_acidosis = "Neonatal metabolic acidosis, odds ratio (95% CI)",
  outc_apgar7 = "Low 5-minute Apgar score (<7), odds ratio (95% CI)",
  outc_laborcomp = "Neonatal complications during delivery, odds ratio (95% CI)",
  outc_lga_p90 = "Large for gestational age (>90th perc.), odds ratio (95% CI)",
  outc_gdm = "Gestational diabetes mellitus, odds ratio (95% CI)"
)

# For every outcome and panel, find the actual minimum lower 95% CI
# and maximum upper 95% CI, then add 15% padding on EACH side.
# Padding is calculated on the log-OR scale (consistent with the log10 axis),
# so the additional space is proportional on both sides of the forest.
forest_panel_limits <- function(d) {
  good <- is.finite(d$CI95_lower) & is.finite(d$CI95_upper) &
    d$CI95_lower > 0 & d$CI95_upper > 0 &
    d$CI95_lower <= d$CI95_upper
  if (!any(good)) return(c(0.8, 1.25))
  observed_min <- min(1, min(d$CI95_lower[good]))
  observed_max <- max(1, max(d$CI95_upper[good]))
  log_min <- log(observed_min)
  log_max <- log(observed_max)
  log_span <- log_max - log_min
  if (log_span < 1e-8) log_span <- 0.04
  exp(c(log_min - 0.15 * log_span,
        log_max + 0.15 * log_span))
}

# The graphical forest occupies the same available width in every panel.
# The OR (95% CI) labels sit OUTSIDE the x-axis limits, in a fixed right
# margin, so that space is not wasted inside the forest plotting area.
forest_plot <- function(d, exposure_names, panel, xlimits, outcome_axis_label) {
  d <- d[d$exposure %in% exposure_names, , drop = FALSE]
  d$exposure <- factor(d$exposure, levels = exposure_names)
  d$model <- factor(d$model, levels = rev(forest_models))
  d$label <- factor(forest_labels[as.character(d$exposure)],
                    levels = forest_labels[exposure_names])
  d$OR_95CI <- ifelse(is.na(d$OR_95CI), "Not estimable", d$OR_95CI)
  
  # 20% less separation than the previous 0.75-unit spacing.
  model_step <- 0.60
  d$y_pos <- as.numeric(d$model) * model_step
  
  # Place coefficients inside the canvas, to avoid a large outer margin.
  # Position in log units so placement works for narrow and wide OR ranges.
  log_span <- log(xlimits[2] / xlimits[1])
  if (log_span < 1e-8) {
    xlimits <- exp(log(xlimits[1]) + c(-0.02, 0.02))
    log_span <- log(xlimits[2] / xlimits[1])
  }
  # Allocate a compact, fixed-width text column within the plotting canvas.
  text_x <- exp(log(xlimits[2]) + 0.28 * log_span)
  full_limits <- c(xlimits[1], exp(log(xlimits[2]) + 0.34 * log_span))
  
  valid <- is.finite(d$OR) & is.finite(d$CI95_lower) &
    is.finite(d$CI95_upper) & d$OR > 0 &
    d$CI95_lower > 0 & d$CI95_upper > 0
  draw <- d[valid, , drop = FALSE]
  
  ggplot2::ggplot(d, ggplot2::aes(y = y_pos)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                        colour = "grey50", linewidth = 0.4) +
    ggplot2::geom_segment(data = draw,
                          ggplot2::aes(x = CI95_lower, xend = CI95_upper,
                                       yend = y_pos, colour = model),
                          linewidth = 0.75, na.rm = TRUE) +
    ggplot2::geom_point(data = draw,
                        ggplot2::aes(x = OR, colour = model),
                        shape = 15, size = 2.1, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(x = text_x, label = OR_95CI),
                       hjust = 1, size = 3.575, colour = "black", na.rm = TRUE) +
    ggplot2::facet_grid(rows = ggplot2::vars(label), switch = "y",
                        scales = "free_y", space = "free_y", drop = FALSE) +
    ggplot2::scale_colour_manual(values = forest_colors, drop = FALSE,
                                 labels = c(Model1 = "Model 1", Model2 = "Model 2",
                                            Model3 = "Model 3", Model4 = "Model 4")) +
    ggplot2::scale_x_log10(
      breaks = scales::breaks_log(n = 5),
      labels = scales::label_number(accuracy = 0.01)) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(forest_models) * model_step,
      labels = paste("Model", 4:1),
      limits = c(model_step * 0.55, model_step * 4.45),
      expand = ggplot2::expansion(add = 0)) +
    ggplot2::labs(title = paste0(panel, ". ", forest_panel_titles[[panel]]),
                  x = outcome_axis_label, y = NULL, colour = "Model") +
    # Include the coefficient text column within the exported plot bounds.
    ggplot2::coord_cartesian(xlim = full_limits, clip = "off", expand = FALSE) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, colour = "grey85", linewidth = 0.25),
      strip.placement = "outside",
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1,
                                                size = 8, lineheight = 0.95),
      axis.text.y = ggplot2::element_text(size = 7.5),
      axis.text.x = ggplot2::element_text(size = 8),
      axis.title.x = ggplot2::element_text(size = 9),
      panel.spacing.y = grid::unit(0.06, "lines"),
      plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0),
      plot.title.position = "plot",
      # Small external margins prevent blank strips in merged figures.
      plot.margin = ggplot2::margin(6, 6, 6, 6),
      legend.position = "none")
}

forest_save <- function(p, filename, width, height) {
  ggplot2::ggsave(filename, plot = p, device = ragg::agg_png,
                  width = width, height = height, units = "in",
                  dpi = 1200, bg = "white", limitsize = FALSE)
}

for (outcome_name in unique(logistic_results$outcome)) {
  outcome_data <- logistic_results[
    logistic_results$outcome == outcome_name &
      logistic_results$exposure %in% unlist(forest_panels), , drop = FALSE]
  
  if (!any(is.finite(outcome_data$OR) & outcome_data$OR > 0)) {
    warning("No valid odds ratios for outcome: ", outcome_name)
    next
  }
  outcome_axis_label <- unname(forest_outcome_labels[outcome_name])
  if (is.na(outcome_axis_label)) stop("Missing outcome label: ", outcome_name)
  
  plots <- lapply(names(forest_panels), function(panel) {
    panel_data <- outcome_data[outcome_data$exposure %in% forest_panels[[panel]],
                               , drop = FALSE]
    panel_limits <- forest_panel_limits(panel_data)
    forest_plot(panel_data, forest_panels[[panel]], panel,
                panel_limits, outcome_axis_label)
  })
  names(plots) <- names(forest_panels)
  
  outcome_dir <- file.path(forest_path, outcome_name)
  dir.create(outcome_dir, recursive = TRUE, showWarnings = FALSE)
  for (panel in names(plots)) {
    panel_height <- if (panel %in% c("A", "C")) 5.4 else 3.7
    forest_save(plots[[panel]],
                file.path(outcome_dir, paste0("Forest_", panel, "_", outcome_name, ".png")),
                width = 7, height = panel_height)
  }
  
  # Top row A | C; bottom row B | D, as requested.
  top <- cowplot::plot_grid(plots$A, plots$C, ncol = 2, align = "h", axis = "tb")
  bottom <- cowplot::plot_grid(plots$B, plots$D, ncol = 2, align = "h", axis = "tb")
  combined <- cowplot::plot_grid(top, bottom, ncol = 1,
                                 rel_heights = c(5.4, 3.7))
  forest_save(combined,
              file.path(outcome_dir, paste0("Forest_ABCD_", outcome_name, ".png")),
              width = 14, height = 9.1)
  cat("Forest plots A-D and combined figure saved for:", outcome_name, "\n")
}


### SPLINES ###
###############

setwd("D:/Artículos/Sara Castro - IMPACT-BCN LTPA")
load("./Data/IMPACT_BCN_LTPA_analysis_ready.RData")

vars00<-c("data_ltpa_baseline_ok","data_ltpa_baseline_ok","data_ltpa_final_ok",
          "data_ltpa_baseline_ok","data_ltpa_baseline_ok","data_ltpa_final_ok")
vars01<-c("mvltpa_prepreg","mvltpa_gw20","mvltpa_gw35",
          "lltpa_prepreg","lltpa_gw20","lltpa_gw35")
vars02<-c("Moderate-to-vigorous LTPA (usual),\nMETs·min/w",
          "Moderate-to-vigorous LTPA (mid-pregnancy),\nMETs·min/w",
          "Moderate-to-vigorous LTPA (late-pregnancy),\nMETs·min/w",
          "Light-intensity LTPA (usual),\nmin/w",
          "Light-intensity LTPA (mid-pregnancy),\nmin/w",
          "Light-intensity LTPA (late-pregnancy),\nmin/w")
vars03<-c("outc_sga_p10","outc_apo",                         
          "outc_pe","outc_pe_late","outc_hdp","outc_sga_pe","outc_sga_severe_p3","outc_ptb",
          "outc_nicu","outc_neonatal_metab_acidosis","outc_apgar7","outc_laborcomp","outc_lga_p90","outc_gdm")
vars04<-c("Small for gestational age (<10th perc.),\nodds ratio (95% CI)",
          "All adverse pregnancy outcomes,\nodds ratio (95% CI)",
          "Pre-eclampsia/eclampsia,\nodds ratio (95% CI)",
          "Late pre-eclampsia/eclampsia,\nodds ratio (95% CI)",
          "Hypertensive disorders of pregnancy,\nodds ratio (95% CI)",
          "SGA (<10th perc.) and pre-eclampsia,\nodds ratio (95% CI)",
          "Small for gestational age (<3rd perc.),\nodds ratio (95% CI)",
          "Preterm birth,\nodds ratio (95% CI)",
          "Neonatal Intensive Care Unit admission,\nodds ratio (95% CI)",
          "Neonatal metabolic acidosis,\nodds ratio (95% CI)",
          "Low 5-minute Apgar score (<7),\nodds ratio (95% CI)",
          "Neonatal complications during delivery,\nodds ratio (95% CI)",
          "Large for gestational age (>90th perc.),\nodds ratio (95% CI)",
          "Gestational diabetes mellitus,\nodds ratio (95% CI)")

tab<-NULL

for(i in 1:length(vars01))
{
  for(j in 1:length(vars03))
  {
    datx<-dat[dat[,vars00[i]]==1,]
    datx$aaa <- datx[[vars01[i]]]
    datx$outcome <- datx[[vars03[j]]]
    #aaa<-log1p(datx[,vars01[i]])
    mod_lin <- glm(
      as.factor(outcome) ~ aaa +
        as.factor(group) +
        mother_age +
        as.factor(mother_nulliparity) +
        as.factor(mother_art) +
        mother_bmi_pre +
        as.factor(mother_education) +
        as.factor(mother_smoking),
      data = datx,
      family = binomial()
    )
    mod_nlin <- glm(
      as.factor(outcome) ~ splines::bs(aaa, df = 4) +
        as.factor(group) +
        mother_age +
        as.factor(mother_nulliparity) +
        as.factor(mother_art) +
        mother_bmi_pre +
        as.factor(mother_education) +
        as.factor(mother_smoking),
      data = datx,
      family = binomial()
    )
    p_nonlin_lrtest<-pval_guapa(lrtest(mod_lin,mod_nlin)[2,5])
    
    # Reference value: minimum observed LTPA
    ref <- min(datx$aaa, na.rm = TRUE)
    
    # Prediction grid
    xgrid <- seq(
      min(datx$aaa, na.rm = TRUE),
      max(datx$aaa, na.rm = TRUE),
      length.out = 200
    )
    
    # Create reference covariate profile
    newdata <- datx[rep(1, length(xgrid)), , drop = FALSE]
    newdata$aaa <- xgrid
    
    # Reference prediction
    refdata <- newdata[1, , drop = FALSE]
    refdata$aaa <- ref
    
    # Obtain spline design matrices
    X <- model.matrix(
      delete.response(terms(mod_nlin)),
      data = newdata
    )
    
    Xref <- model.matrix(
      delete.response(terms(mod_nlin)),
      data = refdata
    )
    
    # Differences relative to reference
    Xdiff <- sweep(X, 2, Xref[1, ], "-")
    
    # Log odds ratios and standard errors
    beta <- coef(mod_nlin)
    V <- vcov(mod_nlin)
    
    logOR <- as.vector(Xdiff %*% beta)
    
    SE <- sqrt(
      rowSums((Xdiff %*% V) * Xdiff)
    )
    
    # Odds ratios and 95% confidence intervals
    plot.data <- data.frame(
      x = xgrid,
      yest = exp(logOR),
      lci = exp(logOR - 1.96 * SE),
      uci = exp(logOR + 1.96 * SE)
    )
    #plot.data$x<-expm1(plot.data$x)
    
    #plot.data<-plot.data[plot.data$lci>=0.1 & plot.data$uci<=3,]
    
    name<-paste("./Outputs/Splines/",vars01[i],"_",vars03[j],".jpg",sep="")
    labely<-vars04[j]
    labelx<-vars02[i]
    
    p_lin2<-pval_guapa(as.numeric(summary(mod_lin)$coefficients[2,4]))
    p_nonlin2<-pval_guapa(lrtest(mod_lin,mod_nlin)[2,5])
    
    p_lin2<-ifelse(p_lin2=="<0.001"," < 0.001",
                   ifelse(p_lin2=="<0.00001"," < 0.00001",paste(" = ",p_lin2,sep="")))
    p_nonlin2<-ifelse(p_nonlin2=="<0.001"," < 0.001",
                      ifelse(p_nonlin2=="<0.00001"," < 0.00001",paste(" = ",p_nonlin2,sep="")))
    leg<-paste("p-value for linearity",p_lin2,"\np-value for non-linearity",p_nonlin2,sep="")
    
    xmax <- if (grepl("^mvltpa_", vars01[i])) 3000 else 1000
    figure <- ggplot(plot.data, aes(x = x, y = yest)) +
      geom_ribbon(
        aes(ymin = lci, ymax = uci),
        alpha = 0.25,
        fill = "grey50",
        colour = NA
      ) +
      geom_line(color = "black", linewidth = 1) +
      geom_hline(yintercept = 1, linetype = 2) +
      theme_bw() +
      scale_x_continuous(
        limits = NULL,
        breaks = pretty(c(0, xmax), n = 6),
        expand = expansion(mult = 0)
      ) +
      scale_y_continuous(
        trans = scales::pseudo_log_trans(sigma = 0.1, base = 10),
        breaks = c(0, 0.12, 0.25, 0.5, 1, 2, 4, 8),
        expand = expansion(mult = 0)
      ) +
      coord_cartesian(
        xlim = c(0, xmax),
        ylim = c(0, 8),
        expand = FALSE
      ) +
      labs(x = labelx, y = labely) +
      annotate(
        "text",
        x = xmax * 0.97,
        y = 3.8,
        label = leg,
        hjust = 1,
        vjust = 1,
        size = 5,
        colour = "black"
      ) +
      theme(
        axis.title.x = element_text(vjust = 0.5, size = 18, face = "bold"),
        axis.title.y = element_text(vjust = 0.5, size = 18, face = "bold"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        plot.margin = margin(15, 25, 15, 15)
      )
    
    ggsave(filename = name, plot = figure, width = 8, height = 8, units = "in", dpi = 1200)
    figure
  }
}


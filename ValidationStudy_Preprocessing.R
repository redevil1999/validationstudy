###
# title: "ValidationData"
# author: "RobinDevillers"
# date: "2024-09-19"
# output: html_document
# ---

#   {r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)
#

#This is to read the validation study data

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)

#
# #from https://www.geeksforgeeks.org/how-to-check-multiple-r-columns-for-a-value/
#   # Define a custom function
#   check_value_in_columns <- function(row, value) {
#     return(any(row == value))
#   }

#
# check_multians_columns <- function(df, column) {
#   return(which(!is.na(df[column])))
# }


#read the data

firstdatacheck <- read_excel("data/data_11122024.xlsx")


#change column name first and second column

names(firstdatacheck)[1] <- "session_id"
names(firstdatacheck)[2] <- "date_created"


#check for duplicates

dup <- duplicated(firstdatacheck$submission_id)


#find the unique values per column

ulst <- lapply(firstdatacheck, unique)


#--------------------------------------------------------------------------------
#check for multiple answers and add to list

#list:

#That are:
#Jeg har en positiv f??lelse inne meg..72,73,74,75

which(!is.na(firstdatacheck[72]))
which(!is.na(firstdatacheck[73]))
which(!is.na(firstdatacheck[74]))
which(!is.na(firstdatacheck[75]))

which(!is.na(firstdatacheck[72]) & !is.na(firstdatacheck[73]))
which(!is.na(firstdatacheck[73]) & !is.na(firstdatacheck[74]))
which(!is.na(firstdatacheck[74]) & !is.na(firstdatacheck[75]))
# 73

MultipleAnswer_List <- 73


#Jeg f??ler meg nerv??s og urolig.. 76,77,78,79

which(!is.na(firstdatacheck[76]))
which(!is.na(firstdatacheck[77]))
which(!is.na(firstdatacheck[78]))
which(!is.na(firstdatacheck[79]))

which(!is.na(firstdatacheck[76]) & !is.na(firstdatacheck[77]))
which(!is.na(firstdatacheck[77]) & !is.na(firstdatacheck[78]))
which(!is.na(firstdatacheck[78]) & !is.na(firstdatacheck[79]))
# 15, 59, 9, 40, 54, 65

newelems <- c(15, 59, 9, 40, 54, 65)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)


#Jeg f??ler meg tilfreds med livet mitt..80,81,82,83

which(!is.na(firstdatacheck[80]))
which(!is.na(firstdatacheck[81]))
which(!is.na(firstdatacheck[82]))
which(!is.na(firstdatacheck[83]))

which(!is.na(firstdatacheck[81]) & !is.na(firstdatacheck[82]))
which(!is.na(firstdatacheck[82]) & !is.na(firstdatacheck[83]))
which(!is.na(firstdatacheck[83]) & !is.na(firstdatacheck[84]))

# 73, 4, 30, 3, 4, 21, 27, 40, 48, 54, 55, 65, 74, 7, 8, 15, 27, 43, 53

newelems <- c(73, 4, 30, 3, 4, 21, 27, 40, 48, 54, 55, 65, 74, 7, 8, 15, 27, 43, 53)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

#Jeg ??nsker at jeg kunne v??re like forn??yd med livet mitt som andre ser ut til ?? v??re..84,85,86,87

which(!is.na(firstdatacheck[84]))
which(!is.na(firstdatacheck[85]))
which(!is.na(firstdatacheck[86]))
which(!is.na(firstdatacheck[87]))

which(!is.na(firstdatacheck[84]) & !is.na(firstdatacheck[85]))
which(!is.na(firstdatacheck[85]) & !is.na(firstdatacheck[86]))
which(!is.na(firstdatacheck[86]) & !is.na(firstdatacheck[87]))
# 0

#Jeg f??ler meg mislykket..88,89,90,91

which(!is.na(firstdatacheck[88]))
which(!is.na(firstdatacheck[89]))
which(!is.na(firstdatacheck[90]))
which(!is.na(firstdatacheck[91]))

which(!is.na(firstdatacheck[88]) & !is.na(firstdatacheck[89]))
which(!is.na(firstdatacheck[89]) & !is.na(firstdatacheck[90]))
which(!is.na(firstdatacheck[90]) & !is.na(firstdatacheck[91]))

# 14, 22
newelems <- c(14, 22)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)


#Jeg f??ler meg fredfull..92,93,94,95
which(!is.na(firstdatacheck[92]))
which(!is.na(firstdatacheck[93]))
which(!is.na(firstdatacheck[94]))
which(!is.na(firstdatacheck[95]))

which(!is.na(firstdatacheck[92]) & !is.na(firstdatacheck[93]))
which(!is.na(firstdatacheck[93]) & !is.na(firstdatacheck[94]))
which(!is.na(firstdatacheck[94]) & !is.na(firstdatacheck[95]))
# 64, 7 ,30 

newelems <- c(64, 7, 30)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)


# Jeg er en rolig person og har kontroll over f??lelse mine.96,97,98,99
which(!is.na(firstdatacheck[96]))
which(!is.na(firstdatacheck[97]))
which(!is.na(firstdatacheck[98]))
which(!is.na(firstdatacheck[99]))

which(!is.na(firstdatacheck[96]) & !is.na(firstdatacheck[97]))
which(!is.na(firstdatacheck[97]) & !is.na(firstdatacheck[98]))
which(!is.na(firstdatacheck[98]) & !is.na(firstdatacheck[99]))
# 31, 33

newelems <- c(31, 33)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg f??ler at vanskene hoper seg s?? mye opp at jeg ikke kan hanskes med dem.100,101,102,103
which(!is.na(firstdatacheck[100]))
which(!is.na(firstdatacheck[101]))
which(!is.na(firstdatacheck[102]))
which(!is.na(firstdatacheck[103]))

which(!is.na(firstdatacheck[100]) & !is.na(firstdatacheck[101]))
which(!is.na(firstdatacheck[101]) & !is.na(firstdatacheck[102]))
which(!is.na(firstdatacheck[102]) & !is.na(firstdatacheck[103]))
# 14, 2

newelems <- c(14,2)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg bekymrer meg for mye om ting som egentlig er uviktige.104,105,106,107
which(!is.na(firstdatacheck[104]))
which(!is.na(firstdatacheck[105]))
which(!is.na(firstdatacheck[106]))
which(!is.na(firstdatacheck[107]))

which(!is.na(firstdatacheck[104]) & !is.na(firstdatacheck[105]))
which(!is.na(firstdatacheck[105]) & !is.na(firstdatacheck[106]))
which(!is.na(firstdatacheck[106]) & !is.na(firstdatacheck[107]))
# 33, 40, 54, 26, 53

newelems <- c(33, 40, 54, 26, 53)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg f??ler meg ganske tilfreds med livet mitt.108,109,110,111
which(!is.na(firstdatacheck[108]) & !is.na(firstdatacheck[109]))
which(!is.na(firstdatacheck[109]) & !is.na(firstdatacheck[110]))
which(!is.na(firstdatacheck[110]) & !is.na(firstdatacheck[111]))
# 73

newelems <- c(73)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg har tanker som gj??r meg oppr??rt.112,113,114,115
which(!is.na(firstdatacheck[112]) & !is.na(firstdatacheck[113]))
which(!is.na(firstdatacheck[113]) & !is.na(firstdatacheck[114]))
which(!is.na(firstdatacheck[114]) & !is.na(firstdatacheck[115]))
# 25, 54

newelems <- c(25, 54)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg mangler selvtillit.116,117,118,119
which(!is.na(firstdatacheck[116]) & !is.na(firstdatacheck[117]))
which(!is.na(firstdatacheck[117]) & !is.na(firstdatacheck[118]))
which(!is.na(firstdatacheck[118]) & !is.na(firstdatacheck[119]))
# 49

newelems <- c(49)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg f??ler meg trygg.120,121,122,123
which(!is.na(firstdatacheck[120]) & !is.na(firstdatacheck[121]))
which(!is.na(firstdatacheck[121]) & !is.na(firstdatacheck[122]))
which(!is.na(firstdatacheck[122]) & !is.na(firstdatacheck[123]))
# 22

newelems <- c(22)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg tar lett avgj??relser.124,125,126,127
which(!is.na(firstdatacheck[124]) & !is.na(firstdatacheck[125]))
which(!is.na(firstdatacheck[125]) & !is.na(firstdatacheck[126]))
which(!is.na(firstdatacheck[126]) & !is.na(firstdatacheck[127]))
# 32, 54, 4, 65

newelems <- c(32, 54, 4, 65)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg f??ler meg utilstrekkelig.128,129,130,131
which(!is.na(firstdatacheck[128]) & !is.na(firstdatacheck[129]))
which(!is.na(firstdatacheck[129]) & !is.na(firstdatacheck[130]))
which(!is.na(firstdatacheck[130]) & !is.na(firstdatacheck[131]))
# 0

# Jeg er forn??yd.132,133,134,135
which(!is.na(firstdatacheck[132]) & !is.na(firstdatacheck[133]))
which(!is.na(firstdatacheck[133]) & !is.na(firstdatacheck[134]))
which(!is.na(firstdatacheck[134]) & !is.na(firstdatacheck[135]))
# 45

newelems <- c(45)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Noen uviktige tanker g??r gjennom hodet mitt og plager meg.136,137,138,139
which(!is.na(firstdatacheck[136]) & !is.na(firstdatacheck[137]))
which(!is.na(firstdatacheck[137]) & !is.na(firstdatacheck[138]))
which(!is.na(firstdatacheck[138]) & !is.na(firstdatacheck[139]))
# 73, 64

newelems <- c(73, 64)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg tar skuffelser s?? tungt at jeg ikke kan f?? dem ut av hodet.140,141,142,143
which(!is.na(firstdatacheck[140]) & !is.na(firstdatacheck[141]))
which(!is.na(firstdatacheck[141]) & !is.na(firstdatacheck[142]))
which(!is.na(firstdatacheck[142]) & !is.na(firstdatacheck[143]))
# 50, 6, 2

newelems <- c(50, 6, 2)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg er en st??dig og stabil person.144,145,146,147
which(!is.na(firstdatacheck[144]) & !is.na(firstdatacheck[145]))
which(!is.na(firstdatacheck[145]) & !is.na(firstdatacheck[146]))
which(!is.na(firstdatacheck[146]) & !is.na(firstdatacheck[147]))
# 64, 73

newelems <- c(64, 73)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

# Jeg blir anspent og urolig av ?? tenke p?? ting som opptar meg eller som jeg driver med for tiden. 148,149,150,151
which(!is.na(firstdatacheck[148]) & !is.na(firstdatacheck[149]))
which(!is.na(firstdatacheck[149]) & !is.na(firstdatacheck[150]))
which(!is.na(firstdatacheck[150]) & !is.na(firstdatacheck[151]))
# 23, 33, 73

newelems <- c(23, 33, 73)
MultipleAnswer_List <- c(MultipleAnswer_List, newelems)

#-----------------------------------------------------------------------------

duplicated(MultipleAnswer_List)
Unique_MA_List <- unique(MultipleAnswer_List)

#------------------------------------------------------------------------------
`%notin%` <- function(x,y) !(x %in% y)

# Check how many and throw them out -------------------------------------------
sliced_MA_data <- firstdatacheck %>%
  slice(Unique_MA_List)

filtered_nonMA_data <- firstdatacheck %>%
  filter(row_number() %notin% Unique_MA_List)


# Then split row-wise ----------------------------------------------------------
MA_rows <- filtered_nonMA_data %>%
    filter_at(vars(72:151), any_vars(!is.na(.)))

nonMA_rows <- filtered_nonMA_data %>%
  select(-c(72:151)) %>%
  na.omit()



# Combine columns and bring back together for MA rows --------------------------

#`Jeg har en positiv f??lelse inne meg..Nesten aldri`)

STAI_Q1 <- fixing_MA(MA_rows, 72,73,74,75, STAI_Q1)
names(STAI_Q1)[2] <- "STAI_Q1"


#Jeg f??ler meg nerv??s og urolig.. 76,77,78,79

STAI_Q2 <- fixing_MA(MA_rows, 76,77,78,79, STAI_Q2)
names(STAI_Q2)[2] <- "STAI_Q2"

#Jeg f??ler meg tilfreds med livet mitt..80,81,82,83

STAI_Q3 <- fixing_MA(MA_rows, 80,81,82,83, STAI_Q3)
names(STAI_Q3)[2] <- "STAI_Q3"

#Jeg ??nsker at jeg kunne v??re like forn??yd med livet mitt som andre ser ut til ?? v??re..84,85,86,87

STAI_Q4 <- fixing_MA(MA_rows, 84,85,86,87, STAI_Q4)
names(STAI_Q4)[2] <- "STAI_Q4"

#Jeg f??ler meg mislykket..88,89,90,91

STAI_Q5 <- fixing_MA(MA_rows, 88,89, 90, 91, STAI_Q5)
names(STAI_Q5)[2] <- "STAI_Q5"


#Jeg f??ler meg fredfull..92,93,94,95
STAI_Q6 <- fixing_MA(MA_rows, 92,93,94,95, STAI_Q6)
names(STAI_Q6)[2] <- "STAI_Q6"


# Jeg er en rolig person og har kontroll over f??lelse mine.96,97,98,99
STAI_Q7 <- fixing_MA(MA_rows, 96,97,98,99, STAI_Q7)
names(STAI_Q7)[2] <- "STAI_Q7"

# Jeg f??ler at vanskene hoper seg s?? mye opp at jeg ikke kan hanskes med dem.100,101,102,103
STAI_Q8 <- fixing_MA(MA_rows, 100,101,102,103, STAI_Q8)
names(STAI_Q8)[2] <- "STAI_Q8"

# Jeg bekymrer meg for mye om ting som egentlig er uviktige.104,105,106,107
STAI_Q9 <- fixing_MA(MA_rows, 104,105,106,107, STAI_Q9)
names(STAI_Q9)[2] <- "STAI_Q9"

# Jeg f??ler meg ganske tilfreds med livet mitt.108,109,110,111
STAI_Q10 <- fixing_MA(MA_rows, 108,109,110,111, STAI_Q10)
names(STAI_Q10)[2] <- "STAI_Q10"

# Jeg har tanker som gj??r meg oppr??rt.112,113,114,115
STAI_Q11 <- fixing_MA(MA_rows, 112,113,114,115, STAI_Q11)
names(STAI_Q11)[2] <- "STAI_Q11"

# Jeg mangler selvtillit.116,117,118,119
STAI_Q12 <- fixing_MA(MA_rows, 116,117,118,119, STAI_Q12)
names(STAI_Q12)[2] <- "STAI_Q12"

# Jeg f??ler meg trygg.120,121,122,123
STAI_Q13 <- fixing_MA(MA_rows, 120,121,122,123, STAI_Q13)
names(STAI_Q13)[2] <- "STAI_Q13"

# Jeg tar lett avgj??relser.124,125,126,127
STAI_Q14 <- fixing_MA(MA_rows, 124,125,126,127, STAI_Q14)
names(STAI_Q14)[2] <- "STAI_Q14"

# Jeg f??ler meg utilstrekkelig.128,129,130,131
STAI_Q15 <- fixing_MA(MA_rows, 128,129,130,131, STAI_Q15)
names(STAI_Q15)[2] <- "STAI_Q15"

# Jeg er forn??yd.132,133,134,135
STAI_Q16 <- fixing_MA(MA_rows, 132,133,134,135, STAI_Q16)
names(STAI_Q16)[2] <- "STAI_Q16"

# Noen uviktige tanker g??r gjennom hodet mitt og plager meg.136,137,138,139
STAI_Q17 <- fixing_MA(MA_rows, 136,137,138,139, STAI_Q17)
names(STAI_Q17)[2] <- "STAI_Q17"

# Jeg tar skuffelser s?? tungt at jeg ikke kan f?? dem ut av hodet.140,141,142,143
STAI_Q18 <- fixing_MA(MA_rows, 140,141,142,143, STAI_Q18)
names(STAI_Q18)[2] <- "STAI_Q18"

# Jeg er en st??dig og stabil person.144,145,146,147
STAI_Q19 <- fixing_MA(MA_rows, 144,145,146,147, STAI_Q19)
names(STAI_Q19)[2] <- "STAI_Q19"

# Jeg blir anspent og urolig av ?? tenke p?? ting som opptar meg eller som jeg driver med for tiden. 148,149,150,151
STAI_Q20 <- fixing_MA(MA_rows, 148,149,150,151, STAI_Q20)
names(STAI_Q20)[2] <- "STAI_Q20"


# bind the columns together
STAI_questionnaire <- merge(STAI_Q1, STAI_Q2, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q3, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q4, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q5, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q6, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q7, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q8, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q9, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q10, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q11, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q12, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q13, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q14, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q15, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q16, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q17, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q18, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q19, by = "session_id")
STAI_questionnaire <- merge(STAI_questionnaire, STAI_Q20, by = "session_id")


MA_rows2 <- MA_rows %>% select(-c(72:171))
MA_rows_complete <- merge(MA_rows2, STAI_questionnaire, by = "session_id")

#---------------------------------------------------------------------------------
#rename the columns from the nonMA rows dataframe

STAI_colnumbers = c(72:91)
STAI_colnames = c("STAI_Q1", "STAI_Q2", "STAI_Q3", "STAI_Q4", "STAI_Q5", "STAI_Q6", "STAI_Q7", "STAI_Q8", "STAI_Q9", "STAI_Q10", "STAI_Q11", "STAI_Q12", "STAI_Q13", "STAI_Q14", 
                  "STAI_Q15", "STAI_Q16", "STAI_Q17", "STAI_Q18", "STAI_Q19", "STAI_Q20")

names(nonMA_rows)[72:91] <- STAI_colnames

#---------------------------------------------------------------------------------
# bind the sets together rowwise

complete_dataset <- bind_rows(MA_rows_complete, nonMA_rows)

#--------------------------------------------------------------------------------
#rename all the columns
names(complete_dataset)[3] <- "consent"
names(complete_dataset)[4] <- "age"
names(complete_dataset)[5] <- "gender"
names(complete_dataset)[6] <- "education"

# RST-PQ questions
RSTPQ_colnames = c("RSTPQ_Q1", "RSTPQ_Q2", "RSTPQ_Q3", "RSTPQ_Q4", "RSTPQ_Q5", "RSTPQ_Q6", "RSTPQ_Q7", "RSTPQ_Q8", "RSTPQ_Q9", "RSTPQ_Q10", "RSTPQ_Q11", "RSTPQ_Q12", "RSTPQ_Q13",
                   "RSTPQ_Q14", "RSTPQ_Q15", "RSTPQ_Q16", "RSTPQ_Q17", "RSTPQ_Q18", "RSTPQ_Q19", "RSTPQ_Q20", "RSTPQ_Q21", "RSTPQ_Q22", "RSTPQ_Q23", "RSTPQ_Q24", "RSTPQ_Q25",
                   "RSTPQ_Q26", "RSTPQ_Q27", "RSTPQ_Q28", "RSTPQ_Q29", "RSTPQ_Q30", "RSTPQ_Q31", "RSTPQ_Q32", "RSTPQ_Q33", "RSTPQ_Q34", "RSTPQ_Q35", "RSTPQ_Q36", "RSTPQ_Q37",
                   "RSTPQ_Q38", "RSTPQ_Q39", "RSTPQ_Q40", "RSTPQ_Q41", "RSTPQ_Q42", "RSTPQ_Q43", "RSTPQ_Q44", "RSTPQ_Q45", "RSTPQ_Q46", "RSTPQ_Q47", "RSTPQ_Q48", "RSTPQ_Q49",
                   "RSTPQ_Q50", "RSTPQ_Q51", "RSTPQ_Q52", "RSTPQ_Q53", "RSTPQ_Q54", "RSTPQ_Q55", "RSTPQ_Q56", "RSTPQ_Q57", "RSTPQ_Q58", "RSTPQ_Q59", "RSTPQ_Q60", "RSTPQ_Q61",
                   "RSTPQ_Q62", "RSTPQ_Q63", "RSTPQ_Q64", "RSTPQ_Q65")

names(complete_dataset)[7:71] <- RSTPQ_colnames

# BSSS questions
BSSS_colnames = c("BSSS_Q1", "BSSS_Q2", "BSSS_Q3", "BSSS_Q4", "BSSS_Q5", "BSSS_Q6", "BSSS_Q7", "BSSS_Q8")

names(complete_dataset)[72:79] <- BSSS_colnames

# PANAS questions

PANAS_colnames = c("PANAS_Q1", "PANAS_Q2", "PANAS_Q3", "PANAS_Q4", "PANAS_Q5", "PANAS_Q6", "PANAS_Q7", "PANAS_Q8", "PANAS_Q9", "PANAS_Q10")

names(complete_dataset)[80:89] <- PANAS_colnames

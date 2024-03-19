lmHAMD_Speech <-function(wl_csv, HAMD_csv, speechVar, subtask) {
  library(rstatix)
  library(tidyverse)
  library(ggplot2)
  library(ggpmisc)
  journalData <- extract.journalling.data(wl_csv, speechVar, subtask)
  HAMDData <- extract.HAMD(HAMD_csv)
  lmDataframe <- lmPrep(HAMDData, journalData, speechVar)
  baselineLM(lmDataframe)
  lm.plot(lmDataframe,speechVar)
}


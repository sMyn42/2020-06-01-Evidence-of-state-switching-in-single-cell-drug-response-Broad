#!/usr/bin/env Rscript
args = commandArgs(trailingOnly = T)

extends <- methods::extends


library(tidyverse)
library(cytominer)
library(stringr)
library(readr)
library(reshape2)
library(data.table)

print("script started")

top_dir <-'/home/ubuntu/bucket/projects/'
proj_dir <- '2015_10_05_DrugRepurposing_AravindSubramanian_GolubLab_Broad'
batch.name <- '2016_04_01_a549_48hr_batch1'
platemap <- "/home/ubuntu/bucket/projects/2015_10_05_DrugRepurposing_AravindSubramanian_GolubLab_Broad/workspace/metadata/2016_04_01_a549_48hr_batch1/platemap/C-7161-01-LM6-017.txt"

output <- "/home/ubuntu/bucket/projects/2015_10_05_DrugRepurposing_AravindSubramanian_GolubLab_Broad/workspace/backend/2016_04_01_a549_48hr_batch1/"


plate.list <- c("SQ00015201", "SQ00015142", "SQ00015143", "SQ00015144", "SQ00015145")


sql.path <- NULL
for (pl in seq_along(plate.list)) {
  sql.path[pl] <- as.vector(paste0("/home/ubuntu/bucket/projects/2015_10_05_DrugRepurposing_AravindSubramanian_GolubLab_Broad/workspace/backend/", batch.name, "/", plate.list[pl], "/", plate.list[pl], ".sqlite"))
}

print("Step1")

# reading sqlite
read_sql<- function(sql.path) {
  db <- DBI::dbConnect(RSQLite::SQLite(), sql.path)
  RSQLite::initExtension(db)
  
  image <- RSQLite::dbReadTable(conn = db, "Image")
  cells <- RSQLite::dbReadTable(conn = db, "Cells")
  nuclei <- RSQLite::dbReadTable(conn = db, "Nuclei")
  cytoplasm <- RSQLite::dbReadTable(conn = db, "Cytoplasm")
  
  dt <- cells %>%
    left_join(cytoplasm, by = c("TableNumber", "ImageNumber", "ObjectNumber")) %>%
    left_join(nuclei, by = c("TableNumber", "ImageNumber", "ObjectNumber")) %>%
    left_join(image, by = c("TableNumber", "ImageNumber"))
  
  return(dt)
  
}

print("Step2")

for (i in 1:length(sql.path)) {
  meta <- data.frame(read.table(platemap, sep="\t", comment.char="", quote = "\"", header=T))
  colnames(meta) <- paste0("Metadata_", colnames(meta))
  meta$Metadata_Plate <- plate.list[i]
  names(meta)[names(meta) == 'Metadata_well_position'] <- 'Metadata_Well'
  meta$Metadata_broad_sample <- as.character(meta$Metadata_broad_sample)
  meta$Metadata_broad_sample[which(meta$Metadata_broad_sample == "")] <- "DMSO"
  
  dmso_wells <- meta %>% 
    filter(Metadata_broad_sample == "DMSO") %>%
    select(Metadata_Well) %>%
    as.matrix() %>%
    as.vector()
  
  
  #Extracting metadata
  
  # reading sqlite file
  sql_data <- as.data.frame(lapply(sql.path[i], read_sql))
  
  
  print("Step4")
  
  sql_data <- sql_data %>% dplyr::filter(Image_Metadata_Well %in% dmso_wells)
  
  
  
  dmso <- merge(tmp, meta, by = c("Image_Metadata_Well", "Metadata_Well"))
  
  
  print("Step5") 
  
  
  
  readr::write_csv(sql_data, paste0(output,plate.list[i], "/", plate.list[i], "_dmso", ".csv"))
  print("Successfully executed") 
}

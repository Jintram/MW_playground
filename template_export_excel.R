# Template for writing to excel file, different sheets, openxlsx

wb <- openxlsx::createWorkbook("Regulon")

openxlsx::addWorksheet(wb = wb, sheetName = paste0(cl_idx))
openxlsx::writeData(wb, sheet = paste0(cl_idx), df_export[[cl_idx]])

openxlsx::saveWorkbook(wb, file = paste0(outputDir_sub,'GO_analysis_co-correlations_v2.xlsx'), overwrite = TRUE)
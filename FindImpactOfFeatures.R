

Short_Commercial_testvars <- c(g_Checkinout, g_DocRecovery, g_DragAndDrop, g_Help, g_Identity, 
                               g_Options, g_PrintPreview, g_QAT, g_Share, g_Styles, 
                               g_TextEffects, p_CtxUIAutoShape, p_CtxUIChartPlotArea, 
                               p_CtxUIFormulaBar, p_TabData, p_TabFormulas, p_TabHeaderFooter)


Long_Consumer_testvars <- c(g_Charts, g_DocRecovery, g_Filter, g_Formulas, g_Identity, 
                            g_Names, g_Options, g_Pictures, g_PivotFieldList, g_Print, 
                            g_Shapes, g_Share, g_Styles, g_TextEffects, g_Theme, g_Zoom, 
                            p_CtxUIChartDataSeries)

Long_Commercial_testvars <- c(g_Charts, g_DocRecovery, g_HeaderFooter, g_Help, g_Identity, g_Options, 
                              g_PivotTables, g_Share, g_SimpleFormula, p_CtxUIConnector, p_CtxUIFormulaBar)


All_Edu_testvars <- c(g_Clear, g_ConditionalFormatting, g_DataValidation, g_Developer, 
                      g_DocRecovery, g_Filter, g_FormsFields, g_Formulas, g_FreezePanes, 
                      g_HeaderFooter, g_Help, g_Identity, g_NumberFormatting, g_Open, 
                      g_Options, g_PivotTables, g_QuickAnalysis, g_Selection, g_Shapes, 
                      g_SmartArt, g_Theme, g_Windows, p_CtxUIFormulaBar, p_StatusBar, 
                      p_TabChartToolsDesign, p_TabData, p_TabTableTools)

# modify as needed for each data set
reducedDF <- rawDF %>% 
  select(g_Clear, g_ConditionalFormatting, g_DataValidation, g_Developer, 
         g_DocRecovery, g_Filter, g_FormsFields, g_Formulas, g_FreezePanes, 
         g_HeaderFooter, g_Help, g_Identity, g_NumberFormatting, g_Open, 
         g_Options, g_PivotTables, g_QuickAnalysis, g_Selection, g_Shapes, 
         g_SmartArt, g_Theme, g_Windows, p_CtxUIFormulaBar, p_StatusBar, 
         p_TabChartToolsDesign, p_TabData, p_TabTableTools)

# get % of dataset affected by each feature
(colSums(reducedDF != 0)/nrow(reducedDF))*100

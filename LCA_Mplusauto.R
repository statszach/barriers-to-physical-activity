library(MplusAutomation)

uri_data <- uri_data %>% dplyr::select(-pred_classes)

uri_lca_model <- mplusObject(MODEL = "",
                         ANALYSIS = "TYPE = MIXTURE;",
                         VARIABLE = "classes = c (3);
                         CATEGORICAL = BPA1 BPA2 BPA3 BPA4 BPA5 BPA6 BPA7 BPA8 BPA9 BPA10 
                         BPA11 BPA12 BPA13;",
                         usevariables = colnames(uri_data),
                         rdata = uri_data)

uri_run_lca <- mplusModeler(uri_lca_model, modelout = "uri_lca3class.inp", run = TRUE)


mturk_lca_model <- mplusObject(MODEL = "",
                             ANALYSIS = "TYPE = MIXTURE;",
                             VARIABLE = "classes = c (3);
CATEGORICAL = BPA1 BPA2 BPA3 BPA4 BPA5 BPA6 BPA7 BPA8 BPA9
BPA10 BPA11 BPA12 BPA13;",
                             usevariables = colnames(mturk_data),
                             rdata = mturk_data)

mturk_run_lca <- mplusModeler(mturk_lca_model, modelout = "mturk_lca3class.inp", run = TRUE)




files <- c("../data/fresh_data/protocol_integration_analysis_sce_fresh_EOS1.rds",
           "../data/fresh_data/protocol_integration_analysis_sce_fresh_EOS2.rds",
           "../data/fresh_data/protocol_integration_analysis_sce_fresh_EOS3.rds",
           "../data/fresh_data/protocol_integration_analysis_sce_fresh_EOS4.rds",
           "../data/fresh_data/protocol_integration_analysis_sce_fresh_EOS5.rds",
           "../data/fresh_data/protocol_integration_analysis_sce_fresh_EOS6.rds",
           "../data/fresh_data/protocol_integration_analysis_sce_fresh_EOS7.rds",
)

file_name <- "../data/fresh_data/protocol_integration_analysis_sce_fresh_EOS5.rds"

sce_fresh <- readRDS(file=file_name)
sce_fresh$Protocol <- sub("Lafayatis", "Tabib_et_al", sce_fresh$Protocol)
sce_fresh$Protocol <- sub("Sole_Boldo", "Sole_Boldo_et_al", sce_fresh$Protocol)
sce_fresh$Protocol <- sub("He_et_al", "He_et_al", sce_fresh$Protocol)
sce_fresh$Protocol <- sub("SSc", "Cultured", sce_fresh$Protocol)
sce_fresh$Protocol <- factor(sce_fresh$Protocol)


# Samples
sce_fresh$Sample <- sub("Lafa_S1", "Tabib_S1", sce_fresh$Sample)
sce_fresh$Sample <- sub("Lafa_S124", "Tabib_S124", sce_fresh$Sample)
sce_fresh$Sample <- sub("Lafa_S125", "Tabib_S125", sce_fresh$Sample)
sce_fresh$Sample <- sub("Lafa_S18", "Tabib_S18", sce_fresh$Sample)
sce_fresh$Sample <- sub("Lafa_S32", "Tabib_S32", sce_fresh$Sample)
sce_fresh$Sample <- sub("Lafa_S33", "Tabib_S33", sce_fresh$Sample)
sce_fresh$Sample <- sub("Lafa_S34", "Tabib_S34", sce_fresh$Sample)
sce_fresh$Sample <- sub("Lafa_S4", "Tabib_S4", sce_fresh$Sample)
sce_fresh$Sample <- sub("Lafa_S50", "Tabib_S50", sce_fresh$Sample)
sce_fresh$Sample <- sub("Lafa_S68", "Tabib_S68", sce_fresh$Sample)
sce_fresh$Sample <- factor(sce_fresh$Sample)


sce_fresh <- saveRDS(sce_fresh, file=file_name)
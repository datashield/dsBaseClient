# 
# Obiba's Opal - Upload Testing Datasets
#

library(DSOpal)
library(opalr)
library(tibble)

upload_testing_dataset_table <- function(opal, project_name, table_name, local_file_path) {
    if (! opal.project_exists(opal, project_name))
        opal.project_create(opal, project_name, database = "mongodb")
  
    dataset_name <- load(file = local_file_path)
    dataset      <- eval(as.symbol(dataset_name))
    data         <- as_tibble(dataset, rownames = '_row_id_')
  
    opal.table_save(opal, data, project_name, table_name, id.name = "_row_id_", force = TRUE)
}

# opal <- opal.login('administrator','datashield_test&', url='https://192.168.56.100:8443/', opts = list(ssl_verifyhost=0, ssl_verifypeer=0))
opal <- opal.login('administrator','datashield_test&', url='https://localhost:8443/', opts = list(ssl_verifyhost=0, ssl_verifypeer=0))

upload_testing_dataset_table(opal, 'ANTHRO', 'anthro1', 'ANTHRO/anthro1.rda')
upload_testing_dataset_table(opal, 'ANTHRO', 'anthro2', 'ANTHRO/anthro2.rda')
upload_testing_dataset_table(opal, 'ANTHRO', 'anthro3', 'ANTHRO/anthro3.rda')

upload_testing_dataset_table(opal, 'CLUSTER', 'CLUSTER_INT1', 'CLUSTER/CLUSTER_INT1.rda')
upload_testing_dataset_table(opal, 'CLUSTER', 'CLUSTER_INT2', 'CLUSTER/CLUSTER_INT2.rda')
upload_testing_dataset_table(opal, 'CLUSTER', 'CLUSTER_INT3', 'CLUSTER/CLUSTER_INT3.rda')

upload_testing_dataset_table(opal, 'CLUSTER', 'CLUSTER_SLO1', 'CLUSTER/CLUSTER_SLO1.rda')
upload_testing_dataset_table(opal, 'CLUSTER', 'CLUSTER_SLO2', 'CLUSTER/CLUSTER_SLO2.rda')
upload_testing_dataset_table(opal, 'CLUSTER', 'CLUSTER_SLO3', 'CLUSTER/CLUSTER_SLO3.rda')

upload_testing_dataset_table(opal, 'CNSIM', 'CNSIM1', 'CNSIM/CNSIM1.rda')
upload_testing_dataset_table(opal, 'CNSIM', 'CNSIM2', 'CNSIM/CNSIM2.rda')
upload_testing_dataset_table(opal, 'CNSIM', 'CNSIM3', 'CNSIM/CNSIM3.rda')

upload_testing_dataset_table(opal, 'DASIM', 'DASIM1', 'DASIM/DASIM1.rda')
upload_testing_dataset_table(opal, 'DASIM', 'DASIM2', 'DASIM/DASIM2.rda')
upload_testing_dataset_table(opal, 'DASIM', 'DASIM3', 'DASIM/DASIM3.rda')

upload_testing_dataset_table(opal, 'DISCORDANT', 'DISCORDANT_STUDY1', 'DISCORDANT/DISCORDANT_STUDY1.rda')
upload_testing_dataset_table(opal, 'DISCORDANT', 'DISCORDANT_STUDY2', 'DISCORDANT/DISCORDANT_STUDY2.rda')
upload_testing_dataset_table(opal, 'DISCORDANT', 'DISCORDANT_STUDY3', 'DISCORDANT/DISCORDANT_STUDY3.rda')

upload_testing_dataset_table(opal, 'FACTOR_LEVELS', 'FACTOR_LEVELS1', 'FACTOR_LEVELS/FACTOR_LEVELS1.rda')
upload_testing_dataset_table(opal, 'FACTOR_LEVELS', 'FACTOR_LEVELS2', 'FACTOR_LEVELS/FACTOR_LEVELS2.rda')
upload_testing_dataset_table(opal, 'FACTOR_LEVELS', 'FACTOR_LEVELS3', 'FACTOR_LEVELS/FACTOR_LEVELS3.rda')

upload_testing_dataset_table(opal, 'GAMLSS', 'gamlss1', 'GAMLSS/gamlss1.rda')
upload_testing_dataset_table(opal, 'GAMLSS', 'gamlss2', 'GAMLSS/gamlss2.rda')
upload_testing_dataset_table(opal, 'GAMLSS', 'gamlss3', 'GAMLSS/gamlss3.rda')

upload_testing_dataset_table(opal, 'SURVIVAL', 'COLLAPSE_WITH_MISSING1', 'SURVIVAL/COLLAPSE_MISSING/COLLAPSE_WITH_MISSING1.rda')
upload_testing_dataset_table(opal, 'SURVIVAL', 'COLLAPSE_WITH_MISSING2', 'SURVIVAL/COLLAPSE_MISSING/COLLAPSE_WITH_MISSING2.rda')
upload_testing_dataset_table(opal, 'SURVIVAL', 'COLLAPSE_WITH_MISSING3', 'SURVIVAL/COLLAPSE_MISSING/COLLAPSE_WITH_MISSING3.rda')

upload_testing_dataset_table(opal, 'SURVIVAL', 'COLLAPSE_NO_MISSING1', 'SURVIVAL/COLLAPSE_NO_MISSING/COLLAPSE_NO_MISSING1.rda')
upload_testing_dataset_table(opal, 'SURVIVAL', 'COLLAPSE_NO_MISSING2', 'SURVIVAL/COLLAPSE_NO_MISSING/COLLAPSE_NO_MISSING2.rda')
upload_testing_dataset_table(opal, 'SURVIVAL', 'COLLAPSE_NO_MISSING3', 'SURVIVAL/COLLAPSE_NO_MISSING/COLLAPSE_NO_MISSING3.rda')

upload_testing_dataset_table(opal, 'SURVIVAL', 'EXPAND_WITH_MISSING1', 'SURVIVAL/EXPAND_MISSING/EXPAND_WITH_MISSING1.rda')
upload_testing_dataset_table(opal, 'SURVIVAL', 'EXPAND_WITH_MISSING2', 'SURVIVAL/EXPAND_MISSING/EXPAND_WITH_MISSING2.rda')
upload_testing_dataset_table(opal, 'SURVIVAL', 'EXPAND_WITH_MISSING3', 'SURVIVAL/EXPAND_MISSING/EXPAND_WITH_MISSING3.rda')

upload_testing_dataset_table(opal, 'SURVIVAL', 'EXPAND_NO_MISSING1', 'SURVIVAL/EXPAND_NO_MISSING/EXPAND_NO_MISSING1.rda')
upload_testing_dataset_table(opal, 'SURVIVAL', 'EXPAND_NO_MISSING2', 'SURVIVAL/EXPAND_NO_MISSING/EXPAND_NO_MISSING2.rda')
upload_testing_dataset_table(opal, 'SURVIVAL', 'EXPAND_NO_MISSING3', 'SURVIVAL/EXPAND_NO_MISSING/EXPAND_NO_MISSING3.rda')

upload_testing_dataset_table(opal, 'TESTING', 'DATASET1', 'TESTING/DATASET1.rda')
upload_testing_dataset_table(opal, 'TESTING', 'DATASET2', 'TESTING/DATASET2.rda')
upload_testing_dataset_table(opal, 'TESTING', 'DATASET3', 'TESTING/DATASET3.rda')

opal.logout(opal)

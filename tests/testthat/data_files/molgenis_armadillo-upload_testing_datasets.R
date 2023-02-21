# 
# Molgenis' Armadillo - Upload Testing Datasets
#

library(MolgenisArmadillo)

upload_testing_dataset_table <- function(project_name, folder_name, table_name, dataset_file_name) {
    dataset_name <- load(file = dataset_file_name)
    dataset      <- eval(as.symbol(dataset_name))
    MolgenisArmadillo::armadillo.upload_table(project_name, folder_name, dataset, table_name)
}

# MolgenisArmadillo::armadillo.set_credentials(server = 'http://127.0.0.1:9000', access_key = "molgenis", secret_key = "molgenis")
MolgenisArmadillo::armadillo.login_basic(armadillo = 'http://127.0.0.1:8080', username = "admin", password = "admin")

if (! 'datashield' %in% MolgenisArmadillo::armadillo.list_projects())
    MolgenisArmadillo::armadillo.create_project('datashield')

upload_testing_dataset_table('datashield', 'anthro', 'anthro1', 'ANTHRO/anthro1.rda')
upload_testing_dataset_table('datashield', 'anthro', 'anthro2', 'ANTHRO/anthro2.rda')
upload_testing_dataset_table('datashield', 'anthro', 'anthro3', 'ANTHRO/anthro3.rda')

upload_testing_dataset_table('datashield', 'cluster', 'CLUSTER_INT1', 'CLUSTER/CLUSTER_INT1.rda')
upload_testing_dataset_table('datashield', 'cluster', 'CLUSTER_INT2', 'CLUSTER/CLUSTER_INT2.rda')
upload_testing_dataset_table('datashield', 'cluster', 'CLUSTER_INT3', 'CLUSTER/CLUSTER_INT3.rda')

upload_testing_dataset_table('datashield', 'cluster', 'CLUSTER_SLO1', 'CLUSTER/CLUSTER_SLO1.rda')
upload_testing_dataset_table('datashield', 'cluster', 'CLUSTER_SLO2', 'CLUSTER/CLUSTER_SLO2.rda')
upload_testing_dataset_table('datashield', 'cluster', 'CLUSTER_SLO3', 'CLUSTER/CLUSTER_SLO3.rda')

upload_testing_dataset_table('datashield', 'cnsim', 'CNSIM1', 'CNSIM/CNSIM1.rda')
upload_testing_dataset_table('datashield', 'cnsim', 'CNSIM2', 'CNSIM/CNSIM2.rda')
upload_testing_dataset_table('datashield', 'cnsim', 'CNSIM3', 'CNSIM/CNSIM3.rda')

upload_testing_dataset_table('datashield', 'dasim', 'DASIM1', 'DASIM/DASIM3.rda')
upload_testing_dataset_table('datashield', 'dasim', 'DASIM2', 'DASIM/DASIM1.rda')
upload_testing_dataset_table('datashield', 'dasim', 'DASIM3', 'DASIM/DASIM2.rda')

upload_testing_dataset_table('datashield', 'discordant', 'DISCORDANT_STUDY1', 'DISCORDANT/DISCORDANT_STUDY1.rda')
upload_testing_dataset_table('datashield', 'discordant', 'DISCORDANT_STUDY2', 'DISCORDANT/DISCORDANT_STUDY2.rda')
upload_testing_dataset_table('datashield', 'discordant', 'DISCORDANT_STUDY3', 'DISCORDANT/DISCORDANT_STUDY3.rda')

upload_testing_dataset_table('datashield', 'factor_levels', 'FACTOR_LEVELS1', 'FACTOR_LEVELS/FACTOR_LEVELS1.rda')
upload_testing_dataset_table('datashield', 'factor_levels', 'FACTOR_LEVELS2', 'FACTOR_LEVELS/FACTOR_LEVELS2.rda')
upload_testing_dataset_table('datashield', 'factor_levels', 'FACTOR_LEVELS3', 'FACTOR_LEVELS/FACTOR_LEVELS3.rda')

upload_testing_dataset_table('datashield', 'survival', 'COLLAPSE_WITH_MISSING1', 'SURVIVAL/COLLAPSE_MISSING/COLLAPSE_WITH_MISSING1.rda')
upload_testing_dataset_table('datashield', 'survival', 'COLLAPSE_WITH_MISSING2', 'SURVIVAL/COLLAPSE_MISSING/COLLAPSE_WITH_MISSING2.rda')
upload_testing_dataset_table('datashield', 'survival', 'COLLAPSE_WITH_MISSING3', 'SURVIVAL/COLLAPSE_MISSING/COLLAPSE_WITH_MISSING3.rda')

upload_testing_dataset_table('datashield', 'survival', 'COLLAPSE_NO_MISSING1', 'SURVIVAL/COLLAPSE_NO_MISSING/COLLAPSE_NO_MISSING1.rda')
upload_testing_dataset_table('datashield', 'survival', 'COLLAPSE_NO_MISSING2', 'SURVIVAL/COLLAPSE_NO_MISSING/COLLAPSE_NO_MISSING2.rda')
upload_testing_dataset_table('datashield', 'survival', 'COLLAPSE_NO_MISSING3', 'SURVIVAL/COLLAPSE_NO_MISSING/COLLAPSE_NO_MISSING3.rda')

upload_testing_dataset_table('datashield', 'survival', 'EXPAND_WITH_MISSING1', 'SURVIVAL/EXPAND_MISSING/EXPAND_WITH_MISSING1.rda')
upload_testing_dataset_table('datashield', 'survival', 'EXPAND_WITH_MISSING2', 'SURVIVAL/EXPAND_MISSING/EXPAND_WITH_MISSING2.rda')
upload_testing_dataset_table('datashield', 'survival', 'EXPAND_WITH_MISSING3', 'SURVIVAL/EXPAND_MISSING/EXPAND_WITH_MISSING3.rda')

upload_testing_dataset_table('datashield', 'survival', 'EXPAND_NO_MISSING1', 'SURVIVAL/EXPAND_NO_MISSING/EXPAND_NO_MISSING1.rda')
upload_testing_dataset_table('datashield', 'survival', 'EXPAND_NO_MISSING2', 'SURVIVAL/EXPAND_NO_MISSING/EXPAND_NO_MISSING2.rda')
upload_testing_dataset_table('datashield', 'survival', 'EXPAND_NO_MISSING3', 'SURVIVAL/EXPAND_NO_MISSING/EXPAND_NO_MISSING3.rda')

upload_testing_dataset_table('datashield', 'testing', 'DATASET1', 'TESTING/DATASET1.rda')
upload_testing_dataset_table('datashield', 'testing', 'DATASET2', 'TESTING/DATASET2.rda')
upload_testing_dataset_table('datashield', 'testing', 'DATASET3', 'TESTING/DATASET3.rda')

print(MolgenisArmadillo::armadillo.list_tables('datashield'))

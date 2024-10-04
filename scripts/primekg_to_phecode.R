devtools::load_all()
dirpath = system.file('extdata', package = 'knowledgegraphs')
df_kg = data.table::fread(file.path(dirpath, 'kg.csv'), data.table = FALSE)
df_kg_disease = subset(df_kg, relation == 'disease_disease')
# nrow(df_kg_disease)
# [1] 64388


fpath = file.path(dirpath, 'umls_mondo.csv')
df_mondo_cui = data.table::fread(fpath, data.table = FALSE)
# rm CN starting CUI codes, none in CUI to phecode mapping anyway
df_mondo_cui %<>% subset(!grepl('^CN', umls_id)) # only 1k out of 30k

df_merge = merge(df_kg_disease, df_mondo_cui, by.x = 'x_id', by.y = 'mondo_id')
df_merge %<>% merge(df_mondo_cui, by.x = 'y_id', by.y = 'mondo_id')
# nrow(df_merge)
# [1] 104874

fpath = file.path(dirpath, 'CUI_Phecode_doudou_05062024.csv')
df_cui_phecode = data.table::fread(fpath, data.table = FALSE)
df_merge %<>% merge(df_cui_phecode, by.x = 'umls_id.x', by.y = 'CUI')
df_merge %<>% merge(df_cui_phecode, by.x = 'umls_id.y', by.y = 'CUI')
# nrow(df_merge)
# [1] 9424

df_merge %<>% subset(!duplicated(paste0(PheCode.x, PheCode.y)))
# nrow(df_merge)
# [1] 3696

df_merge %<>% subset(PheCode.x != PheCode.y)
# nrow(df_merge)
# [1] 3288

df_merge = df_merge[c('PheCode.x', 'PheCode_desc.x', 'PheCode.y', 'PheCode_desc.y')]

data.table::fwrite(df_merge, 'df_phecode_pairs.tsv', sep = '\t')






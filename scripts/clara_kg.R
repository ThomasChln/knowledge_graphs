  


  devtools::load_all()
  dirpath = system.file('data', package = 'knowledgegraphs')
  m_embeds = get(load(file.path(dirpath, 'embedding.Rdata')))

  dirpath = system.file('extdata', package = 'knowledgegraphs')
  fpath = file.path(dirpath, 'RPDR_parent_code_per_day_allpatients_alldata_sorted_mapping_parquet_suicide_adolescent.csv')
  row_names = data.table::fread(fpath, data.table = FALSE)

  rownames(m_embeds) = row_names$Parent_Code[match(rownames(m_embeds), row_names$WordIndex)]

  fpath = file.path(dirpath, 'df_phecode_pairs.tsv')
  df_pairs = data.table::fread(fpath, data.table = FALSE)

  fit_embeds = fit_embeds_to_pairs(m_embeds, df_pairs)

  df_projs = fit_embeds$df_projs

  dirpath = system.file('data', package = 'knowledgegraphs')
  save(df_projs, file = file.path(dirpath, 'df_weights.rds'))


  df_dict = data.frame(id = row_names$Parent_Code)

  dirpath = system.file('extdata', package = 'knowledgegraphs')
  fpath = file.path(dirpath, 'CUI_Phecode_doudou_05062024.csv')
  df_phecodes = data.table::fread(fpath, data.table = FALSE)[c('PheCode', 'PheCode_desc')]

  fpaths = c('CUI_CCS_UMLS2021AA_VID.csv', 'CUI_LOINC_PART_UMLS2021AA_VID.csv',
             'CUI_RXNORM_UMLS2021AA_VID.csv')
  for (fpath in fpaths) {
    df_map = data.table::fread(file.path(dirpath, fpath), data.table = FALSE)
    df_map = df_map[!grepl('^CUI', names(df_map))]

    if (fpath == 'CUI_RXNORM_UMLS2021AA_VID.csv') {
        df_map$RXNORM %<>% paste0('RXNORM:', .)
    } else if (fpath == 'CUI_CCS_UMLS2021AA_VID.csv') {
        df_map$CCS %<>% paste0('CCS-PCS:', .)
    }
    df_map %<>% setNames(c('PheCode', 'PheCode_desc'))
    df_phecodes %<>% rbind(df_map)
  }

  phecode_map = match(df_dict$id, df_phecodes$PheCode)
  df_dict$label = df_phecodes$PheCode_desc[phecode_map]

  dirpath = system.file('data', package = 'knowledgegraphs')
  save(df_dict, file = file.path(dirpath, 'df_dict.rds'))



  ## QC / explo



  df_projs$Var1 %<>% as.character
  phecode_map = match(df_projs$Var1, df_phecodes$PheCode)
  df_projs$Var1[!is.na(phecode_map)] = df_phecodes$PheCode_desc[na.omit(phecode_map)]

  df_projs$Var2 %<>% as.character
  phecode_map = match(df_projs$Var2, df_phecodes$PheCode)
  df_projs$Var2[!is.na(phecode_map)] = df_phecodes$PheCode_desc[na.omit(phecode_map)]

  subset(df_projs, !(grepl('[0-9]$', Var1) | grepl('[0-9]$', Var2)))[1:2] %>%
      head(50) %>% t

  selected_pheno = grep('nxiety', unique(unlist(df_projs[1:2])), value = TRUE)




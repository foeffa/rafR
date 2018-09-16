library(tidyverse)
library(magrittr)
library(lubridate)
library(stringr)
library(DBI)

# Standard analysis functions ----
prep_df_exp <- function(df_input,dim_col,val_col) {
  all_cols <- c(dim_col,val_col)
  df_temp <- df_input %>%
    select(!!all_cols) %>%
    rename(
      "value_col" = !!val_col
    )
  return(df_temp) # Added 25/8 - correct?
}

# test <- df_transactions %>% prep_df_exp(dimensions,value_col) %>% generate_percentiles(dimensions) # zou nog kunnen doen: alles behalve de value_col (-one_of()) gebruiken als dimensie?? want heb hier weer "unknown col dimensions" shizzle
# dus nog altijd experimenteren met hoe ik vector van strings kan gebruiken hiervoor zodat bovenstaande function call werkt

generate_percentiles <- function(df_input,...) {
  qdims <- quos(...)
  df_temp <- df_input %>%
    group_by(!!!qdims) %>%
    nest(.key = "nested_values") %>%
    mutate(
      percentile_calculated = map(nested_values,~quantile(x = .$value_col,probs=seq(from = 0.05, to = 0.95, by = 0.05),names=T,na.rm = T))
    ) %>%
    rowwise() %>%
    mutate(
      percentile.name = list(names(percentile_calculated))
    ) %>%
    select(-nested_values) %>%
    ungroup() %>%
    unnest()
  return(df_temp)
}


generate_percentiles_sym <- function(df_input,...) {
  grouping <- rlang::syms(...)
  df_temp <- df_input %>%
    group_by(!!!grouping) %>%
    nest(.key = "nested_values") %>%
    mutate(
      percentile_calculated = map(nested_values,~quantile(x = .$value_col,probs=seq(from = 0.05, to = 0.95, by = 0.05),names=T,na.rm = T))
    ) %>%
    rowwise() %>%
    mutate(
      percentile.name = list(names(percentile_calculated))
    ) %>%
    select(-nested_values) %>%
    ungroup() %>%
    unnest()
  return(df_temp)
}

# Aka "rank_within_segment_generalized_named_cumedist_classified" function in code_ABCD_rework_v0.2
# Make sure to also load the ABCD mapping matrix (cutoff per interval)
# And to supply interval_id for every bucket
#   ABCD_cutoffs <- read_csv("Import/abcd_classification.csv")
#   ABCD_cutoffs %<>% mutate(interval_id = seq(0:(length(ABCD_cutoffs$label)-1)))
# Probably needs country in the group_by (...) vars to mbe able to join locally


# Below is deleted for package purposes - cannot automatically read from dir
#ABCD_cutoffs <- read_csv("Import/abcd_classification.csv")
#ABCD_cutoffs %<>% mutate(interval_id = seq(0:(length(ABCD_cutoffs$label)-1)))

# use _adv version
classify_ABCD <- function(df,cutoff_matrix,value,object,...) {
  value <- enquo(value)
  object <- enquo(object)
  dims <- enquos(...)
  col_string <- dims %>% map(~quo_name(.)) %>% unlist() %>% str_flatten(collapse = "_")
  col_name <- paste("ABCD","for",quo_name(object),"on",col_string,sep = ".")
  col_name_quoted <- rlang::parse_quosure(col_name)
  df %<>%
    group_by(!!!dims,!!object) %>%
    summarise(
      !!col_name := sum(!!value)
    ) %>%
    arrange(!!!dims,desc(!!col_name_quoted)) %>%
    mutate (
      cumedist.pct = cume_dist(!!col_name_quoted),
      percentile.name = paste0("P",round(cumedist.pct,2)*100)
    )
  df %<>%
    mutate(
      interval_id = findInterval(x = cumedist.pct, vec = cutoff_matrix$min_pct_lower_values, rightmost.closed=TRUE)
    ) %>%
    left_join(cutoff_matrix,by=c("interval_id" = "interval_id"))
  return(df)
}


classify_ABCD_adv <- function(df,cutoff_matrix,value,object,...) {
  value <- enquo(value)
  object <- enquo(object)
  dims <- enquos(...)
  col_string <- dims %>% map(~quo_name(.)) %>% unlist() %>% str_flatten(collapse = "_")
  col_name <- paste("ABCD.aggregated.value","for",quo_name(object),"by",col_string,sep = ".")
  col_name_quoted <- rlang::parse_quosure(col_name)
  df %<>%
    group_by(!!!dims,!!object) %>%
    summarise(
      !!col_name := sum(!!value)
    ) %>%
    arrange(!!!dims,desc(!!col_name_quoted)) %>%
    mutate (
      cumedist.pct = cume_dist(!!col_name_quoted),
      percentile.name = paste0("P",round(cumedist.pct,2)*100)
    )
  df %<>%
    mutate(
      interval_id = findInterval(x = cumedist.pct, vec = cutoff_matrix$min_pct_lower_values, rightmost.closed=TRUE)
    ) %>%
    left_join(cutoff_matrix,by=c("interval_id" = "interval_id"))
  print(col_string)
  label_string <- paste("ABCD.label","for",quo_name(object),"by",col_string,sep = ".")
  print(label_string)
  df %<>%
     rename(
       !!label_string := label
       #!!label_string := label
     )
  return(df)
}

# is het niet makkelijker om die columns een algemene naam te geven zodat ik ze makkelijk kan row_binden() ?
# note: gaat ook uit van een 1:1 mapping ... (i.e. bij sales region dat een klant in transactions ook enkel bij 1 sales region hoort ...)

# editen zodat er string gegeven kan woren (char vector) zoals in

classify_ABCD_adv_long <- function(df,cutoff_matrix,value,object,...) {
  value <- enquo(value)
  object <- enquo(object)
  dims <- enquos(...)
  col_string <- dims %>% map(~quo_name(.)) %>% unlist() %>% str_flatten(collapse = "_")
  #col_name <- paste("ABCD.aggregated.value","for",quo_name(object),"by",col_string,sep = ".")
  #col_name_quoted <- rlang::parse_quosure(col_name)  - was not used?
  df %<>%
    group_by(!!!dims,!!object) %>%
    summarise(
      aggregated_value = sum(!!value)
    ) %>%
    arrange(!!!dims,desc(aggregated_value)) %>%
    mutate (
      cumedist.pct = cume_dist(aggregated_value),
      percentile.name = paste0("P",round(cumedist.pct,2)*100)
    )
  df %<>%
    mutate(
      interval_id = findInterval(x = cumedist.pct, vec = cutoff_matrix$min_pct_lower_values, rightmost.closed=TRUE)
    ) %>%
    left_join(cutoff_matrix,by=c("interval_id" = "interval_id"))
  df %<>%
    mutate(
      identifier_variable = quo_name(object),
      dimension = col_string,
      value_variable = quo_name(value)
    )
  print(paste0("Dimension: ", col_string))
  df %<>%
    ungroup() %>%
    select(!!object,identifier_variable,aggregated_value,value_variable,dimension,label)

  df %<>%
    rename(
      "identifier_value" = !!object
    )
  return(df)
}


# Data read functions ----
clean_col_names_old <- function(df) {
  df %<>% rename_all(. %>% tolower %>% gsub(" ", "\\.",.) %>% gsub("-","\\.",.) %>% gsub("_","\\.",.))
  return(df)
}

clean_col_names <- function(df) {
  df %<>% rename_all(. %>% tolower %>% gsub("[^a-zA-Z0-9]","\\.",.) %>% gsub("[\\.]+","\\.",.))
}

# Use - main function interface
read_files_with_spec <- function(filenames,spec_col_types) {
  df <- filenames %>% map_dfr(.f = read_csv_with_filename_and_spec,spec_col_types) #%>%
  #mutate(file.origin = filenames)
  return(df)
}

# Use - helper function
read_csv_with_filename_and_spec <- function(filename,spec) { # waarschijnlijk eleganter om deze functie on the fly te maken ... i.e. kan k hier parameters mappen of zo in list? check R for Data Science
  read_csv(filename,col_types = spec) %>%
    mutate(SKP.file.full.path = filename)
}

# Deprecated
read_csv_with_filename <- function(filename) {
  read_csv(filename) %>%
    mutate(SKP.file.full.path = filename)
}

get_file_list <- function(main_dir,mask,...) {
  import_dir <- file.path(main_dir,str_flatten(string = ...,collapse = "/"))
  import_files <- list.files(import_dir,pattern = mask)
  import_file_list_full <- paste(import_dir,import_files,sep="/")
  return(import_file_list_full)
}

extract_filename_from_path <- function(filename,separator) {
  detect_separators <- str_locate_all(filename,pattern=separator)
  nr_instances_of_separator <- nrow(detect_separators[[1]])
  last_part_start_position <- detect_separators[[1]][[nr_instances_of_separator]]
  last_part_end_position = str_length(filename)
  extracted_string = str_sub(filename,start = last_part_start_position+1, end = last_part_end_position)
  return(extracted_string)
}

read_db_timed <- function(df,db_con) {
  time_start <- Sys.time()
  df <- DBI::dbReadTable(conn = db_con,name = df)
  time_end <- Sys.time()
  time_passed <- time_end - time_start
  print(paste0("Data read time: ",time_passed))
  return(df)
}

write_db_timed <- function(df,db_con) {
  time_start <- Sys.time()
  DBI::dbWriteTable(conn = db_con, name = df, value = eval(parse(text = df)), overwrite = TRUE, temporary = FALSE)
  time_end <- Sys.time()
  time_passed <- time_end - time_start
  print(paste0("Data write time: ",time_passed))
}

# BOXPLOT functions ----
# AcTUAL FUNCTIONS ----
generate_boxplot_passthrough <- function(df,x_axis,y_axis) { # remember to use x_axis and y_axis variables when banging !! :)
  graph <- ggplot(data = df, aes(x = !!x_axis, y = !!y_axis)) + geom_boxplot(shape = 2, varwidth = T, na.rm = T) + theme_light() # OKAY!
  return(graph)
}


generate_grouped_boxplots <- function(df,value,x_axis,...) { # OKAY!
  grouping_var <- enquos(...)
  value_col <- enquo(value)
  x_axis_col <- enquo(x_axis)
  grouped_plots <- df %>%
    group_by(!!!grouping_var) %>%
    nest(.key = "boxplot_data") %>%
    mutate (
      boxplot_model = map (
        boxplot_data,
        #~ggplot(data = .) + geom_boxplot(aes(x = product.level.2, y = discount.given.on.theoretical.revenue),outlier.colour = "red",outlier.shape = 2,outlier.alpha = 0.75, varwidth = T,na.rm = T) + theme_light() + coord_flip()
        ~generate_boxplot_passthrough(.,x_axis_col,value_col)
      )
    )
  return(grouped_plots)
}

write_boxplots <- function(plots) {
  dimension_names <- plots %>%
    select(-contains("boxplot")) %>%
    unite(sep=" _ ",col = "file_name_initial")
  print(dimension_names)
  dimension_names_full <- dimension_names %>%
    mutate(
      file_name_initial = paste0(getwd(),"/",file_name_initial,".png")
    )
  print(dimension_names_full$file_name_initial)
  #dimension_names_pasted <- dimension_names %>% select_all() %>% unite(sep=" | ")
  # file_names <- paste0(dimension_names,".png")
  # print(file_names)
  map2(dimension_names_full$file_name_initial,plots$boxplot_model,ggsave)
}

# example usage: test_plots4 <- generate_grouped_boxplots(test_set,sales.net.margin.pct,customer.price.band,order.channel,sales.region) %>% write_boxplots()

# Peer pricing by volume ----
# check test_densityPPshizzle file voor violin plots die hier ook die percentiles van gebruiken
prep_df_vol <- function(df_input,dim_col,margin_col,volume_col) {
  all_cols <- c(dim_col,margin_col,volume_col)
  print(all_cols)
  df_temp <- df_input %>%
    select(!!all_cols) %>%
    rename(
      "margin_col" = !!margin_col, # indicator
      "volume_col" = !!volume_col
    ) %>%
    mutate(
      margin_pct = margin_col / volume_col,
      margin_col_rounded = round(margin_pct,2)
    )
  return(df_temp)
}

generate_percentiles_by_volume <- function(df,slice_yesno,...) {
  dims <- rlang::syms(...)
  df_aggregated_volume <- df %>%
    group_by(!!!dims) %>%
    summarise(
      total_volume_for_dimension = sum(volume_col)
    )

  df %<>%
    group_by(!!!dims,margin_col_rounded) %>%
    summarise(
      volume_at_pct_margin = sum(volume_col)
    ) %>%
    left_join(df_aggregated_volume) %>%
    mutate(
      pct_volume_of_dimension = volume_at_pct_margin / total_volume_for_dimension,
      cum_pct_volume_of_dimension = cumsum(pct_volume_of_dimension)
    ) %>%
    mutate(
      volume_quantile = findInterval(x = cum_pct_volume_of_dimension, vec = seq(from = 0.0, to = 1.0, by = 0.1)) # klopt want we kijken op welke marge Pxx van volume (sales) valt
    )

  if(slice_yesno == FALSE) {
    return(df)
  } else {
    df %<>%
      arrange(!!!dims,volume_quantile) %>%
      group_by(!!!dims, volume_quantile) %>%
      slice(1)
    return(df)
  }
}

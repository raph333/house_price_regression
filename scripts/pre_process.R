# variables with only a few missing values:

require(dplyr)
require(tidyr)

get_na_stats <- function(train_df, test_df) {
  
  available_train <- sapply(select(train_df, -SalePrice), function(col) 1 - mean(is.na(col)))
  available_test <- sapply(test_df, function(col) 1 - mean(is.na(col)))
  
  na_stats <- data.frame(available_test, available_train)
  na_stats['variable'] <- row.names(na_stats)
  row.names(na_stats) <- NULL
  
  na_stats <- na_stats %>%
    mutate( available_min = pmin(available_train, available_test) ) %>% 
    mutate( complete = as.factor(available_min == 1) )
  
  return( select(na_stats, c('variable', colnames(na_stats))) )
}

remove_rare_factor_levels <- function(df) {
  # common problem:
  # there are a some factor-levels occuring in just a few houses
  # when splitting training and validation-set, the valication set will contain a factor-level not seen
  # during training => error
  # simple solution for now: combine all rare factor levels into level 'unknown'. If there are still only a few
  # observations of this level, just replace them with the most common factor level
  for (col_name in colnames(df)) {
    column <- df[[col_name]]
    
    if (is.factor(column)) {
      min_level_count <- 20  # a factor level needs at least this many observations
      column <- column %>% as.character() %>% replace_na('unknown')  # NA becomes a factor level
      
      # combine rare-levels to 'unknown
      rare_levels <- names(table(column)[table(column) < min_level_count])
      column <- replace(column, column %in% rare_levels, 'unknown')
      
      # if there are still only few observations with level 'unkown'...
      if (sum(column == 'unknown') < min_level_count) {
        most_common_level <- names(table(column)[table(column) == max(table(column))])
        column <- replace(column, column == 'unknown', most_common_level)  # ...replace them with most common level
      }
      
      if (length(unique(column)) > 1) {
        df[[col_name]] <- factor(column)
      }
      else {  # drop the feature if only one level remains
        df[[col_name]] <- NULL
      }
    }
  }
  return(df)
}

impute_na <- function(df, na_stats, notna_threshold=0.9) {
  
  impute_cols <- filter(na_stats, available_min >= notna_threshold & complete == F)$variable
  remove_cols <- filter(na_stats, available_min < notna_threshold)$variable

  df <- select(df, -remove_cols)
  #cat('\n\nremoved these columns (too many NA-values):\n', remove_cols)
  
  # factor_cols <- Filter(function(x) is.factor(df[[x]]), impute_cols)
  # df[, factor_cols] <- lapply(df[, factor_cols], addNA )  # add NA as a factor level
  # cat('\n\nset NA to factor-level for these columns:\n', factor_cols)
  
  numeric_cols <- Filter(function(col) is.numeric(df[[col]]), impute_cols)
  df[, numeric_cols] <- lapply(df[, numeric_cols], function(col) replace_na(col, median(col, na.rm=T)))
  #cat('\n\nset NA to median value for these columns:\n', numeric_cols)
  
  return(df)
}

remove_na_columns <- function(df, na_stats) {
  # remove all columns with NA values in either trainings- or test-set
  remove_cols <- filter(na_stats, available_min < 1)$variable
  remove_cols <- intersect(remove_cols, colnames(df))
  df <- select(df, -remove_cols)
  return(df)
}

feature_engineer <- function(df) {
  # * make some new features and remove the old ones
  # * change some ordinal features from numeric to factor
  
  # most houses have no pool (area = 0) => only consider if a house has a pool or not:
  df['HasPool'] <- as.factor(df$PoolArea > 0)
  df$PoolArea <- NULL
  
  convert_to_factor <- c('BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath', 'BedroomAbvGr',
                         'KitchenAbvGr', 'Fireplaces', 'GarageCars')
  df[, convert_to_factor] <- lapply(df[, convert_to_factor], as.factor)
  
  current_year <- 2011  # year the dataset was built
  
  df['Age'] <- current_year - df$YearBuilt
  df['YearsSinceSale'] <- current_year - df$YrSold
  df[, c('MoSold', 'YrSold', 'YearBuilt')] <- NULL
  
  df[, 'Id'] <- NULL  # Id is no feature of course
  df[, c('Street', 'Utilities')] <- NULL  # almost only one level
   
  return(df)
}

pre_process_data <- function(df) {
  processed_df <- df %>%
    feature_engineer() %>%
    impute_na(na_stats=na_statsistics) %>%
    remove_rare_factor_levels()  # check function documentation why this is necessary
  return(processed_df)
}

split_training_validation_ds <- function(df, validation_fraction=0.2) {

  validation_row_indices <- sample(row.names(df), size=validation_fraction*nrow(df), replace=F)
  train_row_indices = row.names(df)[!row.names(df) %in% validation_row_indices]
  
  validation <- df[validation_row_indices, ]
  train <- df[train_row_indices, ]
  
  add_all_factor_levels <- function(col) {
    if (is.factor(col)) {
      return( factor(col, levels=levels(df[[col]])) )
    }
    else {
      return( col )
    }
  }
  
  for (col_name in colnames(train)) {
    #all_levels <- unique(c(levels(train[[col_name]]), levels(validation[[col_name]])))
    #train[[col_name]] <- factor(train[[col_name]], levels=all_levels)
    #train[[col_name]] <- factor(train[[col_name]], levels=levels(validation[[col_name]]))
    }
  
  #validation <- lapply(validation, add_all_factor_levels)
  #train <- lapply(train, add_all_factor_levels)
  
  return( list(train, validation) )
}
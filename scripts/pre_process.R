# variables with only a few missing values:

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

impute_na <- function(df, na_stats, notna_threshold=0.9) {
  
  impute_cols <- filter(na_stats, available_min >= notna_threshold & complete == F)$variable
  remove_cols <- filter(na_stats, available_min < notna_threshold)$variable

  df <- select(df, -remove_cols)
  #cat('\n\nremoved these columns (too many NA-values):\n', remove_cols)
  
  factor_cols <- Filter(function(x) is.factor(df[[x]]), impute_cols)
  df[, factor_cols] <- lapply(df[, factor_cols], addNA)  # add NA as a factor level
  #cat('\n\nset NA to factor-level for these columns:\n', factor_cols)
  
  numeric_cols <- Filter(function(col) is.numeric(df[[col]]), impute_cols)
  df[, numeric_cols] <- lapply(df[, numeric_cols], function(col) replace_na(col, median(col, na.rm=T)))
  #cat('\n\nset NA to median value for these columns:\n', numeric_cols)
  
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
  
  return(df)
}

split_training_validation_ds <- function(df, validation_fraction=0.2) {

  validation_row_indices <- sample(row.names(df), size=validation_fraction*nrow(df), replace=F)
  train_row_indices = row.names(df)[!row.names(df) %in% validation_row_indices]
  
  validation <- df[validation_row_indices, ]
  df <- df[train_row_indices, ]
  
  return( list(df, validation) )
}
library(catboost)
library(dplyr)
library(ggplot2)


data = read.csv("D:/AZ/adni/objects/adni_df.csv", row.names = 1)


# Baseline model
data_bsln <- data %>%
  mutate(DIAGNOSIS = case_when(DIAGNOSIS == "Control" ~ 0,
                               DIAGNOSIS == "MCI" ~ 1,
                               DIAGNOSIS == "AZ" ~ 2) ) %>%
  select(-c(RID,age)) %>%
  mutate(apoe = if_else(apoe %in% c(1,2), 0, 1),
         PP = VSBPSYS - VSBPDIA,
         MAP = VSBPDIA + 1/3 * PP,
         RATIO = VSBPSYS/VSBPDIA,
         APP = PP * VSPULSE,
         PPR = VSPULSE /  PP,
         HSI = VSPULSE * MAP / VSBPSYS) %>%
  relocate(DIAGNOSIS, .after = HSI)


params = list(
  iterations = 1300, 
  learning_rate = 0.1014738735791829, 
  l2_leaf_reg = 1.113932097894264, 
  bagging_temperature = 0.5824798842949885,
  random_strength = 1.117577499175244, 
  depth = 6,
  random_seed = 42,
  min_data_in_leaf = 77, 
  colsample_bylevel = 0.7599261618674156,
  objective = "MultiClass"
)

shapviz.catboost.Model <- function(object, X_pred, X = X_pred, collapse = NULL, ...) {
  # Check if the input data is in the correct format for CatBoost
  if (!inherits(X_pred, "catboost.Pool")) {
    X_pred <- catboost.load_pool(X_pred)
  }
  
  # Extract SHAP values using CatBoost for a multi-class model
  S <- catboost.get_feature_importance(object, X_pred, type = "ShapValues", ...)
  
  # Determine the number of classes
  n_classes <- 3
  
  # Initialize a list to store SHAP values for each class
  shap_list <- list()
  
  # Number of features (assuming SHAP values include an extra column for baseline)
  pp <- ncol(X_pred) + 1
  
  # Loop through each class to extract class-specific SHAP values
  for (class_idx in 1:n_classes) {
    # Each class's SHAP values are in different slices of the result matrix
    class_shap <- S[, class_idx, 1:pp]
    
    # Extract baseline for the current class
    baseline <- class_shap[1, pp]
    
    # Remove the last column which is the baseline and assign proper column names
    class_shap <- class_shap[, -pp, drop = FALSE]
    colnames(class_shap) <- colnames(X_pred)
    
    # Store SHAP values for this class in a list
    shap_list[[paste0("class_", class_idx)]] <- shapviz::shapviz(class_shap, X = X, baseline = baseline, collapse = collapse)
  }
  
  # Return the list of SHAP visualizations for each class
  return(shap_list)
}



X_pool <- catboost.load_pool(data_bsln[,-ncol(data_bsln )], label = data_bsln [,ncol(data_bsln )])
fit <- catboost.train(X_pool, params = params)
shp <- shapviz::shapviz(fit, X_pred = X_pool, X = data_bsln)
shapviz::sv_importance(shp[[3]])

shapviz::sv_importance(shp[[2]], 'beeswarm') +
  theme_classic()


tiff(file="D:/AZ/adni_mci/pics/ad_bar_1.tiff", width = 5.5, height = 5, units = 'in', res=300)
shapviz::sv_importance(shp[[3]])+
  theme_classic()
dev.off()

tiff(file="D:/AZ/adni_mci/pics/ad_swarm_1.tiff", width = 7, height = 5, units = 'in', res=300)
shapviz::sv_importance(shp[[3]],'beeswarm')+
  theme_classic()
dev.off()


# Model on set of significant features

feat <- c('VSPULSE',
         'LIMMTOTAL',
         'LDELTOTAL',
         'GDTOTAL',
         'HMT100',
         'HMT15',
         'HMT16',
         'HMT8',
         'PPR',
         'HSI',
         'GDMEMORY',
         'apoe',
         'MHPSYCH',
         'PTEDUCAT',
         'GDDROP',
         'NXGAIT',
         'PTHOME',
         'GDBETTER',
         'GDBORED')

data_feature_select <- data %>%
  mutate(DIAGNOSIS = case_when(DIAGNOSIS == "Control" ~ 0,
                               DIAGNOSIS == "MCI" ~ 1,
                               DIAGNOSIS == "AZ" ~ 2) ) %>%
  mutate(apoe = if_else(apoe %in% c(1,2), 0, 1),
         PP = VSBPSYS - VSBPDIA,
         MAP = VSBPDIA + 1/3 * PP,
         RATIO = VSBPSYS/VSBPDIA,
         APP = PP * VSPULSE,
         PPR = VSPULSE /  PP,
         HSI = VSPULSE * MAP / VSBPSYS) %>%
  select(all_of(c(feat,'DIAGNOSIS')))

X_pool <- catboost.load_pool(data_feature_select[,-ncol(data_feature_select)], label = data_feature_select[,ncol(data_feature_select)])
fit <- catboost.train(X_pool, params = params)
shp <- shapviz::shapviz(fit, X_pred = X_pool, X = data_feature_select)
shapviz::sv_importance(shp[[3]])

shapviz::sv_importance(shp[[2]], 'beeswarm') +
  theme_classic()


tiff(file="D:/AZ/adni_mci/pics/fs_ctr_bar_1.tiff", width = 5.5, height = 5, units = 'in', res=300)
shapviz::sv_importance(shp[[1]])+
  theme_classic()
dev.off()

tiff(file="D:/AZ/adni_mci/pics/fs_ctr_swarm_1.tiff", width = 7, height = 5, units = 'in', res=300)
shapviz::sv_importance(shp[[1]],'beeswarm')+
  theme_classic()
dev.off()


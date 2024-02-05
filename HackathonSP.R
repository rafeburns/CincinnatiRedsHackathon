set.seed(123)
library(dplyr)
library(tidyverse)
library(xgboost)

fangraphs <- read.csv('/Users/rafeburns/Downloads/fangraphs_season_level.csv')

# Selecting variables
fangraphs <- fangraphs %>%
  filter(Role == 'SP')
fangraphs <- select(fangraphs, c(
  "ERA",
  "WHIP",
  "LOB_pct",
  "H_per_9",
  "HR_to_FB",
  "Barrel_pct",
  "LA",
  "GB_to_FB",
  "OSwing_pct",
  "Location_plus",
  "K_minus_BB_pct",
  "ZContact_pct",
  "EV",
  "G",
  "Arsenal_entropy",
  "WPA_to_LI"
))
fangraphs <- fangraphs %>%
  filter(G >= 10)
fangraphs <- fangraphs %>%
  select(-G)

#Split into train and test datasets
train_indexes <- sample(1:nrow(fangraphs), nrow(fangraphs) * .75)

# Creating the training matrix
fangraphs %>% 
  select(-ERA) %>% 
  dplyr::slice(train_indexes) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = fangraphs$ERA[train_indexes]) ->
  train_data
#Creates the test matrix
fangraphs %>% 
  select(-ERA) %>% 
  dplyr::slice(-train_indexes) %>% 
  as.matrix() %>% 
  xgb.DMatrix(label = fangraphs$ERA[-train_indexes]) ->
  test_data

#Finds rounds
find_rounds <- function(rounds) {
  xgb.train(
    params = list(
      eta = .2,
      objective = "reg:squarederror",
      eval_metric = "rmse"
    ),
    data = train_data,
    nrounds = ceiling(rounds),
    watchlist = list(train = train_data,
                     test = test_data)
  )$evaluation_log %>% 
    select(test_rmse) %>% 
    slice_tail() %>% 
    unlist()
}
optimize(find_rounds, c(1, 20), tol = 2)$minimum %>% 
  ceiling() -> rounds

#Tuning Parameters
tune_params <- function(param) {
  xgb.train(
    params = list(
      eta = .2,
      max_depth = ceiling(param[1]),
      max_leaf_nodes = ceiling(param[2]),
      objective = "reg:squarederror",
      eval_metric = "rmse"
    ),
    data = train_data,
    nrounds = rounds,
    watchlist = list(train = train_data,
                     test = test_data)
  )$evaluation_log %>% 
    select(test_rmse) %>% 
    slice_tail() %>% 
    unlist()
}
optim(c(6, 36), tune_params) -> best_param
depth <- best_param$par[1] %>% ceiling()
nodes <- best_param$par[2] %>% ceiling()

eta_rounds <- data.frame(eta = .2 / (1:10),
                         rounds = rounds * (1:10))
eta_rounds %>% 
  pmap(~ xgb.train(
    params = list(
      eta = .x,
      max_depth = depth,
      max_leaf_nodes = nodes,
      objective = "reg:squarederror",
      eval_metric = "rmse"
    ),
    data = train_data,
    nrounds = .y,
    watchlist = list(train = train_data,
                     test = test_data)
  )$evaluation_log %>% 
    select(test_rmse) %>% 
    slice_tail()
  ) %>% 
  unlist() ->
  test_errors

data.frame(n_round = eta_rounds$rounds,
           error = test_errors) %>% 
  ggplot(aes(n_round, error)) +
  geom_line()

final_model <- xgb.train(
  params = list(
    eta = .2,
    max_depth = depth,
    max_leaf_nodes = nodes,
    objective = "reg:squarederror",
    eval_metric = "rmse"
  ),
  data = train_data,
  nrounds = rounds,
  watchlist = list(train = train_data, test = test_data)
)

# Get feature importance scores
importance_scores <- xgb.importance(model = final_model)
print(importance_scores)
xgb.plot.importance(importance_matrix = importance_scores, title = "Importance")
library(writexl)
write_xlsx(importance_scores, path = "/Users/rafeburns/Documents/feature_importance_SP.xlsx")

# Creates dataframe of Relieving pitchers
fangraphs_rp <- read.csv('/Users/rafeburns/Downloads/fangraphs_season_level.csv')
fangraphs_rp <- fangraphs_rp %>%
  filter(Role == 'RP') %>%
  select(c(
    "WHIP",
    "LOB_pct",
    "H_per_9",
    "HR_to_FB",
    "Barrel_pct",
    "LA",
    "GB_to_FB",
    "OSwing_pct",
    "Location_plus",
    "K_minus_BB_pct",
    "ZContact_pct",
    "EV",
    "G",
    "Arsenal_entropy",
    "WPA_to_LI"
  )) %>%
  filter(G >= 40) %>%
  select(-G)

# Creates matrix of relieving pitchers and predicts ERA according to relievers model
rp_matrix <- xgb.DMatrix(data = as.matrix(fangraphs_rp))
predictions <- predict(final_model, rp_matrix)
print(predictions)

# Creates dataframe with relieving pitchers and predicted ERA as a starting pitcher
fangraphs_rp_ID <- read.csv('/Users/rafeburns/Downloads/fangraphs_season_level.csv')
fangraphs_rp_ID <- fangraphs_rp_ID %>%
  filter(Role == "RP") %>%
  filter(G >= 40) %>%
  select(c(MLBAMID, Name, ERA, Season))
predicted_RP_ERA <- data.frame(Name = fangraphs_rp_ID$Name, Season = fangraphs_rp_ID$Season, ID = fangraphs_rp_ID$MLBAMID, RP_ERA = fangraphs_rp_ID$ERA, predicted_ERA = predictions)
predicted_RP_ERA <- predicted_RP_ERA %>%
  filter(predicted_ERA < RP_ERA) %>%
  mutate(ERA_dif = ((RP_ERA - predicted_ERA)/RP_ERA)*100)  
write_xlsx(predicted_RP_ERA, path = "/Users/rafeburns/Documents/predicted_RP_ERA.xlsx")

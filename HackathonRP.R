set.seed(123)
library(dplyr)
library(tidyverse)
library(xgboost)
library(writexl)

fangraphs <- read.csv('/Users/rafeburns/Downloads/fangraphs_season_level.csv')

#Selecting variables
fangraphs <- fangraphs %>%
  filter(Role == 'RP')
fangraphs <- select(fangraphs, c(
  "ERA",
  "LOB_pct",
  "WHIP",
  "HR_to_FB",
  "LA",
  "GB_to_FB",
  "Barrel_pct",
  "H_per_9",
  "OSwing_pct",
  "WPA_to_LI",
  "Location_plus",
  "ZContact_pct",
  "EV",
  "K_minus_BB_pct",
  "CSW_pct",
  "TTO_pct",
  "OContact_pct",
  "FStrike_pct",
  "ZSwing_pct",
  "Arsenal_entropy",
  "HardHit_pct",
  "Pitching_plus",
  "FAv",
  "BABIP",
  "BB_pct",
  "G"
  ))
#Filter for Relievers with over 20 games played
fangraphs <- fangraphs %>%
  filter(G >= 20)
#Omit games from dataset
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

#Creates final model
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
write_xlsx(importance_scores, path = "/Users/rafeburns/Documents/feature_importance_RP.xlsx")

# Creates dataframe of starting pitchers
fangraphs_sp <- read.csv('/Users/rafeburns/Downloads/fangraphs_season_level.csv')
fangraphs_sp <- fangraphs_sp %>%
  filter(Role == 'SP') %>%
  select(c(
    "LOB_pct",
    "WHIP",
    "HR_to_FB",
    "LA",
    "GB_to_FB",
    "Barrel_pct",
    "H_per_9",
    "OSwing_pct",
    "WPA_to_LI",
    "Location_plus",
    "ZContact_pct",
    "EV",
    "K_minus_BB_pct",
    "CSW_pct",
    "TTO_pct",
    "OContact_pct",
    "FStrike_pct",
    "ZSwing_pct",
    "Arsenal_entropy",
    "HardHit_pct",
    "Pitching_plus",
    "FAv",
    "BABIP",
    "BB_pct",
    "G"
  )) %>%
  filter(G >= 20) %>%
  select(-G)

# Creates matrix of starting pitchers and predicts ERA according to relievers model
sp_matrix <- xgb.DMatrix(data = as.matrix(fangraphs_sp))
predictions <- predict(final_model, sp_matrix)
print(predictions)

# Creates dataframe with starting pitchers and predicted ERA as a relieving pitcher
fangraphs_sp_ID <- read.csv('/Users/rafeburns/Downloads/fangraphs_season_level.csv')
fangraphs_sp_ID <- fangraphs_sp_ID %>%
  filter(Role == "SP") %>%
  filter(G >= 20) %>%
  select(c(MLBAMID, Name, ERA, Season))
predicted_SP_ERA <- data.frame(Name = fangraphs_sp_ID$Name, Season = fangraphs_sp_ID$Season, ID = fangraphs_sp_ID$MLBAMID, SP_ERA = fangraphs_sp_ID$ERA, predicted_ERA = predictions)
predicted_SP_ERA <- predicted_SP_ERA %>%
  filter(predicted_ERA < SP_ERA) %>%
  mutate(ERA_dif = SP_ERA - predicted_ERA)  
write_xlsx(predicted_SP_ERA, path = "/Users/rafeburns/Documents/predicted_SP_ERA.xlsx")

library(dplyr)
library(stringr)
library(ggplot2)
library(mgcv)
library(tidymodels)
library(modeltime)
library(timetk)
library(doParallel)
library(themis)
library(reactable)
library(forcats)
library(lubridate)

### Parallel process ----

# speed up computation with parallel processing

doParallel::registerDoParallel()

ncores <- parallel::detectCores(logical = TRUE)
registerDoParallel(cores = ncores)


# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

get_sunday <- function(x){
  x <- as.Date(x)
  sunday <- x - wday(x) + 7
  return(sunday)
}


# Indonesia
# Iran 
# Israel
# Marruecos x
# Qatar
# Arabia Saudita
# Eslovenia
# Sudáfrica

# Liga nigeriana ----

data <- read.csv("NPFL-Nigeria_2022-2023.csv", sep = ",") %>%
  bind_rows(read.csv("NPFL-Nigeria_2021-2022.csv", sep = ","))

data <- data %>%
  mutate(diferencia = home_score - away_score,
         winner = case_when(diferencia > 0 ~ "local",
                            diferencia < 0 ~ "visita",
                            diferencia == 0 ~ "empate"),
         goals = home_score + away_score,
         day_month_year = paste0(day_month, year),
         day_month_year = as.Date(day_month_year, format = "%d-%m-%Y"),
         home = factor(case_when(diferencia > 0 ~ 1,
                                 diferencia <= 0 ~ 0)),
         away = factor(case_when(diferencia < 0 ~ 1,
                                 diferencia >= 0 ~ 0)),
         draw = factor(ifelse(diferencia == 0, 1,0)),
         date_num = as.numeric(day_month_year),
         day_of_week = format(day_month_year, format = "%A"),
         Date_sunday = get_sunday(day_month_year),
         Date_sunday = ifelse(day_of_week == "domingo",
                              Date_sunday-7, Date_sunday),
         Date_sunday = as.Date(Date_sunday + 1 ,
                               origin = "1970-01-01")
  )

prop.table(table(data$winner)) *100

table(data$home_team)

model <- glm(winner ~ home_team,
             family = binomial(link = "logit"),
             data = data)

summary(model)


datos <- data %>%
  filter(winner2 %in% "local") %>%
  group_by(home_team) %>%
  summarise(n = n()) %>% 
  arrange(-n)
datos

data %>%
  filter(home_team %in% "Lobi Stars")

data %>%
  filter(away_team %in% "Remo Stars")


summary(data)

# Liga Macao ----

data <- read.csv("Elite_League-Macao_2022.csv", sep = ",") %>%
  bind_rows(read.csv("Elite_League-Macao_2021.csv", sep = ","))

data <- data %>%
  mutate(diferencia = home_score - away_score,
         winner = case_when(diferencia > 0 ~ "local",
                            diferencia < 0 ~ "visita",
                            diferencia == 0 ~ "empate"),
         goals = home_score + away_score,
         day_month_year = paste0(day_month, year),
         day_month_year = as.Date(day_month_year, format = "%d-%m-%Y"),
         home = factor(case_when(diferencia > 0 ~ 1,
                                 diferencia <= 0 ~ 0)),
         away = factor(case_when(diferencia < 0 ~ 1,
                                 diferencia >= 0 ~ 0)),
         draw = factor(ifelse(diferencia == 0, 1,0)),
         date_num = as.numeric(day_month_year),
         day_of_week = format(day_month_year, format = "%A"),
         Date_sunday = get_sunday(day_month_year),
         Date_sunday = ifelse(day_of_week == "domingo",
                              Date_sunday-7, Date_sunday),
         Date_sunday = as.Date(Date_sunday + 1 ,
                               origin = "1970-01-01")
  )

prop.table(table(data$winner2)) *100

table(data$home_team)

model <- glm(winner ~ home_team,
             family = binomial(link = "logit"),
             data = data)

model <- glm(goals ~ home_team + away_team,
             family = poisson(link = "log"),
             data = data)

summary(model)

plot(model)
preddata <- data.frame(act = data$goals,
           pred = predict(model, type = "response")) %>%
  mutate(res = act - pred)

ggplot(preddata,
       aes(x = act,
           y = pred)) +
  geom_point()

datos <- data %>%
  filter(winner2 %in% "local") %>%
  group_by(home_team) %>%
  summarise(n = n(),
            home_score = mean(home_score)) %>% 
  arrange(-n)
datos

data %>%
  filter(home_team %in% "Casa de Portugal") #%>% summary()


summary(data)

hist(data$goals)
table(data$goals)

library(summarytools)

freq(data$goals)


### Liga de Bolivia ----

data <- read.csv("Primera_Division-Bolivia_2022.csv", sep = ",") %>%
  bind_rows(read.csv("Primera_Division-Bolivia_2023.csv", sep = ","))

data <- data %>%
  mutate(diferencia = home_score - away_score,
         winner = case_when(diferencia > 0 ~ "local",
                            diferencia < 0 ~ "visita",
                            diferencia == 0 ~ "empate"),
         goals = home_score + away_score,
         day_month_year = paste0(day_month, year),
         day_month_year = as.Date(day_month_year, format = "%d-%m-%Y"),
         home = factor(case_when(diferencia > 0 ~ 1,
                                 diferencia <= 0 ~ 0)),
         away = factor(case_when(diferencia < 0 ~ 1,
                                 diferencia >= 0 ~ 0)),
         draw = factor(ifelse(diferencia == 0, 1,0)),
         date_num = as.numeric(day_month_year),
         day_of_week = format(day_month_year, format = "%A"),
         Date_sunday = get_sunday(day_month_year),
         Date_sunday = ifelse(day_of_week == "domingo",
                              Date_sunday-7, Date_sunday),
         Date_sunday = as.Date(Date_sunday + 1 ,
                               origin = "1970-01-01")
  )

prop.table(table(data$winner)) *100

table(data$home_team)

model <- glm(home ~ home_team,
             family = binomial(link = "logit"),
             data = data)

model <- glm(goals ~ home_team * away_team,
             family = poisson(link = "log"),
             data = data)

model <- gam(home ~ home_team * away_team + s(date_num),
             family = binomial(link = "logit"),
             data = data)

summary(model)

plot(model)
preddata <- data.frame(truth = data$home,
                       Class2 = predict(model, type = "response")) %>%
  mutate(#res = act - pred,
    truth = factor(truth),
    predicted = factor(ifelse(Class1 >= 0.05, 1,0)),
    Class1 = 1 - Class2)

ggplot(preddata,
       aes(x = act,
           y = pred)) +
  geom_point()

conf_mat(preddata, truth = truth, estimate = predicted)

classification_metrics <- metric_set(accuracy, specificity,
                                     sensitivity, mcc, f_meas)
classification_metrics(preddata, truth = truth, estimate = predicted)

two_class_curve <- roc_curve(preddata, truth, Class2)
two_class_curve
roc_auc(preddata, truth, Class1)
autoplot(two_class_curve)

datos <- data %>%
  filter(home %in% 1) %>%
  group_by(home_team) %>%
  summarise(n = n(),
            goals = mean(goals)) %>% 
  arrange(-goals)
datos

data %>%
  filter(home_team %in% "Casa de Portugal") #%>% summary()

summary(data)

hist(data$goals)
table(data$goals)

library(summarytools)

freq(data$goals)

### Liga de Egipto ----

data <- read.csv("Egyptian_Premier_League_2022-2023.csv", sep = ",") #%>%
  # bind_rows(read.csv("Primera_Division-Bolivia_2023.csv", sep = ","))

### Liga de Marruecos ----

data <- read.csv("Botola_Pro_Marruecos_2022-2023.csv", sep = ",") %>%
  bind_rows(read.csv("Botola_Pro_Marruecos_2021-2022.csv", sep = ","))

### Liga de Costa Rica ----

data <- read.csv("Primera_Division-Costa_Rica_2021-2022.csv", sep = ",") %>%
  bind_rows(read.csv("Primera_Division-Costa_Rica_2022-2023.csv", sep = ","))

### Liga de Guatemala ----

data <- read.csv("Liga_Nacional-Guatemala_2021-2022.csv", sep = ",") %>%
  bind_rows(read.csv("Liga_Nacional-Guatemala_2022-2023.csv", sep = ","))

### Liga de Irlanda ----

data <- read.csv("Premier-Division_Ireland_2020.csv", sep = ",") %>%
  bind_rows(read.csv("Premier-Division_Ireland_2021.csv", sep = ","),
            read.csv("Premier-Division_Ireland_2022.csv", sep = ","),
            read.csv("Premier-Division_Ireland_2023.csv", sep = ",")
  )

### Liga de Sur Africa ----

data <- read.csv("Premier-League_South-Africa_2019-2020.csv", sep = ",") %>%
  bind_rows(read.csv("Premier-League_South-Africa_2020-2021.csv", sep = ","),
            read.csv("Premier-League_South-Africa_2021-2022.csv", sep = ","),
            read.csv("Premier-League_South-Africa_2022-2023.csv", sep = ",")
  )

### Liga de Algeria ----

data <- read.csv("Ligue-1_Algeria_2019-2020.csv", sep = ",") %>%
  bind_rows(read.csv("Ligue-1_Algeria_2020-2021.csv", sep = ","),
            read.csv("Ligue-1_Algeria_2021-2022.csv", sep = ","),
            read.csv("Ligue-1_Algeria_2022-2023.csv", sep = ",")
  )


### Champions League ----

data <- read.csv("Champions-League_2020-2021.csv", sep = ",") %>%
  bind_rows(read.csv("Champions-League_2021-2022.csv", sep = ","),
            read.csv("Champions-League_2022-2023.csv", sep = ","))

### Copa Libertadores ----

data <- read.csv("Copa-Libertadores_2021.csv", sep = ",") %>%
  bind_rows(read.csv("Copa-Libertadores_2022.csv", sep = ","),
            read.csv("Copa-Libertadores_2023.csv", sep = ","))

### Europa League ----

data <- read.csv("Europa-League_2020-2021.csv", sep = ",") %>%
  filter(!home_score %in% "-") %>%
  bind_rows(read.csv("Europa-League_2021-2022.csv", sep = ";") %>%
              filter(!home_score %in% "-"),
            read.csv("Europa-League_2022-2023.csv", sep = ",") %>%
              filter(!home_score %in% "-"))

### Copa do Brasil ----

data <- read.csv("Copa-do-Brasil_2021.csv", sep = ",") %>%
  filter(!home_score %in% "-") %>%
  bind_rows(read.csv("Copa-do-Brasil_2022.csv", sep = ",") %>%
              filter(!home_score %in% "-"),
            read.csv("Copa-do-Brasil_2023.csv", sep = ",") %>%
              filter(!home_score %in% "-"))

### Transformar data ----

data <- data %>%
  mutate(diferencia = home_score - away_score,
         winner = case_when(diferencia > 0 ~ "local",
                            diferencia < 0 ~ "visita",
                            diferencia == 0 ~ "empate"),
         goals = home_score + away_score,
         goals_mas0.5 = ifelse(goals >0.5, 1, 0),
         goals_mas1.5 = ifelse(goals >1.5, 1, 0),
         goals_mas2.5 = ifelse(goals >2.5, 1, 0),
         goals_mas3.5 = ifelse(goals >3.5, 1, 0),
         goals_mas4.5 = ifelse(goals >4.5, 1, 0),
         goals_mas0.5 = factor(goals_mas0.5,
                               levels = c(1,0)),
         goals_mas1.5 = factor(goals_mas1.5,
                               levels = c(1,0)),
         goals_mas2.5 = factor(goals_mas2.5,
                               levels = c(1,0)),
         goals_mas3.5 = factor(goals_mas3.5,
                               levels = c(1,0)),
         goals_mas4.5 = factor(goals_mas4.5,
                               levels = c(1,0)),
         day_month_year = paste0(day_month, year),
         day_month_year = as.Date(day_month_year, format = "%d-%m-%Y"),
         home = factor(case_when(diferencia > 0 ~ 1,
                                 diferencia <= 0 ~ 0),
                       levels = c(1,0)),
         away = factor(case_when(diferencia < 0 ~ 1,
                                 diferencia >= 0 ~ 0),
                       levels = c(1,0)),
         draw = factor(ifelse(diferencia == 0, 1,0),
                       levels = c(1,0)),
         date_num = as.numeric(day_month_year),
         day_of_week = format(day_month_year, format = "%A"),
         Date_sunday = get_sunday(day_month_year),
         Date_sunday = ifelse(day_of_week == "domingo",
                              Date_sunday-7, Date_sunday),
         Date_sunday = as.Date(Date_sunday + 1 ,
                               origin = "1970-01-01")
  )

str(data)
prop.table(table(data$winner)) *100
prop.table(table(data$goals_mas0.5)) *100
prop.table(table(data$goals_mas1.5)) *100
prop.table(table(data$goals_mas2.5)) *100
prop.table(table(data$goals_mas3.5)) *100
prop.table(table(data$goals_mas4.5)) *100

table(data$home_team)

# Modelamiento predictivo de goles ----

### gamm ----

# Initialize a linear regression object, linear_model
gamm_mgcv_model <- gen_additive_mod() %>% 
  # Set the model engine
  set_engine("mgcv", 
             family=Gamma(link="log"),
             method = "REML",
             random = list(home_team=~1,
                           away_team=~1
             )#,
             # SEM_FENOLOGICA | ID,
             # correlation=corAR1()
  ) %>% #identity, log, inverse
  # Set the model mode
  set_mode('regression')

#### Recipe ----

data_rec <- recipe(`goals` ~ .,
                   data = data) %>%
  # step_select(where(base::is.numeric),
  #             home_team, away_team,
  #             day_month_year) #%>%
  step_mutate_at(contains(c("goals")),
  # fn = list(log = ~log(.+1)),
  fn = ~(.+1))

data_prep <- prep(data_rec)
data_juiced <- juice(data_prep)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
# data_train_prep <- bake(data_prep, new_data = data_train)
# data_test_prep  <- bake(data_prep, new_data = data_test)
# data_new_prep  <- bake(data_prep,
#                        new_data = data_new %>%
#                          dplyr::mutate(ID_x_COD = as.factor(ID_x_COD))
# )

# # No mover el siguiente código
# data_rec <- recipe(`goals` ~ .,
#                    data = data_juiced) %>%
#   # update_role(ID, new_role = "id variable") %>%
#   # update_role(ID_T, new_role = "id turn variable") %>%
#   update_role(COD, new_role = "id evaluation variable") %>%
#   update_role(FUNDO_num, new_role = "splitting variable") %>%
#   update_role(ID_x_COD, new_role = "id_x_cod variable") %>%
#   update_role(Fecha, new_role = "date variable")

#### Definir workflow ----

formula_gamm = goals ~ 
  s(home_team, bs = "re") +
  s(away_team, bs = "re") +
  s(home_team, date_num, bs = "re") +
  s(away_team, date_num, bs = "re") #+
  # s(date_num)

gamm_wf <- workflow() %>%
  add_recipe(data_rec) %>%
  add_model(gamm_mgcv_model,
            formula = formula_gamm) 

#### Definir CV ----

set.seed(234)
# data_folds <- bootstraps(data_juiced,
#                          times = 10,
#                          apparent = TRUE)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  strata = `goals.cum`)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  repeats = 4,
#                                  strata = `goals.cum`)
# data_folds <- group_vfold_cv(data_juiced, 
#                               group = "ID",
#                               v = n_split)

data_folds <- data_juiced %>%
  time_series_cv(
    date_var    = day_month_year,
    initial     = "30 days", #33
    assess      = "28 days", # 17
    skip        = "28 days", # 7
    slice_limit = 5,
    cumulative  = TRUE#,
    # prop = 0.8
  )

data_folds

data_folds %>%
  plot_time_series_cv_plan(day_month_year, goals,
                           .interactive = FALSE)

#### Entrenar gamm ----

get_glm_coefs <- function(x) {
  x %>%
    # get the glm model object
    extract_fit_engine() %>%
    # transform its format
    tidy(parametric = TRUE)
}

multi_met <- yardstick::metric_set(yardstick::rmse, rsq, rsq_trad, smape, 
                                   mape, yardstick::mae, mpe, # mae,  # mase, msd, 
                                   ccc#, mape,
)


# registerDoParallel(cores = detectCores())

tictoc::tic()
validacion_gamm <-
  gamm_wf %>%
  fit_resamples(
    resamples = data_folds,
    metrics = multi_met,
    control = control_resamples( #control_grid
      extract = get_glm_coefs, #identity
      save_pred = TRUE,
      verbose = TRUE#,
      #parallel_over = "resamples"
    )
  )
tictoc::toc()

gamm_resamples <- gamm_wf %>%
  fit_resamples(data_folds, control = control_resamples( #control_grid
    extract = get_glm_coefs, #identity
    event_level = "first",
    save_pred = TRUE,
    verbose = TRUE#,
    #parallel_over = "resamples"
  ))

# head(validacion_mlm)

# gamm_coefs <-
#   validacion_gamm %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts)
# 
# gamm_coefs %>% select(id, term, statistic, p.value)
# 
# gamm_coefs %>%
#   filter(term != "(Intercept)") %>%
#   ggplot(aes(x = term, y = statistic, group = id, col = id)) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_line(alpha = 0.3, lwd = 1.2) +
#   labs(y = "Coefficient", x = NULL) +
#   theme(legend.position = "none") +
#   theme(axis.text.x=element_text(angle=90, hjust=0.5))

splitdat <- NULL

for(i in 1:length(gamm_resamples$splits)){
  splitdat <- splitdat %>% 
    dplyr::bind_rows(assessment(gamm_resamples$splits[[i]]) %>%
                       dplyr::mutate(id = paste0("Slice",i)) %>%
                       dplyr::select(-goals) %>%
                       dplyr::bind_cols(gamm_resamples$.predictions[[i]]))
}

# Media de métricas

splitdat %>%
  group_by(id) %>%
  multi_met(
    truth    = goals,
    estimate = .pred,
    na_rm    = TRUE
  ) #%>%
  # ungroup() %>%
  # dplyr::select(-id)  %>%
  # summarise_all(.funs = c(mean))

# Métricas promedio de todas las particiones
metrics_gamm <- 
  validacion_gamm %>%
  collect_metrics(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm

# Métricas individuales de cada una de las particiones
metrics_gamm_complete <- 
  validacion_gamm %>% 
  collect_metrics(summarize = FALSE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm_complete

# Valores de validación (mae y rmse) obtenidos en cada partición y repetición.
p1 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .estimate, fill = .metric)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(. ~ .metric, scales = "free") +
  theme(axis.text.x=element_text(angle=90, hjust=0.5))
p2 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.1) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  # coord_flip() +
  theme_bw() +
  facet_wrap(.metric ~ ., scales = "free") #+
# theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, align = "v") %>% 
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución errores de validación cruzada", size = 15)
  )

# Predicciones individuales de cada observación.
# Si summarize = TRUE se agregan todos los valores predichos a nivel de
# observación.
# validacion_gamm %>% collect_predictions(summarize = TRUE) %>% head()

validation_gamm_predictions <- 
  validacion_gamm %>% 
  collect_predictions(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")

p1 <- ggplot(
  data = validation_gamm_predictions,
  aes(x = goals, y = .pred)) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick") +
  labs(title = "Valor predicho vs valor real") +
  theme_bw()

p2 <- ggplot(
  data = validation_gamm_predictions,
  aes(x = .row, y = goals - .pred)
) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept =  0, color = "firebrick") +
  labs(title = "Residuos del modelo") +
  theme_bw()

p3 <- ggplot(
  data = validation_gamm_predictions,
  aes(x = goals - .pred)
) +
  geom_density() + 
  labs(title = "Distribución residuos del modelo") +
  theme_bw()

p4 <- ggplot(
  data = validation_gamm_predictions,
  aes(sample = goals - .pred)
) +
  geom_qq() +
  geom_qq_line(color = "firebrick") +
  labs(title = "Q-Q residuos del modelo") +
  theme_bw()

ggpubr::ggarrange(plotlist = list(p1, p2, p3, p4)) %>%
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución residuos", size = 15, face = "bold")
  )

#### Entrenamiento de modelo final ----

# gamm_res <- last_fit(linear_model, data_rec, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]
# 
# gamm_res <- last_fit(gamm_wf, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]

# registerDoFuture()
# plan(multisession)

# set.seed(69)
gamm_model_goals <- gamm_mgcv_model %>%
  fit(formula_gamm,
    data = data_juiced)

# vip(mlm_model, num_features = 100)

# vi_gamm <- 
#   mlm_model %>%
#   vip::vi() %>%
#   dplyr::mutate(Importance = abs(Importance),
#                 Variable = fct_reorder(Variable, Importance),
#                 Total_Importance = sum(Importance),
#                 Importance = Importance/Total_Importance) %>%
#   dplyr::filter(!Importance == 0) %>%
#   ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
#   geom_col() +
#   scale_fill_manual(breaks = c("NEG","POS"),
#                     values = c("red","blue")) +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(y = NULL)
# vi_gamm

gamm_model_goals

# anova(gamm_model$fit) 
# 
# summary(gamm_model$fit)
# 
# par(mfrow = c(2,2))
# gam.check(gamm_model$fit)
# par(mfrow = c(1,1))
# plot(gamm_model$fit)
# vis.gam(gamm_model$fit)
# ## Raw residuals still show correlation, of course...
# acf(residuals(gamm_model$fit),
#     main="raw residual ACF")
# ## But standardized are now fine...
# acf(residuals(gamm_model$fit,
#               type="scaled.pearson"),
#     main="standardized residual ACF")

gamm_model_goals %>% broom::tidy()
gamm_model_goals %>% broom::tidy(parametric = T)

performance::check_model(gamm_model_goals)

### Proyección goles ----

predicciones <- read.csv("predict.csv", sep = ",")%>%
  mutate(day_month_year = paste0(day_month, year),
         day_month_year = as.Date(day_month_year, format = "%d-%m-%Y"),
         date_num = as.numeric(day_month_year)
  )

predfinal <- predicciones %>%
  bind_cols(predict(gamm_model_goals, new_data = predicciones))

predfinal %>%
  # filter(day_month_year <= "2023-02-27") %>%
  arrange(-.pred)

# Modelamiento predictivo de victorias locales ----

### gamm ----

# Initialize a linear regression object, linear_model
gamm_mgcv_model <- gen_additive_mod() %>% 
  # Set the model engine
  set_engine("mgcv", 
             family=binomial(link="logit"),
             method = "REML",
             random = list(home_team=~1,
                           away_team=~1
             )#,
             # SEM_FENOLOGICA | ID,
             # correlation=corAR1()
  ) %>% #identity, log, inverse
  # Set the model mode
  set_mode('classification')

#### Recipe ----

data_rec <- recipe(`home` ~ .,
                   data = data) %>%
  step_upsample(`home`)

data_prep <- prep(data_rec)
data_juiced <- juice(data_prep)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
# data_train_prep <- bake(data_prep, new_data = data_train)
# data_test_prep  <- bake(data_prep, new_data = data_test)
# data_new_prep  <- bake(data_prep,
#                        new_data = data_new %>%
#                          dplyr::mutate(ID_x_COD = as.factor(ID_x_COD))
# )

# # No mover el siguiente código
# data_rec <- recipe(`goals` ~ .,
#                    data = data_juiced) %>%
#   # update_role(ID, new_role = "id variable") %>%
#   # update_role(ID_T, new_role = "id turn variable") %>%
#   update_role(COD, new_role = "id evaluation variable") %>%
#   update_role(FUNDO_num, new_role = "splitting variable") %>%
#   update_role(ID_x_COD, new_role = "id_x_cod variable") %>%
#   update_role(Fecha, new_role = "date variable")

#### Definir workflow ----

formula_gamm = home ~ 
  s(home_team, bs = "re") +
  s(away_team, bs = "re") +
  # home_team:away_team +
  s(home_team, date_num, bs = "re") #+
  # s(away_team, date_num, bs = "re") #+
  # s(date_num)

gamm_wf <- workflow() %>%
  add_recipe(data_rec) %>%
  add_model(gamm_mgcv_model,
            formula = formula_gamm) 

#### Definir CV ----

set.seed(234)
# data_folds <- bootstraps(data_juiced,
#                          times = 10,
#                          apparent = TRUE)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  strata = `goals.cum`)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  repeats = 4,
#                                  strata = `goals.cum`)
# data_folds <- group_vfold_cv(data_juiced, 
#                               group = "ID",
#                               v = n_split)

data_folds <- data_juiced %>%
  time_series_cv(
    date_var    = day_month_year,
    initial     = "30 days", #33
    assess      = "28 days", # 17
    skip        = "28 days", # 7
    slice_limit = 20,
    cumulative  = TRUE#,
    # prop = 0.8
  )

data_folds

data_folds %>%
  plot_time_series_cv_plan(day_month_year, home,
                           .interactive = FALSE)

#### Entrenar gamm ----

get_glm_coefs <- function(x) {
  x %>%
    # get the glm model object
    extract_fit_engine() %>%
    # transform its format
    tidy(parametric = TRUE)
}

multi_met <- yardstick::metric_set(accuracy, bal_accuracy, kap, 
                                   sensitivity, #sens,
                                   specificity, #spec,
                                   precision,
                                   detection_prevalence,
                                   f_meas, j_index,
                                   recall, roc_auc)


# registerDoParallel(cores = detectCores())

tictoc::tic()
validacion_gamm <-
  gamm_wf %>%
  fit_resamples(
    resamples = data_folds,
    metrics = multi_met,
    control = control_resamples( #control_grid
      extract = get_glm_coefs, #identity
      event_level = "first",
      save_pred = TRUE,
      verbose = TRUE#,
      #parallel_over = "resamples"
    )
  )
tictoc::toc()

gamm_resamples <- gamm_wf %>%
  fit_resamples(data_folds,
                control = control_resamples( #control_grid
    extract = get_glm_coefs, #identity
    event_level = "first",
    save_pred = TRUE,
    verbose = TRUE#,
    #parallel_over = "resamples"
  ))

# head(validacion_mlm)

# gamm_coefs <-
#   validacion_gamm %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts)
# 
# gamm_coefs %>% select(id, term, statistic, p.value)
# 
# gamm_coefs %>%
#   filter(term != "(Intercept)") %>%
#   ggplot(aes(x = term, y = statistic, group = id, col = id)) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_line(alpha = 0.3, lwd = 1.2) +
#   labs(y = "Coefficient", x = NULL) +
#   theme(legend.position = "none") +
#   theme(axis.text.x=element_text(angle=90, hjust=0.5))

# gamm_resamples %>%
#   unnest(cols = .predictions)

splitdat <- NULL

for(i in 1:length(gamm_resamples$splits)){
  splitdat <- splitdat %>% 
    dplyr::bind_rows(assessment(gamm_resamples$splits[[i]]) %>%
                       dplyr::mutate(id = paste0("Slice",i)) %>%
                       dplyr::select(-home) %>%
                       dplyr::bind_cols(gamm_resamples$.predictions[[i]]))
}

prop.table(table(splitdat$.pred_class))
prop.table(table(splitdat$home))

# Métricas por folio

# splitdat %>%
#   group_by(id) %>%
#   summarise(mean = mean(ifelse(home == .pred_class,1,0)),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 1) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 0) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(.pred_class %in% 1) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(.pred_class %in% 0) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())

# Media de métricas

splitdat %>%
  group_by(id) %>%
  summarise(mean = mean(ifelse(home == .pred_class,1,0))) %>%
  ungroup() %>%
  dplyr::select(-id)  %>%
  summarise(mean = mean(mean),
            n = n())

# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 1) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto)) %>%
#   ungroup() %>%
#   dplyr::select(-id) %>%
#   summarise(mean = mean(mean),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 0) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto)) %>%
#   ungroup() %>%
#   dplyr::select(-id) %>%
#   summarise(mean = mean(mean),
#             n = n())

splitdat %>%
  group_by(id) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto)) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  summarise(mean = mean(mean),
            n = n())

splitdat %>%
  group_by(id) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto)) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  summarise(mean = mean(mean),
            n = n())

# Métricas por equipo local

home_val <- "Oran"
away_val <- "CR Belouizdad"

splitdat %>%
  group_by(home_team, away_team
           ) %>%
  summarise(mean = mean(ifelse(home == .pred_class,1,0)),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% c(home_val)|
         away_team %in% c(home_val)) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = home_val) %>%
  relocate(team,1)

splitdat %>%
  group_by(home_team, away_team
           ) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% c(home_val)|
         away_team %in% c(home_val)
         ) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = home_val) %>%
  relocate(team,1)

splitdat %>%
  group_by(home_team, away_team) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% home_val|
         away_team %in% home_val) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = home_val) %>%
  relocate(team,1)

# Métricas por equipo visita

splitdat %>%
  group_by(home_team, away_team) %>%
  summarise(mean = mean(ifelse(home == .pred_class,1,0)),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% away_val|
         away_team %in% away_val) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = away_val) %>%
  relocate(team,1)

splitdat %>%
  group_by(home_team, away_team) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% away_val|
         away_team %in% away_val) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = away_val) %>%
  relocate(team,1)

splitdat %>%
  group_by(home_team, away_team) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% away_val|
         away_team %in% away_val) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = away_val) %>%
  relocate(team,1)

# Métricas cara a cara

splitdat %>%
  filter(home_team %in% c(home_val,
                          away_val)&
           away_team %in% c(home_val,
                            away_val)) %>%
  as.data.frame() %>%
  # group_by(home_team, away_team) %>%
  summarise(mean = mean(ifelse(home == .pred_class,1,0)),
            n = n()) %>%
  arrange(-mean, -n) %>%
  # summarise(mean = mean(mean),
  #           n = n()) %>%
  mutate(vs = paste(home_val,"vs", away_val)) %>%
  relocate(vs,1)

splitdat %>%
  filter(home_team %in% c(home_val,
                          away_val) &
           away_team %in% c(home_val,
                            away_val)) %>%
  # group_by(home_team, away_team) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  as.data.frame() %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  mutate(vs = paste(home_val,"vs", away_val)) %>%
  relocate(vs,1)

splitdat %>%
  filter(home_team %in% c(home_val,
                          away_val) &
           away_team %in% c(home_val,
                            away_val)) %>%
  # group_by(home_team, away_team) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  as.data.frame() %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  mutate(vs = paste(home_val,"vs", away_val)) %>%
  relocate(vs,1)

# Goles

splitdat %>%
  filter(home_team %in% home_val)

splitdat %>%
  filter(away_team %in% away_val)

splitdat %>%
  filter(home_team %in% home_val,
         away_team %in% away_val)

# Tasa de acierto en el tiempo

splitdat %>%
  group_by(Date_sunday) %>%
  summarise(mean = mean(ifelse(home == .pred_class,1,0))) %>%
  ungroup() %>%
  group_by(Date_sunday)  %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  ggplot(aes(x = Date_sunday,
         y = mean)) +
  geom_col()

# Métricas promedio de todas las particiones
metrics_gamm <- 
  validacion_gamm %>%
  collect_metrics(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm

# Métricas individuales de cada una de las particiones
metrics_gamm_complete <- 
  validacion_gamm %>% 
  collect_metrics(summarize = FALSE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm_complete

metrics_gamm_complete %>%
  ggplot(aes(x = id,
             y = .estimate)) +
  geom_col() +
  facet_wrap(.~.metric)

# Valores de validación (mae y rmse) obtenidos en cada partición y repetición.
p1 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .estimate, fill = .metric)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(. ~ .metric, scales = "free") +
  theme(axis.text.x=element_text(angle=90, hjust=0.5))
p2 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.1) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  # coord_flip() +
  theme_bw() +
  facet_wrap(.metric ~ ., scales = "free") #+
# theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, align = "v") %>% 
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución errores de validación cruzada", size = 15)
  )

# Predicciones individuales de cada observación.
# Si summarize = TRUE se agregan todos los valores predichos a nivel de
# observación.
# validacion_gamm %>% collect_predictions(summarize = TRUE) %>% head()

validation_gamm_predictions <- 
  validacion_gamm %>% 
  collect_predictions(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")

prop.table(table(validation_gamm_predictions$home))

validation_gamm_predictions %>%
  summarise(mean = mean(ifelse(home == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(home %in% 1) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

validation_gamm_predictions %>%
  filter(home %in% 0) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

validation_gamm_predictions %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

validation_gamm_predictions %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

class_totals <- 
  count(splitdat, home, name = "totals") %>% 
  mutate(class_wts = totals / sum(totals))
class_totals

cell_counts <- 
  splitdat %>% 
  group_by(home, .pred_class) %>% 
  count() %>% 
  ungroup()
cell_counts

# Calcula las cuatro sensibilidades usando 1-vs-todas
one_versus_all <- 
  cell_counts %>% 
  filter(home == .pred_class) %>% 
  full_join(class_totals, by = "home") %>% 
  mutate(sens = n / totals)
one_versus_all


# Tres estimaciones diferentes:

one_versus_all %>% 
  summarize(
    macro = mean(sens), 
    macro_wts = weighted.mean(sens, class_wts),
    micro = sum(n) / sum(totals)
  )

sensitivity(splitdat, home, .pred_class, estimator = "macro")
sensitivity(splitdat, home, .pred_class, estimator = "macro_weighted")
sensitivity(splitdat, home, .pred_class, estimator = "micro")

roc_auc(splitdat, home, .pred_1)

# roc_auc(splitdat, home, .pred_1, estimator = "macro_weighted")

splitdat%>% 
  group_by(id) %>% 
  accuracy(home, .pred_class)

splitdat %>% 
  group_by(id) %>% 
  roc_curve(home, .pred_1) %>% 
  autoplot()

# validation_gamm_predictions <- 
#   validacion_gamm %>% 
#   unnest(.predictions) %>%
#   #group_by(.row) %>%
#   #dplyr::summarise(dplyr::across(c(home,.pred_class),
#   #                               base::mean)) %>%
#   dplyr::mutate(modelo = "gamm")

#### Entrenamiento de modelo final ----

# gamm_res <- last_fit(linear_model, data_rec, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]
# 
# gamm_res <- last_fit(gamm_wf, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]

# registerDoFuture()
# plan(multisession)

# set.seed(69)
gamm_model_home <- gamm_mgcv_model %>%
  fit(formula_gamm,
      data = data_juiced)

# vip(mlm_model, num_features = 100)

# vi_gamm <- 
#   mlm_model %>%
#   vip::vi() %>%
#   dplyr::mutate(Importance = abs(Importance),
#                 Variable = fct_reorder(Variable, Importance),
#                 Total_Importance = sum(Importance),
#                 Importance = Importance/Total_Importance) %>%
#   dplyr::filter(!Importance == 0) %>%
#   ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
#   geom_col() +
#   scale_fill_manual(breaks = c("NEG","POS"),
#                     values = c("red","blue")) +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(y = NULL)
# vi_gamm

gamm_model_home

# anova(gamm_model$fit) 
# 
# summary(gamm_model$fit)
# 
# par(mfrow = c(2,2))
# gam.check(gamm_model$fit)
# par(mfrow = c(1,1))
# plot(gamm_model$fit)
# vis.gam(gamm_model$fit)
# ## Raw residuals still show correlation, of course...
# acf(residuals(gamm_model$fit),
#     main="raw residual ACF")
# ## But standardized are now fine...
# acf(residuals(gamm_model$fit,
#               type="scaled.pearson"),
#     main="standardized residual ACF")

gamm_model_home %>% broom::tidy()
gamm_model_home %>% broom::tidy(parametric = T)

performance::check_model(gamm_model_home)

#### Proyección victorias local ----

predicciones <- read.csv("predict.csv", sep = ",")%>%
  mutate(day_month_year = paste0(day_month, year),
         day_month_year = as.Date(day_month_year, format = "%d-%m-%Y"),
         date_num = as.numeric(day_month_year)
  )

predfinal <- predicciones %>%
  bind_cols(predict(gamm_model_home, new_data = predicciones)) %>%
  bind_cols(predict(gamm_model_home, new_data = predicciones, type = "prob"))

predfinal %>%
  # filter(day_month_year <= "2023-02-27") %>%
  arrange(-.pred_0)

write.csv(predfinal, "Premier League South Africa prediccion.csv", row.names = F)

# Modelamiento predictivo de goles +2.5 ----

### gamm ----

# Initialize a linear regression object, linear_model
gamm_mgcv_model <- gen_additive_mod() %>% 
  # Set the model engine
  set_engine("mgcv", 
             family=binomial(link="logit"),
             method = "REML",
             random = list(home_team=~1,
                           away_team=~1
             )#,
             # SEM_FENOLOGICA | ID,
             # correlation=corAR1()
  ) %>% #identity, log, inverse
  # Set the model mode
  set_mode('classification')

#### Recipe ----

data_rec <- recipe(`goals_mas2.5` ~ .,
                   data = data) %>%
  step_upsample(`goals_mas2.5`)

data_prep <- prep(data_rec)
data_juiced <- juice(data_prep)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
# data_train_prep <- bake(data_prep, new_data = data_train)
# data_test_prep  <- bake(data_prep, new_data = data_test)
# data_new_prep  <- bake(data_prep,
#                        new_data = data_new %>%
#                          dplyr::mutate(ID_x_COD = as.factor(ID_x_COD))
# )

# # No mover el siguiente código
# data_rec <- recipe(`goals` ~ .,
#                    data = data_juiced) %>%
#   # update_role(ID, new_role = "id variable") %>%
#   # update_role(ID_T, new_role = "id turn variable") %>%
#   update_role(COD, new_role = "id evaluation variable") %>%
#   update_role(FUNDO_num, new_role = "splitting variable") %>%
#   update_role(ID_x_COD, new_role = "id_x_cod variable") %>%
#   update_role(Fecha, new_role = "date variable")

#### Definir workflow ----

formula_gamm = goals_mas2.5 ~ 
  s(home_team, bs = "re") +
  s(away_team, bs = "re") +
  # home_team:away_team +
  s(home_team, date_num, bs = "re") #+
  # s(away_team, date_num, bs = "re") #+
# s(date_num)

gamm_wf <- workflow() %>%
  add_recipe(data_rec) %>%
  add_model(gamm_mgcv_model,
            formula = formula_gamm) 

#### Definir CV ----

set.seed(234)
# data_folds <- bootstraps(data_juiced,
#                          times = 10,
#                          apparent = TRUE)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  strata = `goals.cum`)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  repeats = 4,
#                                  strata = `goals.cum`)
# data_folds <- group_vfold_cv(data_juiced, 
#                               group = "ID",
#                               v = n_split)

data_folds <- data_juiced %>%
  time_series_cv(
    date_var    = day_month_year,
    initial     = "30 days", #33
    assess      = "28 days", # 17
    skip        = "28 days", # 7
    slice_limit = 20,
    cumulative  = TRUE#,
    # prop = 0.8
  )

data_folds

data_folds %>%
  plot_time_series_cv_plan(day_month_year, goals_mas2.5,
                           .interactive = FALSE)

#### Entrenar gamm ----

get_glm_coefs <- function(x) {
  x %>%
    # get the glm model object
    extract_fit_engine() %>%
    # transform its format
    tidy(parametric = TRUE)
}

multi_met <- yardstick::metric_set(accuracy, bal_accuracy, kap, 
                                   sensitivity, #sens,
                                   specificity, #spec,
                                   precision,
                                   detection_prevalence,
                                   f_meas, j_index,
                                   recall, roc_auc)


# registerDoParallel(cores = detectCores())

tictoc::tic()
validacion_gamm <-
  gamm_wf %>%
  fit_resamples(
    resamples = data_folds,
    metrics = multi_met,
    control = control_resamples( #control_grid
      extract = get_glm_coefs, #identity
      event_level = "first",
      save_pred = TRUE,
      verbose = TRUE#,
      #parallel_over = "resamples"
    )
  )
tictoc::toc()

gamm_resamples <- gamm_wf %>%
  fit_resamples(data_folds, control = control_resamples( #control_grid
    extract = get_glm_coefs, #identity
    event_level = "first",
    save_pred = TRUE,
    verbose = TRUE#,
    #parallel_over = "resamples"
  ))

# head(validacion_mlm)

# gamm_coefs <-
#   validacion_gamm %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts)
# 
# gamm_coefs %>% select(id, term, statistic, p.value)
# 
# gamm_coefs %>%
#   filter(term != "(Intercept)") %>%
#   ggplot(aes(x = term, y = statistic, group = id, col = id)) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_line(alpha = 0.3, lwd = 1.2) +
#   labs(y = "Coefficient", x = NULL) +
#   theme(legend.position = "none") +
#   theme(axis.text.x=element_text(angle=90, hjust=0.5))

# gamm_resamples %>%
#   unnest(cols = .predictions)

splitdat <- NULL

for(i in 1:length(gamm_resamples$splits)){
  splitdat <- splitdat %>% 
    dplyr::bind_rows(assessment(gamm_resamples$splits[[i]]) %>%
                       dplyr::mutate(id = paste0("Slice",i)) %>%
                       dplyr::select(-goals_mas2.5) %>%
                       dplyr::bind_cols(gamm_resamples$.predictions[[i]]))
}

prop.table(table(splitdat$.pred_class))
prop.table(table(splitdat$goals_mas2.5))

# Métricas por folio

# splitdat %>%
#   group_by(id) %>%
#   summarise(mean = mean(ifelse(home == .pred_class,1,0)),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 1) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 0) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(.pred_class %in% 1) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(.pred_class %in% 0) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())

# Métricas por folio

# splitdat %>%
#   group_by(id) %>%
#   summarise(mean = mean(ifelse(home == .pred_class,1,0)),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 1) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 0) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(.pred_class %in% 1) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(.pred_class %in% 0) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto),
#             n = n())

# Media de métricas

splitdat %>%
  group_by(id) %>%
  summarise(mean = mean(ifelse(goals_mas2.5 == .pred_class,1,0))) %>%
  ungroup() %>%
  dplyr::select(-id)  %>%
  summarise(mean = mean(mean),
            n = n())

# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 1) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto)) %>%
#   ungroup() %>%
#   dplyr::select(-id) %>%
#   summarise(mean = mean(mean),
#             n = n())
# 
# splitdat %>%
#   group_by(id) %>%
#   filter(home %in% 0) %>%
#   mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
#   summarise(mean = mean(acierto)) %>%
#   ungroup() %>%
#   dplyr::select(-id) %>%
#   summarise(mean = mean(mean),
#             n = n())

splitdat %>%
  group_by(id) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(goals_mas2.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto)) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  summarise(mean = mean(mean),
            n = n())

splitdat %>%
  group_by(id) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(goals_mas2.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto)) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  summarise(mean = mean(mean),
            n = n())

# Métricas por equipo local

home_val <- "Oran"
away_val <- "CR Belouizdad"

splitdat %>%
  group_by(home_team, away_team
  ) %>%
  summarise(mean = mean(ifelse(goals_mas2.5 == .pred_class,1,0)),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% c(home_val)|
           away_team %in% c(home_val)) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = home_val) %>%
  relocate(team,1)

splitdat %>%
  group_by(home_team, away_team
  ) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(goals_mas2.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% c(home_val)|
           away_team %in% c(home_val)
  ) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = home_val) %>%
  relocate(team,1)

splitdat %>%
  group_by(home_team, away_team) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(goals_mas2.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% home_val|
           away_team %in% home_val) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = home_val) %>%
  relocate(team,1)

# Métricas por equipo visita

splitdat %>%
  group_by(home_team, away_team) %>%
  summarise(mean = mean(ifelse(goals_mas2.5 == .pred_class,1,0)),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% away_val|
           away_team %in% away_val) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = away_val) %>%
  relocate(team,1)

splitdat %>%
  group_by(home_team, away_team) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(goals_mas2.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% away_val|
           away_team %in% away_val) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = away_val) %>%
  relocate(team,1)

splitdat %>%
  group_by(home_team, away_team) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(goals_mas2.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  as.data.frame() %>%
  filter(home_team %in% away_val|
           away_team %in% away_val) %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  mutate(team = away_val) %>%
  relocate(team,1)

# Métricas cara a cara

splitdat %>%
  filter(home_team %in% c(home_val,
                          away_val)&
           away_team %in% c(home_val,
                            away_val)) %>%
  as.data.frame() %>%
  # group_by(home_team, away_team) %>%
  summarise(mean = mean(ifelse(goals_mas2.5 == .pred_class,1,0)),
            n = n()) %>%
  arrange(-mean, -n) %>%
  # summarise(mean = mean(mean),
  #           n = n()) %>%
  mutate(vs = paste(home_val,"vs", away_val)) %>%
  relocate(vs,1)

splitdat %>%
  filter(home_team %in% c(home_val,
                          away_val) &
           away_team %in% c(home_val,
                            away_val)) %>%
  # group_by(home_team, away_team) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(goals_mas2.5 == .pred_class,1,0)) %>%
  as.data.frame() %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  mutate(vs = paste(home_val,"vs", away_val)) %>%
  relocate(vs,1)

splitdat %>%
  filter(home_team %in% c(home_val,
                          away_val) &
           away_team %in% c(home_val,
                            away_val)) %>%
  # group_by(home_team, away_team) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(goals_mas2.5 == .pred_class,1,0)) %>%
  as.data.frame() %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean, -n) %>%
  mutate(vs = paste(home_val,"vs", away_val)) %>%
  relocate(vs,1)

# Goles

splitdat %>%
  filter(home_team %in% home_val)

splitdat %>%
  filter(away_team %in% away_val)

splitdat %>%
  filter(home_team %in% home_val,
         away_team %in% away_val)

# Tasa de acierto en el tiempo

splitdat %>%
  group_by(Date_sunday) %>%
  summarise(mean = mean(ifelse(home == .pred_class,1,0))) %>%
  ungroup() %>%
  group_by(Date_sunday)  %>%
  summarise(mean = mean(mean),
            n = n()) %>%
  ggplot(aes(x = Date_sunday,
             y = mean)) +
  geom_col()

# Métricas promedio de todas las particiones
metrics_gamm <- 
  validacion_gamm %>%
  collect_metrics(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm

# Métricas individuales de cada una de las particiones
metrics_gamm_complete <- 
  validacion_gamm %>% 
  collect_metrics(summarize = FALSE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm_complete

metrics_gamm_complete %>%
  ggplot(aes(x = id,
             y = .estimate)) +
  geom_col() +
  facet_wrap(.~.metric)

# Valores de validación (mae y rmse) obtenidos en cada partición y repetición.
p1 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .estimate, fill = .metric)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(. ~ .metric, scales = "free") +
  theme(axis.text.x=element_text(angle=90, hjust=0.5))
p2 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.1) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  # coord_flip() +
  theme_bw() +
  facet_wrap(.metric ~ ., scales = "free") #+
# theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, align = "v") %>% 
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución errores de validación cruzada", size = 15)
  )

# Predicciones individuales de cada observación.
# Si summarize = TRUE se agregan todos los valores predichos a nivel de
# observación.
# validacion_gamm %>% collect_predictions(summarize = TRUE) %>% head()

validation_gamm_predictions <- 
  validacion_gamm %>% 
  collect_predictions(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")

prop.table(table(validation_gamm_predictions$goals_mas1.5))

validation_gamm_predictions %>%
  summarise(mean = mean(ifelse(goals_mas1.5 == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(goals_mas1.5 %in% 1) %>%
  mutate(acierto = ifelse(goals_mas1.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

validation_gamm_predictions %>%
  filter(goals_mas1.5 %in% 0) %>%
  mutate(acierto = ifelse(goals_mas1.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

validation_gamm_predictions %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(goals_mas1.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

validation_gamm_predictions %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(goals_mas1.5 == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

class_totals <- 
  count(splitdat, goals_mas1.5, name = "totals") %>% 
  mutate(class_wts = totals / sum(totals))
class_totals

cell_counts <- 
  splitdat %>% 
  group_by(goals_mas1.5, .pred_class) %>% 
  count() %>% 
  ungroup()
cell_counts

# Calcula las cuatro sensibilidades usando 1-vs-todas
one_versus_all <- 
  cell_counts %>% 
  filter(goals_mas1.5 == .pred_class) %>% 
  full_join(class_totals, by = "goals_mas1.5") %>% 
  mutate(sens = n / totals)
one_versus_all


# Tres estimaciones diferentes:

one_versus_all %>% 
  summarize(
    macro = mean(sens), 
    macro_wts = weighted.mean(sens, class_wts),
    micro = sum(n) / sum(totals)
  )

sensitivity(splitdat, home, .pred_class, estimator = "macro")
sensitivity(splitdat, home, .pred_class, estimator = "macro_weighted")
sensitivity(splitdat, home, .pred_class, estimator = "micro")

roc_auc(splitdat, home, .pred_1)

# roc_auc(splitdat, home, .pred_1, estimator = "macro_weighted")

splitdat%>% 
  group_by(id) %>% 
  accuracy(home, .pred_class)

splitdat %>% 
  group_by(id) %>% 
  roc_curve(home, .pred_1) %>% 
  autoplot()

# validation_gamm_predictions <- 
#   validacion_gamm %>% 
#   unnest(.predictions) %>%
#   #group_by(.row) %>%
#   #dplyr::summarise(dplyr::across(c(home,.pred_class),
#   #                               base::mean)) %>%
#   dplyr::mutate(modelo = "gamm")

#### Entrenamiento de modelo final ----

# gamm_res <- last_fit(linear_model, data_rec, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]
# 
# gamm_res <- last_fit(gamm_wf, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]

# registerDoFuture()
# plan(multisession)

# set.seed(69)
gamm_model_home <- gamm_mgcv_model %>%
  fit(formula_gamm,
      data = data_juiced)

# vip(mlm_model, num_features = 100)

# vi_gamm <- 
#   mlm_model %>%
#   vip::vi() %>%
#   dplyr::mutate(Importance = abs(Importance),
#                 Variable = fct_reorder(Variable, Importance),
#                 Total_Importance = sum(Importance),
#                 Importance = Importance/Total_Importance) %>%
#   dplyr::filter(!Importance == 0) %>%
#   ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
#   geom_col() +
#   scale_fill_manual(breaks = c("NEG","POS"),
#                     values = c("red","blue")) +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(y = NULL)
# vi_gamm

gamm_model_home

# anova(gamm_model$fit) 
# 
# summary(gamm_model$fit)
# 
# par(mfrow = c(2,2))
# gam.check(gamm_model$fit)
# par(mfrow = c(1,1))
# plot(gamm_model$fit)
# vis.gam(gamm_model$fit)
# ## Raw residuals still show correlation, of course...
# acf(residuals(gamm_model$fit),
#     main="raw residual ACF")
# ## But standardized are now fine...
# acf(residuals(gamm_model$fit,
#               type="scaled.pearson"),
#     main="standardized residual ACF")

gamm_model_home %>% broom::tidy()
gamm_model_home %>% broom::tidy(parametric = T)

performance::check_model(gamm_model_home)

#### Proyección +1.5goals ----

predicciones <- read.csv("Europa League prediccion.csv", sep = ",")%>%
  mutate(day_month_year = paste0(day_month, year),
         day_month_year = as.Date(day_month_year, format = "%d-%m-%Y"),
         date_num = as.numeric(day_month_year)
  )

predfinal <- predicciones %>%
  select(-c(.pred_class,
            .pred_0,
            .pred_1)) %>%
  bind_cols(predict(gamm_model_home, new_data = predicciones)) %>%
  bind_cols(predict(gamm_model_home, new_data = predicciones, type = "prob"))

predfinal %>%
  # filter(day_month_year <= "2023-02-27") %>%
  arrange(-.pred_0)

write.csv(predfinal, "Copa Libertadores prediccion.csv", row.names = F)

# Modelamiento predictivo de victorias visita ----

### gamm ----

# Initialize a linear regression object, linear_model
gamm_mgcv_model <- gen_additive_mod() %>% 
  # Set the model engine
  set_engine("mgcv", 
             family=binomial(link="probit"),
             method = "REML",
             random = list(home_team=~1,
                           away_team=~1
             )#,
             # SEM_FENOLOGICA | ID,
             # correlation=corAR1()
  ) %>% #identity, log, inverse
  # Set the model mode
  set_mode('classification')

#### Recipe ----

data_rec <- recipe(`away` ~ .,
                   data = data)

data_prep <- prep(data_rec)
data_juiced <- juice(data_prep)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
# data_train_prep <- bake(data_prep, new_data = data_train)
# data_test_prep  <- bake(data_prep, new_data = data_test)
# data_new_prep  <- bake(data_prep,
#                        new_data = data_new %>%
#                          dplyr::mutate(ID_x_COD = as.factor(ID_x_COD))
# )

# # No mover el siguiente código
# data_rec <- recipe(`goals` ~ .,
#                    data = data_juiced) %>%
#   # update_role(ID, new_role = "id variable") %>%
#   # update_role(ID_T, new_role = "id turn variable") %>%
#   update_role(COD, new_role = "id evaluation variable") %>%
#   update_role(FUNDO_num, new_role = "splitting variable") %>%
#   update_role(ID_x_COD, new_role = "id_x_cod variable") %>%
#   update_role(Fecha, new_role = "date variable")

#### Definir workflow ----

formula_gamm = away ~ 
  s(home_team, bs = "re") +
  s(away_team, bs = "re") +
  s(home_team, date_num, bs = "re") +
  s(away_team, date_num, bs = "re") #+
# s(date_num)

gamm_wf <- workflow() %>%
  add_recipe(data_rec) %>%
  add_model(gamm_mgcv_model,
            formula = formula_gamm) 

#### Definir CV ----

set.seed(234)
# data_folds <- bootstraps(data_juiced,
#                          times = 10,
#                          apparent = TRUE)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  strata = `goals.cum`)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  repeats = 4,
#                                  strata = `goals.cum`)
# data_folds <- group_vfold_cv(data_juiced, 
#                               group = "ID",
#                               v = n_split)

data_folds <- data_juiced %>%
  time_series_cv(
    date_var    = day_month_year,
    initial     = "30 days", #33
    assess      = "14 days", # 17
    skip        = "14 days", # 7
    slice_limit = 10,
    cumulative  = TRUE#,
    # prop = 0.8
  )

data_folds

data_folds %>%
  plot_time_series_cv_plan(day_month_year, home,
                           .interactive = FALSE)

#### Entrenar gamm ----

get_glm_coefs <- function(x) {
  x %>%
    # get the glm model object
    extract_fit_engine() %>%
    # transform its format
    tidy(parametric = TRUE)
}

multi_met <- yardstick::metric_set(accuracy, bal_accuracy, kap, 
                                   sensitivity, #sens,
                                   specificity, #spec,
                                   precision,
                                   detection_prevalence,
                                   f_meas, j_index,
                                   recall, roc_auc)


# registerDoParallel(cores = detectCores())

tictoc::tic()
validacion_gamm <-
  gamm_wf %>%
  fit_resamples(
    resamples = data_folds,
    metrics = multi_met,
    control = control_resamples( #control_grid
      extract = get_glm_coefs, #identity
      save_pred = TRUE,
      verbose = TRUE#,
      #parallel_over = "resamples"
    )
  )
tictoc::toc()

gamm_resamples <- gamm_wf %>%
  fit_resamples(data_folds, control = control_resamples( #control_grid
    extract = get_glm_coefs, #identity
    event_level = "first",
    save_pred = TRUE,
    verbose = TRUE#,
    #parallel_over = "resamples"
  ))

# head(validacion_mlm)

# gamm_coefs <-
#   validacion_gamm %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts)
# 
# gamm_coefs %>% select(id, term, statistic, p.value)
# 
# gamm_coefs %>%
#   filter(term != "(Intercept)") %>%
#   ggplot(aes(x = term, y = statistic, group = id, col = id)) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_line(alpha = 0.3, lwd = 1.2) +
#   labs(y = "Coefficient", x = NULL) +
#   theme(legend.position = "none") +
#   theme(axis.text.x=element_text(angle=90, hjust=0.5))

splitdat <- NULL

for(i in 1:length(gamm_resamples$splits)){
  splitdat <- splitdat %>% 
    dplyr::bind_rows(assessment(gamm_resamples$splits[[i]]) %>%
                       dplyr::mutate(id = paste0("Slice",i)) %>%
                       dplyr::select(-away) %>%
                       dplyr::bind_cols(gamm_resamples$.predictions[[i]]))
}

# Métricas por folio

splitdat %>%
  group_by(id) %>%
  summarise(mean = mean(ifelse(away == .pred_class,1,0)))

splitdat %>%
  group_by(id) %>%
  filter(away %in% 1) %>%
  mutate(acierto = ifelse(away == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

splitdat %>%
  group_by(id) %>%
  filter(away %in% 0) %>%
  mutate(acierto = ifelse(away == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

splitdat %>%
  group_by(id) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(away == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

splitdat %>%
  group_by(id) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(away == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto))

# Métricas por equipo local

splitdat %>%
  group_by(home_team) %>%
  summarise(mean = mean(ifelse(away == .pred_class,1,0)),
            n = n()) %>%
  arrange(-mean) %>%
  as.data.frame()

splitdat %>%
  group_by(home_team) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(away == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean) %>%
  as.data.frame()

splitdat %>%
  group_by(home_team) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(away == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean) %>%
  as.data.frame()

# Métricas por equipo visita

splitdat %>%
  group_by(away_team) %>%
  summarise(mean = mean(ifelse(away == .pred_class,1,0)),
            n = n()) %>%
  arrange(-mean) %>%
  as.data.frame()

splitdat %>%
  group_by(away_team) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(away == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean) %>%
  as.data.frame()

splitdat %>%
  group_by(away_team) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(away == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto),
            n = n()) %>%
  arrange(-mean) %>%
  as.data.frame()

# Media de métricas

splitdat %>%
  group_by(id) %>%
  summarise(mean = mean(ifelse(home == .pred_class,1,0))) %>%
  ungroup() %>%
  select(-id) %>%
  summarise_all(mean)

splitdat %>%
  group_by(id) %>%
  filter(home %in% 1) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto)) %>%
  ungroup() %>%
  select(-id) %>%
  summarise_all(mean)

splitdat %>%
  group_by(id) %>%
  filter(home %in% 0) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto)) %>%
  ungroup() %>%
  select(-id) %>%
  summarise_all(mean)

splitdat %>%
  group_by(id) %>%
  filter(.pred_class %in% 1) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto)) %>%
  ungroup() %>%
  select(-id) %>%
  summarise_all(mean)

splitdat %>%
  group_by(id) %>%
  filter(.pred_class %in% 0) %>%
  mutate(acierto = ifelse(home == .pred_class,1,0)) %>%
  summarise(mean = mean(acierto)) %>%
  ungroup() %>%
  select(-id) %>%
  summarise_all(mean)

# Métricas promedio de todas las particiones
metrics_gamm <- 
  validacion_gamm %>%
  collect_metrics(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm

# Métricas individuales de cada una de las particiones
metrics_gamm_complete <- 
  validacion_gamm %>% 
  collect_metrics(summarize = FALSE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm_complete

metrics_gamm_complete %>%
  ggplot(aes(x = id,
             y = .estimate)) +
  geom_col() +
  facet_wrap(.~.metric)

# Valores de validación (mae y rmse) obtenidos en cada partición y repetición.
p1 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .estimate, fill = .metric)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(. ~ .metric, scales = "free") +
  theme(axis.text.x=element_text(angle=90, hjust=0.5))
p2 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.1) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  # coord_flip() +
  theme_bw() +
  facet_wrap(.metric ~ ., scales = "free") #+
# theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, align = "v") %>% 
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución errores de validación cruzada", size = 15)
  )

# Predicciones individuales de cada observación.
# Si summarize = TRUE se agregan todos los valores predichos a nivel de
# observación.
# validacion_gamm %>% collect_predictions(summarize = TRUE) %>% head()

validation_gamm_predictions <- 
  validacion_gamm %>% 
  collect_predictions(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")

prop.table(table(validation_gamm_predictions$away))

validation_gamm_predictions %>%
  summarise(mean = mean(ifelse(away == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(away %in% 1) %>%
  summarise(mean = mean(ifelse(away == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(away %in% 0) %>%
  summarise(mean = mean(ifelse(away == .pred_class,1,0)))

# validation_gamm_predictions <- 
#   validacion_gamm %>% 
#   unnest(.predictions) %>%
#   #group_by(.row) %>%
#   #dplyr::summarise(dplyr::across(c(home,.pred_class),
#   #                               base::mean)) %>%
#   dplyr::mutate(modelo = "gamm")

#### Entrenamiento de modelo final ----

# gamm_res <- last_fit(linear_model, data_rec, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]
# 
# gamm_res <- last_fit(gamm_wf, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]

# registerDoFuture()
# plan(multisession)

# set.seed(69)
gamm_model_away <- gamm_mgcv_model %>%
  fit(formula_gamm,
      data = data_juiced)

# vip(mlm_model, num_features = 100)

# vi_gamm <- 
#   mlm_model %>%
#   vip::vi() %>%
#   dplyr::mutate(Importance = abs(Importance),
#                 Variable = fct_reorder(Variable, Importance),
#                 Total_Importance = sum(Importance),
#                 Importance = Importance/Total_Importance) %>%
#   dplyr::filter(!Importance == 0) %>%
#   ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
#   geom_col() +
#   scale_fill_manual(breaks = c("NEG","POS"),
#                     values = c("red","blue")) +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(y = NULL)
# vi_gamm

gamm_model_away

# anova(gamm_model$fit) 
# 
# summary(gamm_model$fit)
# 
# par(mfrow = c(2,2))
# gam.check(gamm_model$fit)
# par(mfrow = c(1,1))
# plot(gamm_model$fit)
# vis.gam(gamm_model$fit)
# ## Raw residuals still show correlation, of course...
# acf(residuals(gamm_model$fit),
#     main="raw residual ACF")
# ## But standardized are now fine...
# acf(residuals(gamm_model$fit,
#               type="scaled.pearson"),
#     main="standardized residual ACF")

gamm_model_away %>% broom::tidy()
gamm_model_away %>% broom::tidy(parametric = T)

performance::check_model(gamm_model_away)

# Modelamiento predictivo de empates ----

### gamm ----

# Initialize a linear regression object, linear_model
gamm_mgcv_model <- gen_additive_mod() %>% 
  # Set the model engine
  set_engine("mgcv", 
             family=binomial(link="probit"),
             method = "REML",
             random = list(home_team=~1,
                           draw_team=~1
             )#,
             # SEM_FENOLOGICA | ID,
             # correlation=corAR1()
  ) %>% #identity, log, inverse
  # Set the model mode
  set_mode('classification')

#### Recipe ----

data_rec <- recipe(`draw` ~ .,
                   data = data)

data_prep <- prep(data_rec)
data_juiced <- juice(data_prep)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
# data_train_prep <- bake(data_prep, new_data = data_train)
# data_test_prep  <- bake(data_prep, new_data = data_test)
# data_new_prep  <- bake(data_prep,
#                        new_data = data_new %>%
#                          dplyr::mutate(ID_x_COD = as.factor(ID_x_COD))
# )

# # No mover el siguiente código
# data_rec <- recipe(`goals` ~ .,
#                    data = data_juiced) %>%
#   # update_role(ID, new_role = "id variable") %>%
#   # update_role(ID_T, new_role = "id turn variable") %>%
#   update_role(COD, new_role = "id evaluation variable") %>%
#   update_role(FUNDO_num, new_role = "splitting variable") %>%
#   update_role(ID_x_COD, new_role = "id_x_cod variable") %>%
#   update_role(Fecha, new_role = "date variable")

#### Definir workflow ----

formula_gamm = draw ~ 
  s(home_team, bs = "re") +
  s(away_team, bs = "re") #+
  # home_team:away_team #+
  # s(home_team, date_num, bs = "re") +
  # s(away_team, date_num, bs = "re") #+
# s(date_num)

gamm_wf <- workflow() %>%
  add_recipe(data_rec) %>%
  add_model(gamm_mgcv_model,
            formula = formula_gamm) 

#### Definir CV ----

set.seed(234)
# data_folds <- bootstraps(data_juiced,
#                          times = 10,
#                          apparent = TRUE)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  strata = `goals.cum`)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  repeats = 4,
#                                  strata = `goals.cum`)
# data_folds <- group_vfold_cv(data_juiced, 
#                               group = "ID",
#                               v = n_split)

data_folds <- data_juiced %>%
  time_series_cv(
    date_var    = day_month_year,
    initial     = "30 days", #33
    assess      = "21 days", # 17
    skip        = "21 days", # 7
    slice_limit = 10,
    cumulative  = TRUE#,
    # prop = 0.8
  )

data_folds

data_folds %>%
  plot_time_series_cv_plan(day_month_year, home,
                           .interactive = FALSE)

#### Entrenar gamm ----

get_glm_coefs <- function(x) {
  x %>%
    # get the glm model object
    extract_fit_engine() %>%
    # transform its format
    tidy(parametric = TRUE)
}

multi_met <- yardstick::metric_set(accuracy, bal_accuracy, kap, 
                                   sensitivity, #sens,
                                   specificity, #spec,
                                   precision,
                                   detection_prevalence,
                                   f_meas, j_index,
                                   recall, roc_auc)


# registerDoParallel(cores = detectCores())

tictoc::tic()
validacion_gamm <-
  gamm_wf %>%
  fit_resamples(
    resamples = data_folds,
    metrics = multi_met,
    control = control_resamples( #control_grid
      extract = get_glm_coefs, #identity
      save_pred = TRUE,
      verbose = TRUE#,
      #parallel_over = "resamples"
    )
  )
tictoc::toc()

# head(validacion_mlm)

# gamm_coefs <-
#   validacion_gamm %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts)
# 
# gamm_coefs %>% select(id, term, statistic, p.value)
# 
# gamm_coefs %>%
#   filter(term != "(Intercept)") %>%
#   ggplot(aes(x = term, y = statistic, group = id, col = id)) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_line(alpha = 0.3, lwd = 1.2) +
#   labs(y = "Coefficient", x = NULL) +
#   theme(legend.position = "none") +
#   theme(axis.text.x=element_text(angle=90, hjust=0.5))

# Métricas promedio de todas las particiones
metrics_gamm <- 
  validacion_gamm %>%
  collect_metrics(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm

# Métricas individuales de cada una de las particiones
metrics_gamm_complete <- 
  validacion_gamm %>% 
  collect_metrics(summarize = FALSE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm_complete

metrics_gamm_complete %>%
  ggplot(aes(x = id,
             y = .estimate)) +
  geom_col() +
  facet_wrap(.~.metric)

# Valores de validación (mae y rmse) obtenidos en cada partición y repetición.
p1 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .estimate, fill = .metric)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(. ~ .metric, scales = "free") +
  theme(axis.text.x=element_text(angle=90, hjust=0.5))
p2 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.1) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  # coord_flip() +
  theme_bw() +
  facet_wrap(.metric ~ ., scales = "free") #+
# theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, align = "v") %>% 
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución errores de validación cruzada", size = 15)
  )

# Predicciones individuales de cada observación.
# Si summarize = TRUE se agregan todos los valores predichos a nivel de
# observación.
# validacion_gamm %>% collect_predictions(summarize = TRUE) %>% head()

validation_gamm_predictions <- 
  validacion_gamm %>% 
  collect_predictions(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")

prop.table(table(validation_gamm_predictions$draw))

validation_gamm_predictions %>%
  summarise(mean = mean(ifelse(draw == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(draw %in% 1) %>%
  summarise(mean = mean(ifelse(draw == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(draw %in% 0) %>%
  summarise(mean = mean(ifelse(draw == .pred_class,1,0)))

# validation_gamm_predictions <- 
#   validacion_gamm %>% 
#   unnest(.predictions) %>%
#   #group_by(.row) %>%
#   #dplyr::summarise(dplyr::across(c(home,.pred_class),
#   #                               base::mean)) %>%
#   dplyr::mutate(modelo = "gamm")

#### Entrenamiento de modelo final ----

# gamm_res <- last_fit(linear_model, data_rec, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]
# 
# gamm_res <- last_fit(gamm_wf, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]

# registerDoFuture()
# plan(multisession)

# set.seed(69)
gamm_model_draw <- gamm_mgcv_model %>%
  fit(formula_gamm,
      data = data_juiced)

# vip(mlm_model, num_features = 100)

# vi_gamm <- 
#   mlm_model %>%
#   vip::vi() %>%
#   dplyr::mutate(Importance = abs(Importance),
#                 Variable = fct_reorder(Variable, Importance),
#                 Total_Importance = sum(Importance),
#                 Importance = Importance/Total_Importance) %>%
#   dplyr::filter(!Importance == 0) %>%
#   ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
#   geom_col() +
#   scale_fill_manual(breaks = c("NEG","POS"),
#                     values = c("red","blue")) +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(y = NULL)
# vi_gamm

gamm_model_draw

# anova(gamm_model$fit) 
# 
# summary(gamm_model$fit)
# 
# par(mfrow = c(2,2))
# gam.check(gamm_model$fit)
# par(mfrow = c(1,1))
# plot(gamm_model$fit)
# vis.gam(gamm_model$fit)
# ## Raw residuals still show correlation, of course...
# acf(residuals(gamm_model$fit),
#     main="raw residual ACF")
# ## But standardized are now fine...
# acf(residuals(gamm_model$fit,
#               type="scaled.pearson"),
#     main="standardized residual ACF")

gamm_model_draw %>% broom::tidy()
gamm_model_draw %>% broom::tidy(parametric = T)

performance::check_model(gamm_model_draw)

### Proyección victorias visita ----

predicciones <- read.csv("predict.csv", sep = ",")%>%
  mutate(day_month_year = paste0(day_month, year),
         day_month_year = as.Date(day_month_year, format = "%d-%m-%Y"),
         date_num = as.numeric(day_month_year)
  )

predfinal <- predicciones %>%
  bind_cols(predict(gamm_model_draw, new_data = predicciones)) %>%
  bind_cols(predict(gamm_model_draw, new_data = predicciones, type = "prob"))

predfinal %>%
  filter(day_month_year <= "2022-02-27") %>%
  arrange(-.pred_0)

# Modelamiento predictivo multinomial ----

### gamm ----

# Initialize a linear regression object, linear_model
gamm_mgcv_model <- gen_additive_mod() %>% 
  # Set the model engine
  set_engine("mgcv", 
             family=multinom(K = 2)#,
             # method = "REML",
             # random = list(home_team=~1,
             #               draw_team=~1
             # )#,
             # SEM_FENOLOGICA | ID,
             # correlation=corAR1()
  ) %>% #identity, log, inverse
  # Set the model mode
  set_mode('classification')

#### Recipe ----

data_rec <- recipe(`winner_class` ~ .,
                   data = data)

data_prep <- prep(data_rec)
data_juiced <- juice(data_prep)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
# data_train_prep <- bake(data_prep, new_data = data_train)
# data_test_prep  <- bake(data_prep, new_data = data_test)
# data_new_prep  <- bake(data_prep,
#                        new_data = data_new %>%
#                          dplyr::mutate(ID_x_COD = as.factor(ID_x_COD))
# )

# # No mover el siguiente código
# data_rec <- recipe(`goals` ~ .,
#                    data = data_juiced) %>%
#   # update_role(ID, new_role = "id variable") %>%
#   # update_role(ID_T, new_role = "id turn variable") %>%
#   update_role(COD, new_role = "id evaluation variable") %>%
#   update_role(FUNDO_num, new_role = "splitting variable") %>%
#   update_role(ID_x_COD, new_role = "id_x_cod variable") %>%
#   update_role(Fecha, new_role = "date variable")

#### Definir workflow ----

formula_gamm = list(winner_class ~ 
  # s(home_team, bs = "re") #+
  # s(away_team, bs = "re") #+
  home_team +
  away_team +
  # s(home_team, date_num, bs = "re") #+
# s(away_team, date_num, bs = "re") #+
  s(date_num), winner_class ~ home_team +
  away_team + s(date_num))

gamm_wf <- workflow() %>%
  add_recipe(data_rec) %>%
  add_model(gamm_mgcv_model,
            formula = formula_gamm) 

#### Definir CV ----

set.seed(234)
# data_folds <- bootstraps(data_juiced,
#                          times = 10,
#                          apparent = TRUE)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  strata = `goals.cum`)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  repeats = 4,
#                                  strata = `goals.cum`)
# data_folds <- group_vfold_cv(data_juiced, 
#                               group = "ID",
#                               v = n_split)

data_folds <- data_juiced %>%
  time_series_cv(
    date_var    = day_month_year,
    initial     = "30 days", #33
    assess      = "14 days", # 17
    skip        = "14 days", # 7
    slice_limit = 10,
    cumulative  = TRUE#,
    # prop = 0.8
  )

data_folds

data_folds %>%
  plot_time_series_cv_plan(day_month_year, winner_class,
                           .interactive = FALSE)

#### Entrenar gamm ----

get_glm_coefs <- function(x) {
  x %>%
    # get the glm model object
    extract_fit_engine() %>%
    # transform its format
    tidy(parametric = TRUE)
}

multi_met <- yardstick::metric_set(accuracy, bal_accuracy, kap, 
                                   sensitivity, #sens,
                                   specificity, #spec,
                                   precision,
                                   detection_prevalence,
                                   f_meas, j_index,
                                   recall, roc_auc)


# registerDoParallel(cores = detectCores())

tictoc::tic()
validacion_gamm <-
  gamm_wf %>%
  fit_resamples(
    resamples = data_folds,
    metrics = multi_met,
    control = control_resamples( #control_grid
      extract = get_glm_coefs, #identity
      save_pred = TRUE,
      verbose = TRUE#,
      #parallel_over = "resamples"
    )
  )
tictoc::toc()

# head(validacion_mlm)

# gamm_coefs <-
#   validacion_gamm %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts)
# 
# gamm_coefs %>% select(id, term, statistic, p.value)
# 
# gamm_coefs %>%
#   filter(term != "(Intercept)") %>%
#   ggplot(aes(x = term, y = statistic, group = id, col = id)) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_line(alpha = 0.3, lwd = 1.2) +
#   labs(y = "Coefficient", x = NULL) +
#   theme(legend.position = "none") +
#   theme(axis.text.x=element_text(angle=90, hjust=0.5))

# Métricas promedio de todas las particiones
metrics_gamm <- 
  validacion_gamm %>%
  collect_metrics(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm

# Métricas individuales de cada una de las particiones
metrics_gamm_complete <- 
  validacion_gamm %>% 
  collect_metrics(summarize = FALSE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm_complete

metrics_gamm_complete %>%
  ggplot(aes(x = id,
             y = .estimate)) +
  geom_col() +
  facet_wrap(.~.metric)

# Valores de validación (mae y rmse) obtenidos en cada partición y repetición.
p1 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .estimate, fill = .metric)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(. ~ .metric, scales = "free") +
  theme(axis.text.x=element_text(angle=90, hjust=0.5))
p2 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.1) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  # coord_flip() +
  theme_bw() +
  facet_wrap(.metric ~ ., scales = "free") #+
# theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, align = "v") %>% 
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución errores de validación cruzada", size = 15)
  )

# Predicciones individuales de cada observación.
# Si summarize = TRUE se agregan todos los valores predichos a nivel de
# observación.
# validacion_gamm %>% collect_predictions(summarize = TRUE) %>% head()

validation_gamm_predictions <- 
  validacion_gamm %>% 
  collect_predictions(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")

prop.table(table(validation_gamm_predictions$winner_class))

validation_gamm_predictions %>%
  summarise(mean = mean(ifelse(winner_class == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(winner_class %in% 1) %>%
  summarise(mean = mean(ifelse(winner_class == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(winner_class %in% 0) %>%
  summarise(mean = mean(ifelse(winner_class == .pred_class,1,0)))

# validation_gamm_predictions <- 
#   validacion_gamm %>% 
#   unnest(.predictions) %>%
#   #group_by(.row) %>%
#   #dplyr::summarise(dplyr::across(c(home,.pred_class),
#   #                               base::mean)) %>%
#   dplyr::mutate(modelo = "gamm")

#### Entrenamiento de modelo final ----

# gamm_res <- last_fit(linear_model, data_rec, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]
# 
# gamm_res <- last_fit(gamm_wf, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]

# registerDoFuture()
# plan(multisession)

# set.seed(69)
gamm_model_winner_class <- gamm_mgcv_model %>%
  fit(formula_gamm,
      data = data_juiced)

# vip(mlm_model, num_features = 100)

# vi_gamm <- 
#   mlm_model %>%
#   vip::vi() %>%
#   dplyr::mutate(Importance = abs(Importance),
#                 Variable = fct_reorder(Variable, Importance),
#                 Total_Importance = sum(Importance),
#                 Importance = Importance/Total_Importance) %>%
#   dplyr::filter(!Importance == 0) %>%
#   ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
#   geom_col() +
#   scale_fill_manual(breaks = c("NEG","POS"),
#                     values = c("red","blue")) +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(y = NULL)
# vi_gamm

gamm_model_winner_class

# anova(gamm_model$fit) 
# 
# summary(gamm_model$fit)
# 
# par(mfrow = c(2,2))
# gam.check(gamm_model$fit)
# par(mfrow = c(1,1))
# plot(gamm_model$fit)
# vis.gam(gamm_model$fit)
# ## Raw residuals still show correlation, of course...
# acf(residuals(gamm_model$fit),
#     main="raw residual ACF")
# ## But standardized are now fine...
# acf(residuals(gamm_model$fit,
#               type="scaled.pearson"),
#     main="standardized residual ACF")

gamm_model_winner_class %>% broom::tidy()
gamm_model_winner_class %>% broom::tidy(parametric = T)

performance::check_model(gamm_model_winner_class)

# Modelamiento predictivo multinomial ----

### XGBoost with bayes tunning----

#### Definir hiperparámetros para tunning ----

xgb_bayes_spec <- boost_tree(
  trees = tune(), 
  tree_depth = tune(),
  min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(),
  mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>% 
  set_engine("xgboost",                         
             # reg_gamma = tune("reg_gamma"),
             # reg_lambda = tune("reg_lambda"),
             # reg_alpha = tune("reg_alpha"),
             objective = "reg:logistic"#, "multi:softmax"
             # importance = "impurity" #"permutation"
  ) %>% 
  set_mode("classification")

# xgb_spec

#### Recipe ----

data_rec <- recipe(`home` ~ .,
                   data = data) %>% 
  step_dummy(home_team,
             away_team) %>%
  step_select(where(base::is.numeric), home,
              day_month_year,
              -c(home_score, away_score,
                 home_halfscore,
                 away_halfscore,
                 year,
                 diferencia,
                 goals))

data_prep <- prep(data_rec)
data_juiced <- juice(data_prep)

# str(data_juiced)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
# data_train_prep <- bake(data_prep, new_data = data_train)
# data_test_prep  <- bake(data_prep, new_data = data_test)
# data_new_prep  <- bake(data_prep,
#                        new_data = data_new %>%
#                          dplyr::mutate(ID_x_COD = as.factor(ID_x_COD))
# )

# # No mover el siguiente código
data_rec <- recipe(`home` ~ .,
                   data = data_juiced) %>%
  update_role(day_month_year, new_role = "date variable")

#### Definir workflow ----


# formula_xgb = winner_class ~ 
#   home_team +
#   away_team
# 
# gamm_wf <- workflow() %>%
#   add_recipe(data_rec) %>%
#   add_model(gamm_mgcv_model,
#             formula = formula_gamm) 

xgb_bayes_wf <- workflow() %>%
  add_recipe(data_rec) %>%
  add_model(xgb_bayes_spec) 

#### Definir CV ----

data_folds <- data_juiced %>%
  time_series_cv(
    date_var    = day_month_year,
    initial     = "30 days", #33
    assess      = "14 days", # 17
    skip        = "14 days", # 7
    slice_limit = 5,
    cumulative  = TRUE#,
    # prop = 0.8
  )

data_folds

data_folds %>%
  plot_time_series_cv_plan(day_month_year, home,
                           .interactive = FALSE)

#### Definir hiperparámetros ----

xgboost_params <- parameters(
  trees(), learn_rate(),
  tree_depth(), min_n(), 
  loss_reduction(),
  sample_size = sample_prop(),
  # reg_gamma = dials::penalty(),
  # reg_lambda = dials::penalty(),
  # reg_alpha = dials::penalty(),
  finalize(mtry(),
           data_juiced)  
)

# xgboost_params <- xgboost_params %>% update(trees = trees(c(100, 1000)),
#                                             mtry = mtry(c(1,10)))

#### Calibrar hiperparámetros ----

multi_met <- yardstick::metric_set(bal_accuracy,
                                   sensitivity, #sens,
                                   specificity, #spec,
                                   roc_auc,
                                   accuracy,
                                   kap, 
                                   precision,
                                   detection_prevalence,
                                   f_meas, j_index,
                                   recall)

# registerDoFuture()
# plan(multisession)

# set.seed(345)
tictoc::tic()
xgb_bayes_tune <-
  xgb_bayes_wf %>%
  tune_bayes(
    resamples = data_folds,
    param_info = xgboost_params,
    # por defecto es 5 (número aleatorio de combinaciones (puntos de grilla) de hiperparámetros)
    initial = 10, 
    iter = 100,
    metrics = multi_met, # metric_set(rmse,mae,smape),
    control = control_bayes(
      # El corte entero para el número de iteraciones sin mejores resultados.
      no_improve = 10,
      extract = identity,
      save_pred = TRUE,
      pkgs = "recipeselectors",
      verbose = TRUE,
      event_level = "first"#,
      # parallel_over = "resamples"
    )
  )
tictoc::toc()

xgb_bayes_tune

# xgb_resamples <- xgb_bayes_wf %>%
#   fit_resamples(data_folds, control = control_resamples( #control_grid
#     extract = get_glm_coefs, #identity
#     event_level = "first",
#     save_pred = TRUE,
#     verbose = TRUE#,
#     #parallel_over = "resamples"
#   ))

#### Colectar métricas en CV ----

# plot(xgb_bayes_tune)
autoplot(xgb_bayes_tune)

xgb_bayes_tune %>%
  collect_metrics() %>%
  dplyr::select(-std_err) %>%
  pivot_wider(names_from = .metric,
              values_from = c(mean)) %>%
  # filter(.metric == "rmse") %>%
  mutate_if(base::is.numeric, round, 3) %>%
  dplyr::arrange(-sensitivity) %>%
  reactable()

xgb_bayes_tune %>%
  collect_metrics() %>% 
  # filter(.metric == "rmse") %>%
  select(.metric, mean, std_err, min_n, mtry, tree_depth, 
         learn_rate, loss_reduction, sample_size, trees
  ) %>%
  pivot_longer(min_n:mtry:tree_depth:learn_rate:loss_reduction:sample_size:trees,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean - std_err,
                    ymax = mean + std_err)) +
  facet_grid(.metric~parameter, scales = "free") +
  labs(x = NULL, y = "")

#### Seleccionar mejor combinación de hiperparámetros ----

##### Para calibración bayesiana de hiperparámetros ----

best_xgb_bayes_model <- select_best(xgb_bayes_tune, "sensitivity")

xgboost_gamma_bayes_model_final <- xgb_bayes_spec %>%
  finalize_model(best_xgb_bayes_model)

xgTrainFit <- xgboost_gamma_bayes_model_final %>%
  fit(home~., data = data_juiced %>%
        select(-day_month_year))

spec <- xgTrainFit %>% extract_spec_parsnip()

# vip(xgTrainFit, num_features = 60)

vi_xgb <- 
  xgTrainFit %>%
  vip::vi()  %>% 
  dplyr::mutate(Variable = iconv(Variable, "UTF-8", "ISO_8859-1"), 
                Sign = ifelse(Importance >= 0, "POS", "NEG"),
                Importance = abs(Importance),
                Variable = fct_reorder(Variable, Importance),
                Total_Importance = sum(Importance),
                Importance = Importance/Total_Importance) %>%
  dplyr::filter(!Importance == 0) %>%
  ggplot(aes(x = Importance,
             y = Variable,
             fill = Sign)) +
  geom_col() +
  scale_fill_manual(breaks = c("NEG","POS"),
                    values = c("red","blue")) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)
vi_xgb

# xgb_bayes_tune %>% collect_predictions() %>% as.data.frame()

# Métricas promedio de todas las particiones
metrics_xgb_bayes <- 
  xgb_bayes_tune %>% 
  collect_metrics(summarize = TRUE) %>%
  dplyr::mutate(modelo = "xgb") %>% 
  filter(mtry %in% best_xgb_bayes_model$mtry,
         trees %in% best_xgb_bayes_model$trees,
         min_n %in% best_xgb_bayes_model$min_n,
         tree_depth %in% best_xgb_bayes_model$tree_depth,
         learn_rate %in% best_xgb_bayes_model$learn_rate,
         loss_reduction %in% best_xgb_bayes_model$loss_reduction,
         sample_size %in% best_xgb_bayes_model$sample_size,
         .config %in% best_xgb_bayes_model$.config
  )
metrics_xgb_bayes

# Métricas individuales de cada una de las particiones
metrics_xgb_bayes_complete <- 
  xgb_bayes_tune %>% 
  collect_metrics(summarize = FALSE) %>%
  dplyr::mutate(modelo = "xgb") %>% 
  filter(mtry %in% best_xgb_bayes_model$mtry,
         trees %in% best_xgb_bayes_model$trees,
         min_n %in% best_xgb_bayes_model$min_n,
         tree_depth %in% best_xgb_bayes_model$tree_depth,
         learn_rate %in% best_xgb_bayes_model$learn_rate,
         loss_reduction %in% best_xgb_bayes_model$loss_reduction,
         sample_size %in% best_xgb_bayes_model$sample_size,
         .config %in% best_xgb_bayes_model$.config
  )
metrics_xgb_bayes_complete

# Valores de validación obtenidos en cada partición y repetición.
p1 <- ggplot(
  data = metrics_xgb_bayes_complete,
  aes(x = .estimate, fill = .metric)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(. ~ .metric, scales = "free") +
  theme(axis.text.x=element_text(angle=90, hjust=0.5))
p2 <- ggplot(
  data = metrics_xgb_bayes_complete,
  aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.1) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  # coord_flip() +
  theme_bw() +
  facet_wrap(.metric ~ ., scales = "free") #+
# theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, align = "v") %>% 
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución errores de validación cruzada", size = 15)
  )

# Predicciones individuales de cada observación.
# Si summarize = TRUE se agregan todos los valores predichos a nivel de
# observación.
validation_xgb_bayes_predictions <- 
  xgb_bayes_tune %>% 
  collect_predictions(summarize = TRUE)  %>%
  dplyr::mutate(modelo = "xgb") %>% 
  filter(mtry %in% best_xgb_bayes_model$mtry,
         trees %in% best_xgb_bayes_model$trees,
         min_n %in% best_xgb_bayes_model$min_n,
         tree_depth %in% best_xgb_bayes_model$tree_depth,
         learn_rate %in% best_xgb_bayes_model$learn_rate,
         loss_reduction %in% best_xgb_bayes_model$loss_reduction,
         sample_size %in% best_xgb_bayes_model$sample_size,
         .config %in% best_xgb_bayes_model$.config
  )

p1 <- ggplot(
  data = validation_xgb_bayes_predictions,
  aes(x = KG.HA, y = .pred)
) +
  geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "firebrick") +
  labs(title = "Valor predicho vs valor real") +
  theme_bw()


p2 <- ggplot(
  data = validation_xgb_bayes_predictions,
  aes(x = .row, y = KG.HA - .pred)
) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept =  0, color = "firebrick") +
  labs(title = "Residuos del modelo") +
  theme_bw()

p3 <- ggplot(
  data = validation_xgb_bayes_predictions,
  aes(x = KG.HA - .pred)
) +
  geom_density() + 
  labs(title = "Distribución residuos del modelo") +
  theme_bw()

p4 <- ggplot(
  data = validation_xgb_bayes_predictions,
  aes(sample = KG.HA - .pred)
) +
  geom_qq() +
  geom_qq_line(color = "firebrick") +
  labs(title = "Q-Q residuos del modelo") +
  theme_bw()

ggpubr::ggarrange(plotlist = list(p1, p2, p3, p4)) %>%
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución residuos", size = 15, face = "bold")
  )

#### Entrenamiento de modelo final ----

# registerDoFuture()
# plan(multisession)

# set.seed(69)

# spec <- boost_tree(mtry = 33,
#                    trees = 718,
#                    min_n = 27,
#                    tree_depth = 12,
#                    learn_rate = 0.0610157205233847,
#                    loss_reduction = 0.0000170917700887555,
#                    sample_size = 0.478538645338267,
#                    mode = "regression") %>% 
#   set_engine("xgboost",
#              objective = "reg:gamma"
#   )

# xgb_bayes_model <- spec %>%
#   fit(
#     `KG.HA` ~ .,
#     data = data_train_prep %>%
#       select(-ID,-FUNDO_num) #%>% filter(!KG.HA %in% 0)
#     )
# 
# vip(xgb_bayes_model, num_features = 40)

xgb_bayes_model <- xgTrainFit


### nnet ----

# Initialize a linear regression object, linear_model
gamm_mgcv_model <- gen_additive_mod() %>% 
  # Set the model engine
  set_engine("mgcv", 
             family=multinom(K = 2)#,
             # method = "REML",
             # random = list(home_team=~1,
             #               draw_team=~1
             # )#,
             # SEM_FENOLOGICA | ID,
             # correlation=corAR1()
  ) %>% #identity, log, inverse
  # Set the model mode
  set_mode('classification')

#### Recipe ----

data_rec <- recipe(`winner_class` ~ .,
                   data = data)

data_prep <- prep(data_rec)
data_juiced <- juice(data_prep)

# Se aplican las transformaciones al conjunto de entrenamiento y de test
# data_train_prep <- bake(data_prep, new_data = data_train)
# data_test_prep  <- bake(data_prep, new_data = data_test)
# data_new_prep  <- bake(data_prep,
#                        new_data = data_new %>%
#                          dplyr::mutate(ID_x_COD = as.factor(ID_x_COD))
# )

# # No mover el siguiente código
# data_rec <- recipe(`goals` ~ .,
#                    data = data_juiced) %>%
#   # update_role(ID, new_role = "id variable") %>%
#   # update_role(ID_T, new_role = "id turn variable") %>%
#   update_role(COD, new_role = "id evaluation variable") %>%
#   update_role(FUNDO_num, new_role = "splitting variable") %>%
#   update_role(ID_x_COD, new_role = "id_x_cod variable") %>%
#   update_role(Fecha, new_role = "date variable")

#### Definir workflow ----

formula_gamm = list(winner_class ~ 
                      # s(home_team, bs = "re") #+
                      # s(away_team, bs = "re") #+
                      home_team +
                      away_team +
                      # s(home_team, date_num, bs = "re") #+
                      # s(away_team, date_num, bs = "re") #+
                      s(date_num), winner_class ~ home_team +
                      away_team + s(date_num))

gamm_wf <- workflow() %>%
  add_recipe(data_rec) %>%
  add_model(gamm_mgcv_model,
            formula = formula_gamm) 

#### Definir CV ----

set.seed(234)
# data_folds <- bootstraps(data_juiced,
#                          times = 10,
#                          apparent = TRUE)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  strata = `goals.cum`)
# data_folds <- rsample::vfold_cv(data_juiced,
#                                  v = 20,
#                                  repeats = 4,
#                                  strata = `goals.cum`)
# data_folds <- group_vfold_cv(data_juiced, 
#                               group = "ID",
#                               v = n_split)

data_folds <- data_juiced %>%
  time_series_cv(
    date_var    = day_month_year,
    initial     = "30 days", #33
    assess      = "14 days", # 17
    skip        = "14 days", # 7
    slice_limit = 10,
    cumulative  = TRUE#,
    # prop = 0.8
  )

data_folds

data_folds %>%
  plot_time_series_cv_plan(day_month_year, winner_class,
                           .interactive = FALSE)

#### Entrenar gamm ----

get_glm_coefs <- function(x) {
  x %>%
    # get the glm model object
    extract_fit_engine() %>%
    # transform its format
    tidy(parametric = TRUE)
}

multi_met <- yardstick::metric_set(accuracy, bal_accuracy, kap, 
                                   sensitivity, #sens,
                                   specificity, #spec,
                                   precision,
                                   detection_prevalence,
                                   f_meas, j_index,
                                   recall, roc_auc)


# registerDoParallel(cores = detectCores())

tictoc::tic()
validacion_gamm <-
  gamm_wf %>%
  fit_resamples(
    resamples = data_folds,
    metrics = multi_met,
    control = control_resamples( #control_grid
      extract = get_glm_coefs, #identity
      save_pred = TRUE,
      verbose = TRUE#,
      #parallel_over = "resamples"
    )
  )
tictoc::toc()

# head(validacion_mlm)

# gamm_coefs <-
#   validacion_gamm %>%
#   select(id, .extracts) %>%
#   unnest(.extracts) %>%
#   unnest(.extracts)
# 
# gamm_coefs %>% select(id, term, statistic, p.value)
# 
# gamm_coefs %>%
#   filter(term != "(Intercept)") %>%
#   ggplot(aes(x = term, y = statistic, group = id, col = id)) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_line(alpha = 0.3, lwd = 1.2) +
#   labs(y = "Coefficient", x = NULL) +
#   theme(legend.position = "none") +
#   theme(axis.text.x=element_text(angle=90, hjust=0.5))

# Métricas promedio de todas las particiones
metrics_gamm <- 
  validacion_gamm %>%
  collect_metrics(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm

# Métricas individuales de cada una de las particiones
metrics_gamm_complete <- 
  validacion_gamm %>% 
  collect_metrics(summarize = FALSE) %>%
  dplyr::mutate(modelo = "gamm")
metrics_gamm_complete

metrics_gamm_complete %>%
  ggplot(aes(x = id,
             y = .estimate)) +
  geom_col() +
  facet_wrap(.~.metric)

# Valores de validación (mae y rmse) obtenidos en cada partición y repetición.
p1 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .estimate, fill = .metric)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(. ~ .metric, scales = "free") +
  theme(axis.text.x=element_text(angle=90, hjust=0.5))
p2 <- ggplot(
  data = metrics_gamm_complete,
  aes(x = .metric, y = .estimate, fill = .metric, color = .metric)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.1) +
  geom_jitter(width = 0.05, alpha = 0.3) +
  # coord_flip() +
  theme_bw() +
  facet_wrap(.metric ~ ., scales = "free") #+
# theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, align = "v") %>% 
  ggpubr::annotate_figure(
    top = ggpubr::text_grob("Distribución errores de validación cruzada", size = 15)
  )

# Predicciones individuales de cada observación.
# Si summarize = TRUE se agregan todos los valores predichos a nivel de
# observación.
# validacion_gamm %>% collect_predictions(summarize = TRUE) %>% head()

validation_gamm_predictions <- 
  validacion_gamm %>% 
  collect_predictions(summarize = TRUE) %>%
  dplyr::mutate(modelo = "gamm")

prop.table(table(validation_gamm_predictions$winner_class))

validation_gamm_predictions %>%
  summarise(mean = mean(ifelse(winner_class == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(winner_class %in% 1) %>%
  summarise(mean = mean(ifelse(winner_class == .pred_class,1,0)))

validation_gamm_predictions %>%
  filter(winner_class %in% 0) %>%
  summarise(mean = mean(ifelse(winner_class == .pred_class,1,0)))

# validation_gamm_predictions <- 
#   validacion_gamm %>% 
#   unnest(.predictions) %>%
#   #group_by(.row) %>%
#   #dplyr::summarise(dplyr::across(c(home,.pred_class),
#   #                               base::mean)) %>%
#   dplyr::mutate(modelo = "gamm")

#### Entrenamiento de modelo final ----

# gamm_res <- last_fit(linear_model, data_rec, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]
# 
# gamm_res <- last_fit(gamm_wf, data_split, metrics = multi_met)
# 
# gamm_res$.metrics[[1]]

# registerDoFuture()
# plan(multisession)

# set.seed(69)
gamm_model_winner_class <- gamm_mgcv_model %>%
  fit(formula_gamm,
      data = data_juiced)

# vip(mlm_model, num_features = 100)

# vi_gamm <- 
#   mlm_model %>%
#   vip::vi() %>%
#   dplyr::mutate(Importance = abs(Importance),
#                 Variable = fct_reorder(Variable, Importance),
#                 Total_Importance = sum(Importance),
#                 Importance = Importance/Total_Importance) %>%
#   dplyr::filter(!Importance == 0) %>%
#   ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
#   geom_col() +
#   scale_fill_manual(breaks = c("NEG","POS"),
#                     values = c("red","blue")) +
#   scale_x_continuous(expand = c(0, 0)) +
#   labs(y = NULL)
# vi_gamm

gamm_model_winner_class

# anova(gamm_model$fit) 
# 
# summary(gamm_model$fit)
# 
# par(mfrow = c(2,2))
# gam.check(gamm_model$fit)
# par(mfrow = c(1,1))
# plot(gamm_model$fit)
# vis.gam(gamm_model$fit)
# ## Raw residuals still show correlation, of course...
# acf(residuals(gamm_model$fit),
#     main="raw residual ACF")
# ## But standardized are now fine...
# acf(residuals(gamm_model$fit,
#               type="scaled.pearson"),
#     main="standardized residual ACF")

gamm_model_winner_class %>% broom::tidy()
gamm_model_winner_class %>% broom::tidy(parametric = T)

performance::check_model(gamm_model_winner_class)

### Proyección victorias visita ----

predicciones <- read.csv("predict.csv", sep = ",")%>%
  mutate(day_month_year = paste0(day_month, year),
         day_month_year = as.Date(day_month_year, format = "%d-%m-%Y"),
         date_num = as.numeric(day_month_year)
  )

predfinal <- predicciones %>%
  bind_cols(predict(gamm_model_winner_class, new_data = predicciones)) %>%
  bind_cols(predict(gamm_model_winner_class, new_data = predicciones, type = "prob"))

predfinal %>%
  filter(day_month_year <= "2022-02-27") %>%
  arrange(-.pred_0)


library(sgboost)
library(tidyverse)
library(mboost)
library(gridExtra)
set.seed(10)
beta <- c(
  rep(5, 5), c(5, -5, 2, 0, 0), rep(-5, 5),
  c(2, -3, 8, 0, 0), rep(0, (200 - 20))
)
X <- matrix(data = rnorm(20000, mean = 0, sd = 1), 100, 200)
df <- data.frame(X) %>%
  mutate(y = X %*% beta+rnorm(100, mean = 0, sd = 1)) %>%
  mutate_all(function(x){as.numeric(scale(x))})
group_df <- data.frame(group_name = rep(1:40, each = 5), variable_name = colnames(df)[1:200])

# Input data.frames
sgb_formula <- create_formula(
  alpha = 0.4, group_df = group_df, outcome_name = "y", intercept = FALSE,
  group_name = "group_name", var_name = "variable_name",
)
labels(terms(sgb_formula))[[1]]
labels(terms(sgb_formula))[[201]]

sgb_model <- mboost(
  formula = sgb_formula, data = df,
  control = boost_control(nu = 1, mstop = 600)
)
cv_sgb_model <- cvrisk(sgb_model)
png(file="figures/cv_plot.png",
    width=600, height=350)
plot(cv_sgb_model)
dev.off()
plot(cv_sgb_model)
sgb_model[mstop(cv_sgb_model)]


get_varimp(sgb_model = sgb_model)$varimp %>% slice(1:5)
get_varimp(sgb_model = sgb_model)$group_importance
plot_varimp <- plot_varimp(sgb_model = sgb_model, n_predictors = 15, base_size = 16)
ggsave('figures/varimp.png',plot = plot_varimp, width = 7, height = 5, dpi = 900)

slice(get_coef(sgb_model = sgb_model)$aggregate,1:5)
slice(get_coef(sgb_model = sgb_model)$raw,1:5)


p1 <- plot_effects(sgb_model = sgb_model, n_predictors = 4, base_size = 10) + theme(legend.position = "none")
p2 <- plot_effects(sgb_model = sgb_model, n_predictors = 4, plot_type = "clock", base_size = 10)+ theme(legend.position = "none")
p3 <- plot_effects(sgb_model = sgb_model, n_predictors = 4, plot_type = "scatter", base_size = 10)
grid_plot <- grid.arrange(p1, p2,nrow = 1)
ggsave('figures/radar_1.png', plot = grid_plot, width = 8, height = 5, dpi = 900)
ggsave('figures/radar_2.png', plot = p3, width = 8, height = 5, dpi = 900)


path <- get_coef_path(sgb_model = sgb_model)

path_plot <- plot_path(sgb_model = sgb_model)
ggsave('figures/path.png',plot = path_plot, width = 7, height = 5, dpi = 900)

# real data (from using interpretable boosting...)
index_df <- readRDS('index_df.RDS')
model_df <- readRDS('model_df.RDS')
sgb_formula <- create_formula(group_df = index_df, var_name = 'col_names',
                              group_name = 'index', outcome_name = 'S5.4')
model <- mboost(sgb_formula, data = model_df, family = Binomial(link = 'logit'),
                control = boost_control(mstop = 1000, nu = 0.3))
cv_model <- cvrisk(model)
mstop(cv_model)
model <- model[mstop(cv_model)]
## helper function
`-.gg` <- function(plot, layer) {
  if (missing(layer)) {
    stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
  }
  if (!is.ggplot(plot)) {
    stop('Need a plot on the left side')
  }
  plot$layers = c(layer, plot$layers)
  plot
}
## helper function
plot_path(model) #- geom_line(size = 0.2) 
ggsave('figures/cc_path.png', dpi = 900, width = 7, height = 5)
plot_varimp(model)
ggsave('figures/cc_varimp.png', dpi = 900, width = 7, height = 5)
plot_effects(model, plot_type = 'scatter', prop = 0.015, max_char_length = 6)
ggsave('figures/cc_effects.png', dpi = 900, width = 7, height = 5)

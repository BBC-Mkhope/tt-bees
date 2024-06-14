#############################################################################
################### Tidy Tuesday Energy Consumption Data ####################
#############################################################################


# Global options ----------------------------------------------------------
#
options("scipen" = 100, "digits" = 3) 
#


# Attaching libraries -----------------------------------------------------
#
librarian::shelf(
  summarytools, # nice eda
  tidytuesdayR, # data
  tidyverse,    # data wrangling
  corrplot,     # correlation charts
  cowplot,      # displaying charts together
  olsrr,        # checking eigenvalues for multicollinearity
  mice,         # multiple imputations via chained equations
  here,         # file organization
  VIM,          # imputation using k-nearest neighbor
  gt            # nice tables
)
#

# Helper functions --------------------------------------------------------
#
# Custom color palette for charts
uagc_colors <-
  c(
    "#0C234B",
    "#AB0520",
    "#007D8A",
    "#98A4A3",
    "#621244",
    "#EF9600",
    "#EFF1F1",
    "#81D3EB",
    "#F9E17D",
    "#0076A8",
    "#D0D0CE",
    "#495A53",
    "#D6D2C4",
    "#E2E9EB",
    "#0C234B",
    "#F1F5F6"
  )
#
# Mini-palette
uagc_mini <-
  c(
    "#E2E9EB",
    "#98A4A3"
  )
#
# We might get some columns of class integer64, which some packages find
# difficult to handle. We'll define a function to identify columns of this
# class so we can convert it to integer32
is.integer64 <- function(x) {
  class(x) == "integer64"
}
#
## Defining a 'not in' function for lists
`%!in%` <- Negate(`%in%`)
#
## Violin plot
v_plot <- function(.data        = NULL,
                   y_var        = NULL,
                   y_var_label  = NULL,
                   y_axis_label = NULL,
                   title_var    = NULL,
                   fill_var     = NULL){
  
  vp <- 
    ggplot(.data, 
           aes(x = "", y = {{ y_var }})) +
    labs(x = y_var_label,
         y = y_axis_label) + 
    ggtitle(title_var) + 
    geom_violin(trim = F, 
                fill = fill_var) + 
    stat_summary(fun  = "mean",
                 geom = "point",
                 aes(color = "Mean")) +
    stat_summary(fun  = "median",
                 geom = "point",
                 aes(color = "Median")) +
    scale_colour_manual(values = c("white", "#81D3EB"), # Colors
                        name = "") # Remove the legend title
  return(vp)
}



# Reading data with {tidytuesday} -----------------------------------------
tuesdata_df <-
  tidytuesdayR::tt_load('2022-01-11')

colony_df <- 
  tuesdata_df$colony
skimr::skim(colony_df)
stview(dfSummary(colony_df))

stressor_df <- 
  tuesdata_df$stressor
skimr::skim(stressor_df)
stview(dfSummary(stressor_df))


# Imputing colony renovation values using Base R ---------------------------------
#
# Removing data for "Other States" (outside of US) and "United States" 
# (aggregated)
colony_df <- 
  colony_df %>%
  filter(state != c("Other States", "United States")) %>%
# Due to the large number of levels of one of the factors (states), we'll only
# consider numeric data for this
  select(colony_n:colony_reno_pct)

# We can replace missing values with the mean value of the column in all 
# numeric columns
colony_imp_1_df <- 
  colony_df %>% 
  mutate(across(where(is.numeric), 
                ~replace_na(., mean(., na.rm=TRUE))))
# ... or use median values...
colony_imp_2_df <- 
  colony_df %>% 
  mutate(across(where(is.numeric), 
                ~replace_na(., median(., na.rm=TRUE))))
# ... or just use zero
colony_imp_3_df <-
  colony_df %>%
  replace(is.na(.), 0)
#
# How do we know the fit is good? How do we choose between mean and median?
# We know from the skim summary if mean and median are somewhat equal and, 
# if they are not, if the mean is skewed by extreme values. 
# 
# Focusing on ratio columns because their scales make for more deciperable
# visuals
colony_lost_pct_v_plot <- 
  v_plot(.data = colony_df, 
         y_var = colony_lost_pct, 
         y_var_label  = "Percent of Colonies Lost",  
         y_axis_label = "Percent",
         title_var    = "Percent of Colonies Lost\nper Location per Quarter", 
         fill_var     = "#0C234B")
colony_reno_pct_v_plot <- 
  v_plot(.data = colony_df, 
         y_var = colony_reno_pct, 
         y_var_label  = "Percent of Colonies Renovated",  
         y_axis_label = "Percent",
         title_var    = "Percent of Colonies Renovated,\nper Location per Quarter", 
         fill_var     = "#AB0520")

# Percent plots in a grid
pct_violin_plots <- 
  plot_grid(colony_lost_pct_v_plot,
            colony_reno_pct_v_plot,
            ncol = 2)
# Percent plots title
pct_violin_title <- 
  ggdraw() + 
  draw_label("Distributions of Percentage Data",
             size  = 18,
             x     = 0, 
             hjust = 0) + 
  theme(plot.margin = margin(0, 0, 0, 7))
# Altogether
pct_violin_plot <- 
  plot_grid(pct_violin_title, 
            pct_violin_plots,
            ncol        = 1,
            rel_heights = c(0.1, 1)) 
ggsave(here::here("images",
                  "plot_violin_pct.png"),
       plot   = pct_violin_plot,
       width  = 2100, 
       height = 1500, 
       dpi    = 300, 
       units  = "px", 
       bg     = "white")

# Imputed data plots
imp_mean_reno_pct_v_plot <- 
  v_plot(.data = colony_imp_1_df, 
         y_var = colony_reno_pct, 
         y_var_label = "Percent of Colonies Renovated", 
         title_var = "Percent of Colonies Renovated,\nper Location per Quarter\nMean Imputed", 
         fill_var = "#0076A8")
imp_median_reno_pct_v_plot <- 
  v_plot(.data = colony_imp_2_df, 
         y_var = colony_reno_pct, 
         y_var_label = "Percent of Colonies Renovated", 
         title_var = "Percent of Colonies Renovated,\nper Location per Quarter\nMedian Imputed", 
         fill_var = "#EF9600")
imp_zero_reno_pct_v_plot <- 
  v_plot(.data = colony_imp_3_df, 
         y_var = colony_reno_pct, 
         y_var_label = "Percent of Colonies Renovated", 
         title_var = "Percent of Colonies Renovated,\nper Location per Quarter\nZero Imputed", 
         fill_var = "#621244")
# Imputed plots in a grid
imp_violin_plots <- 
  plot_grid(colony_reno_pct_v_plot,
            imp_zero_reno_pct_v_plot,
            imp_mean_reno_pct_v_plot,
            imp_median_reno_pct_v_plot,
            ncol = 2)
# Imputed plots title
imp_violin_title <- 
  ggdraw() + 
  draw_label("Distributions of Original vs Imputed Data",
             size = 18,
             x = 0, hjust = 0) + 
  theme(plot.margin = margin(0, 0, 0, 7))
# Altogether
imp_violin_plot <- 
  plot_grid(imp_violin_title, 
            imp_violin_plots,
            ncol        = 1,
            rel_heights = c(0.1, 1)) 
ggsave(here::here("images",
                  "plot_imputed_violin_pct.png"),
       plot   = imp_violin_plot,
       width  = 2100, 
       height = 2100, 
       dpi    = 300, 
       units  = "px", 
       bg     = "white")

# Imputations with the {mice} package -------------------------------------
#
# Looking at the colony_reno_pct variable, since it has the most variability
# among the base R imputation methods.
set.seed(123)
mice_imputed_df <- 
  data.frame(
    original      = colony_df$colony_reno_pct,
    imputed_pmm   = complete(mice(colony_df, method = "pmm"))$colony_reno_pct,
    imputed_cart  = complete(mice(colony_df, method = "cart"))$colony_reno_pct,
    imputed_lasso = complete(mice(colony_df, method = "lasso.norm"))$colony_reno_pct
    )
# Violin plots
original_reno_pct_v_plot <- 
  v_plot(.data       = mice_imputed_df, 
         y_var       = original, 
         y_var_label = "Percent of Colonies Renovated", 
         title_var   = "Percent of Colonies Renovated,\nper Location\nOriginal Data", 
         fill_var    = "#AB0520")
pmm_reno_pct_v_plot <- 
  v_plot(.data       = mice_imputed_df, 
         y_var       = imputed_pmm, 
         y_var_label = "Percent of Colonies Renovated", 
         title_var   = "Percent of Colonies Renovated,\nper Location\nPredictive Means Matching Imputation", 
         fill_var    = "#0076A8")
cart_reno_pct_v_plot <- 
  v_plot(.data       = mice_imputed_df, 
         y_var       = imputed_cart, 
         y_var_label = "Percent of Colonies Renovated", 
         title_var   = "Percent of Colonies Renovated,\nper Location\nClassification and Regression\nTrees Imputation", 
         fill_var    = "#EF9600")
lasso_reno_pct_v_plot <- 
  v_plot(.data       = mice_imputed_df, 
         y_var       = imputed_lasso, 
         y_var_label = "Percent of Colonies Renovated", 
         title_var   = "Percent of Colonies Renovated,\nper Location\nLasso Normal Imputation", 
         fill_var    = "#621244")
# Imputed plots in a grid
mice_violin_reno_pct_plots <- 
  plot_grid(original_reno_pct_v_plot,
            pmm_reno_pct_v_plot,
            cart_reno_pct_v_plot,
            lasso_reno_pct_v_plot,
            ncol = 2)
# Imputed plots title
mice_violin_reno_pct_title <- 
  ggdraw() + 
  draw_label("Distributions of Original vs MICE-Impute Revonations\nPercentage Data",
             size = 18,
             x = 0, hjust = 0) + 
  theme(plot.margin = margin(0, 0, 0, 7))
# Altogether
mice_violin_reno_pct_plot <- 
  plot_grid(mice_violin_reno_pct_title, 
            mice_violin_reno_pct_plots,
            ncol        = 1,
            rel_heights = c(0.1, 1)) 
ggsave(here::here("images",
                  "plot_mice_violin_reno_pct.png"),
       plot   = mice_violin_reno_pct_plot,
       width  = 2100, 
       height = 2100, 
       dpi    = 300, 
       units  = "px", 
       bg     = "white")


# Detach libraries --------------------------------------------------------
#
librarian::unshelf(
  summarytools, # nice eda
  tidytuesdayR, # data
  tidyverse,    # data wrangling
  corrplot,     # correlation charts
  cowplot,      # displaying charts together
  olsrr,        # checking eigenvalues for multicollinearity
  mice,         # multiple imputations via chained equations
  here,         # file organization
  VIM,          # imputation using k-nearest neighbor
  gt            # nice tables
)
#
  
  
  
  
  
  
  

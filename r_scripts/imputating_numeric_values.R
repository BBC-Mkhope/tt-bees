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
  here,         # file organization
  mice,         # multivariate imputation via chained equations
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
# Plot of colony loss percentages with missing data
lost_pct_plot <-
  ggplot(colony_df,
         aes(colony_lost_pct)) + 
  geom_histogram(color = "#0C234B",
                 fill  = "#0099F8") + 
  ggtitle("colony_lost_pct distribution") + 
  theme_classic() + 
  theme(plot.title = element_text(size = 18))

lost_pct_plot

# Data with Base R imputations
# Original colony_lost_pct vector with missing values
original_vec <- 
  colony_df %>%
  select(colony_lost_pct) %>%
  rename(original = colony_lost_pct) 
# colony_lost_pct vector with missing values replaced with zeros
imputed_zero_vec <-
  colony_df %>%
  select(colony_lost_pct) %>% 
  rename(imputed_zero = colony_lost_pct) %>%
  replace_na(list(imputed_zero = 0))
# colony_lost_pct vector with missing values replaced with the vector mean
imputed_mean_vec <-
  colony_df %>%
  select(colony_lost_pct) %>% 
  rename(imputed_mean = colony_lost_pct) %>% 
  mutate(imputed_mean = replace_na(
    imputed_mean, mean(imputed_mean, na.rm = TRUE))
  )
# colony_lost_pct vector with missing values replaced with the vector median
imputed_median_vec <-
  colony_df %>%
  select(colony_lost_pct) %>% 
  rename(imputed_median = colony_lost_pct) %>% 
  mutate(imputed_median = replace_na(
    imputed_median, median(imputed_median, na.rm = TRUE))
  )
# Binding the vectors into a dataframe
imputed_value_df <-
  data.frame(
    bind_rows(
      original_vec,
      imputed_zero_vec,
      imputed_mean_vec,
      imputed_median_vec
    )
  )
# Plotting the individual columns of the dataframe
# Plot with missing values
h1 <- ggplot(imputed_value_df, aes(x = original)) + 
  geom_histogram(fill = "#0C234B", color = "#000000", position = "identity") + 
  ggtitle("Original distribution") + 
  theme_classic() 
# Plot with imputed zeros
h2 <- ggplot(imputed_value_df, aes(x = imputed_zero)) + 
  geom_histogram(fill = "#AB0520", color = "#000000", position = "identity") + 
  ggtitle("Zero-imputed distribution") + 
  theme_classic() 
# Plot with imputed mean
h3 <- ggplot(imputed_value_df, aes(x = imputed_mean)) + 
  geom_histogram(fill = "#007D8A", color = "#000000", position = "identity") + 
  ggtitle("Mean-imputed distribution") + 
  theme_classic() 
# Plot with imputed median
h4 <- ggplot(imputed_value_df, aes(x = imputed_median)) + 
  geom_histogram(fill = "#621244", color = "#000000", position = "identity") + 
  ggtitle("Median-imputed distribution") + 
  theme_classic() 
# Displaying the plots together
# Plots 
base_r_plots <- 
  plot_grid(h1, h2, h3, h4,
            nrow = 2,
            ncol = 2)
# Title
base_r_title <- ggdraw() + 
  draw_label("Imputations using Base R functions",
             fontfamily = "Avenir", size = 18,
             x = 0, hjust = 0) + 
  theme(plot.margin = margin(0, 0, 0, 7))
# Altogether
base_r_imputed_plot <- 
  plot_grid(base_r_title, base_r_plots,
            ncol        = 1,
            rel_heights = c(0.1, 1)) 
ggsave(here::here("images",
                  "plots_base_r.png"),
       plot   = base_r_imputed_plot,
       width  = 2100, 
       height = 1500, 
       dpi    = 300, 
       units  = "px", 
       bg     = "white")


# Imputations with the {mice} and VIM packages -------------------------------------
#
# For imputation methods that are regression-based, we have to screen for 
# multicollinearity

# First, need a numeric dataframe
colony_num_df <-
  colony_df %>%
  select(!c(months, state, colony_n, colony_max, colony_added))
# Diagnostic tool 1: Using the {corrplot} package for correlations
correlation_plot <- corrplot(cor(colony_num_df, 
                                 use = "pairwise.complete.obs"), 
                             method = "number")
# For other diagnostics with the {olsrr}, a linear regression model is needed
my_model <- lm(colony_lost_pct ~ ., 
               data = colony_num_df)
# Diagnostic tool 2: Variance Inflatin Factor
vif_tbl <- ols_vif_tol(my_model)
# Diagnostic tool 3: eigenvalues and condition indices
eigenvalue_cindex_tbl <- ols_eigen_cindex(my_model)
# Based on the correlation and eigenvalue
# Original colony_lost_pct vector with missing values
original_vec <- 
  colony_num_df %>%
  select(colony_lost_pct) %>%
  rename(original = colony_lost_pct) 
# colony_lost_pct vector with missing values replaced by predictive mean matching
imputed_pmm_vec <-
  colony_num_df %>%
  complete(mice(colony_num_df,
                m      = 5,
                method = "pmm", 
                maxit  = 50, 
                seed   = 500)) %>%
  select(colony_lost_pct) %>% 
  rename(imputed_pmm = colony_lost_pct)
# colony_lost_pct vector with missing values replaced with the vector mean
imputed_mean_vec <-
  colony_num_df %>%
  select(colony_lost_pct) %>% 
  rename(imputed_mean = colony_lost_pct) %>% 
  mutate(imputed_mean = replace_na(
    imputed_mean, mean(imputed_mean, na.rm = TRUE))
  )
# colony_lost_pct vector with missing values replaced with the vector median
imputed_median_vec <-
  colony_num_df %>%
  select(colony_lost_pct) %>% 
  rename(imputed_median = colony_lost_pct) %>% 
  mutate(imputed_median = replace_na(
    imputed_median, median(imputed_median, na.rm = TRUE))
  )
# Binding the vectors into a dataframe
imputed_value_df <-
  data.frame(
    bind_rows(
      original_vec,
      imputed_zero_vec,
      imputed_mean_vec,
      imputed_median_vec
    )
  )
# Plotting the individual columns of the dataframe
# Plot with missing values
h1 <- ggplot(imputed_value_df, aes(x = original)) + 
  geom_histogram(fill = "#0C234B", color = "#000000", position = "identity") + 
  ggtitle("Original distribution") + 
  theme_classic() 
# Plot with imputed zeros
h2 <- ggplot(imputed_value_df, aes(x = imputed_zero)) + 
  geom_histogram(fill = "#AB0520", color = "#000000", position = "identity") + 
  ggtitle("Zero-imputed distribution") + 
  theme_classic() 
# Plot with imputed mean
h3 <- ggplot(imputed_value_df, aes(x = imputed_mean)) + 
  geom_histogram(fill = "#007D8A", color = "#000000", position = "identity") + 
  ggtitle("Mean-imputed distribution") + 
  theme_classic() 
# Plot with imputed median
h4 <- ggplot(imputed_value_df, aes(x = imputed_median)) + 
  geom_histogram(fill = "#621244", color = "#000000", position = "identity") + 
  ggtitle("Median-imputed distribution") + 
  theme_classic() 
# Displaying the plots together
# Plots 
base_r_plots <- 
  plot_grid(h1, h2, h3, h4,
            nrow = 2,
            ncol = 2)
# Title
base_r_title <- ggdraw() + 
  draw_label("Imputations using Base R functions",
             fontfamily = "Avenir", size = 18,
             x = 0, hjust = 0) + 
  theme(plot.margin = margin(0, 0, 0, 7))
# Altogether
base_r_imputed_plot <- 
  plot_grid(base_r_title, base_r_plots,
            ncol        = 1,
            rel_heights = c(0.1, 1)) 
ggsave(here::here("images",
                  "plots_base_r.png"),
       plot   = base_r_imputed_plot,
       width  = 2100, 
       height = 1500, 
       dpi    = 300, 
       units  = "px", 
       bg     = "white")

  
  
  
  
  
  
  
  

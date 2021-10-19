
library(tidyr)
library(devtools)
devtools::install_github("zhengnow/gtsummary", ref="add_n_cluster")

load(here::here("data-raw","dwi_long.RData"))
dat <- dt_long %>%
  dplyr::select(id,
                rad1_t2_bin, rad1_t2_,
                #rad1_dwi_bin, #rad1_dwi_,
                higrade, path) %>%
  mutate(
    rad1_t2_bin = factor(rad1_t2_bin, 0:1, c("1-2","3-5")),
    higrade = factor(higrade, label = c("low","high"))
  ) %>%
  labelled::set_variable_labels(
    rad1_t2_bin = "R1 t2",
    rad1_t2_ = "R1 t2",
    higrade = "High grade",
    #rad1_dwi_bin = "R1 DWI >=3",
    #rad1_dwi_ = "R1 DWI",
    path = "Pathology"
  )

svydat <- survey::svydesign(ids = ~id, data=dat)

tab0 <-
  svydat %>%
  gtsummary::tbl_svysummary(
    by = higrade)

tab1 <-
   tab0 %>%
  gtsummary::add_n()

tab2 <-
  tab0 %>%
  gtsummary::add_overall()

  # gtsummary::modify_table_styling(
  #   columns = c(2,3),
  #   label = c("Low grade", "High grade"),
  #   spanning_header = "High Grade"
  # )

# svy regression
fit1 <-
  survey::svyglm(higrade ~ rad1_t2_bin, design = svydat, family = binomial)
fit2 <-
  glm(higrade ~ rad1_t2_bin, data = dat, family = binomial)

tab3 <-
  fit1 %>%
  gtsummary::tbl_regression(
    exponentiate = TRUE,
    add_estimate_to_reference_rows = TRUE
  ) %>%
  add_n(location = "level")




# tab2 <-
# survey::svytable(~dat$rad1_t2_bin, design=svydat) %>%
#   as_tibble()

#
# var.data <-
#   cbind(svydat$variables$rad1_dwi_bin, data$cluster, svydat$variables$higrade)%>%
#   as_tibble()



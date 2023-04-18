#################################################################
##         prepare univariate scores and distributions         ##
#################################################################
univariate_distribution_parameters <- readRDS("./tables/univariate/univariate_distributions.rds") %>%
  dplyr::group_by(ID, package, initialisation_method) %>%
  dplyr::mutate(N.bootstrap=dplyr::row_number()) %>% dplyr::ungroup()
univariate_time_computation <- readRDS("./tables/univariate/univariate_time_computation.rds")
univariate_configuration <- readRDS("./tables/univariate/univariate_configuration_scenario.rds")

formatted_univariate_distribution_parameters <- univariate_distribution_parameters %>%
  rename_with(~gsub("^sd", "sigma", .x)) %>%
  mutate(color=dplyr::if_else(package %in% c("RGMMBench", "Rmixmod", "mixtools"), 'red', 'green'),
         package=dplyr::if_else(package %in% c("flexmix", "mclust", "mixtools"),
                                glue::glue("<b style='color:{color}; font-size:22pt'>{package}</b>"),
                                glue::glue("<p style='color:{color}'>{package}</p>")))

univariate_parameters_local_scores <- readRDS("./tables/univariate/univariate_local_scores.rds") %>%
  filter(package %in% c("mixtools", "Rmixmod", "mclust", "EMCluster", "flexmix")) %>%
  mutate(package=forcats::fct_recode(package, `Rmixmod / RGMMBench`="Rmixmod", `mclust / bgmm`="mclust", `EMCluster / GMKMcharlie`="EMCluster"))

univariate_parameters_local_scores_summary <- univariate_parameters_local_scores %>% dplyr::filter(scores=="mse") %>%
  rowwise() %>% mutate(global_mse_p = sum(c(p1, p2, p3, p4)),
                       global_mse_mu = sum(c(mu1, mu2, mu3, mu4)),
                       global_mse_sigma=sum(c(sigma1, sigma2, sigma3, sigma4))) %>%
  dplyr::group_by(ID, initialisation_method, package) %>%
  dplyr::summarise(global_mse_p=mean(global_mse_p), global_mse_mu=mean(global_mse_mu), global_mse_sigma=mean(global_mse_sigma)) %>%
  dplyr::inner_join(univariate_parameters_local_scores %>% dplyr::filter(scores=="bias") %>%
                      rowwise() %>% mutate(global_bias_p = sum(abs(c(p1, p2, p3, p4))),
                                           global_bias_mu = sum(abs(c(mu1, mu2, mu3, mu4))),
                                           global_bias_sigma=sum(abs(c(sigma1, sigma2, sigma3, sigma4)))) %>%
                      dplyr::group_by(ID, initialisation_method, package) %>%
                      dplyr::summarise(global_bias_p=mean(global_bias_p), global_bias_mu=mean(global_bias_mu), global_bias_sigma=mean(global_bias_sigma))) %>%
  dplyr::arrange(ID, package, initialisation_method) %>%
  dplyr::relocate(initialisation_method, .after = package) %>%
  dplyr::rename(Package=package, `Initialisation Method`=initialisation_method) %>%
  mutate(across(where(is.numeric), ~format(.x, digits=2, nsmall=0, ) %>% as.numeric)) %>% dplyr::ungroup()


#################################################################
##        prepare bivariate scores and distributions        ##
#################################################################

bivariate_parameters <- readRDS("./tables/bivariate/bivariate_distributions.rds")%>%
  dplyr::group_by(ID, package, initialisation_method) %>%
  dplyr::mutate(N.bootstrap=dplyr::row_number()) %>% dplyr::ungroup()
bivariate_time_computation <- readRDS("./tables/bivariate/bivariate_time_computation.rds")
bivariate_configuration <- readRDS("./tables/bivariate/bivariate_configuration_scenario.rds")

formatted_bivariate_parameters <- bivariate_parameters %>%
  mutate(color=dplyr::if_else(package %in% c("RGMMBench", "Rmixmod", "mixtools"), 'red', 'green'),
         package=dplyr::if_else(package %in% c("flexmix", "mclust", "mixtools"),
                                glue::glue("<b style='color:{color}; font-size:22pt'>{package}</b>"),
                                glue::glue("<p style='color:{color}'>{package}</p>")))


bivariate_parameters_local_scores <- readRDS("./tables/bivariate/bivariate_local_scores.rds") %>%
  filter(package %in% c("mixtools", "Rmixmod", "mclust", "EMCluster", "flexmix")) %>%
  mutate(package=forcats::fct_recode(package, `Rmixmod / RGMMBench`="Rmixmod", `mclust / bgmm`="mclust", `EMCluster / GMKMcharlie`="EMCluster"))

bivariate_parameters_local_scores_summary <- bivariate_parameters_local_scores %>% dplyr::filter(scores=="mse") %>%
  rowwise() %>% mutate(global_mse_p = sum(dplyr::c_across(dplyr::matches("p[[:digit:]]+"))),
                       global_mse_mu = sum(dplyr::c_across(dplyr::matches("mu"))),
                       global_mse_sigma=sum(dplyr::c_across(dplyr::matches("sd")))) %>%
  dplyr::group_by(ID, initialisation_method, package) %>%
  dplyr::summarise(global_mse_p=mean(global_mse_p), global_mse_mu=mean(global_mse_mu), global_mse_sigma=mean(global_mse_sigma)) %>%
  dplyr::inner_join(bivariate_parameters_local_scores %>% dplyr::filter(scores=="bias") %>%
                      rowwise() %>% mutate(global_bias_p = sum(abs(dplyr::c_across(dplyr::matches("p[[:digit:]]+")))),
                                           global_bias_mu = sum(abs(dplyr::c_across(dplyr::matches("mu")))),
                                           global_bias_sigma=sum(abs(dplyr::c_across(dplyr::matches("sd"))))) %>%
                      dplyr::group_by(ID, initialisation_method, package) %>%
                      dplyr::summarise(global_bias_p=mean(global_bias_p), global_bias_mu=mean(global_bias_mu), global_bias_sigma=mean(global_bias_sigma))) %>%
  dplyr::arrange(ID, package, initialisation_method) %>%
  dplyr::relocate(initialisation_method, .after = package) %>%
  dplyr::rename(Package=package, `Initialisation Method`=initialisation_method) %>%
  mutate(across(where(is.numeric), ~format(.x, digits=2, nsmall=0, ) %>% as.numeric)) %>% ungroup()
metric_colnames <- c("global_mse_p", "global_mse_mu", "global_mse_sigma", "global_bias_p", "global_bias_mu", "global_bias_sigma")






#################################################################
##        prepare HD scores and distributions        ##
#################################################################

HD_distributions <- readRDS("./tables/HD/HD_distributions.rds")
HD_configuration <- readRDS("./tables/HD/HD_configuration_scenario.rds")
HD_time_computation <- readRDS("./tables/HD/HD_time_computation.rds")
HD_parameters_local_scores <- readRDS("./tables/HD/HD_local_scores.rds") 


# HD_parameters_local_scores %>%
#   mutate(package = forcats::fct_recode(package, RGMMBench="em R")) %>%
#   saveRDS("./tables/HD/HD_local_scores.rds")
# %>%   dplyr::arrange(ID) %>% relocate(ID)
# saveRDS(HD_time_computation, "./tables/HD/HD_time_computation.rds")
# %>% 
#   dplyr::select(!matches("3$")) %>% relocate(N.bootstrap, .after=nobservations)
# saveRDS(HD_distributions, "./tables/HD/HD_distributions.rds")


formatted_HD_distributions <- HD_distributions %>% 
  mutate(package=forcats::fct_relevel(package, "RGMMBench", "Rmixmod", "mixtools",
                                      "GMKMcharlie", "mclust", "flexmix",
                                      "bgmm", "EMCluster", "HDclassif", "EMMIXmfa")) %>% 
  arrange(ID, initialisation_method, N.bootstrap, package) %>% 
  mutate(color=dplyr::case_match(package, 
                                 c("RGMMBench", "Rmixmod", "mixtools") ~ "red", 
                                 c("HDclassif", "EMMIXmfa") ~"blue", .default = "green"), 
         package=dplyr::case_match(package, 
                                   c("flexmix", "mclust", "mixtools") ~ glue::glue("<b style='color:{color}; font-size:22pt'>{package}</b>"), 
                                   c("HDclassif", "EMMIXmfa") ~ glue::glue("<i style='color:{color}'>{package}</i>"),
                                   .default = glue::glue("<p style='color:{color}'>{package}</p>"))) 


num_failed <- formatted_HD_distributions %>%
  filter(package %in% c("Rmixmod", "mclust", "bgmm", "EMCluster", "HDclassif", "EMMIXmfa")) %>%
  mutate(package=forcats::fct_recode(package, `mixtools / Rmixmod / RGMMBench`="Rmixmod",
                                     `mclust / flexmix / GMKMcharlie`="mclust")) %>% 
  group_by(ID, initialisation_method, package) %>% 
  summarise(Success=n())

HD_parameters_local_scores <- readRDS("./tables/HD/HD_local_scores.rds") %>% 
  filter(package %in% c("Rmixmod", "mclust", "bgmm", "EMCluster", "HDclassif", "EMMIXmfa")) %>%
  mutate(package=forcats::fct_relevel(package, "Rmixmod", "mclust", 
                                      "bgmm", "EMCluster", "HDclassif", "EMMIXmfa")) %>% 
  arrange(ID, initialisation_method, package) %>% 
  mutate(package=forcats::fct_recode(package, `mixtools / Rmixmod / RGMMBench`="Rmixmod",
                                     `mclust / flexmix / GMKMcharlie`="mclust"))

HD_parameters_local_scores_summary <- HD_parameters_local_scores %>% dplyr::filter(scores=="mse") %>%
  rowwise() %>% summarise(ID, initialisation_method, package, 
                          global_mse_p = sum(dplyr::c_across(dplyr::matches("p[[:digit:]]+"))),
                          global_mse_mu = sum(dplyr::c_across(dplyr::matches("mu"))),
                          global_mse_sigma=sum(dplyr::c_across(dplyr::matches("sd")))) %>% 
  dplyr::inner_join(HD_parameters_local_scores %>% dplyr::filter(scores=="bias") %>%
                      rowwise() %>% summarise(ID, initialisation_method, package, 
                                              global_bias_p = sum(abs(dplyr::c_across(dplyr::matches("^p[[:digit:]]+$")))),
                                              global_bias_mu = sum(abs(dplyr::c_across(dplyr::matches("mu")))),
                                              global_bias_sigma=sum(abs(dplyr::c_across(dplyr::matches("sd"))))),
                    by = join_by(ID, initialisation_method, package)) %>%
  dplyr::inner_join(num_failed, join_by(ID, initialisation_method, package)) %>% 
  dplyr::filter(if_all(everything(), ~ !is.na(.))) %>% # remove columns with NULL values  
  dplyr::arrange(ID, package, initialisation_method) %>%
  dplyr::relocate(initialisation_method, .after = package) %>%
  dplyr::rename(Package=package, `Initialisation Method`=initialisation_method) %>%
  mutate(across(where(is.numeric), ~format(.x, digits=2, nsmall=0, ) %>% as.numeric)) 


HD_metric_colnames <- c("global_mse_p", "global_mse_mu", "global_mse_sigma", "global_bias_p",
                        "global_bias_mu", "global_bias_sigma", "Success")

############################################################################
############################################################################
###                                                                      ###
###                       VISUALISATION THEMSELVES                       ###
###                                                                      ###
############################################################################
############################################################################

##################################################################
##                   generate ComplexHeatmaps                   ##
##################################################################
set.seed(5)
kmeans_univariate_cor_matrix <- RGMMBench::plot_correlation_Heatmap(univariate_distribution_parameters %>% filter(ID=="U9"))$random@matrix
packages_bench <- colnames(kmeans_univariate_cor_matrix)
univariate_row_labels <- dplyr::if_else(packages_bench %in% c("mixtools", "flexmix", "mclust"), paste0("<span style='font-size:16pt'>**", 
                                                                                                       packages_bench, "**</span>"),packages_bench)
complex_heatmap_univariate <- ComplexHeatmap::Heatmap(kmeans_univariate_cor_matrix, name = "mat_HD", 
                                                      heatmap_legend_param = list(title = "Pearson's\ncorrelation", title_position = "topcenter"), 
                                                      width = unit(8,"cm"),  height = unit(8, "cm"),
                                                      column_title = NULL, row_title = NULL,
                                                      column_names_rot = 45, cluster_columns = TRUE,column_names_gp = grid::gpar(fontsize = 8), 
                                                      top_annotation = HeatmapAnnotation(foo = anno_block(gp = gpar(fill = c("green", "red", "yellow")),
                                                                                                          labels = c("class 2", "class 1", "outlier"), 
                                                                                                          labels_gp = gpar(col = "black", fontsize = 9, fontfamily="serif"))),
                                                      column_labels = ComplexHeatmap::gt_render(univariate_row_labels), column_km = 3, column_km_repeats = 100,
                                                      cluster_rows = TRUE, row_names_gp = grid::gpar(fontsize = 8), 
                                                      row_labels = ComplexHeatmap::gt_render(univariate_row_labels), row_km = 3, row_km_repeats = 100,
                                                      left_annotation = rowAnnotation(foo = anno_block(gp = gpar(fill = c("green", "red", "yellow")),
                                                                                                       labels = c("class 2", "class 1", "outlier"), 
                                                                                                       labels_gp = gpar(col = "black", fontsize = 9, fontfamily="serif"))))


set.seed(5)
kmeans_bivariate_cor_matrix <- RGMMBench::plot_correlation_Heatmap(bivariate_parameters %>% filter(ID=="B11"))$rebmix@matrix
packages_bench <- colnames(kmeans_bivariate_cor_matrix)
bivariate_row_labels <- dplyr::if_else(packages_bench %in% c("mixtools", "flexmix", "mclust"), paste0("<span style='font-size:16pt'>**", 
                                                                                                      packages_bench, "**</span>"),packages_bench)
complex_heatmap_bivariate <- ComplexHeatmap::Heatmap(kmeans_bivariate_cor_matrix, name = "mat_HD", 
                                                     heatmap_legend_param = list(title = "Pearson's\ncorrelation", title_position = "topcenter"), 
                                                     width = unit(8,"cm"),  height = unit(8, "cm"),
                                                     column_title = NULL, row_title = NULL,
                                                     column_names_rot = 45, cluster_columns = TRUE,column_names_gp = grid::gpar(fontsize = 8), 
                                                     top_annotation = HeatmapAnnotation(foo = anno_block(gp = gpar(fill = c("yellow", "red", "green")),
                                                                                                         labels = c("outlier", "class 1", "class 2"), 
                                                                                                         labels_gp = gpar(col = "black", fontsize = 10, fontfamily="serif"))),
                                                     column_labels = ComplexHeatmap::gt_render(bivariate_row_labels), column_km = 3, column_km_repeats = 100,
                                                     cluster_rows = TRUE, row_names_gp = grid::gpar(fontsize = 8), 
                                                     row_labels = ComplexHeatmap::gt_render(bivariate_row_labels), row_km = 3, row_km_repeats = 100,
                                                     left_annotation = rowAnnotation(foo = anno_block(gp = gpar(fill = c("yellow", "red", "green")),
                                                                                                      labels = c("outlier", "class 1", "class 2"), 
                                                                                                      labels_gp = gpar(col = "black", fontsize = 10, fontfamily="serif"))))



kmeans_HD_cor_matrix <- RGMMBench::plot_correlation_Heatmap(formatted_HD_distributions %>% filter(ID=="7a"))$kmeans@matrix
packages_HD <- colnames(kmeans_HD_cor_matrix)
HD_row_labels <- dplyr::if_else(packages_HD %in% c("mixtools", "flexmix", "mclust"), paste0("<span style='font-size:16pt'>**", 
                                                                                            packages_HD, "**</span>"),packages_HD) 
HD_row_labels <- dplyr::if_else(HD_row_labels %in% c("HDclassif", "EMMIXmfa"), paste0("<i style='color:blue'>", HD_row_labels, "</i>"),HD_row_labels) 
complex_heatmap_HD <- ComplexHeatmap::Heatmap(kmeans_HD_cor_matrix, name = "mat_HD", 
                                              heatmap_legend_param = list(title = "Pearson's\ncorrelation", title_position = "topcenter"), 
                                              width = unit(8,"cm"),  height = unit(8, "cm"),
                                              column_title = NULL, row_title = NULL,
                                              column_names_rot = 45, cluster_columns = TRUE,column_names_gp = grid::gpar(fontsize = 8), 
                                              top_annotation = HeatmapAnnotation(foo = anno_block(gp = gpar(fill = c("blue", "yellow", "red", "green")),
                                                                                                  labels = c("HD", "outliers", "class 1", "class 2"), 
                                                                                                  labels_gp = gpar(col = "black", fontsize = 10, fontfamily="serif"))),
                                              column_labels = ComplexHeatmap::gt_render(HD_row_labels), column_km = 4, column_km_repeats = 100,
                                              cluster_rows = TRUE, row_names_gp = grid::gpar(fontsize = 8), 
                                              row_labels = ComplexHeatmap::gt_render(HD_row_labels), row_km = 4, row_km_repeats = 100,
                                              left_annotation = rowAnnotation(foo = anno_block(gp = gpar(fill = c("blue", "yellow", "red", "green")),
                                                                                               labels = c("HD", "outliers", "class 1", "class 2"), 
                                                                                               labels_gp = gpar(col = "black", fontsize = 10, fontfamily="serif"))))

heatmap_global <- cowplot::plot_grid(plotlist =list(complex_heatmap_univariate %>% draw() %>% grid::grid.grabExpr(),
                                                    complex_heatmap_bivariate %>% draw() %>% grid::grid.grabExpr(), 
                                                    complex_heatmap_HD %>% draw() %>% grid::grid.grabExpr()), 
                                     ncol = 3, nrow = 1, align = "hv", axis = "tblr", labels = "AUTO",
                                     label_size = 20, label_fontface = "bold")

ggsave("./figs/dichotomy_package_conclusion.png", heatmap_global, dpi = 300, width = 18, height=7)


library(data.tree)
decision_tree_package_differences <- Node$new("EM implementation in R")
log_rescaled_packages <- decision_tree_package_differences$AddChild("Log rescaling")
random_restarts <- log_rescaled_packages$AddChild("Random restarts")
mixtools_pkg <- random_restarts$AddChild("mixtools")
remove_irrelevant_component <- log_rescaled_packages$AddChild("Remove iteratively\nthe less prevalent component")
flexmix_pkg <- remove_irrelevant_component$AddChild("flexmix\nGMKMcharlie")
no_elimination <- log_rescaled_packages$AddChild("No removal\nof components")
mclust_pkg <- no_elimination$AddChild("mclust,\nEMCluster")
early_stop_packages <- decision_tree_package_differences$AddChild("Early stop")
Rmixmod_pkg <- early_stop_packages$AddChild("Rmixmod")
substract_log <- decision_tree_package_differences$AddChild("Substract the minimal\nlog-likelihood probability")
bgmm_package <- substract_log$AddChild("bgmm")
hd_packages <- decision_tree_package_differences$AddChild("Reduce the intrinsic\n dimension space")
remove_vars <- hd_packages$AddChild("Discard variables")
var_sel_packages <- remove_vars$AddChild("RobMixReg\nclustvarsel")
factor_mixtures <- hd_packages$AddChild("Factor analysis projection")
hd_packages <- factor_mixtures$AddChild("pgmm, HDclassif,\nEMMXmfa")


# custom the decision tree plot
SetGraphStyle(decision_tree_package_differences, rankdir = "TB", fontSize = 25)
SetEdgeStyle(decision_tree_package_differences, arrowhead = "vee", penwidth = 2)
SetNodeStyle(decision_tree_package_differences, style = "filled,rounded", shape = "box",
             fontname = "helvetica", tooltip = GetDefaultTooltip, fontcolor = "black")
Do(decision_tree_package_differences$leaves, function(node) SetNodeStyle(node, shape = "egg")) 

# beware, add to re-add leaf property, otherwise it is reinitialised!!!!
SetNodeStyle(hd_packages, fillcolor = "blue", shape = "egg")
SetNodeStyle(mixtools_pkg, fillcolor = "red", shape = "egg"); SetNodeStyle(Rmixmod_pkg, fillcolor = "red", shape = "egg")
SetNodeStyle(bgmm_package, fillcolor = "green", shape = "egg"); 
SetNodeStyle(flexmix_pkg, fillcolor = "green", shape = "egg"); SetNodeStyle(mclust_pkg, fillcolor = "green", shape = "egg")
DiagrammeR::export_graph(ToDiagrammeRGraph(decision_tree_package_differences),
                         "./figs/packages_differences.png", file_type = "png")

# system(paste("pdfcrop", "./figs/dichotomy_package_conclusion_complete.pdf"))



##################################################################
##                    generate decision tree                    ##
##################################################################

decision_tree_univariate <- Node$new("Estimation of the MLE in GMMs")
high_dimension <- decision_tree_univariate$AddChild("High dimensional setting,\nspecially when D>N")
HD_classif_node <- high_dimension$AddChild("HDclassif, MclustDR")
separated <- decision_tree_univariate$AddChild("Well-separated components")
rebmix <- separated$AddChild("Rebmix initialisation")
overlapping <- decision_tree_univariate$AddChild("Overlapping components")
balanced <- overlapping$AddChild("Balanced mixture")
kmeans_balanced <- balanced$AddChild("*k*-means initialisation with \n GMKMcharlie or mclust")
unbalanced_numerous_components <- overlapping$AddChild("Unbalanced and number of clusters \U2265 4")
unbalanced_variability <- unbalanced_numerous_components$AddChild("Optimise variability and MSE")
second_class <- unbalanced_variability$AddChild("Second class of packages \n with *k*-means initialisation, \n preferentially with GMKMcharlie \n or EMCluster")
unbalanced_bias <- unbalanced_numerous_components$AddChild("Optimise bias")
first_class <- unbalanced_bias$AddChild("First class of packages \n with *k*-means initialization \n for proportions and variability \n estimations and random \n for centroids estimation")
unbalanced_few_components <- overlapping$AddChild("Unbalanced and number of clusters < 4")
decision_unbalanced_few_components <- unbalanced_few_components$AddChild("rebmix or random initialisation \n with the second class of packages.")

# custom the decision tree plot
SetGraphStyle(decision_tree_univariate, rankdir = "TB", fontSize = 25)
SetEdgeStyle(decision_tree_univariate, arrowhead = "vee", penwidth = 2)
SetNodeStyle(decision_tree_univariate, style = "filled,rounded", shape = "box",
             fontname = "helvetica", tooltip = GetDefaultTooltip)
# SetNodeStyle(separated, fillcolor = "chartreuse1")
# SetNodeStyle(overlapping, fillcolor = "crimson")
Do(decision_tree_univariate$leaves, function(node) SetNodeStyle(node, shape = "egg")) # custom the leaves


DiagrammeR::export_graph(ToDiagrammeRGraph(decision_tree_univariate),
                         "./figs/decision_tree.png", file_type = "png")
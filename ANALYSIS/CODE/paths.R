# This script can be sourced to have all the paths handy.
## source('ANALYSIS/CODE/paths.R')

# Functions ----
path_functions = 'ANALYSIS/CODE/functions'

# Data ----
path_selection_tables = 'ANALYSIS/DATA/selection tables'
path_annotations_2021 = 'ANALYSIS/DATA/overview recordings/annotations - 2021.xlsx'
path_annotations_2020 = 'ANALYSIS/DATA/overview recordings/annotations - 2020.csv'
path_context = 'ANALYSIS/DATA/overview recordings/context - 2020.xlsx'
path_call_type_classification = 'ANALYSIS/CODE/01 audio analysis/call type classification.R'
path_traces_2021 = 'ANALYSIS/DATA/luscinia/all_2021.csv'
path_traces_2020_contact = 'ANALYSIS/DATA/luscinia/contact_2020.csv'
path_traces_2020_remaining = 'ANALYSIS/DATA/luscinia/remaining_2020.csv'
path_bad_traces_2021 = 'ANALYSIS/DATA/luscinia/bad_files_2021.xlsx'
path_bad_traces_2020 = 'ANALYSIS/DATA/luscinia/bad_files_2020.xlsx'
path_audio_20 = '/Volumes/Elements 4/BARCELONA_2020/audio'
path_audio_21 = '/Volumes/Elements 4/BARCELONA_2021/audio'
path_sorted_loud_contact_20 = 'ANALYSIS/RESULTS/contact call sorting 20 - done'
path_sorted_loud_contact_21 = 'ANALYSIS/RESULTS/contact call sorting 21 - done'
path_nest_entry_overview_20 = 'ANALYSIS/DATA/nesting/nest entry overview - 2020 - manual added.csv'
path_nest_entry_overview_21 = 'ANALYSIS/DATA/nesting/nest entry overview - 2021 - manual added.csv'
path_nest_per_ind_20 = 'ANALYSIS/DATA/nesting/nest per ID 2020 - manual added.csv'
path_nest_per_ind_21 = 'ANALYSIS/DATA/nesting/nest per ID 2021 - manual added.csv'
path_nesting_sizes = 'ANALYSIS/RESULTS/02 social analysis/nesting_sizes.RData'
path_nest_overview_20 = 'ANALYSIS/DATA/nesting/nest overview 2020.csv'
path_nest_overview_21 = 'ANALYSIS/DATA/nesting/nest overview 2021.csv'
path_social_data_simeon_20 = 'ANALYSIS/DATA/social data/social data Simeon 2020.csv'
path_social_data_andres_20 = 'ANALYSIS/DATA/social data/social data Andres 2020.csv'
path_social_data_mireia_20 = 'ANALYSIS/DATA/social data/social data Mireia 2020.csv'
path_social_data_simeon_21 = 'ANALYSIS/DATA/social data/social data Simeon 2021.xlsx'
path_social_data_andres_21 = 'ANALYSIS/DATA/social data/social data Andrés 2021.xlsx'
path_sexing_f = 'ANALYSIS/DATA/sex/data Francisca.csv'
path_sexing_v = 'ANALYSIS/DATA/sex/sexing vetgenomics.xlsx'
path_ringing_data = 'ANALYSIS/DATA/age/all ringing info all birds.xlsx'
path_nest_points_2020 = 'ANALYSIS/DATA/waypoints/nests 2020'
path_nest_points_2021 = 'ANALYSIS/DATA/waypoints/nests 2021'
path_overview_recordings_20 = 'ANALYSIS/DATA/overview recordings/overview recordings - 2020.csv'
path_overview_recordings_21 = 'ANALYSIS/DATA/overview recordings/overview recordings - 2021.xlsx'
path_overview_nesting_20 = 'ANALYSIS/DATA/nesting/nest overview 2020.csv'
path_overview_nesting_21 = 'ANALYSIS/DATA/nesting/nest overview 2021.csv'
path_microsat = 'ANALYSIS/DATA/microsat/230331 Microsatellites Results.xlsx'

# 00 genetic analysis ----
## results
path_wang_out = 'ANALYSIS/RESULTS/00 genetic analysis/wang_out.RData'
path_trioml_out = 'ANALYSIS/RESULTS/00 genetic analysis/trioml_out.RData'
  
# 01 audio analysis ----
## code
path_div_model = 'ANALYSIS/CODE/01 audio analysis/m_div.stan'
path_m_sim = 'ANALYSIS/CODE/01 audio analysis/m_sim.stan'
## results
path_data = 'ANALYSIS/RESULTS/01 audio analysis/all_data.RData'
path_dtw_m = 'ANALYSIS/RESULTS/01 audio analysis/dtw_m.RData' # contact calls
path_spcc_m = 'ANALYSIS/RESULTS/01 audio analysis/spcc_m.RData' # trruup calls
path_waves = 'ANALYSIS/RESULTS/01 audio analysis/waves.RData'
path_div_model_results = 'ANALYSIS/RESULTS/01 audio analysis/model_result_div.RData'
path_div_results_slim = 'ANALYSIS/RESULTS/01 audio analysis/div_results_slim.RData'
path_similarity_out = 'ANALYSIS/RESULTS/01 audio analysis/similarity_out.RData'
path_similarity_out_trruup = 'ANALYSIS/RESULTS/01 audio analysis/similarity_out_trruup.RData'
path_post_similarity = 'ANALYSIS/RESULTS/01 audio analysis/similarity_post.RData'
path_pdf_test_diversity = 'ANALYSIS/RESULTS/04 diversity analysis/test traces diversity.pdf'
path_pdf_test_similarity = 'ANALYSIS/RESULTS/03 similarity analysis/test similarity output.pdf'

# 02 social analysis ----
## code
path_gbi_model = 'ANALYSIS/CODE/02 social analysis/simulation gbi/m_degree.stan'
path_group_size_model = 'ANALYSIS/CODE/02 social analysis/m_group_size.stan'
## results
path_sim_dat_gbi = 'ANALYSIS/RESULTS/02 social analysis/simulation gbi/sim_dat.RData'
path_real_dat_gbi_20 = 'ANALYSIS/RESULTS/02 social analysis/gbi_20.RData'
path_real_dat_gbi_21 = 'ANALYSIS/RESULTS/02 social analysis/gbi_21.RData'
path_sim_dat_gbi_results = 'ANALYSIS/RESULTS/02 social analysis/simulation gbi/sim_results.RData'
path_real_dat_gbi_results = 'ANALYSIS/RESULTS/02 social analysis/gbi_results.RData'
path_foraging_network = 'ANALYSIS/RESULTS/02 social analysis/foraging_network.RData'
path_pdf_foraging_measures = 'ANALYSIS/RESULTS/02 social analysis/foraging measures.pdf'
path_net_meas = 'ANALYSIS/RESULTS/02 social analysis/net_meas.RData'
path_samp_size_gbi = 'ANALYSIS/RESULTS/02 social analysis/samp_size_gbi.RData'
path_group_sizes = 'ANALYSIS/RESULTS/02 social analysis/group_sizes.RData'
path_dat_mate_id = 'ANALYSIS/RESULTS/02 social analysis/dat_mate_id.RData'
path_mate_network = 'ANALYSIS/RESULTS/02 social analysis/mate_network.RData'
path_spatial_network = 'ANALYSIS/RESULTS/02 social analysis/spatial_network.RData'
path_pdf_group_size = 'ANALYSIS/RESULTS/02 social analysis/group sizes.pdf'
path_group_size_results = 'ANALYSIS/RESULTS/02 social analysis/group_size_bayes.RData'
path_pdf_prop_missing = 'ANALYSIS/RESULTS/02 social analysis/proportion missing tags.pdf'
path_pdf_aggressive_network = 'ANALYSIS/RESULTS/02 social analysis/aggressive network.pdf'
path_aggressive_network = 'ANALYSIS/RESULTS/02 social analysis/aggressive_network.RData'
path_pdf_tolerance_network = 'ANALYSIS/RESULTS/02 social analysis/tolerance network.pdf'
path_tolerance_network = 'ANALYSIS/RESULTS/02 social analysis/tolerance_network.RData'
path_pdf_networks_20 = 'ANALYSIS/RESULTS/02 social analysis/all networks 2020.pdf'
path_pdf_networks_21 = 'ANALYSIS/RESULTS/02 social analysis/all networks 2021.pdf'
path_multi_out = 'ANALYSIS/RESULTS/02 social analysis/multi_out.RData'
path_age_dat = 'ANALYSIS/RESULTS/age/age_dat.RData'

# 03 similarity analysis ----
## code
path_m_foraging_total = 'ANALYSIS/CODE/03 similarity analysis/m_foraging_total.stan'
path_m_foraging_direct = 'ANALYSIS/CODE/03 similarity analysis/m_foraging_direct.stan'
path_m_mate_total = 'ANALYSIS/CODE/03 similarity analysis/m_mate_total.stan'
path_m_spat_total = 'ANALYSIS/CODE/03 similarity analysis/m_spat_total.stan'
path_m_clust_total = 'ANALYSIS/CODE/03 similarity analysis/m_clust_total.stan'
path_m_location_total = 'ANALYSIS/CODE/03 similarity analysis/m_location_total.stan'
path_m_location_direct = 'ANALYSIS/CODE/03 similarity analysis/m_location_direct.stan'
path_m_aggressive = 'ANALYSIS/CODE/03 similarity analysis/m_aggressive.stan'
path_m_gen_total = 'ANALYSIS/CODE/03 similarity analysis/m_gen_total.stan'
path_m_gen_direct = 'ANALYSIS/CODE/03 similarity analysis/m_gen_direct.stan'
## results
path_sim_dat_mate = 'ANALYSIS/RESULTS/03 similarity analysis/sim_dat_mate.RData'
path_real_dat_mate = 'ANALYSIS/RESULTS/03 similarity analysis/real_dat_mate.RData'
path_real_dat_for = 'ANALYSIS/RESULTS/03 similarity analysis/real_dat_for.RData'
path_real_model_results_for = 'ANALYSIS/RESULTS/03 similarity analysis/real_model_results_for.RData'
path_pdf_similarity_all_data = 'ANALYSIS/RESULTS/03 similarity analysis/all data.pdf'
path_real_dat_sim = 'ANALYSIS/RESULTS/03 similarity analysis/real_dat_sim.RData'
path_dat_similarity = 'ANALYSIS/RESULTS/03 similarity analysis/dat.RData'
path_dat_similarity_trruup = 'ANALYSIS/RESULTS/03 similarity analysis/dat_trruup.RData'
path_sim_models = 'ANALYSIS/RESULTS/03 similarity analysis/sim_models.RData'
path_sim_models_trruup = 'ANALYSIS/RESULTS/03 similarity analysis/sim_models_trruup.RData'
path_pdf_similarity_models_20 = 'ANALYSIS/RESULTS/03 similarity analysis/all results - similarity - 2020.pdf'
path_pdf_similarity_models_21 = 'ANALYSIS/RESULTS/03 similarity analysis/all results - similarity - 2021.pdf'
path_pdf_similarity_models_trruup_20 = 
  'ANALYSIS/RESULTS/03 similarity analysis/all results - trruup - similarity - 2020.pdf'
path_pdf_similarity_models_trruup_21 = 
  'ANALYSIS/RESULTS/03 similarity analysis/all results - trruup - similarity - 2021.pdf'
path_mrqap = 'ANALYSIS/RESULTS/03 similarity analysis/mrqap.txt'
path_pdf_similarity_main_figure = 'ANALYSIS/RESULTS/03 similarity analysis/similarity main figure.pdf'
path_pdf_similarity_main_figure_reduced = 
  'ANALYSIS/RESULTS/03 similarity analysis/similarity main figure - reduced.pdf'

# 04 diversity analysis ----
## code
path_simple_degree_model = 'ANALYSIS/CODE/04 diversity analysis/m_simple_degree.stan'
path_simple_age_model = 'ANALYSIS/CODE/04 diversity analysis/m_simple_age.stan'
path_age_un_model = 'ANALYSIS/CODE/04 diversity analysis/m_age_un.stan'
path_simple_sex_model = 'ANALYSIS/CODE/04 diversity analysis/m_simple_sex.stan'
path_simple_total_model = 'ANALYSIS/CODE/04 diversity analysis/m_simple_total.stan'
path_total_model_un = 'ANALYSIS/CODE/04 diversity analysis/m_total_un.stan'
path_group_un_model = 'ANALYSIS/CODE/04 diversity analysis/m_group_un.stan'
path_m_direct_effect_age = 'ANALYSIS/CODE/04 diversity analysis/m_direct_effect_age.stan'
path_m_total_effect_entry = 'ANALYSIS/CODE/04 diversity analysis/m_total_effect_entry.stan'
path_m_direct_effect_entry = 'ANALYSIS/CODE/04 diversity analysis/m_direct_effect_entry.stan'
path_m_total_effect_tree_type = 'ANALYSIS/CODE/04 diversity analysis/m_total_tree_type.stan'
path_m_direct_effect_tree_type = 'ANALYSIS/CODE/04 diversity analysis/m_direct_tree_type.stan'
path_m_degree = 'ANALYSIS/CODE/04 diversity analysis/m_degree.stan'
path_m_netpos = 'ANALYSIS/CODE/04 diversity analysis/m_div_network_position.stan'
path_m_div_total_sex = 'ANALYSIS/CODE/04 diversity analysis/m_total_sex.stan'
path_sex_age_nest_group_degree_model = 'ANALYSIS/CODE/04 diversity analysis/m_sex_age_nest_group_degree.stan'
path_sex_age_nest_group_degree_imp_model = 
  'ANALYSIS/CODE/04 diversity analysis/m_sex_age_nest_group_degree_imp.stan'
path_degree_imp_model = 'ANALYSIS/CODE/04 diversity analysis/m_degree_imp.stan'
## results
path_pdf_data_overview_20 = 'ANALYSIS/RESULTS/04 diversity analysis/data oveview - 2020.pdf'
path_pdf_data_overview_21 = 'ANALYSIS/RESULTS/04 diversity analysis/data oveview - 2021.pdf'
path_pdf_sample_sizes = 'ANALYSIS/RESULTS/04 diversity analysis/sample sizes.pdf'
path_master_dat = 'ANALYSIS/RESULTS/04 diversity analysis/master_dat.RData'
path_pdf_bayes_degree_div = 'ANALYSIS/RESULTS/04 diversity analysis/bayesian degree results.pdf'
path_pdf_sex_div = 'ANALYSIS/RESULTS/04 diversity analysis/sex results.pdf'
path_pdf_age_div = 'ANALYSIS/RESULTS/04 diversity analysis/age results.pdf'
path_pdf_age_un_div = 'ANALYSIS/RESULTS/04 diversity analysis/age results - uncertainty.pdf'
path_pdf_betweenness_div = 'ANALYSIS/RESULTS/04 diversity analysis/betweenness results.pdf'
path_pdf_degree_div = 'ANALYSIS/RESULTS/04 diversity analysis/degree results.pdf'
path_pdf_eigen_div = 'ANALYSIS/RESULTS/04 diversity analysis/eigenvector results.pdf'
path_pdf_entry_div = 'ANALYSIS/RESULTS/04 diversity analysis/entry results.pdf'
path_pdf_nest_div = 'ANALYSIS/RESULTS/04 diversity analysis/nest results.pdf'
path_pdf_tree_div = 'ANALYSIS/RESULTS/04 diversity analysis/tree results.pdf'
path_pdf_group_div = 'ANALYSIS/RESULTS/04 diversity analysis/group results.pdf'
path_pdf_group_div_un = 'ANALYSIS/RESULTS/04 diversity analysis/group results - uncertainty.pdf'
path_pdf_com_div = 'ANALYSIS/RESULTS/04 diversity analysis/community results.pdf'
path_pdf_nest_div_un = 'ANALYSIS/RESULTS/04 diversity analysis/nest results - uncertainty.pdf'
path_pdf_sex_age_nest_group_degree = 
  'ANALYSIS/RESULTS/04 diversity analysis/sex, age, nest, group, degree - uncertainty.pdf'
path_pdf_sex_age_nest_group_degree_imp = 
  'ANALYSIS/RESULTS/04 diversity analysis/sex, age, nest, group, degree - uncertainty + imputation.pdf'
path_pdf_div_on_network = 'ANALYSIS/RESULTS/04 diversity analysis/diversity on network.pdf'
path_all_results_div = 'ANALYSIS/RESULTS/04 diversity analysis/all_results.RData'
path_pdf_all_results_div_20 = 'ANALYSIS/RESULTS/04 diversity analysis/all results - diversity - 2020.pdf'
path_pdf_all_results_div_21 = 'ANALYSIS/RESULTS/04 diversity analysis/all results - diversity - 2021.pdf'
path_pdf_diversity_main_figure = 'ANALYSIS/RESULTS/04 diversity analysis/diversity main figure.pdf'

# 05 repertoire analysis ----
## code
path_m_full_tree_size_effort = 'ANALYSIS/CODE/05 repertoire analysis/m_full_tree_size_effort.stan'
path_m_full_tree_size_effort_no_type = 
  'ANALYSIS/CODE/05 repertoire analysis/m_full_tree_size_effort_no_type.stan'
path_m_full_group_size = 'ANALYSIS/CODE/05 repertoire analysis/m_full_group_size.stan'
path_m_direct_age_sex = 'ANALYSIS/CODE/05 repertoire analysis/m_direct_age_sex.stan'
path_m_total_age = 'ANALYSIS/CODE/05 repertoire analysis/m_total_age.stan'
path_m_total_sex = 'ANALYSIS/CODE/05 repertoire analysis/m_total_sex.stan'
path_m_total_tree_size = 'ANALYSIS/CODE/05 repertoire analysis/m_total_tree_size.stan'
path_simple_total_model_rep = 'ANALYSIS/CODE/05 repertoire analysis/m_simple_total.stan'
path_m_full_tree_size = 'ANALYSIS/CODE/05 repertoire analysis/m_full_tree_size.stan'
## results
path_pdf_sample_effort_20 = 'ANALYSIS/RESULTS/05 repertoire analysis/sample effort - 2020.pdf'
path_pdf_sample_effort_21 = 'ANALYSIS/RESULTS/05 repertoire analysis/sample effort - 2021.pdf'
path_master_dat_rep_size = 'ANALYSIS/RESULTS/05 repertoire analysis/master_dat.RData'
path_master_dat_rep_dists = 'ANALYSIS/RESULTS/05 repertoire analysis/master_dat_rep_dists.RData'
path_pdf_data_overview_rep_size_20 = 'ANALYSIS/RESULTS/05 repertoire analysis/data overview - 2020.pdf'
path_pdf_data_overview_rep_size_21 = 'ANALYSIS/RESULTS/05 repertoire analysis/data overview - 2021.pdf'
path_pdf_betweenness_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/betweenness results.pdf'
path_pdf_degree_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/degree results.pdf'
path_pdf_eigen_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/eigenvector results.pdf'
path_pdf_entry_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/entry results.pdf'
path_pdf_nest_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/nest results.pdf'
path_pdf_tree_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/tree results.pdf'
path_pdf_group_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/group results.pdf'
path_pdf_type_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/tree type results.pdf'
path_pdf_full_tree_size_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/full tree size results.pdf'
path_pdf_full_tree_size_effort_rep = 
  'ANALYSIS/RESULTS/05 repertoire analysis/full tree size results - effort.pdf'
path_pdf_full_tree_size_effort_rep_no_type = 
  'ANALYSIS/RESULTS/05 repertoire analysis/full tree size results - effort - no type.pdf'
path_pdf_full_group_size_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/full group size results.pdf'
path_pdf_direct_age_sex = 'ANALYSIS/RESULTS/05 repertoire analysis/direct effect age and sex.pdf'
path_pdf_total_age = 'ANALYSIS/RESULTS/05 repertoire analysis/total effect age.pdf'
path_pdf_total_sex = 'ANALYSIS/RESULTS/05 repertoire analysis/total effect sex.pdf'
path_pdf_total_tree_size = 'ANALYSIS/RESULTS/05 repertoire analysis/total effect tree size.pdf'
path_all_results_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/all_results.RData'
path_pdf_all_results_rep = 'ANALYSIS/RESULTS/05 repertoire analysis/all results - repertoire.pdf'
path_all_results_rep_ent = 'ANALYSIS/RESULTS/05 repertoire analysis/all_results_entropy.RData'
path_pdf_relative_frequencies_20 = 'ANALYSIS/RESULTS/05 repertoire analysis/relative frequencies - 2020.pdf'
path_pdf_relative_frequencies_21 = 'ANALYSIS/RESULTS/05 repertoire analysis/relative frequencies - 2021.pdf'
path_pdf_all_results_rep_ent_20 = 
  'ANALYSIS/RESULTS/05 repertoire analysis/all results - repertoire entropy - 2020.pdf'
path_pdf_all_results_rep_ent_21 = 
  'ANALYSIS/RESULTS/06 repertoire analysis/all results - repertoire entropy - 2021.pdf'
path_pdf_repertoire_main_figure = 'ANALYSIS/RESULTS/06 repertoire analysis/repertoire main figure.pdf'
path_pdf_repertoire_main_figure_reduced = 
  'ANALYSIS/RESULTS/06 repertoire analysis/repertoire main figure - reduced.pdf'
  'ANALYSIS/RESULTS/05 repertoire analysis/all results - repertoire entropy - 2021.pdf'
path_pdf_repertoire_main_figure = 'ANALYSIS/RESULTS/05 repertoire analysis/repertoire main figure.pdf'

## results
path_master_dat_info = 'ANALYSIS/RESULTS/06 information content/master_dat.RData'
path_all_results_info = 'ANALYSIS/RESULTS/06 information content/all_results.RData'
path_pdf_all_results_info_20 = 
  'ANALYSIS/RESULTS/06 information content/all results - information content - 2020.pdf'
path_pdf_all_results_info_21 = 
  'ANALYSIS/RESULTS/06 information content/all results - information content - 2021.pdf'

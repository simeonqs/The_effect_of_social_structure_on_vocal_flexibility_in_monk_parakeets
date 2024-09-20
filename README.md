# Voice paper

The R code and data needed to replicate results from the article:

```
Complexity paper
```

------------------------------------------------

**Abstract**

The social complexity hypothesis argues that communicative complexity arises as a result of social complexity, with this occurring through several potential mechanisms including plasticity and selection. Most research to date has focused on ultimate drivers of repertoire size, for example finding that cooperative breeding species exhibit larger repertoires. Until this date no study has focused on individual-level drivers of vocal diversity. Here, we examine social networks and vocalisations in wild colonial-nesting monk parakeets (*Myiopsitta monachus*). First, we recorded foraging, tolerance and aggression networks for 337 individuals over two years, as well as co-nesting and relatedness, and matched these with 5599 vocalisations recorded for 229 individuals over this same period. Overall, we found that all individuals exhibited high contact-call diversity; however, individual-level diversity increased with age and with number of nest-mates. Call similarity was not predicted by relatedness, but individuals with stronger affiliative bonds had more dissimilar calls, suggesting an active process to sound unique amongst close associates. Finally, as predicted by the social complexity hypothesis, individuals living in larger groups had more diverse repertoires. Altogether, our results demonstrate a multi-faceted social influence on call content, diversity and repertoire size, exhibiting how fine-scale variation in social structure can influence expressed vocal complexity.

------------------------------------------------

**Requirements:**

R version 4.2.0 or later. 

Required packages are installed and loaded in each script. However, some need manual installation:

* To run the Stan models *Stan* needs to be installed. This is not an R package, but *Stan* can be run from R. For installation see: https://mc-stan.org/users/interfaces/. 

* To run *Stan* from R *cmdstanr* is required. It can be installed from CRAN, but you need to finish the set-up, see: https://mc-stan.org/cmdstanr/. 

------------------------------------------------

**Reproducing all results:**

To reproduce all results run all folders in the `CODE` folder and the scripts inside in order. Large files have to be downloaded from **ZENODO** since they cannot be store on GitHub. Most paths will be sourced from the `paths.R` script. These do not need to be updated as long as scripts are run with the project folder as working directory. The working directory is set correctly if you open the project file and open script from within the project. 

------------------------------------------------

**The folders contain:**

ANALYSIS:
  - CODE: the code to replicate results
  - DATA: raw data
  - RESULTS: results that cannot be reproduced or take very long to reproduce, all other results are not included, but can be reproduced by the scripts in the CODE folder

------------------------------------------------

**File information and meta data:**

Below are all files in the repository. The first bullet point under the path is a short explanation of the file. Other bullet points are meta data for the columns if relevant.

- `README.md` overview of repo and all files
- `.gitignore` which files not to sync to GitHub
- `project.Rproj` R Studio Project file; if you open the code from this file all paths are relative to the main folder
	
- `ANALYSIS/CODE/create_maps.R` script to create the maps with nesting locations, this is one of the few scripts where paths have to be updates within the script because it generates some temporary files that have to be manually deleted
- `ANALYSIS/CODE/DAGs.R` script to create the DAGs, generates `combined DAG.pdf`
- `ANALYSIS/CODE/paths.R` script that can be sourced to store most paths to the global environment
- `ANALYSIS/CODE/00 genetic analysis/01 calculate relatedness.R` script to analyse the data from the microsatellites, note that raw data is not shared, since it includes unpublished data on other individuals, therefore this script cannot be run and is only included for transparencies sake, stores `wang_out.RData` and `trioml_out.RData` as output, 
- `ANALYSIS/CODE/01 audio analysis/00_prepare_audio.R` script to load audio data for further steps, stores `all_data.RData` and `waves.RData`
- `ANALYSIS/CODE/01 audio analysis/01_run_dtw.R` script to run dynamic time warping, note this is a time consuming step, stores `dtw_m.RData`
- `ANALYSIS/CODE/01 audio analysis/02_check_dtw.R` script to visually check the dtw output, plots to any open plotting window
- `ANALYSIS/CODE/01 audio analysis/03_run_diversity_model.R` script to run the model summarising the contact call diversity per individual, stores `model_result_div.RData` and `div_results_slim.RData`
- `ANALYSIS/CODE/01 audio analysis/04_check_diversity_model.R` script to run model checks, plots output to `test traces diversity.pdf`
- `ANALYSIS/CODE/01 audio analysis/05_run_similarity_model.R` script to run the model summarising the contact call similarity between individuals, stores `similarity_out.RData` and `similarity_post.RData`
- `ANALYSIS/CODE/01 audio analysis/06_check_similarity_model.R` script to run model checks plots output to `test similarity output.pdf`
- `ANALYSIS/CODE/01 audio analysis/07_report_sigmas.R` script to print sigma values to the console
- `ANALYSIS/CODE/01 audio analysis/m_div.stan` model definition for the diversity model
- `ANALYSIS/CODE/01 audio analysis/m_sim.stan` model definition for the similarity model
- `ANALYSIS/CODE/02 social analysis/00_prepare_mate_network.R` script to prepare the mate network, stores `mate_network.RData`
- `ANALYSIS/CODE/02 social analysis/01_load_age_data.R` script to load the age data, note that raw data is not shared, since it includes unpublished data on other individuals, therefore this script cannot be run and is only included for transparencies sake, stores `age_dat.RData`
- `ANALYSIS/CODE/02 social analysis/02_prepare_foraging_network.R` script to generate group by individual matrices from the foraging group scans, stores `gbi_20.RData`, `gbi_21.RData` and `samp_size_gbi.RData`
- `ANALYSIS/CODE/02 social analysis/03_run_classic_sna.R` script to run classical social network analysis on the GBIs from the previous step, stores a data frame with multiple network measures in `net_meas.RData`
- `ANALYSIS/CODE/02 social analysis/04_calc_nest_size.R` script to calculate the number of individuals per tree, nest and nest chamber, stores `nesting_sizes.RData`
- `ANALYSIS/CODE/02 social analysis/05_spatial_network.R` script to generate the spatial network based on nesting location, stores `spatial_network.RData`
- `ANALYSIS/CODE/02 social analysis/06_aggressive_network.R` script to generate the aggressive network, stores `aggressive network.pdf`
- `ANALYSIS/CODE/02 social analysis/07_tolerance_network.R` script to generate the tolerance network, stores `tolerance_network.RData`
- `ANALYSIS/CODE/02 social analysis/08_multiplex.R` script to generate the multiplex network, stores `multi_out.RData`
- `ANALYSIS/CODE/03 similarity analysis/00_prep_all_data.R` script to prepare all data for the models explaining contact call similarity, stores `03 similarity analysis/dat.RData`
- `ANALYSIS/CODE/03 similarity analysis/01_run_models.R` script to run all models explaining contact calls similarity, all model output is stored in `sim_models.RData`
- `ANALYSIS/CODE/03 similarity analysis/02_final_figure.R` script to produce the final figure and other figures for the supplemental materials, stores `all results - similarity - 2020.pdf`, `all results - similarity - 2021.pdf`, `similarity main figure.pdf` and `similarity main figure - reduced.pdf`
- `ANALYSIS/CODE/03 similarity analysis/m_aggressive.stan` model definition for the total and direct effect of the aggressive network on contact call similarity
- `ANALYSIS/CODE/03 similarity analysis/m_clust_total.stan` model definition for the total effect of the whether or not individuals came from the same nesting cluster on contact call similarity
- `ANALYSIS/CODE/03 similarity analysis/m_foraging_direct.stan` model definition for the direct effect of the foraging network on contact call similarity
- `ANALYSIS/CODE/03 similarity analysis/m_foraging_total.stan` model definition for the total effect of the foraging network on contact call similarity
- `ANALYSIS/CODE/03 similarity analysis/m_gen_direct.stan` model definition for the direct effect of the relatedness (genetic) network on contact call similarity
- `ANALYSIS/CODE/03 similarity analysis/m_gen_total.stan` model definition for the total effect of the relatedness (genetic) network on contact call similarity
- `ANALYSIS/CODE/03 similarity analysis/m_mate_total.stan` model definition for the total effect of the mate network on contact call similarity
- `ANALYSIS/CODE/03 similarity analysis/m_spat_total.stan` model definition for the total effect of the spatial network on contact call similarity
- `ANALYSIS/CODE/04 diversity analysis/00_prep_all_data.R` script to load all data for the models explaining contact call diversity, stores `04 diversity analysis/master_dat.RData`, note this cannot be reproduced because the data from `data Francisca.csv` is published under *Dawson Pell (2021)* and cannot be shared here
- `ANALYSIS/CODE/04 diversity analysis/01_clean_model.R` script to run all the models explaining contact call diversity, all model output is stored in `04 diversity analysis/all_results.RData`
- `ANALYSIS/CODE/04 diversity analysis/02_final_figure.R` script to plot the figures with the results of the models explainging contact call diversity, stores `all results - diversity - 2020.pdf`, `all results - diversity - 2021.pdf` and `diversity main figure.pdf`
- `ANALYSIS/CODE/04 diversity analysis/03_model_checks.R` script to run model checks on the models explaining contact call diversity
- `ANALYSIS/CODE/04 diversity analysis/m_direct_effect_age.stan` model definition for the direct effect of the age on contact call diversity
- `ANALYSIS/CODE/04 diversity analysis/m_direct_effect_entry.stan` model definition for the direct effect of the chamber (entry) size on contact call diversity
- `ANALYSIS/CODE/04 diversity analysis/m_direct_tree_type.stan` model definition for the direct effect of the tree type on contact call diversity
- `ANALYSIS/CODE/04 diversity analysis/m_dir_network_position.stan` model definition for the direct effect of the network position on contact call diversity, is used for multiple measures of network position (e.g., degree)
- `ANALYSIS/CODE/04 diversity analysis/m_total_effect_entry.stan` model definition for the total effect of the chamber (entry) size on contact call diversity
- `ANALYSIS/CODE/04 diversity analysis/m_total_sex.stan` model definition for the total effect of sex on contact call diversity
- `ANALYSIS/CODE/04 diversity analysis/m_total_tree_type.stan` model definition for the total effect of tree type on contact call diversity
- `ANALYSIS/CODE/04 diversity analysis/model definitions.Rmd` R markdown file with mathematical model definitions for the manuscript
- `ANALYSIS/CODE/05 repertoire analysis/00_prep_data.R` script to prepare all data for the models explaining repertoire entropy, note script also prepares unused variables such as number of unique items in the repertoire, stores `master_dat.RData`, note this cannot be reproduced because the data from `data Francisca.csv` is published under *Dawson Pell (2021)* and cannot be shared here
- `ANALYSIS/CODE/05 repertoire analysis/01_models_entropy.R` script to run the models explaining repertoire entropy, stores all model output in `all_results_entropy.RData`
- `ANALYSIS/CODE/05 repertoire analysis/02_figure_entropy.R` script to plot the figure with entropy results, stores `all results - repertoire entropy - 2020.pdf`, `all results - repertoire entropy - 2021.pdf`, `repertoire main figure.pdf` and `repertoire main figure - reduced.pdf`
- `ANALYSIS/CODE/05 repertoire analysis/m_rep_ent_age_direct.stan` model definition for the direct effect of age on repertoire entropy
- `ANALYSIS/CODE/05 repertoire analysis/m_rep_ent_age_total.stan`  model definition for the total effect of age on repertoire entropy
- `ANALYSIS/CODE/05 repertoire analysis/m_rep_ent_entry_total.stan`  model definition for the total effect of chamber (entry) size on repertoire entropy
- `ANALYSIS/CODE/05 repertoire analysis/m_rep_ent_sex_total.stan`  model definition for the total effect of sex on repertoire entropy
- `ANALYSIS/CODE/05 repertoire analysis/distribution analysis/00_prep_data.R` script to prepare all data for the distribution models, stores `master_dat_rep_dists.RData`
- `ANALYSIS/CODE/05 repertoire analysis/distribution analysis/01_run_models_complete.R` script to run the distribution models, stores all model output in `model_results.RData` and plots multiple PDF files with results, note that paths are sometimes included in this script directly
- `ANALYSIS/CODE/05 repertoire analysis/distribution analysis/02_plot_final_figure.R` script to plot the distribution model output, note paths are included in the script
- `ANALYSIS/CODE/05 repertoire analysis/distribution analysis/m_age_direct.stan` model definition for the direct effect of age on distribution of call types
- `ANALYSIS/CODE/05 repertoire analysis/distribution analysis/m_age_total.stan` model definition for the total effect of age on distribution of call types
- `ANALYSIS/CODE/05 repertoire analysis/distribution analysis/m_entry_total.stan` model definition for the total effect of chamber (entry) size on distribution of call types
- `ANALYSIS/CODE/05 repertoire analysis/distribution analysis/model definitions.Rmd` markdown file with mathematical model definitions for the manuscript
- `ANALYSIS/CODE/06 information content/01_calc_cont.R` script to calculate the information content for the contact calls, stores results in `06 information content/master_dat.RData`, note this cannot be reproduced because the data from `data Francisca.csv` is published under *Dawson Pell (2021)* and cannot be shared here
- `ANALYSIS/CODE/06 information content/02_run_models.R` script to run all models of the information content, stores all model output in `06 information content/all_results.RData`
- `ANALYSIS/CODE/06 information content/03_final_figure.R` script to plot the results of the information content models, stores `all results - information content - 2020.pdf` and `all results - information content - 2021.pdf`
- `ANALYSIS/CODE/06 information content/m_info_age_total.stan` model definition for the total effect of age on the information content of contact calls
- `ANALYSIS/CODE/06 information content/m_info_degree.stan` model definition for the direct and total effect of age on the information content of contact calls
- `ANALYSIS/CODE/06 information content/m_info_tree_total.stan` model definition for the total effect of tree on the information content of contact calls
- `ANALYSIS/CODE/06 information content/model structures.Rmd` markdown file with mathematical model definitions for the manuscript
- `ANALYSIS/CODE/function` folder with all functions needed to replicate the results

- `ANALYSIS/DATA/luscinia/all_2021.csv`the fundamental frequency traces made in Luscinia for all calls in 2021
	- Individual: not used
	- Song: file-selection.wav, where file is the original wav file name and selection is the selection from the Raven selection table
	- Syllable: not used
	- Phrase: not used
	- Element: identifier for the element, some calls contained fundamental frequencies with short silent periods, this leads to multiple traces/elements
	- Time: time within the clip in milli seconds
	- Fundamental_frequency: the recorded fundamental frequency in Herz
- `ANALYSIS/DATA/luscinia/contact_2020.csv` the fundamental frequency traces made in Luscinia for most contact calls in 2020
	- Individual: not used
	- Song: file-selection.wav, where file is the original wav file name and selection is the selection from the Raven selection table
	- Syllable: not used
	- Phrase: not used
	- Element: identifier for the element, some calls contained fundamental frequencies with short silent periods, this leads to multiple traces/elements
	- Time: time within the clip in milli seconds
	- Fundamental_frequency: the recorded fundamental frequency in Herz
- `ANALYSIS/DATA/luscinia/remaining_2020.csv` the fundamental frequency traces made in Luscinia for the remaining calls in 2020
	- Individual: not used
	- Song: file-selection.wav, where file is the original wav file name and selection is the selection from the Raven selection table
	- Syllable: not used
	- Phrase: not used
	- Element: identifier for the element, some calls contained fundamental frequencies with short silent periods, this leads to multiple traces/elements
	- Time: time within the clip in milli seconds
	- Fundamental_frequency: the recorded fundamental frequency in Herz
- `ANALYSIS/DATA/luscinia/bad_files_2020.xlsx` an overview over which files from 2020 to exclude from the Luscinia traces due to poor quality
	- file_end: the end of the Song column in the Lucinia trace files, used to match and exclude
	- total_useless: if 1 the call was very poor quality, if none the fundamental frequency was poor quality but the 2-4 kHz range was still good
- `ANALYSIS/DATA/luscinia/bad_files_2021.xlsx` an overview over which files from 2021 to exclude from the Luscinia traces due to poor quality
	- file_end: the end of the Song column in the Lucinia trace files, used to match and exclude
	- total_useless: if 1 the call was very poor quality, if none the fundamental frequency was poor quality but the 2-4 kHz range was still good
- `ANALYSIS/DATA/microsat/230331 Microsattelites Results.xlsx` microsattelite results from Vetgenomics, note this is a xlsx file with incorrect headers, this is handled in the script loading the genetic data
  - Vetgenomics ID: the id given by Vetgenomics (not used)
  - ID: the name of the individual as used in all other data
  - subsequent columns: the names of the microsattelites, each column contains the specific value for one microsattelite for each individual
- `ANALYSIS/DATA/nesting/nest entry overview - 2020 - manual added.csv` csv file with the overview of which individuals nest in each chamber (entry) for 2020
 	- tree: the tree id (numeric)
  - nest: the nest id within the tree (alphabetic)
  - entry: the entry id (numeric)
  - birds: ids of birds assigned to that nest, comma separated
  - birds_observed_in_nest: ids of birds observed in the nest (multiple entries per individual possible), comma separated
  - birds_observed_at_nest: ids of birds observed at the nest (multiple entries per individual possible), comma separated
- `ANALYSIS/DATA/nesting/nest entry overview - 2021 - manual added.csv` csv file with the overview of which individuals nest in each chamber (entry) for 2021
 	- tree: the tree id (numeric)
  - nest: the nest id within the tree (alphabetic)
  - entry: the entry id (numeric)
  - birds: ids of birds assigned to that nest, comma separated
- `ANALYSIS/DATA/nesting/nest overview 2020.csv` an overview of all nests recorded in 2020
 	- tree: the tree id (numeric)
  - nest: the nest id within the tree (alphabetic)
  - n_entries: the number of nest entries within that nest
  - cluster: the location in the park (named based on clustering of nests)
- `ANALYSIS/DATA/nesting/nest overview 2021.csv` an overview of all nests recorded in 2021
 	- tree: the tree id (numeric)
  - nest: the nest id within the tree (alphabetic)
  - n_entries: the number of nest entries within that nest
  - type: the type of tree
- `ANALYSIS/DATA/nesting/nest per ID 2020 - manual added.csv` csv file with overview of where all individuals from 2020 nested
  - id: ids of birds
  - tree_manual: manually assigned tree id
  - nest_manual: manually assigned full nest id, format "TreeID NestID EntryID"
  - area: emtpy
  - final_nest: not used 
  - nest_locations: all nesting locations recorded for the individual, comma separated
- `ANALYSIS/DATA/nesting/nest per ID 2021 - manual added.csv` csv file with overview of where all individuals from 2021 nested
  - id: ids of birds
  - tree_manual: manually assigned tree id
  - nest_manual: manually assigned full nest id, format "TreeID NestID EntryID"
  - area: emtpy: which part of the study area the nest is located
  - nest: all nesting locations recorded for the individual, format "TreeID NestID EntryID", comma separated 
  - nest_locations: all tree locations recorded for the individual, comma separated
- `ANALYSIS/DATA/nesting/nest recordings Andres 2020.csv` csv file with recordings of nesting activity from Andrés in 2020
  - observer: name of the observer (Andrés)
  - recording_file: not used
  - date: date of the survey, format dd_mm_yy
  - time_start: time of the start of the survey, format hh_mm 	
  - tree: tree id (numeric)
  - nest: nest id (alphabetical)
  - entry: entry id (numeric)
  - bird_id: id of the observed birds, X is used if part of the tag couldn't be read, unknown or un is used for untagged individuals, collar is used for individuals that lost the tag, but not the collar, 
  - at_nest: 1 if the bird was observed at the nest, missing if not
  - in_nest: 1 if the bird was observed in the nest, missing if not
  - building: 1 if the bird was observed building on the nest (does not include steeling of sticks), missing if not
  - note: other notes
- `ANALYSIS/DATA/nesting/nest recordings Andres 2021.xlsx` excel file with recordings of nesting activity from Andrés in 2021
  - observer: name of the observer (Andrés)
  - recording_file: not used
  - date: date of the survey, format dd_mm_yy
  - time_start: time of the start of the survey, format hh_mm 	
  - tree: tree id (numeric)
  - nest: nest id (alphabetical)
  - entry: entry id (numeric)
  - bird_id: id of the observed birds, X is used if part of the tag couldn't be read, unknown or un is used for untagged individuals, collar is used for individuals that lost the tag, but not the collar 
  - at_nest: 1 if the bird was observed at the nest, missing if not
  - in_nest: 1 if the bird was observed in the nest, missing if not
  - building: 1 if the bird was observed building on the nest (does not include steeling of sticks), missing if not
  - note: other notes
- `ANALYSIS/DATA/nesting/nest recordings Simeon 2020.csv` csv file with recordings of nesting activity from Simeon in 2020
  - observer: name of the observer (Simeon)
  - recording_file: name of the wav file in which recordings were made verbally, includes date and time information in the format yyyy_mm_yy_hhmmss, missing if recordings were made without microphone
  - tree: tree id (numeric)
  - nest: nest id (alphabetical)
  - entry: entry id (numeric)
  - bird_id: id of the observed birds, X is used if part of the tag couldn't be read, unknown or un is used for untagged individuals, stump is used for individuals that lost the tag, but not the collar, some unmarked are described based on visual characteristics (e.g., missing_right_eye), multiple individuals are separated by commas
  - at_nest: 1 if the bird was observed at the nest, missing if not
  - in_nest: 1 if the bird was observed in the nest, missing if not
  - building: 1 if the bird was observed building on the nest (does not include steeling of sticks), missing if not
  - note: other notes
  - date: date of the survey, format dd_mm_yy, missing if recording file is present
  - time_start: time of the start of the survey, format hh_mm, missing if recording file is present
- `ANALYSIS/DATA/nesting/nest recordings Simeon 2021.xlsx` excel file with recordings of nesting activity from Simeon in 2021
  - observer: name of the observer (Simeon)
  - recording_file: name of the wav file in which recordings were made verbally, includes date and time information in the format yyyy_mm_yy_hhmmss, missing if recordings were made without microphone
  - tree: tree id (numeric)
  - nest: nest id (alphabetical)
  - entry: entry id (numeric)
  - bird_id: id of the observed birds, X is used if part of the tag couldn't be read, unknown or un is used for untagged individuals, stump is used for individuals that lost the tag, but not the collar, some unmarked are described based on visual characteristics (e.g., missing_right_eye), multiple individuals are separated by commas
  - at_nest: 1 if the bird was observed at the nest, missing if not
  - in_nest: 1 if the bird was observed in the nest, missing if not
  - building: 1 if the bird was observed building on the nest (does not include steeling of sticks), missing if not
  - note: other notes
  - date: date of the survey, format dd_mm_yy, missing if recording file is present
  - time_start: time of the start of the survey, format hh_mm, missing if recording file is present
- `ANALYSIS/DATA/overview recordings/annotations - 2020.csv` annotations linked to the selection tables from Raven for 2020, note that these follow a different format compared to the 2021 data
	- annotation_ref: annotation reference in the selection table (from the Annotation column in the selection table files) 
	- uncertain: if uncertain about the ID = 1
	- bird: the ID on the collar of the individual
	- behaviour: which behaviours at vocalisation or just before, can be multiple separated by a comma
	- location: location of the bird during vocalisation
	- other: which other individuals were around, 'none' of the bird was alone if unknown nothing was noted down
	- association: the behaviour of the other birds towards the focal bird
	- notes: other observations
- `ANALYSIS/DATA/overview recordings/annotations - 2021.xlsx` annotations linked to the selection tables from Raven for 2021, note that these follow a different format compared to the 2020 data
	- file: audio file name with extension
	- selection: the selection number in the selection table
	- bird: the ID on the collar of the individual
	- location: where the focal bird was recorded
	- context: the context of the call, if it was recorded as single vocalisation, few notes or part of a larger sequence
	- call type: what call type the vocalisation belonged to
	- larger sequence: the full sequence that this call is part of (if available)
	- complete sequence: 1 if all calls in the sequence were included in this file
	- sequence_ID: unique integer per sequence
	- uncertain: 1 if uncertain about any of the information
	- others: which other individuals were present
	- association: the behaviour of the other birds towards the focal bird
	- notes: other observations
- `ANALYSIS/DATA/overview recordings/context - 2020.xlsx` context for each call of 2020 (for 2021 this is part of the annotations)
	- file: the full file name of the associated Raven selection table
	- selection: the selection number of the Raven selection table; 1t4 means selection 1 until and including 4
	- context: the context of the call, if it was recorded as single vocalisation, few notes or part of a larger sequence; if uncertain a ? is added
	- call type: what call type the vocalisation belonged to
	- larger sequence: the full sequence that this call is part of (if available)
	- sequence_ID: unique integer per sequence
	- notes: other observations
- `ANALYSIS/DATA/overview recordings/overview recordings - 2020.csv` overview per audio recording for 2020
	- file: audio file name without extension
	- selected: whether or not I'm finished with this file
	- discard: 1 if there are no selections for this file
	- situation_recording: notes on what happens in the recording
	- birds: ID's of vocalising birds, incomplete
	- utm: the UTM coordinates of the recording, separated by a space
	- nest: the nest location if recorded at a nest
	- notes: other observations
- `ANALYSIS/DATA/overview recordings/overview recordings - 2021.xlsx` overview per audio recording for 2021
	- file: audio file name with extension
	- selected: whether or not I'm finished with this file
	- discard: 1 if there are no selections for this file
	- situation_recording: notes on what happens in the recording
	- birds: ID's of vocalising birds, incomplete
	- utm: the UTM coordinates of the recording, separated by a space
	- nest: the nest location if recorded at a nest
	- notes: other observations
- `ANALYSIS/DATA/selection tables` a folder with the Raven selection tables; the file name is the original wav file name without extension with .Table.1.selections.txt added to the end; note that each selection is listed twice (once for the Waveform 1 and once for the Spetrogram 1) and for analysis one has to be removed
	- Selection: the selection number (can be discontinuous if selections were later removed)
	- View: not used
	- Channel: not used
	- Begin Time (s): begin time of selection within the wav file in seconds
	- End Time (s): end time of selection within the wav file in seconds
	- Low Freq (Hz)	: not used
	- High Freq (Hz): not used
	- Annotation: annotation reference for the annotations of 2020 (multiple selections can have the same annotation), for 2021 this was not used and the selection number was used in combination with the file name to match annotations
- `ANALYSIS/DATA/sex/data Francisca.csv` this file is not shared because the data is from *Dawson Pell (2021)*
 	- ID: individual id
  - sex: NA, F or M
- `ANALYSIS/DATA/sex/sexing vetgenomics.xlsx` file with all sexings done by Vetgenomics
  - ID: individual ID
  - sex: f or m
- `ANALYSIS/DATA/social data/social data Andres 2020.csv` a csv file with the social data collected by Andrés Manza in 2020 (**data format is different from the 2021 data**); note that this file contains both group scans (which individuals were present) and interaction data (between two or more individuals), therefore not all columns are filled out for each entry
	- observer: first name of person collecting and entering data
	- recording: if data was collected as verbal annotation, the name of the wav file with the annotations
	- date: the date of the recording, format dd_mm_yy; for recordings that have an audio file name in the 'file' column the date and time are not noted down but are in the file name
	- time_start: the time at which the recording started, format hh_mm
	- tree: the ID of the tree in which the recording took place
	- nest: the ID of the nest in which the recording took place
	- entry: the ID of the nest entry in/at which the recording took place
	- utm: the UTM coordinates separated by a space of the recording; the area code for Barcelona needs to be added; for recordings that have an audio file name in the 'file' column the UTM can be retrieved from the recording overview file
	- what: what the social group was doing
	- food: if foraging, what the broad food category was, can be multiple separated by a comma
	- location: where the group was located, can be the nest tree, nest or nest chamber ID
	- group_size: the approximate size of the group at the beginning of the recording (only for group scans)
	- group: the IDs of the birds present at the beginning of the recording separated by commas (only for group scans); uk = unknown if tagged, un = untagged (can still have a ring), stump = tagged but lost the dog-tag, only collar is left, X = tagged but tag could not be read, x within an ID is used when only a partial ID was available
	- from: the ID of the individual(s) starting the interaction (only for interactions, directionality only relevant for preening, displacement, peck); for abbreviations see 'group';
	- to: the ID of the individual(s) receiving the interaction; for details see 'from'
	- which: which kind of interaction took place; peck = one individual pecking another individual once, displacement = one individual physically and intentionally displacing another with only minor aggression, beak fight = minor aggression where only the beaks are interlocked, fight = aggressive behaviour from both individuals or displacement with repeated physical contact, close = individuals being within one body length and facing each other without aggression, touching = individuals physically touching each other without aggression from either side, preening = one individual taking feathers or the tag of another individual in the beak without aggression from either side
	- winner: the ID of the individual(s) winning the displacement or fight; for displacement the 'from' individual has won if the field is empty; for fights empty fields are unknown winner
	- started: the ID of the individuals(s) that started the interaction if known; for displacements the 'from' bird started the interaction
	- note: any other observation
- `ANALYSIS/DATA/social data/social data Andrés 2021.xlsx` an excel file with the social data collected by Andrés Manza in 2021 (**data format is different from the 2020 data**); note that this file contains both group scans (which individuals were present) and interaction data (between two or more individuals), therefore not all columns are filled out for each entry
	- observer: first name of person collecting and entering data
	- recording: if data was collected as verbal annotation, the name of the wav file with the annotations
	- what: what the social group was doing
	- food: if foraging, what the broad food category was, can be multiple separated by a comma
	- location: where the group was located, can be the nest tree, nest or nest chamber ID
	- group_size: the approximate size of the group at the beginning of the recording (only for group scans)
	- group: the IDs of the birds present at the beginning of the recording separated by commas (only for group scans); uk = unknown if tagged, un = untagged (can still have a ring), stump = tagged but lost the dog-tag, only collar is left, X = tagged but tag could not be read, x within an ID is used when only a partial ID was available
	- from: the ID of the individual(s) starting the interaction (only for interactions, directionality only relevant for preening, displacement, peck); for abbreviations see 'group';
	- to: the ID of the individual(s) receiving the interaction; for details see 'from'
	- which: which kind of interaction took place; peck = one individual pecking another individual once, displacement = one individual physically and intentionally displacing another with only minor aggression, beak fight = minor aggression where only the beaks are interlocked, fight = aggressive behaviour from both individuals or displacement with repeated physical contact, close = individuals being within one body length and facing each other without aggression, touching = individuals physically touching each other without aggression from either side, preening = one individual taking feathers or the tag of another individual in the beak without aggression from either side
	- winner: the ID of the individual(s) winning the displacement or fight; for displacement the 'from' individual has won if the field is empty; for fights empty fields are unknown winner
	- started: the ID of the individuals(s) that started the interaction if known; for displacements the 'from' bird started the interaction
	- note: any other observation
	- date: the date of the recording, format dd_mm_yy; for recordings that have an audio file name in the 'file' column the date and time are not noted down but are in the file name
	- time_start: the time at which the recording started, format hh_mm
	- tree: the ID of the tree in which the recording took place
	- nest: the ID of the nest in which the recording took place
	- entry: the ID of the nest entry in/at which the recording took place
	- utm: the UTM coordinates separated by a space of the recording; the area code for Barcelona needs to be added; for recordings that have an audio file name in the 'file' column the UTM can be retrieved from the recording overview file
- `ANALYSIS/DATA/social data/social data Mireia 2020.csv` csv file with the output of a compilation of Google Questionnaires filled out by Mireia Fuertes in 2020 with social data
  - Tidsstempel: date stamp, format yyyy/mm/dd h:mm:ss AM CET
  - What?: type of recording, either Group = group scan or Interaction = social interaction
  - Group size: count of the individuals present
  - Where?: location of the group 
  - How?: what the group was doing
  - Who? (2 m or whole tree): the individuals presen, separated by commas, uk = unknown if tagged, un = untagged (can still have a ring), stump = tagged but lost the dog-tag, only collar is left, X = tagged but tag could not be read, x within an ID is used when only a partial ID was available
  - From: the ID of the individual(s) starting the interaction (only for interactions, directionality only relevant for preening, displacement, peck); for abbreviations see 'group';
	- To: the ID of the individual(s) receiving the interaction; for details see 'from'
	- Type: which kind of interaction took place; peck = one individual pecking another individual once, displacement = one individual physically and intentionally displacing another with only minor aggression, beak fight = minor aggression where only the beaks are interlocked, fight = aggressive behaviour from both individuals or displacement with repeated physical contact, close = individuals being within one body length and facing each other without aggression, touching = individuals physically touching each other without aggression from either side, preening = one individual taking feathers or the tag of another individual in the beak without aggression from either side
	- Started by: the ID of the individuals(s) that started the interaction if known; for displacements the 'from' bird started the interaction
	- Won by: the ID of the individual(s) winning the displacement or fight; for displacement the 'from' individual has won if the field is empty; for fights empty fields are unknown winner
  - Copy paste waypoint name: the name of the waypoint (stored as GPX) for the location of observation
- `ANALYSIS/DATA/social data/social data Simeon 2020.csv` a csv file with the social data collected by Simeon Q. Smeele in 2020 (**data format is different from the 2021 data**); note that this file contains both group scans (which individuals were present) and interaction data (between two or more individuals), therefore not all columns are filled out for each entry
	- observer: first name of person collecting and entering data
	- recording: if data was collected as verbal annotation, the name of the wav file with the annotations
	- new_flock: if 1 or if it's the first recording for that session, the individuals in the group column are a new group, else the group column should be ignored 
	- what: what the social group was doing
	- food: if foraging, what the broad food category was, can be multiple separated by a comma
	- location: where the group was located, can be the nest tree, nest or nest chamber ID
	- group_size: the approximate size of the group at the beginning of the recording (only for group scans)
	- group: the IDs of the birds present at the beginning of the recording separated by commas (only for group scans); uk = unknown if tagged, un = untagged (can still have a ring), stump = tagged but lost the dog-tag, only collar is left, X = tagged but tag could not be read, x within an ID is used when only a partial ID was available
	- from: the ID of the individual(s) starting the interaction (only for interactions, directionality only relevant for preening, displacement, peck); for abbreviations see 'group';
	- to: the ID of the individual(s) receiving the interaction; for details see 'from'
	- which: which kind of interaction took place; peck = one individual pecking another individual once, displacement = one individual physically and intentionally displacing another with only minor aggression, beak fight = minor aggression where only the beaks are interlocked, fight = aggressive behaviour from both individuals or displacement with repeated physical contact, close = individuals being within one body length and facing each other without aggression, touching = individuals physically touching each other without aggression from either side, preening = one individual taking feathers or the tag of another individual in the beak without aggression from either side
	- winner: the ID of the individual(s) winning the displacement or fight; for displacement the 'from' individual has won if the field is empty; for fights empty fields are unknown winner
	- started: the ID of the individuals(s) that started the interaction if known; for displacements the 'from' bird started the interaction
	- note: any other observation
	- date: the date of the recording, format dd_mm_yy; for recordings that have an audio file name in the 'file' column the date and time are not noted down but are in the file name
	- time_start: the time at which the recording started, format hh_mm
	- tree: the ID of the tree in which the recording took place
	- nest: the ID of the nest in which the recording took place
	- entry: the ID of the nest entry in/at which the recording took place
	- utm: the UTM coordinates separated by a space of the recording; the area code for Barcelona needs to be added; for recordings that have an audio file name in the 'file' column the UTM can be retrieved from the recording overview file
- `ANALYSIS/DATA/social data/social data Simeon 2021.xlsx` an excel file with the social data collected by Simeon Q. Smeele in 2021 (**data format is different from the 2020 data**); note that this file contains both group scans (which individuals were present) and interaction data (between two or more individuals), therefore not all columns are filled out for each entry
	- observer: first name of person collecting and entering data
	- recording: if data was collected as verbal annotation, the name of the wav file with the annotations
	- what: what the social group was doing
	- food: if foraging, what the broad food category was, can be multiple separated by a comma
	- location: where the group was located, can be the nest tree, nest or nest chamber ID
	- group_size: the approximate size of the group at the beginning of the recording (only for group scans)
	- group: the IDs of the birds present at the beginning of the recording separated by commas (only for group scans); uk = unknown if tagged, un = untagged (can still have a ring), stump = tagged but lost the dog-tag, only collar is left, X = tagged but tag could not be read, x within an ID is used when only a partial ID was available
	- from: the ID of the individual(s) starting the interaction (only for interactions, directionality only relevant for preening, displacement, peck); for abbreviations see 'group';
	- to: the ID of the individual(s) receiving the interaction; for details see 'from'
	- which: which kind of interaction took place; peck = one individual pecking another individual once, displacement = one individual physically and intentionally displacing another with only minor aggression, beak fight = minor aggression where only the beaks are interlocked, fight = aggressive behaviour from both individuals or displacement with repeated physical contact, close = individuals being within one body length and facing each other without aggression, touching = individuals physically touching each other without aggression from either side, preening = one individual taking feathers or the tag of another individual in the beak without aggression from either side
	- winner: the ID of the individual(s) winning the displacement or fight; for displacement the 'from' individual has won if the field is empty; for fights empty fields are unknown winner
	- started: the ID of the individuals(s) that started the interaction if known; for displacements the 'from' bird started the interaction
	- note: any other observation
	- date: the date of the recording, format dd_mm_yy; for recordings that have an audio file name in the 'file' column the date and time are not noted down but are in the file name
	- time_start: the time at which the recording started, format hh_mm
	- tree: the ID of the tree in which the recording took place
	- nest: the ID of the nest in which the recording took place
	- entry: the ID of the nest entry in/at which the recording took place
	- utm: the UTM coordinates separated by a space of the recording; the area code for Barcelona needs to be added; for recordings that have an audio file name in the 'file' column the UTM can be retrieved from the recording overview file
- `ANALYSIS/DATA/waypoints/Mireia/23_des.gpx` GPX file with all waypoints from the social recordings made by Mireia Fuertes in 2020, names of waypoint correspond to the data recorded in `social data Mireia 2020.csv`
- `ANALYSIS/DATA/waypoints/nests 2020` folder with GPX files for all nesting trees in 2020, names of the GPX file corresponds to the ID of the nesting tree
- `ANALYSIS/DATA/waypoints/nests 2021` folder with GPX files for all nesting trees in 2021, names of the GPX file corresponds to the ID of the nesting tree

- `ANALYSIS/RESULTS/00 genetic analysis/trioml_out.RData` not used
- `ANALYSIS/RESULTS/00 genetic analysis/wang_out.RData` RData file with the genetic similarity matrix, row and col names correspond to the individual IDs
- `ANALYSIS/RESULTS/01 audio analysis/all_data.RData` RData file with all data resulting from the audio analysis
- `ANALYSIS/RESULTS/01 audio analysis/div_results_slim.RData` RData file with two dataframes including the average and se on diversity per individual
- `ANALYSIS/RESULTS/01 audio analysis/dtw_m.RData` RData file with two distance matrices from dynamic timewarping with the acoustic distance between calls, row and col names are the file-selection for each call
- `ANALYSIS/RESULTS/01 audio analysis/model_result_div.RData` RData file with the full model results for the diversity analysis, note this file is not included on GitHub because of size
- `ANALYSIS/RESULTS/01 audio analysis/similarity_out.RData` RData file with the summarised results of the similarity analysis
- `ANALYSIS/RESULTS/01 audio analysis/similarity_post.RData` RData file with the model output (posterior distributions) of the similarity analysis, note this file is not included on GitHub because of size
- `ANALYSIS/RESULTS/01 audio analysis/spcc_m.RData` RData file with two distance matrices from spectrographic cross correlation with the acoustic distance between calls, row and col names are the file-selection for each call
- `ANALYSIS/RESULTS/01 audio analysis/waves.RData` RData file with wave objects for all calls, note this file is not included on GitHub because of size
- `ANALYSIS/RESULTS/02 social analysis/aggressive_network.RData` RData file with the aggresive networks (binary), row and col names are individual IDs 
- `ANALYSIS/RESULTS/02 social analysis/dat_mate_id.RData` RData file with two dataframes dat contain the individual ID and to which mate ID they belong
- `ANALYSIS/RESULTS/02 social analysis/foraging_network.RData` RData file with the foraging network (edge weights), row and col names are individual IDs 
- `ANALYSIS/RESULTS/02 social analysis/gbi_20.RData` RData file with the group by individual matrix for 2020
- `ANALYSIS/RESULTS/02 social analysis/gbi_21.RData` RData file with the group by individual matrix for 2020
- `ANALYSIS/RESULTS/02 social analysis/mate_network.RData` RData file with the mate networks (binary), row and col names are individual IDs 
- `ANALYSIS/RESULTS/02 social analysis/multi_out.RData` RData file with the degree versatility from the multiplex networks per individual 
- `ANALYSIS/RESULTS/02 social analysis/nesting_size.RData` RData file with the sizes of chamber, nest and tree for each individual
- `ANALYSIS/RESULTS/02 social analysis/net_meas.RData` RData file with the networks measurement for each individual
- `ANALYSIS/RESULTS/02 social analysis/samp_size_gbi.RData` RData file with with the number of recordings in group scans for each individual
- `ANALYSIS/RESULTS/02 social analysis/spatial_network.RData` RData file with the spatial networks (edgeweight is distance in meters), row and col names are individual IDs 
- `ANALYSIS/RESULTS/02 social analysis/tolerance_network.RData` RData file with the tolerance networks (binary), row and col names are individual IDs 
- `ANALYSIS/RESULTS/03 similarity analysis/dat.RData` RData file with dataframes containing all information for the models explaining contact call similarity
- `ANALYSIS/RESULTS/04 diversity analysis/all_results.RData` RData file with all the cleaned data, fits and posterior distributions for the models explaining contact call diversity
- `ANALYSIS/RESULTS/04 diversity analysis/master_dat.RData` RData file with the combined dataframes with all data
- `ANALYSIS/RESULTS/05 repertoire analysis/all_results_entropy.RData` RData file with all the cleaned data, fits and posterior distributions for the models explaining repertoire entropy
- `ANALYSIS/RESULTS/05 repertoire analysis/master_dat.RData` RData file with the combined data for the repertoire analysis
- `ANALYSIS/RESULTS/05 repertoire analysis/distribution analysis/model_results.RData` RData file with the results of the model explaining repertoire distribution, note this file is not included on GitHub because of size
- `ANALYSIS/RESULTS/06 information content/all_results.RData` RData file with all the cleaned data, fits and posterior distributions for the models explaining contact call information content
- `ANALYSIS/RESULTS/06 information content/master_dat.RData` RData file with the combined data for the results for contact call information content
- `ANALYSIS/RESULTS/age/age_dat.RData` RData file with the age per individual
- `ANALYSIS/RESULTS`

NOTE: each code file contains additional information in the header. 

------------------------------------------------

**Maintainers and contact:**

Please contact Simeon Q. Smeele, <simeonqs@hotmail.com>, if you have any questions or suggestions. 





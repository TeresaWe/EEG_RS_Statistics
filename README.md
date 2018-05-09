# EEG_RS_Statistics

## Preprocessing and Network Analyis in Matlab
https://github.com/TeresaWe/RS_network_MAT

## Read EEG results (MATLAB) and combine
readMatresultsEO.R
readMatresults EC.R

## Various statistics
* Overview over group differences in network parameters for different thresholds and connectivity bands: groupstats_netparam.R
* Linear Models for selected network parameters: LM_selected_netparam.R
* descriptive statistics of "allresults" (general results table of experimental results and behavioural tests): desc_stats_allresults.R
(for scripts to process experimental results see: https://github.com/TeresaWe/...PAT, ...NM_PAT,...unipark,...AEFT,...AGLT,...Navon

## Plots
### ggplot2
* Plot correlations of various parameters: Navon_subgr_beta.R,Navon_subgr_delta.R
### ggplot2, igraph
* Plot result Matrices (FDR, Cohens D, Pvalue etc)/graphs of single connection differences across groups: plot_FDR_Matrices
* Plot averaged binary matrices (of one threshold) of allsubjects between groups: plot_averaged_Matrices
* plotting possibilites script by Richard Bethlehem, ARC, Cambridge: rb_visualize network.R
* Plot Matrix/Graph of single subjects: plot_subjectgraphs

## Specific Statistics and Plots
* Models (an Plots) for different results regarding different network parameters: CvsL_stats_plot.R
* Clusteranalysis (and Plots) because of subgroups seen within visual inspection (see below): cluster_kmeans.R, hierarch_cluster.R
* Compare connectivity (measures) and degree distributions across groups: compare_connectivity_dstributions.R, degree_distributions.R






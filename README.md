# Biological invasions as burdens to primary economic sectors

*Repo to accompany the manuscript by* Anna J. Turbelin\*, Emma J. Hudgins\*, Jane A. Catford, Ross N. Cuthbert, Christophe Diagne, Melina Kourantidou, David Roiz, and Franck Courchamp https://doi.org/10.21203/rs.3.rs-2444595/v1

\* co-first authorship


scripts:
- Data cleaning script: R_Script_S1_Invacost_ActivitySectors.R
- Modelling script: extrapolation_script.R
- STAN models used in modelling script: .stan files

data:
- CABI database: cabi_by_sector_v2.csv
- sTwist database: SInAS_AlienSpeciesDB_2.4.1.csv and sTwist_database.csv (older version with more descriptor columns)
- taxonomic groupings: species_coarse_categories_IC4_1.xlsx
- full taxonomy by sector: .RDS files
- pathways: Pathways_4_1.csv

output:
- data_pred* by sector - set of data used for extrapolation
- predicted_costs\* by sector - extrapolated costs
- brt_glob.csv - relative influence results of BRT model (also in relinf.pdf)
- cost_by_continent_sector.rds - extrapolated BRT costs by continent and sector
- \*_bayes.pdf - model averaging result for extrapolated cost distribuion in distributional scenario

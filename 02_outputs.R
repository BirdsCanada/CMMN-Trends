#Create output tables. This is done for each station separately. 

## Set up output tables
#This is in a separate file because if loop through species fails, don't want to re-write these files and delete everything that has already been done. Can then just re-start the loop at the next species, and keep going.

## Create text file for indices

indices.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 13, byrow = FALSE,
                                    dimnames = NULL))
names(indices.csv) <- c("results_code", "area_code", "year", "season", "period", "species_code", "species_id", "index", "stderr", "stdev", "upper_ci", "lower_ci", "LOESS_index")  

write.table(indices.csv, file = paste(out.dir, 
                                      site, "_AnnualIndices",".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


## Create text file for trends (appending year periods into one file)
trends.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 42, 
                                   byrow = FALSE, dimnames = NULL))
names(trends.csv) <- c("results_code", "version", "area_code", "species_code", "species_id", "season", "period", "years", "year_start", "year_end", "trnd", "index_type", "upper_ci", "lower_ci", "stderr", "model_type", "model_fit", "percent_change", "percent_change_low", "percent_change_high", "prob_decrease_0", "prob_decrease_25", "prob_decrease_30", "prob_decrease_50", "prob_increase_0", "prob_increase_33", "prob_increase_100", "reliability", "precision_num", "precision_cat", "coverage_num", "coverage_cat", "goal", "goal_lower", "sample_size", "sample_total", "subtitle", "prob_LD", "prob_MD", "prob_LC", "prob_MI", "prob_LI")

write.table(trends.csv, file = paste(out.dir, 
                                     site, "_Trends", ".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


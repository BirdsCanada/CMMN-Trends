#Create output tables. This is done for each station separately. 

## Set up output tables
#This is in a separate file because if loop through species fails, don't want to re-write these files and delete everything that has already been done. Can then just re-start the loop at the next species, and keep going.

## Create text file for indices

indices.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 13, byrow = FALSE,
                                    dimnames = NULL))
names(indices.csv) <- c("results_code", "area_code", "year", "season", "period", "species_code", "species_id", "index", "stdev", "upper_ci", "lower_ci", "LOESS_index", "meanObs")  

write.table(indices.csv, file = paste(out.dir, 
                                      site, "_AnnualIndices",".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


## Create text file for trends (appending year periods into one file)
trends.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 18, 
                                   byrow = FALSE, dimnames = NULL))
names(trends.csv) <- c("results_code", "version", "area_code", "species_code", "species_id", "season", "period", "years", "year_start", "year_end", "trnd", "lower_ci", "upper_ci", "stdev", "percent_change", "width_ci", "model_type", "index_type")

write.table(trends.csv, file = paste(out.dir, 
                                     site, "_Trends", ".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


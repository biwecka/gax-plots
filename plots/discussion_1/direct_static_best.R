# Load necessary libraries
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(svglite)
library(data.table, warn.conflicts = FALSE)

# Define the path to the data directory and plots directory
data_dir <- "data"
plot_dir <- "rendered"

instance <- "hdtt4"
alg_name <- "alg_11" # direct encoding
cfg_name <-
    "cfg_P:1000_MR:0.0100_SE:trn-4_CX:trd1_MU:trd_RE:eli-abs-1_TE:g-500000-ov-0"



### Select Run #################################################################
# Assemble the path to the configuration directory.
cfg_path <- paste(data_dir, instance, alg_name, cfg_name, sep = "/")

# Initialize data table for storing all runs.
runs <- data.table()

# Iterate runs, read their `general.csv` and store the data into the table.
for (run_dir in list.dirs(cfg_path, full.names = TRUE, recursive = FALSE)) {
    # Define path to `general.csv`.
    general_csv_path <- file.path(run_dir, "general.csv")

    # Read CSV file
    data <- fread(general_csv_path)

    # Add the directory of this run to the data (needed later on).
    data[, run_dir := run_dir] # add run directory to data

    # Add the data to the `runs` table
    runs <- rbind(runs, data, fill = TRUE)
}


# Filter for successful runs
successful_runs <- runs[success == TRUE]

# Extract the best run (by runtime). `which.min` returns an index!
best_successful_run <- successful_runs[which.min(runtime)]



### Load Run Data ##############################################################
# Define path to `generations.csv`
generations_csv_path <-
    file.path(best_successful_run$run_dir, "generations.csv")

# Read generations data
generations <- fread(generations_csv_path)



### Plot Run ###################################################################
div_scale <- round(max(generations$worst) / 100) * 100

plot <- ggplot(generations, aes(x = gen)) +
    geom_line(aes(y = best, color = "Best"), size = 0.2) +
    geom_line(aes(y = worst, color = "Worst"), size = 0.2) +
    geom_line(aes(y = mean, color = "Mean"), size = 0.2) +
    geom_line(aes(y = median, color = "Median"), size = 0.2) +
    geom_line(
        # Scaling diversity values for better visualization
        aes(y = diversity * div_scale, color = "Diversity"),
        size = 0.2
    ) +
    scale_y_continuous(
        # Label for primary y-axis
        name = "Cost",

        # Label for secondary y-axis
        sec.axis = sec_axis(
            ~ . / div_scale, # revert value scaling for correct axis scale
            name = "Diversity"
        )
    ) +
    labs(
        title = "Generations Metrics",
        x = "Generation",
        y = "Metric Value",
        color = "" # Legend title
    ) +
    scale_color_manual(values = c(
        "Best" = "blue",
        "Worst" = "red",
        "Mean" = "green",
        "Median" = "purple",
        "Diversity" = "orange"
    )) +
    theme_bw()


ggsave(
    file.path(
        plot_dir,
        instance,
        alg_name,
        "discussion_1",
        "direct_static_best.png"
    ),
    plot = plot,
    device = "png",
    create.dir = TRUE,
    width = 14,
    # height = height
)

################################################################################

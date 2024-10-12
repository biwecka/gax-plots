# Load necessary libraries
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

# Set the path to the directory containing your configuration directories
base_path <- "data/alg_12"

# Initialize a list to store data frames
data_list <- list()

# Loop through configuration folders
for (cfg in list.dirs(base_path, full.names = TRUE, recursive = FALSE)) {
    # Get the configuration name
    cfg_name <- basename(cfg)

    # Loop through run folders
    for (run in list.dirs(cfg, full.names = TRUE, recursive = FALSE)) {
        # Read the general.csv file
        file_path <- file.path(run, "general.csv")
        run_data <- read.csv(
            file_path,
            sep = ";",
            header = TRUE,
            strip.white = TRUE
        )

        # Add a column for the configuration and run identifier
        run_data$configuration <- cfg_name
        # run_data$run_id <- run_data$run

        run_data$success <- as.logical(run_data$success)

        # Store in the list
        data_list[[length(data_list) + 1]] <- run_data
    }
}

# Combine all data frames into one
all_data <- bind_rows(data_list)

print(">>> ALL DATA <<<")
print(all_data)
print("")


# Summarize data: count successes and failures
summary_data <- all_data %>%
    group_by(configuration) %>%
    summarise(
        success_count = sum(success == TRUE),
        failure_count = sum(success == FALSE),
        total_count = n(),
        success_percentage = (success_count / n()) * 100
    )

print(">>> SUMMARY DATA <<<")
print(summary_data)
print("")

# Reshape the data for plotting
plot_data <- summary_data %>%
    pivot_longer(
        cols = c(success_count, failure_count),
        names_to = "status",
        values_to = "count"
    )

print(">>> PLOT DATA <<<")
print(plot_data)

# Stacked
ggplot(
    plot_data,
    aes(fill = status, y = count, x = configuration)
) +
    geom_bar(position = "stack", stat = "identity") +

    # Labels
    labs(
        title = "Algorithm Configuration Performance",
        x = "Configuration",
        y = "Number of Runs",
        fill = "Run Status"
    ) +

    # Success Percentage
    geom_text(
        data = plot_data,
        aes(
            label = paste0(round(success_percentage, 2), "%"),
            x = configuration,
            y = 0
        ),
        # position = position_stack(vjust = 0.5), color = "white"
        # label.padding = unit(0.25, "lines"),
    )


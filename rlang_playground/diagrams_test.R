# Load necessary libraries
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(svglite)

# Set the path to the directory containing your configuration directories
base_path <- "data/alg_12"

# Initialize a list to store data frames
data_list <- list()

# Loop through configuration folders
for (cfg in list.dirs(base_path, full.names = TRUE, recursive = FALSE)) {
    # Get the configuration name
    cfg_name <- basename(cfg)

    # Read the params.csv
    params <- read.csv(
        file.path(cfg, "params.csv"),
        sep = ";",
        header = TRUE,
        strip.white = TRUE,
    )

    # if (as.logical(params$dynamic) == TRUE) {
    #     next
    # }

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

        # Add dynamic information from params
        # run_data$dynamic <- as.logical(params$dynamic)

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
        success_percentage = (success_count / n()) * 100,
        # dynamic = first(dynamic)
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
    ) %>%
    mutate(
        status = recode(
            status,
            success_count = "success",
            failure_count = "failure"
        )
    )

print(">>> PLOT DATA <<<")
print(plot_data)

# Stacked
barchart <- ggplot(
    plot_data,
    aes(fill = status, y = count, x = configuration)
) +
    geom_bar(position = "stack", stat = "identity") +

    # Labels
    labs(
        title = "XHSTT GA Success Rates",
        x = "Configuration",
        y = "Executions",
        fill = "Result Status"
    ) +

    # # Success Percentage
    annotate(
        "text",
        x = plot_data$configuration,
        y = 0,
        label = paste0(plot_data$success_percentage, "%"),
        vjust = -1,
        hjust = 0.5,
        size = 3,
        color = "#2a2a2a"
    ) +

    theme_bw() +

    # Rotate x-axis labels
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.8))


ggsave("plots.png", plot = barchart, device = "png")

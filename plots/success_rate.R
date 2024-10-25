# Load necessary libraries
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(svglite)

# Define the path to the data directory and plots directory
data_dir <- "data"
plot_dir <- "rendered"

# Read `params.csv` file and make sure all columns are converted into the
# correct data type.
read_params_csv <- function(path) {
    # Read the CSV
    params <- read.csv(
        path,
        sep = ";",
        strip.white = TRUE
    )

    # Parse the `dynamic` column as boolean
    params$dynamic <- as.logical(params$dynamic)

    # Return
    return(params)
}

# Read `run_xxx/general.csv` file and ensure correct data types.
read_run_general <- function(path) {
    # Read the CSV
    general <- read.csv(
        path,
        sep = ";",
        strip.white = TRUE
    )

    # Parse `success` column as boolean
    general$success <- as.logical(general$success)

    # Return
    return(general)
}

# Summarize and transform run data
sum_transform_run_data <- function(run_data) {
    # Combine all frames into one
    data <- bind_rows(run_data)

    # Summarize data: cound successes and failures
    summary <- data %>%
        # group data by configuration
        group_by(cfg) %>%

        # summarize columns
        summarise(
            success_count = sum(success == TRUE),
            failure_count = sum(success == FALSE),
            total_count = n(),
            success_rate = success_count / total_count
        )

    # Reshape data for plotting
    new <- summary %>%
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

    return(new)
}

plot_barchart <- function(data, problem_name, alg_name, dynamic) {
    # Construct plot title
    title <- ""
    if (dynamic == TRUE) {
        title <- paste(alg_name, "Success Rates (dynamic) @", problem_name)
    } else {
        title <- paste(alg_name, "Success Rates (static) @", problem_name)
    }


    ggplot(
        data,
        aes(fill = status, y = count, x = cfg)
    ) +
        geom_bar(position = "stack", stat = "identity") +

        # Labels
        labs(
            title = title,
            x = "Configuration",
            y = "Executions",
            fill = "Result Status"
        ) +

        # Success Percentage
        annotate(
            "text",
            x = data$cfg,
            y = 0,
            label = round(data$success_rate, 4), #paste0(data$success_rate, "%")
            vjust = -1,
            hjust = 0.5,
            size = 3,
            color = "#2a2a2a"
        ) +

        theme_bw() +

        # Rotate x-axis labels
        # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.8))
        scale_x_discrete(guide = guide_axis(angle = 90))

}

# Loop through all the problem instances
for (problem_dir in list.dirs(data_dir, full.names = TRUE, recursive = FALSE)) {
    # Extract the problem intsances name
    problem_name <- basename(problem_dir)

    # Loop through all the algorithms (directories) in the data directory.
    for (alg_dir in list.dirs(
        problem_dir, full.names = TRUE, recursive = FALSE
    )) {
        # The `alg_dir` variable now contains the full path to the algorithm
        # directory (e.g. `data/alg_12`).

        # Extract the algorithm name
        alg_name <- basename(alg_dir)

        # Initialize lists for static and dynamic configurations
        static_cfgs <- list()
        dynamic_cfgs <- list()

        # Loop through all the configurations directories of this algorithm.
        for (cfg_dir in list.dirs(
            alg_dir, full.names = TRUE, recursive = FALSE
        )) {
            # Extract the configuration name
            cfg_name <- basename(cfg_dir)

            # Read the params CSV
            params <- read_params_csv(file.path(cfg_dir, "params.csv"))

            # Read in data of all runs
            for (run_dir in list.dirs(
                cfg_dir, full.names = TRUE, recursive = FALSE
            )) {
                # Read general.csv
                run <- read_run_general(file.path(run_dir, "general.csv"))

                # Add the configuration name to the run dataset
                run$cfg <- cfg_name

                # Push the run to the correct list
                if (params$dynamic == TRUE) {
                    dynamic_cfgs[[length(dynamic_cfgs) + 1]] <- run
                } else {
                    static_cfgs[[length(static_cfgs) + 1]] <- run
                }
            }
        }

        # Plot static configurations
        if (length(static_cfgs) != 0) {
            static_data <- sum_transform_run_data(static_cfgs)
            barchart <- plot_barchart(
                static_data, problem_name, alg_name, FALSE
            )

            ggsave(
                file.path(
                    plot_dir,
                    problem_name,
                    alg_name,
                    "static_success_rate.png"
                ),
                plot = barchart,
                device = "png",
                create.dir = TRUE,
                # height = 14,
            )
        }

        # Plot dynamic configurations
        if (length(dynamic_cfgs) != 0) {
            dynamic_data <- sum_transform_run_data(dynamic_cfgs)
            barchart <- plot_barchart(
                dynamic_data, problem_name, alg_name, TRUE
            )

            ggsave(
                file.path(
                    plot_dir,
                    problem_name,
                    alg_name,
                    "dynamic_success_rate.png"
                ),
                plot = barchart,
                device = "png",
                create.dir = TRUE,
                height = 10
            )
        }
    }
}

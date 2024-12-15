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

# Transform data for plot
transform_data <- function(data) {
    # Combine all frames into one
    data <- bind_rows(data)

    # Calculate generations per second.
    data$gen_per_sec <- data$gen / data$runtime

    return(data)
}

# Plot function
plot_points <- function(data, problem_name, alg_name, dynamic) {
    # Construct plot title
    title <- ""
    if (dynamic == TRUE) {
        title <- paste(
            alg_name,
            "Generations/s (dynamic) @",
            problem_name
        )
    } else {
        title <- paste(
            alg_name,
            "Generations/s (static) @",
            problem_name
        )
    }

    # Calculate median runtimes
    medians <- data %>%
        group_by(cfg) %>%
        summarize(
            median_success_runtime = median(
                runtime[success == TRUE], # na.rm = TRUE
            ),
            median_failure_runtime = median(
                runtime[success == FALSE], # na.rm = TRUE
            )
        )

    ggplot(
        data,
        aes(x = cfg, y = gen_per_sec)
    ) +
        # geom_point(alpha = 0.2, color = "blue") +  # Points with transparency
        geom_boxplot() +

        # Labels
        labs(
            title = title,
            x = "Configuration",
            y = "Generations/s",
        ) +

        theme_bw() +

        # Expand x-axis limits to create space for annotations
        scale_y_continuous(expand = expansion(mult = c(0.4, 0.0))) +

        # Add annotation boxes for success and failure aligned with x values
        geom_label(
            data = medians,
            aes(
                x = cfg,
                y = min(data$gen_per_sec) * 0.75,
                label = round(median_success_runtime, 2),
                fill = "Success"
            ),
            # inherit.aes = FALSE,
            size = 2.4,
            # color = "white"
            vjust = 0.0,
        ) +
        geom_label(
            data = medians,
            aes(
                x = cfg,
                y = min(data$gen_per_sec) * 0.75,
                label = round(median_failure_runtime, 2),
                fill = "Failure"
            ),
            # inherit.aes = FALSE,
            size = 2.4,
            # color = "white"
            vjust = 1.0,
        ) +

        # Define fill colors for labels
        scale_fill_manual(
            name = "Median Runtime\nin seconds",
            values = c("Success" = "green", "Failure" = "red"),
            labels = c("Success" = "on success", "Failure" = "on failure"),
            guide = guide_legend(
                override.aes = list(
                    hjust = 0,  # Center the text in the legend
                    vjust = 0.35   # Center vertically in the legend
                )
            )
        ) +

        # Rotate x-axis labels
        # scale_x_discrete(guide = guide_axis(angle = 90)) +

        coord_flip()
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
            static_data <- transform_data(static_cfgs)
            boxplot <- plot_points(static_data, problem_name, alg_name, FALSE)

            num_bars <- length(unique(static_data$cfg))

            width <- 12
            height <- num_bars * 0.3 + 1

            ggsave(
                file.path(
                    plot_dir,
                    problem_name,
                    alg_name,
                    "static_gen_per_sec.png"
                ),
                plot = boxplot,
                device = "png",
                create.dir = TRUE,
                width = width,
                height = height,
            )
        }

        # Plot dynamic configurations
        if (length(dynamic_cfgs) != 0) {
            dynamic_data <- transform_data(dynamic_cfgs)
            boxplot <- plot_points(dynamic_data, problem_name, alg_name, TRUE)

            num_bars <- length(unique(dynamic_data$cfg))

            width <- 12
            height <- num_bars * 0.3 + 1

            ggsave(
                file.path(
                    plot_dir,
                    problem_name,
                    alg_name,
                    "dynamic_gen_per_sec.png"
                ),
                plot = boxplot,
                device = "png",
                create.dir = TRUE,
                width = width,
                height = height,
            )
        }
    }
}

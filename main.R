# print(class(gens))
# print(str(gens))
# print(typeof(gens))

### Read GENERAL ###
data <- read.csv(
    "data/alg_12/cfg_0/run_0/general.csv",
    sep = ";",
    strip.white = TRUE
)

# Convert strings to booleans.
data$success <- as.logical(data$success)

### Read GENERATIONS ###
gens <- read.csv(
    "data/alg_12/cfg_0/run_0/generations.csv",
    sep = ";",
    strip.white = TRUE
)

gens$all_values <- lapply(gens$all_values, function(x) {
    as.numeric(unlist(strsplit(x, ",")))
})

gens$best <- sapply(gens$all_values, min)
gens$avg <- sapply(gens$all_values, mean)
gens$median <- sapply(gens$all_values, median)

print(gens)

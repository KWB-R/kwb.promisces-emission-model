library(kwb.odm)
library(kwb.utils)
library(kwb.ogre)
require(dplyr)

# Directory to be adapted by user:

# Directory of additional data

project_path <- "Y:/WWT_Department/Projects/PROMISCES"
data.dir <- file.path(
  project_path, "Work-packages/WP 2 - F&T&Exposure&Risk/F&T",
  "modelling_urban-setting/PROMISCES_LoadModel/data_LoadModel")

# Define unit conversion factors

CONVERSION_FACTORS <- c(
  "ug/L" = 1 / 10000, "mg/L" = 1 / 100
)

# Define file types (name without extension and content description)

FILE_NAMES <- c(
  "conc_rain" = "stormwater",
  "vol_rain" = "vol_rain",
  "vol_sewage" = "vol_sewage",
  "c_in_sewage" = "c_in_wwtp",
  "c_out_sewage" = "c_out_wwtp"
)

# MAIN -------------------------------------------------------------------------
if (FALSE)
{
  runs <- 10

  # Load Data and prepare Monte Carlo simulation

  conc_rain <- get_rain_conc_mcs(
    path = data.dir,
    filename = "stormwater",
    runs = runs)

  vol_rain <- get_rain_vol_mcs(
    path = data.dir,
    filename = "vol_rain")

  # Number of Monte Carlo simulations
  runs <- 1000

  # 1. Calculate loads of rainwater-based substances (for three pathways)

  load_by_rain <- get_load_by_rain(
    data.dir = data.dir,
    files = FILE_NAMES,
    runs = runs,
    substance = "TP" # If only one substance is calculated, number of runs can be higher
  )

  list(d)

  boxplot(load_by_rain$TP ~ load_by_rain$month)
  boxplot(load_by_rain$TP ~ load_by_rain$water_source+ load_by_rain$month, outline = FALSE)
  average_load <- load_by_rain %>% group_by(month, waterbody, water_source) %>%
    summarize("mean_load" = mean(TP))

  # 2. Calculate loads of sewage based substances

  x_monthly_loads_sew <- monthly_load_sewage(
    data.dir = data.dir,
    files = FILE_NAMES)
}

### FUNCTIONS ### --------------------------------------------------------------
# Load concentrations in rainwater runoff

get_rain_conc_mcs <- function(path, filename, runs){
  x_conc_rain <- readTableOrStop(
    data.dir = path,
    name = filename)
  initMonteCarlo_by_data(df = x_conc_rain, runs = runs)
}

# Load volumes of rainwater (separate sewer system to waterbody,
# CSO to waterbody, entering WWTP)

get_rain_vol_mcs <- function(path, filename, runs){
  vol_rain <- readTableOrStop(
    data.dir = path,
    name = filename)

  vol_rain$water_source <- unlist(lapply(vol_rain$Parameter, function(x){
    strsplit(
      x = strsplit(x = x, split = ", ")[[1]][2],
      split = " ")[[1]][1]
  }))

  months <- c("january", "february", "march", "april", "may", "june", "july",
              "august", "september", "october", "november", "december")

  monthly_volume <- lapply(X = months, function(month){
    MC_vol_rain <- initMonteCarlo(
      x = vol_rain,
      runs = runs,
      log = FALSE,
      set.names = paste(vol_rain$SUW, vol_rain$water_source, sep = "_"),
      seed = 3,
      column.mean = paste0(month, ".mean"),
      column.sd = paste0(month, ".sd")
    )
  })

  source_list <- split(x = vol_rain, f = vol_rain$water_source)

}

# monthly_load_rain -------------------------------------------------------------

get_load_by_rain <- function(
  data.dir,
  files,
  runs = 100,
  substance = NULL
)
{
  # Load data (# concentration data in ug/L)
  x_conc_rain <- readTableOrStop(
    data.dir = data.dir,
    name = files["conc_rain"])

  if(!is.null(substance)){
    if(!(substance %in% colnames(x_conc_rain))){
      stop("Given substance name is no column name in the concentration table.")
    }
    x_conc_rain <- data.frame(x_conc_rain[,colnames(x_conc_rain) == substance])
    colnames(x_conc_rain) <- substance
  }

  # Rain volume in m3/month

  vol_rain <- readTableOrStop(
    data.dir = data.dir,
    name = files["vol_rain"])

  ### Loads of rainwater based substances via separate sewer system and CSO

  months <- c("january", "february", "march", "april", "may", "june", "july",
    "august", "september", "october", "november", "december")

  rain_load <- lapply(months, function(month){
    rain_load_per_month(
      vol_rain = vol_rain,
      x_conc_rain = x_conc_rain,
      month = month,
      runs = runs)
  })

  df_out <- do.call(rbind, rain_load)
  df_out$month <- factor(x = df_out$month, levels = months)
  df_out
}

rain_load_per_month <- function(vol_rain, x_conc_rain, month, runs){
  MC_conc_rain <- initMonteCarlo_by_data(df = x_conc_rain, runs = runs)

  # Depends on the definition in column "Parameter"

  vol_rain$water_source <- unlist(lapply(vol_rain$Parameter, function(x){
    strsplit(
      x = strsplit(x = x, split = ", ")[[1]][2],
      split = " ")[[1]][1]
  }))

  # In m3/month

  MC_vol_rain <- initMonteCarlo(
    x = vol_rain,
    runs = runs,
    log = FALSE,
    set.names = paste(vol_rain$SUW, vol_rain$water_source, sep = "_"),
    seed = 3,
    column.mean = paste0(month, ".mean"),
    column.sd = paste0(month, ".sd")
  )

  load_rain <- lapply(
    seq_along(MC_vol_rain), function(i){
      df_out <- data.frame(MC_vol_rain[,i] * MC_conc_rain)
      df_out$waterbody <- vol_rain$SUW[i]
      df_out$water_source <- vol_rain$water_source[i]
      df_out$month <- month
      df_out
    })

  do.call(rbind, load_rain)
}

# readTableOrStop --------------------------------------------------------------

readTableOrStop <- function(
    data.dir, name,
    ...
    ### Additional arguments passed to read.table and eventually overwriting the
    ### default settings
)
{
  # Compose the full path to the file
  filename <- paste0(name, ".csv")
  file <- file.path(data.dir, filename)

  # Set default arguments
  args <- list(sep = ";", dec = ".", stringsAsFactors = FALSE, header = TRUE)

  # Call read.table with the default arguments but eventually overwritten by
  # additional arguments given in "...". call With() is from "kwb.utils"
  callWith(read.table, args, file = file, ...)
}

# initMonteCarlo ---------------------------------------------------------------

initMonteCarlo <- function(
    x, runs, log = TRUE, set.names = NULL, column.mean = "mean_ln",
    column.sd = "sd_ln", seed = NULL
)
{
  # Set the seed for the random number generator if a seed is given
  if (! is.null(seed)) {
    set.seed(seed)
  }

  c_mean <- grep(
    pattern = column.mean,
    x = colnames(x),
    ignore.case = TRUE)
  c_sd <- grep(
    pattern = column.sd,
    x = colnames(x),
    ignore.case = TRUE)
  if(length(c_sd) == 0L | length(c_mean) == 0L){
    stop("Wrong column names for mean and/or standard deviation column: '",
         column.mean, "', '", column.sd, "'")
  }

  # Set the normal distribution function to either rlnorm() or rnorm()

  FUN.norm <- ifelse(log, rlnorm, rnorm)

  # Create a vector of row indices 1:nrow(x)

  rows <- seq_len(nrow(x))

  # For each row index, call a function that looks up the mean and the standard
  # deviation from the appropriate columns and calls the normal distribution
  # function with these values. The result is a list.

  result <- lapply(rows, FUN = function(row) {
    FUN.norm(n = runs, x[row, c_mean], x[row, c_sd])
  })

  # Provide a vector of (column) names

  names <- if (!is.null(set.names)) {
    set.names
  } else {
    paste0("X", rows) # Default names: X1, X2, X3, ...
  }

  # Convert the list into a data frame and set the column names of that
  # data frame by setting its attribute "name". Use structure() to nicely set
  # attributes "on the fly"
  structure(as.data.frame(result), names = names)
}

initMonteCarlo_by_data <- function(
    df, # A data frame where each column contains values of one parameter
    runs = 10000,
    seed = NULL
){
  sapply(df, function(v){
    sapply(1:runs, data_to_mean, v = v)
  })
}

data_to_mean <- function(v, seed = NULL){
  v <- v[!is.na(v)]
  n_values <- length(v)
  set.seed(seed)
  random_sample <- sample(x = v, size = n_values, replace = TRUE)
  mean(random_sample)
}

# toNumeric --------------------------------------------------------------------

toNumeric <- function(x, columns)
{
  for (column in columns) {
    x[, column] <- as.numeric(x[, column])
  }
  x
}

# getLoads ---------------------------------------------------------------------

getLoads <- function
(
  concentration,
  ### Concentration of rain (for CSO) or rain (for separate sewer system)
  ### or sewage (CSO)
  units,
  ### Abbreviated unit names
  volume,
  ### rainwater or sewage volume
  parameter,
  removal = NULL
)
{
  suwNames <- unique(volume$SUW)

  # Filter volume data frame for the given parameter
  volume <- volume[volume$Parameter == parameter, ]

  # Calculate loads in list
  load_x <- lapply(seq_len(ncol(concentration)), function(i) {

    # Initialise the output data frame with a unit column
    out <- data.frame(unit = rep(units[i], times = nrow(concentration)))

    # Get the loads for each SUW
    for (suwName in suwNames) {

      # From the volume data frame, already filtered for the given parameter,
      # select the row representing the current SUW name.
      volume_suw <- volume[volume$SUW == suwName, ]

      # Add an empty column, named according to the current SUW
      out[, suwName] <- NA

      # Fill the empty column with the actual loads, calculated for each run
      for (run in seq_len(runs)) {

        # Skip the first 2 columns, SUW and Parameter, in volume_suw: 2 + run
        load <- concentration[run, i] * volume_suw[, 2 + run]

        # Lookup the removal rate or set it to 0 if no removals are given
        removalRate <- if (is.null(removal)) 0 else 0.01 * removal[run, i]

        out[run, suwName] <- load * (1 - removalRate)
      }
    }

    changeunit(out)
  })

  structure(load_x, names = colnames(concentration))
}

# changeunit--------------------------------------------------------------------

changeunit <- function(x, factors = CONVERSION_FACTORS)
{
  #x <- load_x

  unit <- as.character(unique(selectColumns(x, "unit")))

  if (is.na(factors[unit])) {

    stop("No conversion factor defined for unit: '", unit, "'! Conversion ",
         "factors are defined for: ", stringList(names(factors)))
  }

  # Apply conversion of values to all columns except for "unit"
  columns <- setdiff(names(x), "unit")

  x[, columns] <- x[, columns] * factors[unit]

  x
}

# sumPaths ---------------------------------------------------------------------

sumPaths <- function(suwNames, variables, runs, inputs)
{
  out.init <- data.frame(matrix(ncol = length(suwNames), nrow = runs))
  colnames(out.init) <- suwNames

  result <- lapply(seq_along(variables), function(i) {

    out <- out.init

    for (j in seq_along(suwNames)) {

      # Get the appropriate column vectors from the input lists
      vectors <- lapply(inputs, function(input) input[[i]][, 1 + j])

      # Calculate the sum vector and assign it to out[, j]
      out[, j] <- Reduce("+", vectors)
    }

    out
  })

  structure(result, names = variables)
}

# monthly_load_sewage -----------------------------------------------------------

monthly_load_raw_sewage <- function # calculates the load for each substance
### separates pathways (CSO and WWTP)
(
  data.dir,
  ### Path of model data("Vol_sewage.csv",
  ### Removal at WWTP "substance_info.csv",
  ### Just for names "x_conc_rain")
  files
)
{
  # Load data

  name <- "vol_sewage"
  vol_sewage <- readTableOrStop(data.dir, files["vol_sewage"])

  name <- "substance_info"
  sub_sew_info <- readTableOrStop(data.dir, name, types[name])

  ### Loads of sewage based substances via CSO and WWTP

  # Step 1: Monte Carlo simulations to get sewage volume

  MC_vol_sew <- initMonteCarlo(
    x = vol_sewage, runs = runs, log = FALSE, set.names = FALSE, seed = 2
  )

  MC_vol_sew_1 <- vol_sewage[, 1:2]
  MC_vol_sewage <- cbind(MC_vol_sew_1, t(MC_vol_sew))

  # Step 2: Simulations of concentrations in wastewater and of removal rates

  # Provide units
  units <- selectColumns(x_conc_rain, "UnitsAbbreviation")

  # Step 3: Calculation of loads in list, CSO + WWTP

  ## CSO
  load_sew_cso <- getLoads(
    concentration = MC_conc_sew,
    units = units,
    volume = MC_vol_sewage,
    parameter = "ROWvol, CSO [m3/month]"
  )

  ## WWTP
  load_sew_wwtp <- getLoads(
    concentration = MC_conc_sew,
    units = units,
    volume = MC_vol_sewage,
    parameter = "ROWvol, WWTP [m3/month]",
    removal = MC_removal_rates
  )

  # sum paths (in list)
  load_sew_sum <- sumPaths(
    suwNames = unique(vol_sewage$SUW),
    variables = colnames(MC_conc_sew),
    runs = runs,
    inputs = list(load_sew_cso, load_sew_wwtp)
  )

  # output
  list(
    load_sew_cso = load_sew_cso,
    load_sew_wwtp = load_sew_wwtp,
    load_sew_sum = load_sew_sum,
    MC_vol_sewage = MC_vol_sewage
  )
}

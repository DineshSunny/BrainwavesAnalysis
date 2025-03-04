
    #Author: Zak Groenewold, Dinesh Seveti, Arne Samuel
    #Date: 12/2/2024
    #CSCI310 Final Task
    #
    #Unified version: Plotting Python x R #141 
    #https://github.com/3C-SCSU/Avatar/issues/141



# Import required libraries
library(R6)                                         # For object-oriented programming
suppressPackageStartupMessages(library(tidyverse))  # For data manipulation and visualization
library(readr)                                      # For reading CSV files
suppressPackageStartupMessages(library(janitor))    # For cleaning column names
library(stringr)                                    # For string manipulation

# retrieves the value of file_path from the global environment, then verifys it
file_path <- get("file_path", envir = .GlobalEnv)
if (is.null(file_path) || file_path == "") {
  stop("No file_path provided. Please ensure the Python script sets file_path.")
}

# retrieves the value of output_folder from the global environment, then verifys it
output_folder <- get("output_folder", envir = .GlobalEnv)
if (is.null(output_folder) || output_folder == "") {
  stop("No output_folder provided. Please ensure the Python script sets output_folder.")
}

# Define the BrainWaveAnalysis R6 class
BrainWaveAnalysis <- R6Class("BrainWaveAnalysis",
  public = list(
    df = NULL, # Data frame to hold the raw data
    df_long = NULL, # Long-format data
    df_binned = NULL, # Binned data for plotting
    bin_size = 88, # Default bin size
    model = NULL, # Polynomial model
    df_subset = NULL, # Subset for plotting
    xdf = NULL, # Data for prediction

    # Constructor to initialize the object with files
    initialize = function(file_path) {
      if (is.null(file_path) || file_path == "") {
        stop("No file_path provided. Please ensure the Python script sets file_path.")
      }

      # Load the data from CSV files in the specified path
      list_of_files <- list.files(
        path = file_path,
        recursive = TRUE,
        pattern = "\\.csv$",
        full.names = TRUE
      )
      # Read CSV files into a data frame, suppressing column type messages
      self$df <- readr::read_csv(list_of_files, show_col_types = FALSE)
      self$clean_data()
    },

    # Method to clean column names
    clean_data = function() {
      self$df <- self$df %>% clean_names()
      self$reshape_data()
    },

    # Method to reshape data into long format
    reshape_data = function() {
      self$df_long <- self$df %>%
        pivot_longer(
          cols = starts_with("exg"),
          names_to = "Channel",
          values_to = "Amplitude"
        ) %>%
        # Modify the 'Channel' column to show numbers starting from 01 to 16
        mutate(ChannelNumber = str_pad(as.integer(str_extract(Channel, "\\d+")) + 1, width = 2, pad = "0"))
      self$bin_data()
    },

    # Method to bin the data
    bin_data = function() {
      self$df_binned <- self$df_long %>%
        group_by(ChannelNumber) %>%
        mutate(Bin = floor(sample_index / self$bin_size) * self$bin_size) %>%
        group_by(ChannelNumber, Bin) %>%
        summarise(
          AmplitudeMean = mean(Amplitude, na.rm = TRUE),
          .groups = 'drop'
        )
      self$prepare_for_plotting()
    },

    # Prepare the data subset for plotting (every 120th point)
    prepare_for_plotting = function() {
      self$df_subset <- self$df_long %>%
        group_by(ChannelNumber) %>%
        mutate(Row = row_number()) %>%
        filter(Row %% 120 == 0)
      self$fit_model()
    },

    # Fit the polynomial model to the data subset
    fit_model = function() {
      self$model <- lm(Amplitude ~ poly(sample_index, 12), data = self$df_subset)
      self$predict_data()
    },

    # Generate predictions using the fitted model
    predict_data = function() {
      min_index <- min(self$df_subset$sample_index)
      max_index <- max(self$df_subset$sample_index)
      self$xdf <- data.frame(
        sample_index = seq(min_index, max_index, length.out = 300)
      )

      # Generate fitted amplitude values using the polynomial model
      self$xdf$AmplitudeFitted <- predict(self$model, newdata = self$xdf)
      self$plot_data()
    },

    # Plot the data using ggplot2 and save as .png file in output folder
    plot_data = function() {
      plot_file <- file.path(output_folder, paste0("plot_", basename(file_path), ".png"))
      png(filename = plot_file, width = 800, height = 600)
      p <- ggplot(self$df_subset, aes(x = sample_index, y = Amplitude)) +
        geom_smooth(
          se = FALSE,
          method = "loess",
          span = 0.25,
          aes(group = ChannelNumber, color = ChannelNumber),
          linewidth = 1
        ) +
        geom_line(
          data = self$xdf,
          aes(x = sample_index, y = AmplitudeFitted),
          linewidth = 5,
          color = "red",
          linetype = "twodash"
        ) +
        theme_minimal() +
        labs(
          title = paste("Brain Wave Signals", basename(file_path), sep = ":"),
          x = "Number of Samples",
          y = "EEG Signal Amplitude",
          color = "Channel"
        ) +
        guides(color = guide_legend(title = "Channel")) +
        coord_cartesian(ylim = c(-30000, 30000), xlim = c(-1, 300)) +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          plot.margin = margin(10, 10, 10, 10),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9)
        )
      # prints to PNG device
      print(p)
      dev.off()

      # Display the plot on the screen as well
      dev.new()
      print(p)
    }
  )
)

# Create an object of the BrainWaveAnalysis class with the specified file path
analysis <- BrainWaveAnalysis$new(file_path)

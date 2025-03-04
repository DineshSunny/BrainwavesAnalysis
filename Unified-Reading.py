"""
    Author: Zak Groenewold, Dinesh Seveti, Arne Samuel
    Date: 12/2/2024
    CSCI310 Final Task
    
    Unified version: Plotting Python x R #141 
    https://github.com/3C-SCSU/Avatar/issues/141

"""

import os
import rpy2.robjects as robjects

# Function to execute the R script with the specified file path and output folder
def execute_r_script(r_script_path, file_path, output_folder):
    try:
        # Set the file_path and output_folder in the R environment
        robjects.r.assign("file_path", file_path)
        robjects.r.assign("output_folder", output_folder)
        print(f"Executing R script: {r_script_path} for folder: {file_path}")

        # Load and execute the R script
        robjects.r.source(r_script_path)
        print("R script executed successfully.")
    except Exception as e:
        print(f"An error occurred while executing the R script: {e}")


# Main function, process each folder and execute the R script
def main():
    # sets locations, expected to change when in the full avatar project
    base_dir = "brainwaves-csv"             #"/Avatar/plots/plot_waves/brainwaves-csv"
    r_script_path = "Plot-Generation.R"     #"/Avatar/plots/Plot-Generation.R"
    output_folder = "Plots_Output"          #"/Avatar/plots/Plots_Output"
    
    folders = ["backward", "forward", "land", "left", "right", "takeoff"]

    # Validate R script path
    if not os.path.exists(r_script_path):
        print(f"Error: R script not found at: {r_script_path}")
        return

    # Create the output folder if it doesn't exist
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    # Loop through each folder, validate and execute the R script
    for folder in folders:
        # Construct the full path to the current folder
        file_path = os.path.join(base_dir, folder)
        if not os.path.exists(file_path):
            print(f"Error: Folder not found: {file_path}")
            continue
        # Execute the R script with the current folder's path and output folder
        execute_r_script(r_script_path, file_path, output_folder)
        
    input("Press ENTER to close plots.")
    robjects.r('graphics.off()')

if __name__ == "__main__":
    main()

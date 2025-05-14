library(data.table)


#####################################################
# Create a temporary file for the ZIP archive
temp_zip <- tempfile(fileext = ".zip")

# Download the ZIP file
download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_weekly_CSV.zip", temp_zip)

# List the files in the ZIP archive and assume the CSV is the first file
files_in_zip <- unzip(temp_zip, list = TRUE)$Name
csv_file_name <- files_in_zip[1]

# Extract the CSV file to a temporary directory
extracted_path <- tempdir()
unzip(temp_zip, files = csv_file_name, exdir = extracted_path)

# Construct the full path to the extracted CSV file
csv_path <- file.path(extracted_path, csv_file_name)

# Read the CSV file using fread
ff3w_data <- fread(csv_path)
head(ff3w_data)

# Optionally, remove the temporary ZIP file
unlink(temp_zip)

######################################################
# Create a temporary file for the ZIP archive
temp_zip <- tempfile(fileext = ".zip")

# Download the ZIP file
download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip", temp_zip)

# List the files in the ZIP archive and assume the CSV is the first file
files_in_zip <- unzip(temp_zip, list = TRUE)$Name
csv_file_name <- files_in_zip[1]

# Extract the CSV file to a temporary directory
extracted_path <- tempdir()
unzip(temp_zip, files = csv_file_name, exdir = extracted_path)

# Construct the full path to the extracted CSV file
csv_path <- file.path(extracted_path, csv_file_name)

# Read the CSV file using fread
ff5d_data <- fread(csv_path)
head(ff5d_data)

# Optionally, remove the temporary ZIP file
unlink(temp_zip)

######################################################
# Create a temporary file for the ZIP archive
temp_zip <- tempfile(fileext = ".zip")

# Download the ZIP file
download.file("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_ME_Daily_CSV.zip", temp_zip)

# List the files in the ZIP archive and assume the CSV is the first file
files_in_zip <- unzip(temp_zip, list = TRUE)$Name
csv_file_name <- files_in_zip[1]

# Extract the CSV file to a temporary directory
extracted_path <- tempdir()
unzip(temp_zip, files = csv_file_name, exdir = extracted_path)

# Construct the full path to the extracted CSV file
csv_path <- file.path(extracted_path, csv_file_name)

# Read the CSV file using fread
pfosd_data <- fread(csv_path)
head(pfosd_data)

# Optionally, remove the temporary ZIP file
unlink(temp_zip)


colnames(pfosd_data)[1:20] <- c("Date", "<= 0", "Lo 30", "Med 40", "Hi 30", "Lo 20",
                                "Qnt 2", "Qnt 3", "Qnt 4", "Hi 20", "Lo 10",
                                "2-Dec", "3-Dec", "4-Dec", "5-Dec", "6-Dec",
                                "7-Dec", "8-Dec", "9-Dec", "Hi 10")
pfosd_data <- pfosd_data[-1, ]
head(pfosd_data)




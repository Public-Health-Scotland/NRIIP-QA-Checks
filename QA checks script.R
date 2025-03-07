# -	Move file manually in the actual folders 
# 
# CHECKS
# REQUESTS
# -	Check request file for blank in column O (request health descry) and N (Requesting healthboard name) , if blank requesting location code
# -	Dates format checks 
# -	Times check  
# -	Check against previous to check number of rows 
# -	Request ID no blanks
# -	Request ID no dups 
# -	Count n of rows for header
# -	Check against previous submission increment and add plus 1 
# -	Save as today’s date and new format 
# -	Clean column X ( clinical indications ) Remove all nonprintable characters from text 
# 
# MASTER
# -	Unique master ID 
# -	NO BLANKS – Modality code / description, Room ID (check for room id blanks and booked statuts) , Exam healthboard code, Exam healthboard description, Exam Location Code, Exam location description 
# -	Date checks 
# -	Time checks 
# -	If exam start date is blank use exam end date to populate 
# -	Check against previous to check number of rows
# -	save the new file properly 
# 
# 
# write code on checkes for all healthbo

#### NRIIP DQ CHECKS #####
#### SCRIPT COPYRIGHT ####
### AUTHORS :; EILISH MACKINNON & GABRY NAVARRO ###


# Load Libraries


library(phsmethods)
library(tidyverse)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(lubridate)
library(data.table)
library(openxlsx)
library(purrr)
library(arrow)
library(phsopendata)


error_path <- "/PHI_conf/diag_radiology/Data Submissions/Radiology - to be uploaded/A&A/"

# Read in csv
output <- read_csv("/PHI_conf/diag_radiology/Data Submissions/Radiology - to be uploaded/A&A/gabry_eilish.csv", skip = 1)
output <- clean_names(output)


# Todays date

date1 <- format(today(), "%Y%m%d")

# Create a workbook to save the errors into
wb = createWorkbook()


# Check for blanks in column O OR N(Request_health_desc/Requesting_health_code) to ensure that there are no blanks.
check_blanks <- output |>
  filter(is.na(requesting_health_description) | is.na(requesting_health_code)) |>
  # Match health board name onto code
  mutate(requesting_health_description = case_when(
    !is.na(requesting_health_code) & is.na(requesting_health_description) ~
      match_area(requesting_health_code)
  ))

#If blanks still remain, move these to a separate file
# Add a worksheet to the workbook

addWorksheet(wb, "blanks_checks")


# Write in the newly added sheet

writeData(wb, "blanks_checks",
          paste0("checking for blanks in column O and N, and saving out blanks"))

writeData(wb, "blanks_checks",
          x = check_blanks, startRow = 5)


# Check Column O or N(Request health desc/Request health code) Requesting health board code) do not contain any rogue information
#Creating HB code list & separate list for HB desc

hb_code_list = c("S08000015", "S08000016", "S08000017", "S08000018","S08000019", "S08000020", "S08000021","S08000022","S08000023", "S08000024",
                 "S08000025", "S08000026", "S08000027", "S08000028","S08000001", "S08000008")
hb_code_desc = c("Ayrshire and Arran", "Borders","Dumfries and Galloway", "Fife", "Forth Valley", "Grampian", "Greater Glasgow and Clyde",
                 "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside","Western Isles", "Golden Jubilee Hospital",
                 "The State Hospital")


# if not in col 0 (requesting health code ) then take row out and save it
#Filtering HB codes that are not in the hb code list
check_hb_name <- output |> filter(!requesting_health_code %in% hb_code_list)
# Read in hospital codes from opendata
# Always takes the latest version of the reference file
hospital_codes <- get_resource("c698f450-eeed-41a0-88f7-c1e40a568acc")


hospital_code_join <- check_hb_name |>
  #left join hosp codes from opendata to  our df
  left_join(hospital_codes |>
              #renaming column names in opendata file to match
              select(requesting_health_code = HospitalCode, HealthBoard_new = HealthBoard)) |>
  #distinct names means each code/hb must be unique
  distinct(requesting_health_code, HealthBoard_new)

#For codes that don't match, add column with HB Codes matching on from opendata file
output <- output |>
  left_join(hospital_code_join) |>
  # Creating new column. When code matches opendata source, in the new column when it is NA leave the code we have.
  #If it's not N/A take the new code
  mutate(requesting_health_code = case_when(
    is.na(HealthBoard_new) ~ requesting_health_code,
    TRUE ~ HealthBoard_new
  ))
# #filtering for true rogue information in column 
# rogue_codes <- output |>
#   filter(!requesting_health_code %in% hb_code_list)

# substituting old GGC code to new one and changing location codes to HB codes 
output_final <- output |> 
  mutate(requesting_health_code = ifelse(requesting_health_code == "S08000031", "S08000021", requesting_health_code)) |> 
  mutate(requesting_health_code = ifelse(requesting_health_code == "A227V", "S08000015", requesting_health_code)) |> 
  mutate(requesting_health_code = ifelse(requesting_health_code == "S226H", "S08000024", requesting_health_code)) |> 
  filter(requesting_health_code %in% hb_code_list)

# filter left over rogue rows 
rogue_rows <- output |> 
  mutate(requesting_health_code = ifelse(requesting_health_code == "S08000031", "S08000021", requesting_health_code)) |> 
  mutate(requesting_health_code = ifelse(requesting_health_code == "A227V", "S08000015", requesting_health_code)) |> 
  mutate(requesting_health_code = ifelse(requesting_health_code == "S226H", "S08000024", requesting_health_code)) |> 
  filter(!requesting_health_code %in% hb_code_list) 

#add work sheet 
addWorksheet(wb, "Rogue_information")

writeData(wb, "Rogue_information",
          paste0("Check Column O or N(Request health desc/Request health code) Requesting health board code) do not contain any rogue information"))

writeData(wb, "Rogue_information",
          x = rogue_rows, startRow = 5)

#Date Format, this will not check for errros but will convert dates to what we want.
#If we want checks this is not good 

output_final<- output_final |> 
  mutate(request_received_date = ymd(request_received_date)) |> 
  mutate(request_received_date = gsub("-", "", output_final$request_received_date))

#Time Checks
output_final <- output_final |> 
  mutate(request_received_time = format(request_received_time, "%H: %M: %S"))


# create a header


#### Generating File Header ####
# header row should say "RADIOLOGY", "REQUEST", "A", "[YYYYMMDD]", "submission n, "[nrow]"
row_header_request = tibble(
  title = "RADIOLOGY", "REQUEST", "A", date = date1, "INCREMENT", n_rows = nrow(output))

#### Save out file ####
fwrite(row_header_request,
       paste0(error_path, "RADIOLOGY","_REQUEST","_A_", date1, ".csv"),
       # Start the CSV file with header, no column names
       append = FALSE, col.names = FALSE)

# Add data below header
fwrite(output_final,
       paste0(error_path, "RADIOLOGY","_REQUEST","_A_", date1, ".csv"),
       append = TRUE, col.names = TRUE, row.names = FALSE, na = '')


#################################################################

# Save the workbook

saveWorkbook(wb, paste0("/PHI_conf/diag_radiology/Data Submissions/Radiology - to be uploaded/A&A/", "ERROR_RADIOLOGY_MASTER_A_", date1,
                        ".xlsx"))

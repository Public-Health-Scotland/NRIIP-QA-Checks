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

error_path <- "/PHI_conf/diag_radiology/Data Submissions/Radiology - to be uploaded/A&A"

# Read in csv
output <- read_csv("/PHI_conf/diag_radiology/Data Submissions/Radiology - to be uploaded/A&A/gabry_eilish.csv", skip = 1)
output <- clean_names(output)
View(output)

# Todays date

date1 <- format(today(), "%Y%m%d")

# Create a workbook to save the errors into
wb = createWorkbook()


# Check for blanks in column O OR N(Request_health_desc/Requesting_health_code) to ensure that there are no blanks.
check_1 <- output |>
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
          x = check_1, startRow = 5)


# Check Column O or N(Request health desc/Request health code) Requesting health board code) do not contain any rogue information
#Creating HB code list & separate list for HB desc

hb_code_list = c("S08000015", "S08000016", "S08000017", "S08000018","S08000019", "S08000020", "S08000021","S08000022","S08000023", "S08000024",
                 "S08000025", "S08000026", "S08000027", "S08000028","S08000001", "S08000008")
hb_code_desc = c("Ayrshire and Arran", "Borders","Dumfries and Galloway", "Fife", "Forth Valley", "Grampian", "Greater Glasgow and Clyde",
                 "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside","Western Isles", "Golden Jubilee Hospital",
                 "The State Hospital")

# if not in col 0 (requesting health code ) then take row out and save it

#dyplr for inside dataframe
library(phsopendata)

#Filtering HB codes that are not in the hb code list
check_2 <- output |> filter(!requesting_health_code %in% hb_code_list)
# Read in hospital codes from opendata
# Always takes the latest version of the reference file
hospital_codes <- get_resource("c698f450-eeed-41a0-88f7-c1e40a568acc")


check_2_match <- check_2 |>
  #left join hosp codes from opendata to  our df
  left_join(hospital_codes |>
              #renaming column names in opendata file to match
              select(requesting_health_code = HospitalCode, HealthBoard_new = HealthBoard)) |>
  #distinct names means each code/hb must be unique
  distinct(requesting_health_code, HealthBoard_new)

#For codes that don't match, add column with HB Codes matching on from opendata file
output2 <- output |>
  left_join(check_2_match) |>
  # Creating new column. When code matches opendata source, in the new column when it is NA leave the code we have.
  #If it's not N/A take the new code
  mutate(requesting_health_code = case_when(
    is.na(HealthBoard_new) ~ requesting_health_code,
    TRUE ~ HealthBoard_new
  ))
#filtering for true rogue information in column
after_care <- output2 |>
  filter(!requesting_health_code %in% hb_code_list)
addWorksheet(wb, "Rogue_information")

writeData(wb, "Rogue_information",
          paste0("Check Column O or N(Request health desc/Request health code) Requesting health board code) do not contain any rogue information"))

writeData(wb, "Rogue_information",
          x = after_care, startRow = 5)




#Filtering HB codes that are not in the hb code list
check_3 <- output |> filter(!requesting_health_description %in% hb_code_desc)

# Read in hospital codes from opendata
# Always takes the latest version of the reference file

check_3_match <- check_3 |>
  #left join hosp codes from opendata to  our df
  left_join(hospital_codes |>
              #renaming column names in opendata file to match
              select(requesting_health_description = HospitalName, HealthBoard_desc_new = HealthBoard)) |>
  #distinct names means each code/hb must be unique
  distinct(requesting_health_description, HealthBoard_desc_new)

#EM inserted step still not right - Need another step to convert hb codes in column N into to hb desc. Need to add another table don't have a list stating 15=A&A so we need to do this)
#Converting HB code to HB Name
hospital_desc <- get_resource("652ff726-e676-4a20-abda-435b98dd7bdc")
converting_desc_match <- check_3 |>
  left_join(hospital_desc |>
              select(requesting_health_description = HB, HealthBoard_desc_convert = HBName)) |>
  #distinct names means each code/hb must be unique
  distinct(requesting_health_description, HealthBoard_desc_convert)


#For codes that don't match, add column with HB Codes matching on from opendata file
output3 <- output |>
  left_join(check_3_match) |>
  # Creating new column. When code matches opendata source, in the new column when it is NA leave the code we have.
  #If it's not N/A take the new code
  mutate(requesting_health_description = case_when(
    is.na(HealthBoard_desc_new) ~ requesting_health_description,
    TRUE ~ HealthBoard_desc_new))

    #filtering for true rogue information in column
    after_care_desc <- output3 |>
    filter(!requesting_health_description %in% hb_code_desc)
  addWorksheet(wb, "Rogue_information_desc")

  writeData(wb, "Rogue_information_desc",
            paste0("Check Column N (Request health desc)do not contain any rogue information"))

  writeData(wb, "Rogue_information_desc",
            x = after_care_desc, startRow = 5)



  ###########################

  # Save the workbook

  saveWorkbook(wb, paste0("/PHI_conf/diag_radiology/Data Submissions/Radiology - to be uploaded/A&A/", "ERROR_RADIOLOGY_MASTER_A_", date1,
                          ".xlsx"))

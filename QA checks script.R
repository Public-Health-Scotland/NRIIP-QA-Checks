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


#EM NEEDS HELP!! Check Column O or N(Request health desc/Request health code) Requesting health board code) do not contain any rogue information
#REVIEW - Is there a way to check against the PHS List/phsmethods? Think it only works for HB codes, but could find list from National Ref Files

hb_code_list = c("S08000015", "S08000016", "S08000017", "S08000018","S08000019", "S08000020", "S08000021","S08000022","S08000023", "S08000024",
                 "S08000025", "S08000026", "S08000027", "S08000028","S08000001", "S08000008")
hb_code_desc = c("Ayrshire and Arran", "Borders","Dumfries and Galloway", "Fife", "Forth Valley", "Grampian", "Greater Glasgow and Clyde", 
                 "Highland", "Lanarkshire", "Lothian", "Orkney", "Shetland", "Tayside","Western Isles", "Golden Jubilee Hospital",
                 "The State Hospital")

# if not in col 0 (requesting health code ) then take row out and save it

#dyplr for inside dataframe 
library(phsopendata)

check_2 <- output |> filter(!requesting_health_code %in% hb_code_list)
# Read in hospital codes from opendata
# Al;ways takes the latest version of the reference file 
hospital_codes <- get_resource("c698f450-eeed-41a0-88f7-c1e40a568acc")

check_2_match <- check_2 |> 
  left_join(hospital_codes |> 
              select(requesting_health_code = HospitalCode, HealthBoard_new = HealthBoard)) |> 
  distinct(requesting_health_code, HealthBoard_new)


output2 <- output |> 
  left_join(check_2_match) |> 
  mutate(requesting_health_code = case_when(
    is.na(HealthBoard_new) ~ requesting_health_code,
    TRUE ~ HealthBoard_new
  ))
after_care <- output2 |> 
  filter(!requesting_health_code %in% hb_code_list)
addWorksheet(wb, "Rogue_information")

writeData(wb, "Rogue_information",
          paste0("Check Column O or N(Request health desc/Request health code) Requesting health board code) do not contain any rogue information"))

writeData(wb, "Rogue_information",
          x = check_2, skip = 5)


hb_code_list as.character(unique(output$requesting_health_code)) ~
  if (hb_code_list != requesting_health_code)
    
    #write.xlsx(check_2, file = "/PHI_conf/diag_radiology/Data Submissions/Radiology - to be uploaded/A&A/ERROR2_RADIOLOGY_MASTER_A_202410004.csv",
    #               rowNames=FALSE,colNames=FALSE,sep=",",na="",quote=TRUE)   
    
    
    #Date Format Checks
    #I'm still playing about with this
    date_check <- output |>
  mutate(request_received_date),
~format(.x, "%Y%m%d")


~format(.x, "%Y%m%d"))
as.Date('1/15/2001',format='%m/%d/%Y')

?data.table
#Time Checks

# create a header
# unsure of HB cypher/today/increment
fwrite(data.table(t(c("RADIOLOGY","REQUEST",health_board_cypher, today, "1", n))),
       paste0(output, "EIC_", health_board, "_DATA_", today, "_1.csv"),
       append = FALSE, col.names = FALSE)

# save out file
fwrite(save_data, paste0(output, "RADIOLOGY_REQUEST", health_board_cypher, today,".csv"),
       append = TRUE, col.names = TRUE, row.names = FALSE, na = '')
}

ok




###########################

# Save the workbook

saveWorkbook(wb, paste0("/PHI_conf/diag_radiology/Data Submissions/Radiology - to be uploaded/A&A/", "ERROR_RADIOLOGY_MASTER_A_", date1,
                        ".xlsx"))

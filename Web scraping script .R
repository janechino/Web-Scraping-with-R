#' ---
#' title: "Tesla 10K Report Analysis"
#' author: "Jane Chimnonyerem Chilaka"
#' date: "`r Sys.Date()`"
#' output: html_document
#' ---

knitr::spin("TSLA Rscript.R", knit = TRUE)
#' Install necessary packages (run this once, not every time)
#' install.packages("rvest")  #' For web scraping
#' install.packages("httr")   #' For making GET requests
#' install.packages("dplyr")   #' For data manipulation
#' install.packages("tidyverse") #' For data wrangling and visualization
#' install.packages("tidyr")
#' install.packages(stringr)


#' Load libraries
library(rvest)
library(magrittr)  # Load magrittr for the pipe operator %>%
library(httr)
library(dplyr)
library(tidyr)
library(stringr)



#' DATA SCRAPING
#' URL and User-Agent example
url <- "https://www.sec.gov/Archives/edgar/data/1318605/000162828024002390/tsla-20231231.htm"

#' Updated request with additional headers
response <- GET(
  url,
  add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9",
    `Accept-Encoding` = "gzip, deflate, br",
    `Connection` = "keep-alive",
    `Upgrade-Insecure-Requests` = "1"
  )
)



#' Check if the request was successful
if (status_code(response) == 200) {
  # Parse the HTML content
  webpage <- content(response, "text")
  webpage <- read_html(webpage)
} else {
  print("Failed to retrieve the page.")
}

print(status_code(response))


#' Extract financial tables (such as Balance Sheet, Income Statement, Cash Flow)
#' SEC 10-K filings typically have tables with financial statements. Extract all tables from the page.
tables <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

#' View the structure of the extracted tables (usually the first table might contain balance sheet)
print(length(tables))  # Number of tables found

lapply(tables, head)

print(head(tables[[23]]))  # View the first few rows of the first table (balance sheet)
print(head(tables[[25]]))  # View the first few rows of the second table (income statement)
print(head(tables[[27]]))  # View the first few rows of the third table (cash flow statement)

#' BALANCE SHEET ANALYSIS

#' Filter and clean the data from specific tables
#' Example: Clean and extract the Balance Sheet table
balance_sheet <- tables[[23]] %>%
  janitor::clean_names() 

#' Check the column names to ensure the correct column is being used
print(colnames(balance_sheet))
#' View the first few rows of the balance sheet data
head(balance_sheet)



#' Remove the unwanted column column
balance_sheet <- balance_sheet %>%
  select(-x2, -x3, -x4, -x6, -x7,-x8,-x9, -x10, -x12)



#' View the cleaned balance sheet data
print(balance_sheet)


#' Rename columns of a data frame
balance_sheet <- balance_sheet %>%
  rename(
    matric = x1,
    `USD values in millions (31 December 2023)` = x5,
    `USD values in millions (31 December 2022)` = x11
  )

#' Load necessary library
library(stringr)

#' Filter the balance sheet based on specific text in the 'matric' column
balance_sheet <- balance_sheet %>%
  mutate(matric = str_squish(matric)) %>%  # This trims excess spaces from the 'matric' column
  filter(!str_detect(matric, fixed("Commitments and contingencies (Note 15)", ignore_case = TRUE)))



#' View the unique values in the column to see what might be causing issues
str(balance_sheet$`USD values in millions (31 December 2023)`)

#' View the unique values in the column to see what might be causing issues
str(balance_sheet$`USD values in millions (31 December 2022)`)




#' Remove dollar signs, commas, and other non-numeric characters (if applicable)
balance_sheet$`USD values in millions (31 December 2023)` <- gsub("[^0-9.-]", "", balance_sheet$`USD values in millions (31 December 2023)`)
balance_sheet$`USD values in millions (31 December 2022)` <- gsub("[^0-9.-]", "", balance_sheet$`USD values in millions (31 December 2022)`)



#' Now convert the column to numeric
balance_sheet$`USD values in millions (31 December 2023)` <- as.numeric(balance_sheet$`USD values in millions (31 December 2023)`)
balance_sheet$`USD values in millions (31 December 2022)` <- as.numeric(balance_sheet$`USD values in millions (31 December 2022)`)

#' Check if the conversion was successful
summary(balance_sheet$`USD values in millions (31 December 2023)`)
summary(balance_sheet$`USD values in millions (31 December 2022)`)


#' Replace NA values with 0 (or any other value of your choice)
balance_sheet$`USD values in millions (31 December 2023)`[is.na(balance_sheet$`USD values in millions (31 December 2023)`)] <- " "

balance_sheet$`USD values in millions (31 December 2022)`[is.na(balance_sheet$`USD values in millions (31 December 2022)`)] <- " "




#'Analyze key metrics
#'To analyze key metrics and calculate key ratios by comparing assets and liabilities, let's focus on a few commonly used financial ratios, such as:Current Ratio, Quick Ratio, Debt to Equity Ratio, Return on Assets (ROA), Asset to Liability Ratio
#' Given Data
Total_current_assets_2023 <- 49616
Total_current_liabilities_2023 <- 28748
inventory_2023 <- 13626
total_assets_2023 <- 106618
total_liabilities_2023 <- 43009
total_equity_2023 <- 62634


#' 1. Current Ratio
current_ratio <- Total_current_assets_2023 / Total_current_liabilities_2023

#' 2. Quick Ratio
quick_ratio <- (Total_current_assets_2023 - inventory_2023) / Total_current_liabilities_2023

#' 3. Debt to Equity Ratio
debt_to_equity_ratio <- total_liabilities_2023 / total_equity_2023

#' 4. Asset to Liability Ratio
asset_to_liability_ratio <- total_assets_2023 / total_liabilities_2023


#' Print results
cat("Current Ratio: ", current_ratio, "\n")
cat("Quick Ratio: ", quick_ratio, "\n")
cat("Debt to Equity Ratio: ", debt_to_equity_ratio, "\n")
cat("Asset to Liability Ratio: ", asset_to_liability_ratio, "\n")



total_assets <- balance_sheet$`USD values in millions (31 December 2023)`[balance_sheet$matric == "Total Assets"] 
liabilities_to_assets <- total_liabilities_2023/ total_assets_2023
cat("Liabilities-to-Assets Ratio:", liabilities_to_assets, "\n")


#' Print the selected data
print(balance_sheet)


#' Identify high-level categories and subcategories
balance_sheet <- balance_sheet %>%
  mutate(Category = case_when(
    str_detect(matric, fixed("Assets", ignore_case = TRUE)) ~ "Assets",
    str_detect(matric, fixed("Liabilities", ignore_case = TRUE)) ~ "Liabilities",
    str_detect(matric, fixed("Equity", ignore_case = TRUE)) ~ "Equity",
    TRUE ~ "Subcategory"  # For everything else, treat it as a subcategory
  ))

#' Handle subcategories by grouping them under their respective categories
#' For simplicity, you could create a new column that describes the subcategory for rows under the same category.
balance_sheet <- balance_sheet %>%
  mutate(Subcategory = ifelse(Category == "Subcategory", matric, NA))

#' Replace NA in Subcategory with the category it belongs to
library(tidyr)
balance_sheet <- balance_sheet %>%
  fill(Subcategory, .direction = "down")  # Fill down the Subcategory names for each group

#' Clean up the data by removing any rows where the Category is "Subcategory" but the Subcategory is still NA
balance_sheet <- balance_sheet %>%
  filter(!is.na(Subcategory))


library(dplyr)

#' Add 'Asset' to the Category column for specific rows
balance_sheet <- balance_sheet %>%
  mutate(
    Category = case_when(
      matric %in% c("Cash and cash equivalents", "Short-term investments", "Accounts receivable, net", "Inventory", "Prepaid expenses and other current assets") ~ "Current assets",  # Adjust the Current asset-related values as needed
      matric %in% c("Operating lease vehicles, net", "Solar energy systems, net", "Property, plant and equipment, net", "Operating lease right-of-use assets", "Digital assets, net", "Intangible assets, net", "Goodwill", "Deferred tax assets", "and other current assets") ~ "assets",  # Adjust the asset-related values as needed
      matric %in% c("Accounts payable", "Accrued liabilities and other", "Deferred revenue", "Current portion of debt and finance leases") ~ "Current liabilities",  # Adjust the Current liabilities related values as needed
      matric %in% c("Debt and finance leases, net of current portion", "Deferred revenue, net of current portion", "Other long-term liabilities") ~ "liabilities",  # Adjust the liabilities-related values as needed
      matric %in% c("Preferred stock; $0.001 par value; 100 shares authorized; no shares issued and outstanding", "Common stock; $0.001 par value; 6,000 shares authorized; 3,185 and 3,164 shares issued and outstanding as of December 31, 2023 and 2022, respectively", "Additional paid-in capital", "Accumulated other comprehensive loss", "Retained earnings") ~ "Stockholders’ equity",  # Adjust the Stock holders' equity-related values as needed
      matric %in% c("Noncontrolling interests in subsidiaries", "Redeemable noncontrolling interests in subsidiaries") ~ "equity",  # Adjust the equity-related values as needed
      TRUE ~ Category # Keep the existing Category values for other rows
    )
  )

balance_sheet <- balance_sheet %>%
  mutate(Subcategory = matric)


library(dplyr)

#' List of rows to delete
rows_to_delete <- c(
  "Assets",
  "Current assets",
  "Liabilities",
  "Current liabilities",
  "Equity",
  "Stockholders’ equity",
  "Total current assets",
  "Total assets",
  "Total current liabilities",
  "Total liabilities",
  "Total stockholders’ equity",
  "Total liabilities and equity"
)

#' Filter out rows with these values in the matric column
balance_sheet <- balance_sheet %>%
  filter(!tolower(matric) %in% tolower(rows_to_delete))


#' Replace empty spaces in USD values in millions (31 December 2023) with a specific value
balance_sheet$`USD values in millions (31 December 2023)`[24] <- 3

#' Replace empty spaces in USD values in millions (31 December 2022) with a specific value
balance_sheet$`USD values in millions (31 December 2022)`[24] <- 3

#' Replace empty spaces in USD values in millions (31 December 2022) with a specific value
balance_sheet$`USD values in millions (31 December 2022)`[21] <- 5330

#' Replace empty spaces in USD values in millions (31 December 2022) with a specific value
balance_sheet$`USD values in millions (31 December 2023)`[21] <- 8153


#' Replace empty spaces in USD values in millions (31 December 2023) with a specific value
balance_sheet$`USD values in millions (31 December 2023)`[23] <- "0"


#' Replace empty spaces in USD values in millions (31 December 2022) with a specific value
balance_sheet$`USD values in millions (31 December 2022)`[23] <- "0"


#' Replace empty spaces in USD values in millions (31 December 2023) with a specific value
balance_sheet$`USD values in millions (31 December 2023)`[26] <- "-143"


#' Replace empty spaces in USD values in millions (31 December 2022) with a specific value
balance_sheet$`USD values in millions (31 December 2022)`[26] <- "-361"

#' View the restructured data
head(balance_sheet)
View(balance_sheet)

library(dplyr)

#' Remove empty rows
balance_sheet <- balance_sheet %>%
  filter(rowSums(is.na(.)) < ncol(.))


#' Remove rows where 'ColumnName' is an empty string
balance_sheet <- balance_sheet %>% filter(matric != "")


library(dplyr)

# Assuming 'tesla_balance_sheet' is your dataset
# Add 'date_2022' and 'date_2023' columns
balance_sheet <- balance_sheet %>%
  mutate(
    Date_2022 = "2022",  # Assign the 2022 date to all rows
    Date_2023 = "2023"   # Assign the 2023 date to all rows
  )

# Preview the updated dataset
head(balance_sheet)





#' CASH FLOW STATEMENT ANALYSIS

#' Example for cash flow statement;Clean and extract the Balance Sheet table
cash_flow_statement <- tables[[27]] %>%
  janitor::clean_names()
#' Remove the unwanted column column
cash_flow_statement <- cash_flow_statement %>%
  select(-x2, -x3, -x4, -x6, -x7, -x8, -x9, -x10, -x12, -x13, -x14, -x15, -x16, -x17, -x18)


#'Rename columns of a data frame
cash_flow_statement <- cash_flow_statement %>%
  rename(
    matric = x1,
    `USD values in millions (31 December 2023)` = x5,
    `USD values in millions (31 December 2022)` = x11
  )

#' View the cash flow data
head(cash_flow_statement)
str(cash_flow_statement)


#' Remove dollar signs, commas, and other non-numeric characters (if applicable)
cash_flow_statement$`USD values in millions (31 December 2023)` <- gsub("[^0-9.-]", "", cash_flow_statement$`USD values in millions (31 December 2023)`)
cash_flow_statement$`USD values in millions (31 December 2022)` <- gsub("[^0-9.-]", "", cash_flow_statement$`USD values in millions (31 December 2022)`)



#' Now convert the column to numeric
cash_flow_statement$`USD values in millions (31 December 2023)` <- as.numeric(cash_flow_statement$`USD values in millions (31 December 2023)`)
cash_flow_statement$`USD values in millions (31 December 2022)` <- as.numeric(cash_flow_statement$`USD values in millions (31 December 2022)`)

# Check if the conversion was successful
summary(cash_flow_statement$`USD values in millions (31 December 2023)`)
summary(cash_flow_statement$`USD values in millions (31 December 2022)`)


# Replace NA values with 0 (or any other value of your choice)
cash_flow_statement$`USD values in millions (31 December 2023)`[is.na(cash_flow_statement$`USD values in millions (31 December 2023)`)] <- " "
cash_flow_statement$`USD values in millions (31 December 2022)`[is.na(cash_flow_statement$`USD values in millions (31 December 2022)`)] <- " "


#' Identify rows for "Net cash provided by operating activities"
operating_activities_2023 <- cash_flow_statement$`USD values in millions (31 December 2023)`[cash_flow_statement$matric == "Net cash provided by operating activities"]
operating_activities_2022 <- cash_flow_statement$`USD values in millions (31 December 2022)`[cash_flow_statement$matric == "Net cash provided by operating activities"]

#' Print the results
cat("Operating Cash Flow for 2023:", operating_activities_2023, "\n")
cat("Operating Cash Flow for 2022:", operating_activities_2022, "\n")


#' Identify rows for "Net cash used in investing activities" and "Net cash provided by (used in) financing activities"
investing_activities_2023 <- cash_flow_statement$`USD values in millions (31 December 2023)`[cash_flow_statement$matric == "Net cash used in investing activities"] 
investing_activities_2022 <- cash_flow_statement$`USD values in millions (31 December 2022)`[cash_flow_statement$matric == "Net cash used in investing activities"]


financing_activities_2023 <- cash_flow_statement$`USD values in millions (31 December 2023)`[cash_flow_statement$matric == "Net cash provided by (used in) financing activities"]
financing_activities_2022 <- cash_flow_statement$`USD values in millions (31 December 2022)`[cash_flow_statement$matric == "Net cash provided by (used in) financing activities"]



#' Print the results
cat("Investing Cash Flow for 2023:", investing_activities_2023, "\n")
cat("Investing Cash Flow for 2022:", investing_activities_2022, "\n")


cat("Financing Cash Flow for 2023:", financing_activities_2023, "\n")
cat("Financing Cash Flow for 2022:", financing_activities_2022, "\n")




#' Identify rows for "Net increase (decrease) in cash and cash equivalents and restricted cash"
cash_change_2023 <- cash_flow_statement$`USD values in millions (31 December 2023)`[cash_flow_statement$matric == "Net increase (decrease) in cash and cash equivalents and restricted cash"] 
cash_change_2022 <- cash_flow_statement$`USD values in millions (31 December 2022)`[cash_flow_statement$matric == "Net increase (decrease) in cash and cash equivalents and restricted cash"] 


#' Print the results
cat("Net Cash Change for 2023:", cash_change_2023, "\n")
cat("Net Cash Change for 2022:", cash_change_2022, "\n")

print(cash_flow_statement)
View (cash_flow_statement)


#' Assuming you have a data frame called `cash_flow
cash_flow <- na.omit(cash_flow_statement)


library(dplyr)
library(stringr)

#' Assuming your balance_sheet already contains the cleaned data

#' Step 1: Identify high-level categories and subcategories
cash_flow_statement <- cash_flow_statement %>%
  mutate(Category = case_when(
    str_detect(matric, fixed("Cash Flows from Operating Activities", ignore_case = TRUE)) ~ "Operating Activities",
    str_detect(matric, fixed("Cash Flows from Investing Activities", ignore_case = TRUE)) ~ "Investing Activities",
    str_detect(matric, fixed("Cash Flows from Financing Activities", ignore_case = TRUE)) ~ "Financing Activities",
    TRUE ~ "Subcategory"  # For everything else, treat it as a subcategory
  ))

#' Step 2: Handle subcategories by grouping them under their respective categories
#' For simplicity, you could create a new column that describes the subcategory for rows under the same category.
cash_flow_statement <- cash_flow_statement %>%
  mutate(Subcategory = ifelse(Category == "Subcategory", matric, NA))

#' Step 3: Replace NA in Subcategory with the category it belongs to
library(tidyr)
cash_flow_statement <- cash_flow_statement %>%
  fill(Subcategory, .direction = "down")  # Fill down the Subcategory names for each group

#' Step 4: Clean up the data by removing any rows where the Category is "Subcategory" but the Subcategory is still NA
cash_flow_statement <- cash_flow_statement %>%
  filter(!is.na(Subcategory))



#' Add 'Operating Activities' to the Category column for specific rows
cash_flow_statement <- cash_flow_statement %>%
  mutate(
    Category = case_when(
      matric %in% c("Net income", "Depreciation, amortization and impairment", "Stock-based compensation", "Inventory and purchase commitments write-downs", "Foreign currency transaction net unrealized (gain) loss", "Deferred income taxes", "Non-cash interest and other operating activities", "Digital assets loss (gain), net", "Accounts receivable", "Inventory", "Operating lease vehicles", "Prepaid expenses and other assets", "Accounts payable, accrued and other liabilities", "Deferred revenue") ~ "Operating Activities",  # Adjust the Operating Activities-related values as needed
      matric %in% c("Purchases of property and equipment excluding finance leases, net of sales", "Purchases of solar energy systems, net of sales", "Purchases of digital assets", "Proceeds from sales of digital assets", "Purchase of intangible assets", "Purchases of investments", "Proceeds from maturities of investments", "Proceeds from sales of investments", "Receipt of government grants", "Business combinations, net of cash acquired") ~ "Investing Activities",  # Adjust the Investing Activities-related values as needed
      matric %in% c("Proceeds from issuances of debt", "Repayments of debt", "Collateralized lease repayments", "Proceeds from exercises of stock options and other stock issuances", "Principal payments on finance leases", "Debt issuance costs", "Proceeds from investments by noncontrolling interests in subsidiaries", "Distributions paid to noncontrolling interests in subsidiaries", "Payments for buy-outs of noncontrolling interests in subsidiaries") ~ "Financing Activities",  # Adjust the Financing Activities-related values as needed
      TRUE ~ Category # Keep the existing Category values for other rows
    )
  )

#' Specify the row numbers to delete
rows_to_delete <- c(1, 2, 3, 4, 6, 14, 22,34,45, 46, 47, 48, 49, 50, 51, 52, 53)  # Example row numbers

#' Remove those rows
cash_flow_statement <- cash_flow_statement %>%
  slice(-rows_to_delete)



#' Replace empty spaces in USD values in millions (31 December 2023) with a specific value
cash_flow_statement$`USD values in millions (31 December 2023)`[8]<- "0"
cash_flow_statement$`USD values in millions (31 December 2023)`[5] <- "-144"
cash_flow_statement$`USD values in millions (31 December 2022)`[6] <- "-196"
cash_flow_statement$`USD values in millions (31 December 2023)`[6] <- "-6349"
cash_flow_statement$`USD values in millions (31 December 2023)`[9] <- "-586"
cash_flow_statement$`USD values in millions (31 December 2022)`[9] <- "-1124"
cash_flow_statement$`USD values in millions (31 December 2022)`[10] <- "-6465"
cash_flow_statement$`USD values in millions (31 December 2023)`[10] <- "-1195"
cash_flow_statement$`USD values in millions (31 December 2022)`[11] <- "-1570"
cash_flow_statement$`USD values in millions (31 December 2023)`[11] <- "-1952"
cash_flow_statement$`USD values in millions (31 December 2023)`[12] <- "-2652"
cash_flow_statement$`USD values in millions (31 December 2022)`[12] <- "-3713"
cash_flow_statement$`USD values in millions (31 December 2023)`[16] <- "-8898"
cash_flow_statement$`USD values in millions (31 December 2022)`[16] <- "-7158"
cash_flow_statement$`USD values in millions (31 December 2023)`[17] <- "-1"
cash_flow_statement$`USD values in millions (31 December 2022)`[17]<- "-5"
cash_flow_statement$`USD values in millions (31 December 2023)`[18]<- "0"
cash_flow_statement$`USD values in millions (31 December 2022)`[20]<- "-9"
cash_flow_statement$`USD values in millions (31 December 2023)`[21]<- "-19112"
cash_flow_statement$`USD values in millions (31 December 2022)`[21] <- "-5835"
cash_flow_statement$`USD values in millions (31 December 2023)`[23]<- "138"
cash_flow_statement$`USD values in millions (31 December 2023)`[25] <- "-64"
cash_flow_statement$`USD values in millions (31 December 2023)`[28]<- "-1351"
cash_flow_statement$`USD values in millions (31 December 2022)`[28] <- "-3364"
cash_flow_statement$`USD values in millions (31 December 2023)`[31]<- "-464"
cash_flow_statement$`USD values in millions (31 December 2022)`[31] <- "-502"
cash_flow_statement$`USD values in millions (31 December 2023)`[32]<- "-29"
cash_flow_statement$`USD values in millions (31 December 2023)`[34]<- "-144"
cash_flow_statement$`USD values in millions (31 December 2022)`[34] <- "-157"
cash_flow_statement$`USD values in millions (31 December 2023)`[35]<- "-54"
cash_flow_statement$`USD values in millions (31 December 2022)`[35] <- "-45"
cash_flow_statement$`USD values in millions (31 December 2022)`[18] <- "0"
cash_flow_statement$`USD values in millions (31 December 2023)`[19] <- "0"
cash_flow_statement$`USD values in millions (31 December 2023)`[20] <- "0"
cash_flow_statement$`USD values in millions (31 December 2022)`[20] <- "-9"
cash_flow_statement$`USD values in millions (31 December 2022)`[23] <- "0"
cash_flow_statement$`USD values in millions (31 December 2023)`[24] <- "0"
cash_flow_statement$`USD values in millions (31 December 2022)`[25] <- "0"
cash_flow_statement$`USD values in millions (31 December 2022)`[27] <- "0"
cash_flow_statement$`USD values in millions (31 December 2023)`[29] <- "0"
cash_flow_statement$`USD values in millions (31 December 2022)`[29] <- "0"
cash_flow_statement$`USD values in millions (31 December 2022)`[32] <- "0"
cash_flow_statement$`USD values in millions (31 December 2022)`[33] <- "0"
cash_flow_statement$`USD values in millions (31 December 2023)`[33] <- "0"


#' View the updated dataset
head(cash_flow_statement)

library(dplyr)

# Assuming 'tesla_balance_sheet' is your dataset
# Add 'date_2022' and 'date_2023' columns
cash_flow_statement<- cash_flow_statement %>%
  mutate(
    Date_2022 = "2022",  # Assign the 2022 date to all rows
    Date_2023 = "2023"   # Assign the 2023 date to all rows
  )

# Preview the updated dataset
head(cash_flow_statement)




#' INCOME STATEMENT ANALYSIS

#' Step 4: Repeat similar steps for the income statement and cash flow statement (assuming they are in other tables)
#' Example for income statement; Clean and extract the income statement table 
income_statement <- tables[[25]] %>%
  janitor::clean_names()

#' Remove the unwanted column column
income_statement <- income_statement %>%
  select(-x2, -x3, -x4, -x6, -x7, -x8, -x9, -x10, -x12, -x13, -x14, -x15, -x16, -x17, -x18)



#' Rename columns of a data frame
income_statement <- income_statement %>%
  rename(
    matric = x1,
    `USD values in millions (31 December 2023)` = x5,
    `USD values in millions (31 December 2022)` = x11
  )


#' Remove dollar signs, commas, and other non-numeric characters (if applicable)
income_statement$`USD values in millions (31 December 2023)` <- gsub("[^0-9.-]", "", income_statement$`USD values in millions (31 December 2023)`)
income_statement$`USD values in millions (31 December 2022)` <- gsub("[^0-9.-]", "", income_statement$`USD values in millions (31 December 2022)`)


#' Now convert the column to numeric
income_statement$`USD values in millions (31 December 2023)` <- as.numeric(income_statement$`USD values in millions (31 December 2023)`)
income_statement$`USD values in millions (31 December 2022)` <- as.numeric(income_statement$`USD values in millions (31 December 2022)`)

#' Check if the conversion was successful
summary(income_statement$`USD values in millions (31 December 2023)`)
summary(income_statement$`USD values in millions (31 December 2022)`)


#' Check if the conversion was successful
str(income_statement$`USD values in millions (31 December 2023)`)
str(income_statement$`USD values in millions (31 December 2022)`)



#' Replace NA values with 0 (or any other value of your choice)
income_statement$`USD values in millions (31 December 2023)`[is.na(income_statement$`USD values in millions (31 December 2023)`)] <- " "
income_statement$`USD values in millions (31 December 2022)`[is.na(income_statement$`USD values in millions (31 December 2022)`)] <- " "


#' Replace empty spaces in USD values in millions (31 December 2023) with a specific value
income_statement$`USD values in millions (31 December 2022)`[8] <- "0"


#' Specify the row numbers to delete
rows_to_delete <- c(1, 2, 3, 4, 5)  # Example row numbers

#' Remove those rows
income_statement <- income_statement %>%
  slice(-rows_to_delete)

income_statement$`USD values in millions (31 December 2023)`[5]<- "-23"
income_statement$`USD values in millions (31 December 2022)`[1]<- "-392"
income_statement$`USD values in millions (31 December 2022)`[5]<- "31"
income_statement$`USD values in millions (31 December 2022)`[2]<- "-23"

print(income_statement)


library(dplyr)

# Assuming 'tesla_balance_sheet' is your dataset
# Add 'date_2022' and 'date_2023' columns
income_statement<- income_statement %>%
  mutate(
    Date_2022 = "2022",  # Assign the 2022 date to all rows
    Date_2023 = "2023"   # Assign the 2023 date to all rows
  )

# Preview the updated dataset
head(income_statement)


#' SAVE DATA
#' Save cleaned dataset as CSV
write.csv(balance_sheet, "balance_sheet.csv", row.names = FALSE)
write.csv(cash_flow_statement, "cash_flow_statement.csv", row.names = FALSE)
write.csv(income_statement, "income_statement.csv", row.names = FALSE)

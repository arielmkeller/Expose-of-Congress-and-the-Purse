# Earmark Data Notes

## Files Read
- Community Project Funding Request Table - 06222021.csv_.xlsx | sheet: Appropriations Project Requests
- FY 2024 House CPF Requests 2023-04-27 (430pm).xlsx | sheet: FY24 House CPF Requests
- FY23 CPF Requests 2022-06-06.xlsx | sheet: FY23 CPF Requests 2022-06-06
- fy25-house-cpfs-as-requested-06.28.2024.xlsx | sheet: asRequestedCPFs_FY25_6.20.24

## Column Mapping
- Community Project Funding Request Table - 06222021.csv_.xlsx
  member_name: Member Firstname, Member Lastname
  state: NA
  district: NA
  subcommittee: Subcommittee
  recipient_name: NA
  project_description: Program/Language/Project Title
  amount_requested: Amount Requested for FY22
- FY 2024 House CPF Requests 2023-04-27 (430pm).xlsx
  member_name: Member First Name, Member Last Name
  state: NA
  district: District
  subcommittee: Subcommittee
  recipient_name: Recipient
  project_description: Project Purpose
  amount_requested: Amount Requested
- FY23 CPF Requests 2022-06-06.xlsx
  member_name: Member
  state: NA
  district: NA
  subcommittee: Subcommittee
  recipient_name: Recipient
  project_description: Request
  amount_requested: Amount Requested
- fy25-house-cpfs-as-requested-06.28.2024.xlsx
  member_name: Member First Name, Member Last Name
  state: NA
  district: District
  subcommittee: Subcommittee
  recipient_name: Recipient
  project_description: Project Purpose
  amount_requested: Amount Requested

## Unmatched Columns
- Community Project Funding Request Table - 06222021.csv_.xlsx: Recipient Address; Explanation
- FY 2024 House CPF Requests 2023-04-27 (430pm).xlsx: Party; Recipient Address; Member Website
- FY23 CPF Requests 2022-06-06.xlsx: Explanation
- fy25-house-cpfs-as-requested-06.28.2024.xlsx: Party; Recipient Address; Member Website

## Ambiguities
- none

## Rows Dropped
- Community Project Funding Request Table - 06222021.csv_.xlsx: 0
- FY 2024 House CPF Requests 2023-04-27 (430pm).xlsx: 0
- FY23 CPF Requests 2022-06-06.xlsx: 0
- fy25-house-cpfs-as-requested-06.28.2024.xlsx: 0

## Fiscal Year
- Community Project Funding Request Table - 06222021.csv_.xlsx: fiscal_year from column_name
- FY 2024 House CPF Requests 2023-04-27 (430pm).xlsx: fiscal_year from filename
- FY23 CPF Requests 2022-06-06.xlsx: fiscal_year from filename
- fy25-house-cpfs-as-requested-06.28.2024.xlsx: fiscal_year from filename

## Amount Parse Issues
- Community Project Funding Request Table - 06222021.csv_.xlsx: none
- FY 2024 House CPF Requests 2023-04-27 (430pm).xlsx: none
- FY23 CPF Requests 2022-06-06.xlsx: 1 non-numeric values. Examples: $-
- fy25-house-cpfs-as-requested-06.28.2024.xlsx: none

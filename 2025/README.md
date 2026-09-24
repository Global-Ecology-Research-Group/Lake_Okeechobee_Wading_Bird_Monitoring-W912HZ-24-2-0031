This repository contains all the code and data used to produce tables and figures presented in the 2025 Wading Bird Monitoring Annual Report (W912HZ-24-2-0031) submitted to the Army Corps of Engineers.

### Code

This folder contains the R script `2025_annual_report.R` which contains all the code used to summarize the data.

### Data

This folder contains all the data used in the `2025_annual_report.R` script. The files are as follows:

**nest_check_spatial_files:** Folder containing shapefiles of nest check transects and polygons of approximate colony extent.

**2025_Colonies.csv:** The colonies observed during 2025 wading bird monitoring, their geographic coordinates in Latitude and Longitude, and "Y" (yes) or "N" (no) whether we conducted nest checks on that colony.

**aerial_survey_route.shp:** Shapefile of aerial track route.

**Historic_Nest_Counts_Lake_O.csv:** Specific-specific peak nest counts by species, colony, and year (2008-2023). This data was extracted from the South Florida Wading Bird Report, published annually by the South Florida Water Management District (<https://www.sfwmd.gov/documents-by-tag/wadingbirdreport>).

**Historic_Nest_Counts_Lake_O_with_2025.csv:** This is an export of the R script. It contains the 2025 species-specific nests counts in addition to the data in Historic_Nest_Counts_Lake_O.csv file.

**historic_nest_success.csv:** This is nest success values by year and species, extracted from the South Florida Wading Bird Report.

**historic_timing.csv:** This is the median nest initiation date by year and species, extracted from the South Florida Wading Bird Report.

**rainfall.csv:** This is daily mean rainfall in inches for the stations located in the pelagic zone (L001, L005, L006, LZ40) of Lake Okeechobee for 2025. Data was obtained from the South Florida Water Management District's DBHYDRO Insights database.

**species_codes.csv:** In some databases, we use four letter codes for species names. This data frame defines the common name that is associated with each species code.

**Wading_Bird_Aerial_Ground_Survey_Database.csv:** This contains all nest counts made during aerial and ground surveys.

**Wading_Bird_Nest_Check_Database.csv:** This contains data on nest status and egg, hatching, and fledgling counts taken during nest surveys.

**water_stage:** This contains average daily water level in Lake Okeechobee in feet between 1979 and 2025. For the report, we focus on stations located in the pelagic zone (L001, L005, L006, LZ40). The elevation data are relative to NGVD 1928. This data was obtained from the South Florida Water Management District's DBHYDRO Insights database.

### Figures

This folder contains all the figures that are exported from the `2025_annual_report.R` script and that are presented in the annual report.

### Tables

This folder contains the tables exported from the `2025_annual_report.R` script and that are presented in the annual report.

# STA380-Project
## File Structure

The repo is structured as:

- `scripts` contains all R scripts used to obtain and process the data.
  - `00_download_data.R` downloads the raw World Population Prospects (WPP 2024) dataset from the United Nations website and saves it locally (raw data files are not tracked in the repository).
  - `01_data_filtering.R` cleans and filters the raw WPP data to produce a country-level analysis.
- `submissions` contains all materials required for course submission.
  - `submissions/Data/` contains the cleaned, analysis-ready datasets used in the project.
  - `submissions/GroupFormation/` contains group informations.
  - `submissions/Proposal/` contains the project proposal files, including the rendered PDF, source files, and bibliography.

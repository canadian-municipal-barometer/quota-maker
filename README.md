# Census quotas

Builds survey quotas (sex, age, degree, etc.) from the Census Profile using the
[cancensus](https://mountainmath.github.io/cancensus/) R package. Copy this
folder into a study, edit the settings at the top of a script, and run it from
the folder. Results are written to the same folder.

| File                       | What it does                                                                                                                                                                 |
| -------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `census-quotas-csd.R`      | Quotas for each region listed in `municipalities.csv` (CSDs, CDs or provinces). Run with `Rscript census-quotas-csd.R`. Writes `census-raw.csv` and `quotas.csv`.            |
| `census-quotas-national.R` | One Canada-wide set of quotas for a total sample size set by `CAP`. Run with `Rscript census-quotas-national.R`. Writes `census-raw-national.csv` and `quotas-national.csv`. Set `ENGLISH_ONLY <- TRUE` for quotas among people who can speak English (English only, or English and French); this pulls Statistics Canada tables 98-10-0619 and 98-10-0365, needs the `httr2` package, and writes `census-raw-national-english.csv` and `quotas-national-english.csv`. |
| `municipalities.csv`       | Example input for `census-quotas-csd.R`: `name`, `census_id` (7-digit CSD, 4-digit CD or 2-digit province code) and `cap` (sample size for that region).                     |

Both scripts need a CensusMapper API key. They currently set one at the top
with `set_cancensus_api_key()`; you can instead set `CM_API_KEY` in
`~/.Renviron`.

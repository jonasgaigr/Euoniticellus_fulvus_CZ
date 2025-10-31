# Euoniticellus_fulvus_CZ

**Compiling faunistic records of *Euoniticellus fulvus* (Goeze, 1777) in Czechia**

Compiling the faunistic records of *Euoniticellus fulvus* (Goeze, 1777) in Czechia and their import into the [Species Occurrence Database](https://portal23.nature.cz/nd/find.php). Data mining recent and historic data from published literature.

---

## Repository overview

The repository is organised to separate raw/derived data, code, literature review and generated outputs. Main folders (as found in the repository):

* `Data/ Input/` — raw inputs and intermediate data files used to compile occurrences (source records, CSV/XLS, extracted coordinates).
* `Literature_rev/` — scanned/collected literature, extracted references and notes used during data mining.
* `R/` — R scripts used to parse, clean, harmonise and export records; analysis and helper functions.
* `Outputs/` — generated products (tables, cleaned CSVs, figures, draft exports for the [Species Occurrence Database](https://portal23.nature.cz/nd/find.php)).
* `Text/` — manuscript or report drafts, RMarkdown/TeX or final text files describing results.

Top-level files you will find in the repository include the project RStudio file `Euoniticellus_fulvus_CZ.Rproj`, a minimal `README.md` and a `.gitignore` file.

---

## Purpose and scope

The project aims to:

* Collate historic and recent occurrence records of *E. fulvus* in Czechia from literature and data providers.
* Clean and harmonise records (standardise coordinates, dates, collectors, locality strings).
* Prepare a high-quality dataset for upload to the national Species Occurrence Database (portal23.nature.cz) and to support a short faunistic report or manuscript.

Geographic scope: Czechia.

Taxonomic scope: a single focal species — *Euoniticellus fulvus*.

---

## Main scripts (what to run and in which order) - TO BE COMPILED

> The following is a suggested order. Exact script names depend on the files present in `R/` — adjust as needed.

1. `00_setup.R` (or similar): install / load required packages and set paths.
2. `01_read_inputs.R`: read all raw source files from `Data/ Input/` and standardise column names.
3. `02_clean_localities.R`: functions to clean locality strings and extract coordinates.
4. `03_georeference_and_validate.R`: georeferencing helpers and spatial checks (e.g. using `sf`).
5. `04_compile_records.R`: join records, deduplicate, harmonise dates and collectors.
6. `05_prepare_portal23_export.R`: prepare final CSV/Excel export matching portal23 import specification.
7. `06_figures_and_tables.R` (optional): generate maps and summary tables for Outputs and manuscript.

**How to run**

Open the R project (`Euoniticellus_fulvus_CZ.Rproj`) in RStudio, then source the scripts in the order above (or run a master `run_all.R` if present). Example from an R console:

```r
# set working dir to project root (RStudio will do this if you open the .Rproj)
source("R/00_setup.R")
source("R/01_read_inputs.R")
# ...and so on
```

---

## Dependencies

The analysis relies on common R packages for data wrangling, spatial operations and reporting. Make sure you have (at least) the following installed:

* `tidyverse` (or `dplyr`, `readr`, `tibble`, `stringr`, `purrr`)
* `sf` (spatial validation, if geospatial operations are used)
* `readxl` / `writexl` (if reading/writing Excel files)
* `lubridate` (date handling)
* `janitor` (cleaning column names)
* `knitr` / `rmarkdown` (if there are RMarkdown documents)

If the repository contains specific helper packages or custom functions, they should be documented in `R/00_setup.R`.

---

## Data provenance and attribution

All extracted records should keep a clear provenance trail: the original literature citation (author, year, page), collector/observer name, date (as precisely provided) and how the coordinates were obtained (reported vs. georeferenced). The folder `Literature_rev/` contains the bibliography and notes used during data mining — keep those records intact for full transparency.

Before any public data release, double-check licensing and permissions for each source (especially for records obtained from third-party datasets or digitised literature).

---

## Outputs

The `Outputs/` folder should contain:

* Cleaned occurrence table(s) ready for portal23 import (CSV/Excel).
* Mapping figures and summary tables used in the manuscript/report.
* A small changelog or README inside `Outputs/` explaining which script generated each file and the generation date.

---

## Manuscript / Text

The `Text/` folder includes draft manuscript(s) and supporting notes. If RMarkdown is used, the compiled HTML/PDF outputs can be added to `Outputs/` for convenience.


## Contact and authorship

Repository owner: jonasgaigr (GitHub). For questions, issues or data-provenance clarifications, please open a GitHub Issue or contact the repository owner.

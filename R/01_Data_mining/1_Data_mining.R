#----------------------------------------------------------#
# Mertlik -----
#----------------------------------------------------------#

#--------------------------------------------------#
## 0) Define input files ----
#--------------------------------------------------#
pdf_files <- c(
  "Literature_rev/Mertlik_2020_Elateridarium.pdf",
  "Literature_rev/Mertlik_2021_Elateridarium.pdf"
)

#--------------------------------------------------#
## Loop through each PDF ----
#--------------------------------------------------#
for (pdf_file in pdf_files) {
  
  cat("Processing:", pdf_file, "\n")
  
  #--------------------------------------------------#
  ## 1) Load PDF ----
  #--------------------------------------------------#
  pages <- pdftools::pdf_text(pdf_file)
  lines <- unlist(strsplit(pages, "\n"))
  lines <- stringr::str_trim(lines)
  lines <- lines[lines != ""]
  
  #--------------------------------------------------#
  ## 2) Detect species headers ----
  #--------------------------------------------------#
  # Species names in format: "Genus species (Author, Year)" or "Genus (Subgenus) species (Author, Year)"
  sp_pattern <- "^([A-Z][a-z]+(?:\\s\\([^)]*\\))?\\s[a-z]+)\\s\\(.*\\)$"
  
  # Get all headers
  species_headers <- lines[stringr::str_detect(lines, sp_pattern)] %>% stringr::str_trim()
  
  # Build species map directly as separators
  species_map <- tibble::tibble(
    line_idx  = which(lines %in% species_headers),
    Druh_full = species_headers
  ) %>%
    dplyr::arrange(line_idx) %>%
    dplyr::mutate(end_idx = dplyr::lead(line_idx, default = length(lines) + 1) - 1)
  
  #--------------------------------------------------#
  ## 3) Group by occurrences ----
  #--------------------------------------------------#
  records <- list()
  buf_lines <- NULL
  buf_start <- NULL
  
  for (i in seq_along(lines)) {
    ln <- lines[i]
    if (stringr::str_detect(ln, "^\\d{4}:")) {
      if (!is.null(buf_lines)) {
        records <- append(records, list(list(text = buf_lines, start = buf_start)))
      }
      buf_lines <- ln
      buf_start <- i
    } else if (!is.null(buf_lines)) {
      buf_lines <- c(buf_lines, ln)
    }
  }
  
  # add the final record
  if (!is.null(buf_lines)) {
    records <- append(records, list(list(text = buf_lines, start = buf_start)))
  }
  
  #--------------------------------------------------#
  ## 4) Build dataframe and assign species by intervals ----
  #--------------------------------------------------#
  df <- tibble::tibble(
    raw   = vapply(records, function(x) paste(x$text, collapse = " "), ""),
    start = vapply(records, `[[`, NA_integer_, "start")
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      Druh_full = {
        hit <- species_map %>%
          dplyr::filter(start >= line_idx & start <= end_idx)
        if (nrow(hit) == 1) hit$Druh_full else NA_character_
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(Druh_full)) %>%
    dplyr::mutate(
      Druh = stringr::str_extract(Druh_full, "^[A-Z][a-z]+(\\s\\([^)]*\\))?\\s[a-z]+"),
      Popis_druhu = stringr::str_extract(Druh_full, "(?<=\\().*(?=\\))"),
      Ctverec  = stringr::str_extract(raw, "^\\d{4}"),
      Text     = stringr::str_trim(stringr::str_remove(raw, "^\\d{4}:")),
      Lokalita = stringr::str_extract(Text, "^.*?(?=\\d| \\()"),
      Datum    = stringr::str_extract(raw, "\\d{1,2}\\.\\d{1,2}\\.\\d{4}"),
      Pocet    = stringr::str_extract(raw, "\\d+ ?ex\\.|\\d+ ?♂|\\d+ ?♀"),
      Substrat = stringr::str_extract(raw, "Cervus|Equus|ovčí pastvina|světelná UV past"),
      Lat      = as.numeric(stringr::str_extract(raw, "\\d{2}\\.\\d+(?=N)")),
      Lon      = as.numeric(stringr::str_extract(raw, "\\d{2}\\.\\d+(?=E)")),
      Zpusob   = dplyr::case_when(
        stringr::str_detect(raw, "observ") ~ "observ.",
        stringr::str_detect(raw, "leg\\. et coll\\.") ~ "leg. et coll.",
        stringr::str_detect(raw, "leg\\.") & stringr::str_detect(raw, "det\\. et coll\\.") ~ "leg., det. et coll.",
        TRUE ~ NA_character_
      ),
      Autor = dplyr::case_when(
        stringr::str_detect(raw, "Mertlik")    ~ "J. Mertlik",
        stringr::str_detect(raw, "Hron")       ~ "V. Hron",
        stringr::str_detect(raw, "Mikát")      ~ "M. Mikát",
        stringr::str_detect(raw, "Brabec")     ~ "M. Brabec",
        stringr::str_detect(raw, "Jiříček")    ~ "V. Jiříček",
        stringr::str_detect(raw, "Král")       ~ "D. Král",
        stringr::str_detect(raw, "Pelikán")    ~ "J. Pelikán",
        stringr::str_detect(raw, "Resl")       ~ "J. Resl",
        stringr::str_detect(raw, "Trávníček")  ~ "P. Trávníček",
        stringr::str_detect(raw, "Bunalski")   ~ "M. Bunalski",
        TRUE                                   ~ NA_character_
      ),
      Poznamka = stringr::str_extract(raw, "(lezl[^,]+|Březový potok[^,]+|pastvina[^,]+)"),
      
      # ✅ NEW: extract short locality (up to first comma or semicolon)
      Lokalita_short = stringr::str_extract(Lokalita, "^[^,;]+")
    ) %>%
    dplyr::select(
      Druh, Popis_druhu, Ctverec, Lokalita, Lokalita_short, Text, Datum,
      Pocet, Substrat, Lat, Lon, Zpusob, Autor, Poznamka
    )
  
  #--------------------------------------------------#
  ## 5) Export ----
  #--------------------------------------------------#
  output_file <- paste0(
    "Outputs/Data/nalezy_clean_",
    tools::file_path_sans_ext(basename(pdf_file)),
    ".xlsx"
  )
  
  openxlsx::write.xlsx(df, output_file)
  cat("✅ Exported to:", output_file, "\n\n")
}

googledrive::drive_ls(
  googledrive::as_id("https://drive.google.com/drive/u/0/folders/10LoZ3M6qQYVwOkO7XAbZJjWfZhw3hO6D")
)

freq_local_fname <- file.path(tempdir(), "Downslope_freq_2024.xlsx")
pres_local_fname <- file.path(tempdir(), "Downslope_pres_2024.xlsx")
tel_10x10_local_fname <- file.path(tempdir(), "us_dev_tel_10m-x-10m-plot_survey-observations_raw_2013.csv")

googledrive::drive_download(file = googledrive::as_id("1Xmih9fo41LqR8y2fSflCn1-5ELr3IPOA"), path = freq_local_fname, overwrite = TRUE)
googledrive::drive_download(file = googledrive::as_id("1Z7mKhZ5L-ZtLp0yWE3_eSwusC9AUhN0g"), path = pres_local_fname, overwrite = TRUE)
# googledrive::drive_download(file = googledrive::as_id("10Qh2YnAUwJLVPYnp2R4k-lgZIzsgSRZm"), path = tel_10x10_local_fname)

tel <- readr::read_csv(tel_10x10_local_fname)

pres <- readxl::read_excel(pres_local_fname) |> dplyr::rename(comments = Comments) |> dplyr::mutate(data_subtype = "presence")
freq <- readxl::read_excel(freq_local_fname) |> dplyr::mutate(data_subtype = "pointing_hits")

downslope <- rbind(pres, freq) |> 
  dplyr::rename(
    transect = `transect#`, 
    peak = summit, 
    target_region = target_reg
  ) |>  
  dplyr::mutate(
    target_region = stringr::str_trim(tolower(target_region)),
    peak = tolower(peak),
    year = lubridate::year(date),
    country = "us",
    aspect = ifelse(test = peak == "cpt", yes = "e", no = "se"),
    data_type = "downslope"
  ) |> 
  dplyr::select(country, target_region, peak, year, data_type, data_subtype, aspect, species, tidyselect::starts_with("seg_")) |> 
  dplyr::arrange(country, target_region, peak, year, data_type, aspect, species, dplyr::desc(data_subtype)) |> 
  dplyr::mutate(
    dirname = glue::glue(
      "{country}_{target_region}_{peak}_{data_type}_{aspect}_raw"
    ),
    filename = glue::glue(
      "{country}_{target_region}_{peak}_{data_type}_{aspect}_survey-observations_raw_{year}.csv"
    )
  ) |> 
  dplyr::group_by(filename) |> 
  dplyr::group_split()

split_downslope_data <- function(x) {
  
  out_csv <- x |> 
    dplyr::select(-dirname, -filename)
  
  dir <- unique(x$dirname)
  file <- file.path(dir, unique(x$filename))
  
  dir.create(dir, showWarnings = FALSE)
  
  readr::write_csv(
    x = out_csv, file = file, append = FALSE
  )
  
  unique(x$filename)
  
}

purrr::map(
  .x = downslope, 
  .f = split_downslope_data, 
  .progress = TRUE
)

split_downslope_data(downslope[[1]])

length(downslope)
downslope[[1]]

downslope_dirnames <-
  downslope |> 
  dplyr::select(country, target_region, peak, data_type, aspect) |> 
  unique() |> 
  dplyr::mutate(dirname = glue::glue("{country}_{target_region}_{peak}_{data_type}_{aspect}_raw"))

downslope_dirnames


downslope |> 
  dplyr::filter(is.na(year))

freq |> dplyr::
  
# Census quotas
#
# Builds per-municipality survey quotas from the Census Profile via cancensus R
# package.
# Copy this folder into a study, edit the settings, and run from the folder:
#   Rscript census-quotas.R

suppressPackageStartupMessages({
  library(cancensus)
  library(dplyr)
  library(tidyr)
  library(readr)
})

# Reed's key
set_cancensus_api_key("CensusMapper_e8117c5ce4c23c5b5ce1fb530f7aea86")

# Used in QUOTAS below; resolved to census vectors after the settings.
REST <- "REST"
ages <- function(from, to = Inf, sex = c("Total", "Male", "Female")) {
  # create a list with a class attribute of "ages"
  # Specifies an age range and sex for later resolution to census vectors.
  structure(list(from = from, to = to, sex = match.arg(sex)), class = "ages")
}

# ---- settings ----------------------------------------------------------------
DATASET <- "CA21"
MUNICIPALITIES <- "municipalities.csv" # name, census_id, cap (all required)
OUT_DIR <- "output"

# Each dimension lists mutually exclusive categories. A category is a vector of
# census ids (summed) or ages(from, to, sex). REST is `.base` minus the other
# categories. Find ids with find_census_vectors("bachelor", dataset = DATASET).
QUOTAS <- list(
  sex = list(
    Male = ages(18, Inf, sex = "Male"),
    Female = ages(18, Inf, sex = "Female")
  ),
  age = list(
    "18-29" = ages(18, 29),
    "30-44" = ages(30, 44),
    "45-59" = ages(45, 59),
    "60+" = ages(60, Inf)
  ),
  degree = list(
    .base = "v_CA21_5817", # 15+ in private households
    Yes = "v_CA21_5847", # bachelor's degree or higher
    No = REST
  )
)

# ---- helpers -----------------------------------------------------------------
stop_if_any <- function(bad, msg) {
  if (length(bad)) stop(msg, ": ", paste(unique(bad), collapse = ", "))
}

# Coarsest Census age groups that exactly cover from..to, e.g. 18-29 is
# 18, 19, 20 to 24, 25 to 29. Fewer cells means less rounding noise.
age_labels <- function(from, to) {
  out <- character()
  a <- from
  while (a <= to) {
    if (is.infinite(to) && a %in% c(65, 85, 100)) {
      return(c(out, paste(a, "years and over")))
    }
    if (a >= 100) {
      stop("ages over 99 are only published as 100+; use to = Inf")
    }
    if (a %% 5 == 0 && a + 4 <= to) {
      out <- c(out, sprintf("%d to %d years", a, a + 4))
      a <- a + 5
    } else {
      out <- c(out, if (a == 0) "Under 1 year" else as.character(a))
      a <- a + 1
    }
  }
  out
}

# Looked up by label, not vector number, so nothing depends on offsets.
resolve <- function(x, catalogue) {
  if (!inherits(x, "ages")) {
    return(x)
  }
  labels <- age_labels(x$from, x$to)
  hits <- catalogue |>
    filter(
      grepl("; Total - Age", details),
      type == x$sex,
      label %in% labels
    )
  if (nrow(hits) != length(labels) || anyDuplicated(hits$label)) {
    stop(sprintf("can't resolve ages(%s, %s, %s)", x$from, x$to, x$sex))
  }
  hits$vector
}

# Largest-remainder rounding: cells sum exactly to the cap.
allocate <- function(proportion, cap) {
  exact <- proportion * cap
  out <- floor(exact)
  top <- order(exact - out, decreasing = TRUE)[seq_len(cap - sum(out))]
  out[top] <- out[top] + 1
  as.integer(out)
}

# ---- resolve the spec --------------------------------------------------------
if (!nzchar(Sys.getenv("CM_API_KEY"))) {
  stop("CM_API_KEY is not set; add it to ~/.Renviron.")
}

catalogue <- list_census_vectors(DATASET, quiet = TRUE)
spec <- lapply(QUOTAS, \(dim) lapply(dim, resolve, catalogue = catalogue))

for (d in names(spec)) {
  is_rest <- vapply(spec[[d]], identical, logical(1), REST)
  has_base <- ".base" %in% names(spec[[d]])
  if (sum(is_rest) > 1) {
    stop(d, ": only one category can be REST")
  }
  if (any(is_rest) != has_base) stop(d, ": REST and .base go together")
}

vectors <- setdiff(unique(unlist(spec)), REST)
stop_if_any(setdiff(vectors, catalogue$vector), paste("not a", DATASET, "id"))

# ---- municipalities ----------------------------------------------------------
# census_id stays character: a numeric read drops leading zeros.
muns <- read_csv(MUNICIPALITIES, col_types = cols(.default = col_character()))
stop_if_any(
  setdiff(c("name", "census_id", "cap"), names(muns)),
  paste(MUNICIPALITIES, "is missing column")
)
stop_if_any(muns$census_id[duplicated(muns$census_id)], "duplicate census_id")
# Every municipality needs an explicit cap: a positive whole number.
stop_if_any(
  muns$name[!grepl("^[1-9][0-9]*$", trimws(coalesce(muns$cap, "")))],
  "missing or invalid cap for"
)
muns <- muns |>
  mutate(cap = as.integer(cap))

# ---- census pull -------------------------------------------------------------
# Level follows the id length: 7 digits = CSD, 4 = CD, 2 = PR.
id_level <- c("7" = "CSD", "4" = "CD", "2" = "PR")[
  as.character(nchar(muns$census_id))
]
stop_if_any(muns$census_id[is.na(id_level)], "census_id not 2, 4 or 7 digits")

message("querying cancensus for ", nrow(muns), " regions ...")
by_level <- split(muns$census_id, id_level)
census <- Map(
  \(ids, level) {
    get_census(
      dataset = DATASET,
      regions = setNames(list(ids), level),
      level = level,
      vectors = vectors,
      labels = "short",
      use_cache = TRUE,
      quiet = TRUE
    ) |>
      select(census_id = GeoUID, population = Population, all_of(vectors))
  },
  by_level,
  names(by_level)
) |>
  bind_rows()

stop_if_any(setdiff(muns$census_id, census$census_id), "no census data for")
stop_if_any(census$census_id[duplicated(census$census_id)], "repeated rows for")

raw <- muns |>
  left_join(census, by = "census_id")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
write_csv(raw, file.path(OUT_DIR, "census-raw.csv")) # before checks, to inspect

stop_if_any(
  filter(raw, if_any(all_of(vectors), is.na))$name,
  "suppressed or missing census cells for"
)

# ---- quotas ------------------------------------------------------------------
dimension_counts <- function(d) {
  cats <- spec[[d]][names(spec[[d]]) != ".base"]
  is_rest <- vapply(cats, identical, logical(1), REST)
  pops <- lapply(cats[!is_rest], \(v) rowSums(raw[v]))
  if (any(is_rest)) {
    pops[[names(cats)[is_rest]]] <- rowSums(raw[spec[[d]]$.base]) -
      Reduce(`+`, pops)
  }

  raw |>
    select(name, census_id, cap) |>
    bind_cols(as_tibble(pops[names(cats)])) |>
    pivot_longer(
      all_of(names(cats)),
      names_to = "category",
      values_to = "population"
    ) |>
    mutate(dimension = d, order = match(category, names(cats)))
}

counts <- bind_rows(lapply(names(spec), dimension_counts))

stop_if_any(
  with(filter(counts, population < 0), paste(name, dimension)),
  "REST came out negative for"
)

quotas <- counts |>
  group_by(census_id, dimension) |>
  mutate(total = sum(population)) |>
  ungroup()
stop_if_any(
  with(filter(quotas, total == 0), paste(name, dimension)),
  "no population in"
)

quotas <- quotas |>
  group_by(census_id, dimension) |>
  mutate(
    proportion = population / total,
    quota = allocate(proportion, first(cap))
  ) |>
  ungroup() |>
  arrange(
    match(census_id, muns$census_id),
    match(dimension, names(spec)),
    order
  ) |>
  select(
    name,
    census_id,
    cap,
    dimension,
    category,
    population,
    proportion,
    quota
  )

off <- quotas |>
  summarise(allocated = sum(quota), .by = c(name, cap, dimension)) |>
  filter(allocated != cap)
stop_if_any(paste(off$name, off$dimension), "quotas don't sum to cap for")

write_csv(quotas, file.path(OUT_DIR, "quotas.csv"))
message(
  "wrote census-raw.csv and quotas.csv for ",
  nrow(muns),
  " municipalities to ",
  OUT_DIR
)

# Census quotas: national level
#
# Builds a single set of Canada-wide survey quotas from the Census Profile via
# cancensus R package. For per-municipality quotas use census-quotas-csd.R.
# Copy this folder into a study, edit the settings, and run from the folder:
#   Rscript census-quotas-national.R

suppressPackageStartupMessages({
  library(cancensus)
  library(dplyr)
  library(tidyr)
  library(readr)
})

# Reed's key
set_cancensus_api_key("CensusMapper_e8117c5ce4c23c5b5ce1fb530f7aea86")

# Used in QUOTAS below; resolved to census vectors after the settings.

# Placeholder category: the dimension's `.base` population minus the sum of
# its other categories (e.g. "no degree" = 15+ population minus "degree").
REST <- "REST"

# Describes an age range (inclusive, in years) for one sex, e.g. ages(18, 29)
# or ages(60, Inf, "Female"). Returns a small "ages" object rather than census
# ids; resolve() later turns it into the matching age-group vectors.
ages <- function(from, to = Inf, sex = c("Total", "Male", "Female")) {
  structure(list(from = from, to = to, sex = match.arg(sex)), class = "ages")
}

# ---- settings ----------------------------------------------------------------
# CHANGE THIS SECTION TO SPECIFY YOUR STUDY'S QUOTAS

DATASET <- "CA21"
CAP <- 1000 # total national sample size
# TRUE: quotas for people who can speak English (English only, or English and
# French), from Statistics Canada cross-tabs. See "english speakers" below.
ENGLISH_ONLY <- FALSE

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

# END SETTINGS. The rest of the script is generic and doesn't need editing.

# ---- helpers -----------------------------------------------------------------
# Validation helper: if `bad` holds any offending values, stop with `msg`
# followed by the de-duplicated list of them. Does nothing when `bad` is empty.
stop_if_any <- function(bad, msg) {
  if (length(bad)) stop(msg, ": ", paste(unique(bad), collapse = ", "))
}

# Returns the Census age-group labels that together cover ages from..to.
# Picks the coarsest groups that fit exactly, e.g. 18-29 is 18, 19, 20 to 24,
# 25 to 29. Fewer cells means less rounding noise. Open-ended ranges
# (to = Inf) finish with a "65/85/100 years and over" group.
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

# Turns one QUOTAS category into census vector ids. Plain ids (and REST) pass
# through unchanged; an ages() object becomes the ids of its age groups for the
# requested sex, found in `catalogue` (from list_census_vectors()). Looked up
# by label, not vector number, so nothing depends on offsets. Errors if any
# age group is missing or matches more than once.
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

# Splits `cap` interviews across categories in line with `proportion` (which
# should sum to 1). Uses largest-remainder rounding: round everything down,
# then give the leftover interviews to the cells with the biggest fractional
# parts, so the integer quotas sum exactly to the cap.
allocate <- function(proportion, cap) {
  exact <- proportion * cap
  out <- floor(exact)
  top <- order(exact - out, decreasing = TRUE)[seq_len(cap - sum(out))]
  out[top] <- out[top] + 1
  as.integer(out)
}

# Builds a Statistics Canada table coordinate: one member id per dimension, in
# dimension order, padded with zeros to the 10 positions the API expects.
coord <- function(...) {
  ids <- c(...)
  paste(c(ids, rep(0, 10 - length(ids))), collapse = ".")
}

# Fetches the 2021 value of each cell `coords` in Statistics Canada table
# `pid`, via the Web Data Service. Returns values in the order of `coords`.
statcan_values <- function(pid, coords) {
  coords <- unname(coords) # a named list would be sent as a JSON object
  body <- lapply(
    coords,
    \(x) list(productId = pid, coordinate = x, latestN = 1)
  )
  res <- httr2::request(
    "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromCubePidCoordAndLatestNPeriods"
  ) |>
    httr2::req_body_json(body) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  ok <- vapply(res, \(r) identical(r$status, "SUCCESS"), logical(1))
  if (!all(ok)) {
    stop("Statistics Canada table ", pid, " returned no data for some cells")
  }
  got <- vapply(res, \(r) r$object$coordinate, character(1))
  vals <- vapply(
    res,
    \(r) as.numeric(r$object$vectorDataPoint[[1]]$value %||% NA),
    numeric(1)
  )
  vals[match(coords, got)]
}

# Member ids of dimension `position` in Statistics Canada table `pid`, named by
# member label.
statcan_members <- function(pid, position) {
  meta <- httr2::request(
    "https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata"
  ) |>
    httr2::req_body_json(list(list(productId = pid))) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  dim <- Filter(
    \(d) d$dimensionPositionId == position,
    meta[[1]]$object$dimension
  )[[1]]
  setNames(
    vapply(dim$member, \(m) m$memberId, integer(1)),
    vapply(dim$member, \(m) m$memberNameEn, character(1))
  )
}

# ---- resolve the spec --------------------------------------------------------
if (!nzchar(Sys.getenv("CM_API_KEY"))) {
  stop("CM_API_KEY is not set; add it to ~/.Renviron.")
}
if (!(length(CAP) == 1 && CAP >= 1 && CAP == round(CAP))) {
  stop("CAP must be a single positive whole number")
}
CAP <- as.integer(CAP)

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

# ---- census pull -------------------------------------------------------------
# Canada as a whole is region "01" at level "C".
message("querying cancensus for Canada ...")
raw <- get_census(
  dataset = DATASET,
  regions = list(C = "01"),
  level = "C",
  vectors = vectors,
  labels = "short",
  use_cache = TRUE,
  quiet = TRUE
) |>
  select(population = Population, all_of(vectors))

if (nrow(raw) != 1) {
  stop("expected one row for Canada, got ", nrow(raw))
}

# before checks, to inspect
write_csv(raw, "census-raw-national.csv")

stop_if_any(
  vectors[vapply(raw[vectors], is.na, logical(1))],
  "suppressed or missing census cells"
)

# ---- english speakers --------------------------------------------------------
# The Census Profile doesn't cross language with age or education, so with
# ENGLISH_ONLY each vector's count is replaced by an English-speaker count
# from one of two Statistics Canada tables:
#   98-10-0619 (knowledge of languages by age and gender): each age-group
#     vector is scaled by the share of its sex and 10-year age band (15-24,
#     25-34, ..., 65+) who can speak English. This assumes the share is flat
#     within a band, e.g. the same for 18-year-olds as for 24-year-olds.
#   98-10-0365 (knowledge of official languages by highest degree, age and
#     gender, 15+ in private households): each education vector is replaced
#     by its English only plus English and French count.
# Other kinds of vectors aren't published by language and stop the script.
suffix <- ""
if (ENGLISH_ONLY) {
  message("querying Statistics Canada for English speakers ...")
  suffix <- "-english"
  info <- catalogue[match(vectors, catalogue$vector), ]
  gender <- c(Total = 1, Male = 2, Female = 3)[as.character(info$type)]
  is_age <- grepl("; Total - Age", info$details)
  is_edu <- grepl("; Education; Total - Highest certificate", info$details)
  stop_if_any(
    vectors[!is_age & !is_edu],
    "ENGLISH_ONLY only handles ages() and highest-degree vectors, not"
  )

  # 98-10-0619 dims: geography, age, gender, mother tongue, knowledge of
  # languages, generation status. Age members 2-8 start at these ages;
  # knowledge member 1 is everyone and 3 is "English".
  from <- suppressWarnings(as.numeric(sub("^(\\d+).*", "\\1", info$label)))
  from[info$label == "Under 1 year"] <- 0
  band <- findInterval(from, c(0, 15, 25, 35, 45, 55, 65)) + 1
  cells <- mapply(
    \(b, g, k) coord(1, b, g, 1, k, 1),
    rep(band[is_age], 2),
    rep(gender[is_age], 2),
    rep(c(3, 1), each = sum(is_age))
  )
  vals <- statcan_values(98100619, cells)
  share <- vals[seq_len(sum(is_age))] / vals[-seq_len(sum(is_age))]

  # 98-10-0365 dims: geography, degree, immigrant status, work activity, age,
  # gender, income statistics, knowledge of official languages. Degree members
  # are matched by label; age member 1 is 15+ and 3 is 25-64; knowledge
  # members 2 and 4 are English only and English and French.
  edu <- statcan_members(98100365, 2)
  names(edu) <- gsub("’", "'", names(edu))
  label <- sub("^Total - .*", names(edu)[1], info$label[is_edu])
  age <- ifelse(
    grepl("aged 15 years and over", info$details[is_edu]),
    1,
    ifelse(grepl("aged 25 to 64 years", info$details[is_edu]), 3, NA)
  )
  stop_if_any(
    vectors[is_edu][is.na(edu[label]) | is.na(age)],
    "no Statistics Canada match for"
  )
  cells <- mapply(
    \(e, a, g, k) coord(1, e, 1, 1, a, g, 1, k),
    rep(edu[label], 2),
    rep(age, 2),
    rep(gender[is_edu], 2),
    rep(c(2, 4), each = sum(is_edu))
  )
  vals <- statcan_values(98100365, cells)
  english <- vals[seq_len(sum(is_edu))] + vals[-seq_len(sum(is_edu))]

  stop_if_any(
    c(vectors[is_age][is.na(share)], vectors[is_edu][is.na(english)]),
    "suppressed or missing Statistics Canada cells for"
  )
  raw[vectors[is_age]] <- as.list(unlist(raw[vectors[is_age]]) * share)
  raw[vectors[is_edu]] <- as.list(english)
  write_csv(raw, "census-raw-national-english.csv")
}

# ---- quotas ------------------------------------------------------------------
# National census population of each category in dimension `d`. Sums each
# category's vectors from `raw`, computes REST as `.base` minus the other
# categories, and returns one row per category in QUOTAS order.
dimension_counts <- function(d) {
  cats <- spec[[d]][names(spec[[d]]) != ".base"]
  is_rest <- vapply(cats, identical, logical(1), REST)
  pops <- vapply(cats[!is_rest], \(v) sum(unlist(raw[v])), numeric(1))
  if (any(is_rest)) {
    pops[names(cats)[is_rest]] <- sum(unlist(raw[spec[[d]]$.base])) -
      sum(pops)
  }

  tibble(dimension = d, category = names(cats), population = pops[names(cats)])
}

counts <- bind_rows(lapply(names(spec), dimension_counts))

stop_if_any(
  filter(counts, population < 0)$dimension,
  "REST came out negative for"
)

quotas <- counts |>
  mutate(total = sum(population), .by = dimension)
stop_if_any(filter(quotas, total == 0)$dimension, "no population in")

quotas <- quotas |>
  mutate(
    cap = CAP,
    proportion = population / total,
    quota = allocate(proportion, CAP),
    .by = dimension
  ) |>
  select(cap, dimension, category, population, proportion, quota)

off <- quotas |>
  summarise(allocated = sum(quota), .by = dimension) |>
  filter(allocated != CAP)
stop_if_any(off$dimension, "quotas don't sum to cap for")

out <- paste0("quotas-national", suffix, ".csv")
write_csv(quotas, out)
message("wrote ", out)

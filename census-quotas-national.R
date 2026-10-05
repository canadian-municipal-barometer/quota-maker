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
DATASET <- "CA21"
CAP <- 1000 # total national sample size
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

if (nrow(raw) != 1) stop("expected one row for Canada, got ", nrow(raw))

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
# before checks, to inspect
write_csv(raw, file.path(OUT_DIR, "census-raw-national.csv"))

stop_if_any(
  vectors[vapply(raw[vectors], is.na, logical(1))],
  "suppressed or missing census cells"
)

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

write_csv(quotas, file.path(OUT_DIR, "quotas-national.csv"))
message("wrote census-raw-national.csv and quotas-national.csv to ", OUT_DIR)

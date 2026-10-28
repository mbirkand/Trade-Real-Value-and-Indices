# Trade Real Values and Indices
# Base period: 2014-2016 = 100
#
# Outputs: aggregates only (item groups, for countries and regions): real values,
# and value and quantity indices. Item real values are calculated as the building
# block of the aggregate real values but are not written out.
# No item-level indices, no unit value index and no real value index are produced.
# No data are filtered out: small, unusual or flagged records are kept as reported
# (only live animals are excluded and one-sided zeros are set to missing, Step 1).
# Item groups in excluded_groups (Step 3) are not calculated at all; their items
# still count in every other group they belong to.

library(data.table)
library(openxlsx)

# All generated outputs are written to the results/ subfolder.
dir.create("results", showWarnings = FALSE)
dir.create("results/checks", showWarnings = FALSE)   # QA / diagnostic files

base_period <- c("2014", "2015", "2016")

RUN_STATS <- list()

# STEP 1 — Load data and reshape to wide format

cat("\n=== STEP 1: LOAD AND RESHAPE ===\n")

trade <- readRDS("TradeData.rds")
setDT(trade)

cat("Rows (long format)  :", formatC(nrow(trade), format = "d", big.mark = ","), "\n")
cat("Unique countries    :", trade[, uniqueN(geographicAreaM49)], "\n")
cat("Unique items        :", trade[, uniqueN(measuredItemCPC)], "\n")
cat("Year range          :", paste(range(trade$timePointYears), collapse = " - "), "\n")
cat("Elements present    :", paste(sort(unique(trade$measuredElementTrade)), collapse = ", "), "\n")

trade_wide <- dcast(
  trade,
  geographicAreaM49 + measuredItemCPC + timePointYears ~ measuredElementTrade,
  value.var = "Value"
)
setnames(trade_wide,
         c("5610", "5622", "5910", "5922"),
         c("imp_q", "imp_v", "exp_q", "exp_v"))

trade_wide[, timePointYears := as.character(timePointYears)]

cat("\nRows after reshape   :", formatC(nrow(trade_wide), format = "d", big.mark = ","), "\n")
cat("Columns             :", paste(names(trade_wide), collapse = ", "), "\n")

cat("\nNA counts per column after reshape:\n")
cat("  imp_q:", trade_wide[is.na(imp_q), .N], "\n")
cat("  imp_v:", trade_wide[is.na(imp_v), .N], "\n")
cat("  exp_q:", trade_wide[is.na(exp_q), .N], "\n")
cat("  exp_v:", trade_wide[is.na(exp_v), .N], "\n")

# --- capture for the run summary
RUN_STATS$rows_long        <- nrow(trade)
RUN_STATS$countries_start  <- trade[, uniqueN(geographicAreaM49)]
RUN_STATS$items_start      <- trade[, uniqueN(measuredItemCPC)]
RUN_STATS$year_min         <- min(trade$timePointYears)
RUN_STATS$year_max         <- max(trade$timePointYears)
RUN_STATS$rows_wide        <- nrow(trade_wide)
RUN_STATS$cells_total      <- nrow(trade_wide) * 4L
RUN_STATS$na_imp_q_start   <- trade_wide[is.na(imp_q), .N]
RUN_STATS$na_imp_v_start   <- trade_wide[is.na(imp_v), .N]
RUN_STATS$na_exp_q_start   <- trade_wide[is.na(exp_q), .N]
RUN_STATS$na_exp_v_start   <- trade_wide[is.na(exp_v), .N]
RUN_STATS$na_cells_start   <- with(RUN_STATS, na_imp_q_start + na_imp_v_start +
                                              na_exp_q_start + na_exp_v_start)

# STEP 1.2 — Exclude live animals (CPC codes starting with "021")

cat("\n=== STEP 1.2: EXCLUDE LIVE ANIMALS (CPC 021x) ===\n")

n_before_animals <- nrow(trade_wide)
items_before_animals <- trade_wide[, uniqueN(measuredItemCPC)]

trade_wide <- trade_wide[!grepl("^'?021", measuredItemCPC)]

n_dropped_animals <- n_before_animals - nrow(trade_wide)
items_dropped_animals <- items_before_animals - trade_wide[, uniqueN(measuredItemCPC)]

cat(sprintf("Rows removed  : %s\n", formatC(n_dropped_animals, format = "d", big.mark = ",")))
cat(sprintf("Items removed : %d (CPC codes starting with 021)\n", items_dropped_animals))
cat(sprintf("Rows remaining: %s\n", formatC(nrow(trade_wide), format = "d", big.mark = ",")))

RUN_STATS$rows_dropped_live_animals  <- n_dropped_animals
RUN_STATS$items_dropped_live_animals <- items_dropped_animals
RUN_STATS$rows_after_live_animals    <- nrow(trade_wide)

# STEP 1.5 — Fix asymmetric Q / V pairs

cat("\n=== STEP 1.5: FIX ASYMMETRIC Q / V PAIRS ===\n")

imp_q0_vpos <- trade_wide[!is.na(imp_q) & imp_q == 0 & !is.na(imp_v) & imp_v > 0, .N]
trade_wide[!is.na(imp_q) & imp_q == 0 & !is.na(imp_v) & imp_v > 0, imp_q := NA]

imp_v0_qpos <- trade_wide[!is.na(imp_v) & imp_v == 0 & !is.na(imp_q) & imp_q > 0, .N]
trade_wide[!is.na(imp_v) & imp_v == 0 & !is.na(imp_q) & imp_q > 0, imp_v := NA]

exp_q0_vpos <- trade_wide[!is.na(exp_q) & exp_q == 0 & !is.na(exp_v) & exp_v > 0, .N]
trade_wide[!is.na(exp_q) & exp_q == 0 & !is.na(exp_v) & exp_v > 0, exp_q := NA]

exp_v0_qpos <- trade_wide[!is.na(exp_v) & exp_v == 0 & !is.na(exp_q) & exp_q > 0, .N]
trade_wide[!is.na(exp_v) & exp_v == 0 & !is.na(exp_q) & exp_q > 0, exp_v := NA]

cat(sprintf("Import — Q=0 but V>0: %s rows → imp_q set to NA\n",
            formatC(imp_q0_vpos, format = "d", big.mark = ",")))
cat(sprintf("Import — V=0 but Q>0: %s rows → imp_v set to NA\n",
            formatC(imp_v0_qpos, format = "d", big.mark = ",")))
cat(sprintf("Export — Q=0 but V>0: %s rows → exp_q set to NA\n",
            formatC(exp_q0_vpos, format = "d", big.mark = ",")))
cat(sprintf("Export — V=0 but Q>0: %s rows → exp_v set to NA\n",
            formatC(exp_v0_qpos, format = "d", big.mark = ",")))

RUN_STATS$asym_imp_q_to_na <- imp_q0_vpos
RUN_STATS$asym_imp_v_to_na <- imp_v0_qpos
RUN_STATS$asym_exp_q_to_na <- exp_q0_vpos
RUN_STATS$asym_exp_v_to_na <- exp_v0_qpos
RUN_STATS$asym_total_to_na <- imp_q0_vpos + imp_v0_qpos + exp_q0_vpos + exp_v0_qpos

# STEP 2 — Negligible observation check (REPORT ONLY)

cat("\n=== STEP 2: NEGLIGIBLE OBSERVATION CHECK (REPORT ONLY - NOTHING REMOVED) ===\n")

report_negligible <- function(qcol, vcol, label) {
  usable <- trade_wide[!is.na(get(qcol)) & get(qcol) > 0 &
                       !is.na(get(vcol)) & get(vcol) > 0, .N]
  small  <- trade_wide[!is.na(get(qcol)) & !is.na(get(vcol)) &
                       get(qcol) > 0 & get(vcol) > 0 &
                       (get(qcol) < 10 | get(vcol) < 10), .N]
  nearz  <- trade_wide[!is.na(get(qcol)) & !is.na(get(vcol)) &
                       get(qcol) > 0 & get(vcol) > 0 & get(qcol) < 1, .N]
  sm110  <- trade_wide[!is.na(get(qcol)) & !is.na(get(vcol)) &
                       get(vcol) > 0 & get(qcol) >= 1 & get(qcol) < 10, .N]
  smallv <- trade_wide[!is.na(get(qcol)) & !is.na(get(vcol)) &
                       get(qcol) > 0 & get(vcol) > 0 & get(vcol) < 10, .N]
  zeroed <- trade_wide[!is.na(get(qcol)) & !is.na(get(vcol)) &
                       (get(qcol) == 0 | get(vcol) == 0), .N]

  cat(sprintf("\n%s — usable observations (Q>0 & V>0): %s\n", label,
              formatC(usable, format = "d", big.mark = ",")))
  cat(sprintf("  A Q<10 OR V<10 filter WOULD drop: %s  (%.2f%% of usable)  [not applied]\n",
              formatC(small, format = "d", big.mark = ","),
              if (usable > 0) 100 * small / usable else NA_real_))
  cat(sprintf("    of which near-zero quantity (0<Q<1) : %s\n",
              formatC(nearz, format = "d", big.mark = ",")))
  cat(sprintf("    of which small quantity  (1<=Q<10)  : %s\n",
              formatC(sm110, format = "d", big.mark = ",")))
  cat(sprintf("    of which small value     (V<10)     : %s\n",
              formatC(smallv, format = "d", big.mark = ",")))
  cat(sprintf("  Zero-side records (Q=0 or V=0, no UV possible): %s\n",
              formatC(zeroed, format = "d", big.mark = ",")))

  list(usable = usable, small = small, nearz = nearz,
       sm110 = sm110, smallv = smallv, zeroed = zeroed)
}

neg_imp <- report_negligible("imp_q", "imp_v", "Import")
neg_exp <- report_negligible("exp_q", "exp_v", "Export")

RUN_STATS$obs_usable_imp   <- neg_imp$usable
RUN_STATS$obs_usable_exp   <- neg_exp$usable
RUN_STATS$negligible_imp   <- neg_imp$small
RUN_STATS$negligible_exp   <- neg_exp$small
RUN_STATS$negl_nearq_imp   <- neg_imp$nearz
RUN_STATS$negl_nearq_exp   <- neg_exp$nearz
RUN_STATS$negl_q1to10_imp  <- neg_imp$sm110
RUN_STATS$negl_q1to10_exp  <- neg_exp$sm110
RUN_STATS$negl_smallv_imp  <- neg_imp$smallv
RUN_STATS$negl_smallv_exp  <- neg_exp$smallv

cat("\nNo records removed — every observation continues to Step 3, even if it looks problematic.\n")

# STEP 3 — Reference tables and dissemination / aggregation control

cat("\n=== STEP 3: REFERENCE TABLES AND DISSEMINATION CONTROL ===\n")

agg_table <- setDT(read.xlsx("ti_aggregation_table.xlsx"))
names(agg_table) <- c("var_type", "group_code", "member_code",
                      "factor", "group_name", "member_name")

# --- Area dissemination / aggregation control
# disseminationDataset.xlsx: Dissemination decides which areas appear in
# outputs; Aggregation decides which areas feed regional aggregates. Areas
# absent from the file are excluded from both. Codes are normalised (leading
# zeros stripped) to match the trade data and the aggregation table.
norm_area <- function(x) sub("^0+(?=\\d)", "", gsub("'", "", as.character(x)), perl = TRUE)
diss_areas <- character(0); aggr_areas <- character(0)
if (file.exists("disseminationDataset.xlsx")) {
  dset <- setDT(read.xlsx("disseminationDataset.xlsx"))
  dset[, ncode := norm_area(Code)]
  dset[, diss_flag := as.logical(Dissemination)]
  dset[, aggr_flag := as.logical(Aggregation)]
  diss_areas <- unique(dset[diss_flag == TRUE, ncode])
  aggr_areas <- unique(dset[aggr_flag == TRUE, ncode])
  cat(sprintf("Area control: %d disseminated, %d aggregated (of %d listed)\n",
              length(diss_areas), length(aggr_areas), nrow(dset)))
} else {
  cat("NOTE: disseminationDataset.xlsx not found — no area filtering applied.\n")
}
# Keep only disseminated areas (no-op if the list is absent).
keep_diss <- function(dt, col)
  if (length(diss_areas)) dt[norm_area(get(col)) %in% diss_areas] else dt

item_grp_map <- agg_table[var_type == "item", .(
  measuredItemCPC = member_code,
  item_group_code = group_code,
  item_group_name = group_name
)]

norm_cpc <- function(x) gsub("'", "", x)
disseminated_codes <- character(0)
if (file.exists("DisseminationList.csv")) {
  diss <- fread("DisseminationList.csv", colClasses = "character")
  disseminated_codes <- unique(norm_cpc(diss[tolower(dissemination) == "true", code]))
  cat(sprintf("Dissemination list: %d codes with dissemination==TRUE\n",
              length(disseminated_codes)))
} else {
  cat("NOTE: DisseminationList.csv not found — disseminated breakdown skipped.\n")
}

# --- Item groups excluded from the calculations (decided 2026-10-20)
# These groups are in DisseminationList.csv but are not calculated: no aggregate
# real values and no indices, in any output. Their items are still calculated and
# still count in every other group they belong to (e.g. Agricultural Products).
# They are listed in a separate section of the dashboard's Dissemination groups tab.
# Natural Rubber and Textile Fibres may be added next year.
excluded_groups <- data.table(
  code = c("F1898", "F1900", "F1846", "F1901", "F2076", "F1902",
           "F1845", "F1892", "F1849", "F1847", "F1848"),
  note = c("", "May be added next year", "", "May be added next year", "", "",
           "", "", "", "", "")
)

# --- Area names (added to the output files for readability)
area_names <- data.table(Area_Code = character(0), Area_Name = character(0))
if (file.exists("CountryCode.csv")) {
  cc <- fread("CountryCode.csv", colClasses = "character")
  area_names <- unique(cc[, .(Area_Code = norm_area(`M49 Code`), Area_Name = Area)]
                       [!is.na(Area_Code) & Area_Code != ""], by = "Area_Code")
  rm(cc)
}
area_name_of <- function(code) area_names$Area_Name[match(norm_area(code), area_names$Area_Code)]

# Flow order used when sorting stacked Import/Export tables
flow_ord <- function(f) match(f, c("Import", "Export"))

cat("\n--- Step 3 complete (reference tables loaded; no rows removed). ---\n")

# STEP 4 — Base unit values (2014–2016): per-year reconstruction

cat("\n=== STEP 4: BASE UNIT VALUES — PER-YEAR RECONSTRUCTION ===\n")

# --- 3x rule for base-year unit values (decided 2026-10-27), checked within the
# country's own base-year series:
#   0 or 1 own base year  -> the group's base unit value is used
#   2 own base years      -> within 3x of each other: the missing year is estimated
#                            with the growth rate approach; more than 3x apart: the
#                            group's base unit value is used
#   3 own base years      -> a year more than 3x above or below the median of the three
#                            is replaced with the growth rate approach; otherwise no change
UV_RANGE <- 3

# Valid base-year records (2026-10-27, from the Discussions section): Q >= 1 t and
# V >= 1 thousand USD. Near-zero records give meaningless unit values (e.g. pears in Middle
# Africa at 34,160,000; Argentina raw milk imports at 188,480 $/t from 0.013 t).
#   - a smaller record is not used as the country's own base year (treated as missing)
#   - nor as a donor to a group unit value
#   (all records still count in quantities, values and real values of every year)
#   - a regional group needs at least 3 donor countries other than the country being
#     estimated, otherwise the next (bigger) group is used
# The global level has no minimum, and falls back to all records if no donor exists.
MIN_DONOR_Q <- 1
MIN_DONOR_V <- 1
MIN_GROUP_N <- 3
# A regional group is skipped in a year when one country (other than the country being
# estimated) holds more than DOM_SHARE of the group's valid tonnes: its weighted median
# would just be that country's price (2026-10-27; e.g. Western Africa wine exports,
# Ghana 82% of the tonnes). The next, bigger group is used. Not applied to all countries.
DOM_SHARE <- 0.5
# largest share of the group's tonnes held by one of the OTHER countries, for every member
loo_dom <- function(q) {
  n <- length(q)
  if (n < 2L) return(rep(NA_real_, n))
  o <- order(-q)
  mx <- fifelse(seq_len(n) == o[1], q[o[2]], q[o[1]])
  mx / (sum(q) - q)
}

# Quantity-weighted median unit value (decided 2026-10-27 for the regional groups of
# the fallback hierarchy): the lowest unit value at which the members' cumulative
# quantity reaches half of the group's total quantity.
wmed <- function(uv, w) {
  o <- order(uv); cw <- cumsum(w[o])
  uv[o][which(cw >= cw[length(cw)] / 2)[1]]
}
# Leave-one-out version: for every member, the weighted median of the OTHER members
# (NA when it is the only member).
loo_wmed <- function(uv, w) {
  n <- length(uv)
  if (n < 2L) return(rep(NA_real_, n))
  o <- order(uv); us <- uv[o]; ws <- w[o]; cw <- cumsum(ws)
  j <- seq_len(n)
  h <- (cw[n] - ws) / 2                                    # half of the others' total
  k <- findInterval(h, cw, left.open = TRUE) + 1L          # first position with cw >= h
  k2 <- pmax(findInterval(h + ws, cw, left.open = TRUE) + 1L, j + 1L)
  pos <- fifelse(k < j, k, pmin(k2, n))
  out <- numeric(n); out[o] <- us[pos]
  out
}

# --- 4A: Regional hierarchy (most specific = smallest group)
# Donor groups excluded from the UV fallback hierarchy. These are either
# "excluding intra-trade" variants (any code containing a dot) or economic /
# non-geographic groupings whose members are too heterogeneous to lend a
# meaningful unit value. They are still used for aggregation in Step 6.
UV_FALLBACK_EXCLUDE <- c(
  "053",   # Australia and New Zealand
  "097",   # European Union (27)
  "199",   # Least Developed Countries
  "432",   # Land Locked Developing Countries
  "722",   # Small Island Developing States
  "901",   # Low Income Food Deficit Countries
  "902"    # Net Food Importing Developing Countries
)
UV_FALLBACK_EXCLUDE <- unique(c(UV_FALLBACK_EXCLUDE, norm_area(UV_FALLBACK_EXCLUDE)))

area_map <- agg_table[
  var_type == "area" & as.numeric(factor) == 1 & !grepl("\\.", group_code) &
    !(group_code %in% UV_FALLBACK_EXCLUDE) &
    !(norm_area(group_code) %in% UV_FALLBACK_EXCLUDE),
  .(group_code, group_name, member_code)
]
cat(sprintf("UV fallback: %d donor groups excluded (%s) + all '.xx' intra-trade variants\n",
            length(unique(norm_area(UV_FALLBACK_EXCLUDE))),
            paste(sort(unique(norm_area(UV_FALLBACK_EXCLUDE))), collapse = ", ")))

group_size <- area_map[, .(n_members = .N), by = .(group_code, group_name)]
country_region_map <- merge(area_map, group_size[, .(group_code, n_members)], by = "group_code")
country_region_map <- country_region_map[order(member_code, n_members)]
country_region_map[, tier := seq_len(.N), by = member_code]

cat(sprintf("Regional mapping: %d area groups, %d country-group pairs (max tier %d)\n",
            uniqueN(area_map$group_code), nrow(area_map), max(country_region_map$tier)))

# --- 4B: Reconstruction engine (flow-agnostic)
build_base_uv <- function(qcol, vcol) {

  base_sub <- trade_wide[
    timePointYears %in% base_period &
      !is.na(get(qcol)) & get(qcol) > 0 &
      !is.na(get(vcol)) & get(vcol) > 0,
    .(geographicAreaM49, measuredItemCPC, timePointYears,
      q = get(qcol), v = get(vcol))
  ]

  base_sub[, uv := v / q]
  # valid records (Q >= 1 t and V >= 1 thousand USD): the only ones used as the country's own
  # base years (2026-10-27) and as donors to group unit values. A smaller own record is
  # treated as a missing base year, so the 3x rule and the fallback hierarchy take over.
  base_d <- base_sub[q >= MIN_DONOR_Q & v >= MIN_DONOR_V]
  own <- dcast(base_d, geographicAreaM49 + measuredItemCPC ~ timePointYears,
               value.var = "uv")
  for (y in c("2014", "2015", "2016")) if (!y %in% names(own)) own[, (y) := NA_real_]
  setnames(own, c("2014", "2015", "2016"), c("o14", "o15", "o16"))
  own <- own[, .(geographicAreaM49, measuredItemCPC, o14, o15, o16)]

  bg <- merge(base_sub, area_map[, .(member_code, group_code, group_name)],
              by.x = "geographicAreaM49", by.y = "member_code", allow.cartesian = TRUE)
  # regional groups: quantity-weighted median of the member countries' unit values (2026-10-27),
  # donor records only, and only with at least MIN_GROUP_N donor countries
  bgd <- bg[q >= MIN_DONOR_Q & v >= MIN_DONOR_V]
  grp <- bgd[, .(uv = if (.N >= MIN_GROUP_N && max(q) / sum(q) <= DOM_SHARE) wmed(uv, q) else NA_real_),
             by = .(group_code, group_name, measuredItemCPC, timePointYears)]
  # without the country: needs MIN_GROUP_N donors other than the country, none of them
  # holding more than DOM_SHARE of the others' tonnes
  bgd[, uv_loo := if (.N - 1L >= MIN_GROUP_N) fifelse(loo_dom(q) <= DOM_SHARE, loo_wmed(uv, q), NA_real_) else NA_real_,
      by = .(group_code, measuredItemCPC, timePointYears)]
  grp_w <- dcast(grp, group_code + group_name + measuredItemCPC ~ timePointYears,
                 value.var = "uv")
  for (y in c("2014", "2015", "2016")) if (!y %in% names(grp_w)) grp_w[, (y) := NA_real_]
  setnames(grp_w, c("2014", "2015", "2016"), c("g14", "g15", "g16"))
  grp_w[, gb := rowMeans(.SD, na.rm = TRUE), .SDcols = c("g14", "g15", "g16")]
  grp_w[is.nan(gb), gb := NA_real_]

  gl_d <- base_d[, .(uv_d = sum(v) / sum(q)), by = .(measuredItemCPC, timePointYears)]
  gl <- base_sub[, .(uv_a = sum(v) / sum(q)), by = .(measuredItemCPC, timePointYears)]
  gl <- merge(gl, gl_d, by = c("measuredItemCPC", "timePointYears"), all.x = TRUE)
  gl[, uv := fcoalesce(uv_d, uv_a)]   # donor records; all records only if the item has no donor
  gl_w <- dcast(gl, measuredItemCPC ~ timePointYears, value.var = "uv")
  for (y in c("2014", "2015", "2016")) if (!y %in% names(gl_w)) gl_w[, (y) := NA_real_]
  setnames(gl_w, c("2014", "2015", "2016"), c("gg_14", "gg_15", "gg_16"))
  gl_w[, global_base := rowMeans(.SD, na.rm = TRUE), .SDcols = c("gg_14", "gg_15", "gg_16")]
  gl_w[is.nan(global_base), global_base := NA_real_]

  res <- unique(trade_wide[!is.na(get(qcol)) | !is.na(get(vcol)),
                           .(geographicAreaM49, measuredItemCPC)])

  for (k in 1:3) {
    tk <- country_region_map[tier == k,
            .(geographicAreaM49 = member_code, group_code)]
    setnames(tk, "group_code", paste0("gc", k))
    res <- merge(res, tk, by = "geographicAreaM49", all.x = TRUE)
  }

  attach_tier <- function(res, k) {
    gk <- copy(grp_w)
    setnames(gk,
             c("group_code", "group_name", "g14", "g15", "g16", "gb"),
             c(paste0("gc", k), paste0("gn", k),
               paste0("g", k, "_14"), paste0("g", k, "_15"), paste0("g", k, "_16"),
               paste0("gb", k)))
    merge(res, gk, by = c(paste0("gc", k), "measuredItemCPC"), all.x = TRUE)
  }
  res <- attach_tier(res, 1)
  res <- attach_tier(res, 2)
  res <- attach_tier(res, 3)

  res <- merge(res, gl_w[, .(measuredItemCPC, gg_14, gg_15, gg_16, global_base)],
               by = "measuredItemCPC", all.x = TRUE)
  res <- merge(res, own, by = c("geographicAreaM49", "measuredItemCPC"), all.x = TRUE)

  # Leave-one-out (2026-10-22): a country's own record is taken out of its parent groups'
  # (and the global) unit values before they are used for that country. Its estimated base
  # years then depend only on the other countries' prices, never on its own data.
  # Regional groups (tiers 1-3): weighted median of the other members' unit values (2026-10-27).
  # All countries (global): (sum V - v_c) / (sum Q - q_c).
  # NA when c is the only member with data that year, so the next tier is used.
  # Years where c has no own data are unchanged.
  glsum   <- base_sub[, .(V = sum(v), Q = sum(q), N = .N), by = .(measuredItemCPC, timePointYears)]
  glsum_d <- base_d[, .(V = sum(v), Q = sum(q), N = .N), by = .(measuredItemCPC, timePointYears)]
  loo_uv <- function(cur, s, o) {
    inside <- !is.na(o$q) & !is.na(s$N)
    fifelse(inside, fifelse(s$N > 1 & (s$Q - o$q) > 0, (s$V - o$v) / (s$Q - o$q), NA_real_), cur)
  }
  for (y in base_period) {
    yy <- substr(y, 3, 4)
    oy <- base_sub[timePointYears == y, .(geographicAreaM49, measuredItemCPC, q, v)]
    o  <- oy[res[, .(geographicAreaM49, measuredItemCPC)], on = .(geographicAreaM49, measuredItemCPC), mult = "first"]
    # the country's record as a donor (NA when it is below the donor minimum)
    od <- copy(o)[!(q >= MIN_DONOR_Q & v >= MIN_DONOR_V), `:=`(q = NA_real_, v = NA_real_)]
    for (k in 1:3) {
      key <- data.table(geographicAreaM49 = res$geographicAreaM49, group_code = res[[paste0("gc", k)]],
                        measuredItemCPC = res$measuredItemCPC)
      lw <- bgd[timePointYears == y][key, on = .(geographicAreaM49, group_code, measuredItemCPC),
                                     uv_loo, mult = "first"]
      col <- paste0("g", k, "_", yy)
      # country is a donor in the group: the group without it; otherwise the group as it is
      set(res, j = col, value = fifelse(!is.na(od$q), lw, res[[col]]))
    }
    ykey <- res[, .(measuredItemCPC)]
    s_d <- glsum_d[timePointYears == y][ykey, on = .(measuredItemCPC), mult = "first"]
    s_a <- glsum[timePointYears == y][ykey, on = .(measuredItemCPC), mult = "first"]
    g_d <- gl[timePointYears == y][ykey, on = .(measuredItemCPC), uv_d, mult = "first"]
    g_a <- gl[timePointYears == y][ykey, on = .(measuredItemCPC), uv_a, mult = "first"]
    col <- paste0("gg_", yy)
    # donor records first; all records only when no other country is a donor
    set(res, j = col, value = fcoalesce(loo_uv(g_d, s_d, od), loo_uv(g_a, s_a, o)))
  }
  # group base unit values (average of the group's base-year prices), also without the country.
  # A regional group's base UV needs a unit value (>= MIN_GROUP_N valid donors) in ALL three
  # base years; otherwise the next, bigger group is used (2026-10-27: a base built from one
  # thin year, e.g. South America palm kernel exports for Chile, 2016 only, 240,153 $/t).
  for (k in 1:3) {
    gbk <- rowMeans(as.matrix(res[, paste0("g", k, "_", c("14", "15", "16")), with = FALSE]))
    set(res, j = paste0("gb", k), value = gbk)
  }
  ggb <- rowMeans(as.matrix(res[, c("gg_14", "gg_15", "gg_16")]), na.rm = TRUE)
  res[, global_base := fifelse(is.nan(ggb), NA_real_, ggb)]

  pr <- function(res, tn, an) {
    g <- function(k, s) res[[paste0("g", k, s)]]
    gg <- function(s) res[[paste0("gg", s)]]
    fcoalesce(
      fifelse(!is.na(g(1, tn)) & !is.na(g(1, an)) & g(1, an) != 0, g(1, tn) / g(1, an), NA_real_),
      fifelse(!is.na(g(2, tn)) & !is.na(g(2, an)) & g(2, an) != 0, g(2, tn) / g(2, an), NA_real_),
      fifelse(!is.na(g(3, tn)) & !is.na(g(3, an)) & g(3, an) != 0, g(3, tn) / g(3, an), NA_real_),
      fifelse(!is.na(gg(tn))   & !is.na(gg(an))   & gg(an)   != 0, gg(tn)   / gg(an),   NA_real_)
    )
  }
  res[, ratio_15_16 := pr(res, "_15", "_16")]
  res[, ratio_15_14 := pr(res, "_15", "_14")]
  res[, ratio_14_15 := pr(res, "_14", "_15")]
  res[, ratio_16_15 := pr(res, "_16", "_15")]

  res[, direct_14 := fcoalesce(g1_14, g2_14, g3_14, gg_14)]
  res[, direct_15 := fcoalesce(g1_15, g2_15, g3_15, gg_15)]
  res[, direct_16 := fcoalesce(g1_16, g2_16, g3_16, gg_16)]

  res[, group_base_co := fcoalesce(gb1, gb2, gb3, global_base)]
  res[, group_base_src := fcase(!is.na(gb1), gn1, !is.na(gb2), gn2,
                                !is.na(gb3), gn3, !is.na(global_base), "global",
                                default = NA_character_)]

  res[, n_own := as.integer(!is.na(o14)) + as.integer(!is.na(o15)) + as.integer(!is.na(o16))]

  # --- 3x rule (2026-10-27), within the country's own base-year series
  res[, own_hi := pmax(o14, o15, o16, na.rm = TRUE)]
  res[, own_lo := pmin(o14, o15, o16, na.rm = TRUE)]
  res[, own_md := fifelse(n_own == 3L, o14 + o15 + o16 - own_hi - own_lo, NA_real_)]
  far <- function(x, m) !is.na(x) & !is.na(m) & (x > UV_RANGE * m | x * UV_RANGE < m)
  res[, `:=`(x14 = far(o14, own_md), x15 = far(o15, own_md), x16 = far(o16, own_md))]
  res[, uv_rule := fcase(
        n_own == 0L, "group_0",
        n_own == 1L, "group_1",
        n_own == 2L & own_hi > UV_RANGE * own_lo, "group_2_far",
        n_own == 2L, "fill_2",
        n_own == 3L & (x14 | x15 | x16), "fix_3",
        default = "own_3")]
  # the group's base unit value replaces the country's (only if a group has one)
  res[, swap := uv_rule %in% c("group_0", "group_1", "group_2_far") & !is.na(group_base_co)]
  # 3 own years: the year(s) far from the median are set aside and re-estimated below
  res[x14 == TRUE, o14 := NA_real_]
  res[x15 == TRUE, o15 := NA_real_]
  res[x16 == TRUE, o16 := NA_real_]

  res[, h14 := !is.na(o14)]
  res[, h15 := !is.na(o15)]
  res[, h16 := !is.na(o16)]
  res[, n_use := as.integer(h14) + as.integer(h15) + as.integer(h16)]

  res[, u15 := fifelse(h15, o15,
        fifelse(h16, fifelse(!is.na(ratio_15_16), o16 * ratio_15_16, direct_15),
          fifelse(h14, fifelse(!is.na(ratio_15_14), o14 * ratio_15_14, direct_15),
            NA_real_)))]
  res[, u14 := fifelse(h14, o14,
        fifelse(!is.na(u15) & !is.na(ratio_14_15), u15 * ratio_14_15, direct_14))]
  res[, u16 := fifelse(h16, o16,
        fifelse(!is.na(u15) & !is.na(ratio_16_15), u15 * ratio_16_15, direct_16))]

  res[, s15 := fcase(
        h15, "own",
        !h15 & h16 & !is.na(ratio_15_16), "imputed",
        !h15 & !h16 & h14 & !is.na(ratio_15_14), "imputed",
        n_use > 0 & !h15, "direct",
        default = NA_character_)]
  res[, s14 := fcase(
        h14, "own",
        n_use > 0 & !h14 & !is.na(u15) & !is.na(ratio_14_15), "imputed",
        n_use > 0 & !h14, "direct",
        default = NA_character_)]
  res[, s16 := fcase(
        h16, "own",
        n_use > 0 & !h16 & !is.na(u15) & !is.na(ratio_16_15), "imputed",
        n_use > 0 & !h16, "direct",
        default = NA_character_)]

  # swapped to the group's base unit value: no base year is built from the country's data
  res[swap == TRUE, `:=`(s14 = NA_character_, s15 = NA_character_, s16 = NA_character_)]

  res[, n_imputed := (s14 == "imputed" & !is.na(s14)) +
                     (s15 == "imputed" & !is.na(s15)) +
                     (s16 == "imputed" & !is.na(s16))]
  res[, n_direct  := (s14 == "direct"  & !is.na(s14)) +
                     (s15 == "direct"  & !is.na(s15)) +
                     (s16 == "direct"  & !is.na(s16))]

  # --- How each base year was built (read by the diagnostics dashboard)
  # status: own | imputed (own anchor year x group change) | direct (group's price that year)
  #         | group (no own base year: the group's base unit value is used)
  # tier 1-3 = smallest to largest parent group, 4 = all countries
  tier_ok <- function(k, tn, an) {
    a <- if (k == 4L) res[[paste0("gg", an)]] else res[[paste0("g", k, an)]]
    b <- if (k == 4L) res[[paste0("gg", tn)]] else res[[paste0("g", k, tn)]]
    !is.na(a) & !is.na(b) & a != 0
  }
  ratio_tier <- function(tn, an) fcase(tier_ok(1L, tn, an), 1L, tier_ok(2L, tn, an), 2L,
                                       tier_ok(3L, tn, an), 3L, tier_ok(4L, tn, an), 4L, default = NA_integer_)
  first_tier <- function(y) fcase(!is.na(res[[paste0("g1_", y)]]), 1L, !is.na(res[[paste0("g2_", y)]]), 2L,
                                  !is.na(res[[paste0("g3_", y)]]), 3L, !is.na(res[[paste0("gg_", y)]]), 4L,
                                  default = NA_integer_)
  base_tier <- fcase(!is.na(res$gb1), 1L, !is.na(res$gb2), 2L, !is.na(res$gb3), 3L,
                     !is.na(res$global_base), 4L, default = NA_integer_)
  tier_pick <- function(t, cols) {
    out <- rep(if (is.character(res[[cols[1]]])) NA_character_ else NA_real_, nrow(res))
    for (k in seq_along(cols)) { i <- which(t == k); if (length(i)) out[i] <- res[[cols[k]]][i] }
    out
  }
  tier_name <- function(t) { out <- tier_pick(t, c("gn1", "gn2", "gn3")); out[which(t == 4L)] <- "All countries"; out }
  tier_uv   <- function(t, y) tier_pick(t, c(paste0("g", 1:3, "_", y), paste0("gg_", y)))
  # the country's share of the donor group's base-year trade value (its own record moves the group's price)
  bg[, sh := v / sum(v), by = .(group_code, measuredItemCPC, timePointYears)]
  base_sub[, shg := v / sum(v), by = .(measuredItemCPC, timePointYears)]
  tier_share <- function(t, y) {
    gc <- tier_pick(t, c("gc1", "gc2", "gc3"))
    key <- data.table(geographicAreaM49 = res$geographicAreaM49, group_code = gc,
                      measuredItemCPC = res$measuredItemCPC, timePointYears = y)
    out <- bg[key, on = .(geographicAreaM49, group_code, measuredItemCPC, timePointYears), sh, mult = "first"]
    i4 <- which(t == 4L)
    if (length(i4)) out[i4] <- base_sub[key[i4], on = .(geographicAreaM49, measuredItemCPC, timePointYears), shg, mult = "first"]
    fcoalesce(out, 0)
  }
  how <- res[, .(geographicAreaM49, measuredItemCPC)]
  for (y in c("14", "15", "16")) {
    st <- res[[paste0("s", y)]]
    st[res$swap] <- "group"
    anc <- if (y == "15") fifelse(res$h16, "16", "14") else rep("15", nrow(res))
    rt_ <- if (y == "15") fifelse(res$h16, ratio_tier("_15", "_16"), ratio_tier("_15", "_14")) else ratio_tier(paste0("_", y), "_15")
    t <- fcase(st %in% "imputed", rt_, st %in% "direct", first_tier(y), st %in% "group", base_tier, default = NA_integer_)
    gm <- tier_uv(t, y)
    ga <- fifelse(anc == "16", tier_uv(t, "16"), fifelse(anc == "14", tier_uv(t, "14"), tier_uv(t, "15")))
    imp <- st %in% "imputed"
    shr <- pmax(tier_share(t, paste0("20", y)), fifelse(imp, tier_share(t, paste0("20", anc)), 0))
    how[, (paste0("st", y)) := st]
    how[, (paste0("u", y)) := fifelse(st %in% "group", NA_real_, res[[paste0("u", y)]])]
    how[, (paste0("anc", y)) := fifelse(imp, as.integer(paste0("20", anc)), NA_integer_)]
    how[, (paste0("grp", y)) := fifelse(st %in% c("imputed", "direct", "group"), tier_name(t), NA_character_)]
    how[, (paste0("gm", y)) := gm]
    how[, (paste0("ga", y)) := fifelse(imp, ga, NA_real_)]
    how[, (paste0("sh", y)) := fifelse(st %in% c("imputed", "direct", "group"), shr, NA_real_)]
    how[, (paste0("x", y)) := res[[paste0("x", y)]]]   # own value set aside (more than 3x from the median)
  }
  how[, rule := res$uv_rule]
  how[, own_md := res$own_md]

  res[, base_uv := fifelse(swap, group_base_co, (u14 + u15 + u16) / 3)]
  res[, uv_source := fcase(
        swap, group_base_src,
        uv_rule == "own_3", "country",
        default    = "imputed")]
  res[is.na(base_uv), uv_source := NA_character_]

  list(
    base = res[, .(geographicAreaM49, measuredItemCPC, base_uv, uv_source,
                   n_own, n_imputed, n_direct, uv_rule)],
    base_sub = base_sub[, .(geographicAreaM49, measuredItemCPC, timePointYears, q, v)],
    how = how
  )
}

imp_built <- build_base_uv("imp_q", "imp_v")
exp_built <- build_base_uv("exp_q", "exp_v")

imp_uv_base <- copy(imp_built$base)
setnames(imp_uv_base,
         c("base_uv", "uv_source", "n_own", "n_imputed", "n_direct", "uv_rule"),
         c("base_uv_imp", "uv_source_imp", "base_imp_yrs_valid", "n_imp_imputed", "n_imp_direct", "uv_rule_imp"))
exp_uv_base <- copy(exp_built$base)
setnames(exp_uv_base,
         c("base_uv", "uv_source", "n_own", "n_imputed", "n_direct", "uv_rule"),
         c("base_uv_exp", "uv_source_exp", "base_exp_yrs_valid", "n_exp_imputed", "n_exp_direct", "uv_rule_exp"))

imp_uv_how <- imp_built$how   # per-year build of the base UV, for the diagnostics dashboard
exp_uv_how <- exp_built$how
imp_base <- copy(imp_built$base_sub); setnames(imp_base, c("q", "v"), c("imp_q", "imp_v"))
exp_base <- copy(exp_built$base_sub); setnames(exp_base, c("q", "v"), c("exp_q", "exp_v"))

# --- 4C: Reporting
cat("\n-- Import base-UV source breakdown --\n")
print(imp_uv_base[, .N, by = uv_source_imp][order(-N)])
cat("\n-- Export base-UV source breakdown --\n")
print(exp_uv_base[, .N, by = uv_source_exp][order(-N)])
UV_RULE_LAB <- c(own_3 = "3 own years, within 3x - kept", fix_3 = "3 own years, year(s) >3x from median - growth rate",
                 fill_2 = "2 own years, within 3x - growth rate", group_2_far = "2 own years, >3x apart - group's UV",
                 group_1 = "1 own year - group's UV", group_0 = "0 own years - group's UV")
cat("\n-- Base-UV rule applied (3x rule, 2026-10-27) --\n")
print(merge(imp_uv_base[, .(Import = .N), by = .(rule = uv_rule_imp)],
            exp_uv_base[, .(Export = .N), by = .(rule = uv_rule_exp)], by = "rule", all = TRUE)[
        order(match(rule, names(UV_RULE_LAB)))][, label := UV_RULE_LAB[rule]][])

report_uv <- function(b, uvc, srcc, yrsc, impc, dirc, label) {
  cat(sprintf("\n%s: %s flows with a base UV  |  %s with none\n", label,
      formatC(b[!is.na(get(uvc)), .N], format = "d", big.mark = ","),
      formatC(b[is.na(get(uvc)), .N],  format = "d", big.mark = ",")))
  cat(sprintf("   all-3-own (country): %s | partial (imputed): %s | direct-hierarchy: %s\n",
      formatC(b[get(srcc) == "country", .N], format = "d", big.mark = ","),
      formatC(b[get(srcc) == "imputed", .N], format = "d", big.mark = ","),
      formatC(b[!is.na(get(uvc)) & !get(srcc) %in% c("country", "imputed"), .N],
              format = "d", big.mark = ",")))
  cat(sprintf("   base-years filled — own: %s | imputed: %s | direct: %s\n",
      formatC(b[, sum(get(yrsc))], format = "d", big.mark = ","),
      formatC(b[, sum(get(impc))], format = "d", big.mark = ","),
      formatC(b[, sum(get(dirc))], format = "d", big.mark = ",")))
}
report_uv(imp_uv_base, "base_uv_imp", "uv_source_imp",
          "base_imp_yrs_valid", "n_imp_imputed", "n_imp_direct", "Import")
report_uv(exp_uv_base, "base_uv_exp", "uv_source_exp",
          "base_exp_yrs_valid", "n_exp_imputed", "n_exp_direct", "Export")

cat("\nImport base UV distribution (1000 USD / tonne):\n")
cat(sprintf("  Min: %.4f | p1: %.4f | Median: %.4f | p99: %.4f | Max: %.4f\n",
    quantile(imp_uv_base$base_uv_imp, 0.00, na.rm = TRUE),
    quantile(imp_uv_base$base_uv_imp, 0.01, na.rm = TRUE),
    quantile(imp_uv_base$base_uv_imp, 0.50, na.rm = TRUE),
    quantile(imp_uv_base$base_uv_imp, 0.99, na.rm = TRUE),
    quantile(imp_uv_base$base_uv_imp, 1.00, na.rm = TRUE)))
cat("\nExport base UV distribution (1000 USD / tonne):\n")
cat(sprintf("  Min: %.4f | p1: %.4f | Median: %.4f | p99: %.4f | Max: %.4f\n",
    quantile(exp_uv_base$base_uv_exp, 0.00, na.rm = TRUE),
    quantile(exp_uv_base$base_uv_exp, 0.01, na.rm = TRUE),
    quantile(exp_uv_base$base_uv_exp, 0.50, na.rm = TRUE),
    quantile(exp_uv_base$base_uv_exp, 0.99, na.rm = TRUE),
    quantile(exp_uv_base$base_uv_exp, 1.00, na.rm = TRUE)))

cat("\n--- Step 4 complete. Objects: imp_uv_base, exp_uv_base, imp_base, exp_base ---\n")
cat("Proceed to Step 5 (real values and indices) when ready.\n")

# STEP 5 — Real values (item level)
# Item real value = quantity x 2014-2016 base unit value. It is the building block
# of the aggregate real values (Step 6) and is not written out on its own.

cat("\n=== STEP 5: REAL VALUES (ITEM LEVEL) ===\n")

# --- 5A: Merge the base unit values onto trade_wide

trade_calc <- copy(trade_wide)

trade_calc <- merge(trade_calc,
  imp_uv_base[, .(geographicAreaM49, measuredItemCPC, base_uv_imp, uv_source_imp)],
  by = c("geographicAreaM49", "measuredItemCPC"), all.x = TRUE)

trade_calc <- merge(trade_calc,
  exp_uv_base[, .(geographicAreaM49, measuredItemCPC, base_uv_exp, uv_source_exp)],
  by = c("geographicAreaM49", "measuredItemCPC"), all.x = TRUE)

# --- 5B: Real values

trade_calc[!is.na(imp_q) & !is.na(base_uv_imp),
           real_v_imp := imp_q * base_uv_imp]

trade_calc[!is.na(exp_q) & !is.na(base_uv_exp),
           real_v_exp := exp_q * base_uv_exp]

# --- 5C: Reporting

cat("Real value coverage (non-NA rows):\n")
cat("  Import real value:", formatC(trade_calc[!is.na(real_v_imp), .N], format="d", big.mark=","), "\n")
cat("  Export real value:", formatC(trade_calc[!is.na(real_v_exp), .N], format="d", big.mark=","), "\n")

cat("\n--- Step 5 complete. Object: trade_calc ---\n")

# STEP 5 — OUTPUT: Write base unit values (QA file)

cat("\n=== STEP 5 OUTPUT: WRITING FILES ===\n")

# --- Base unit values (QA file): the 2014-2016 base UV behind every real value,
# and how it was derived. All items (every item feeds the aggregate real values),
# disseminated areas.
base_uv_chk <- rbindlist(list(
  Import = imp_uv_base[, .(Area_M49 = geographicAreaM49, Item_CPC = measuredItemCPC,
                           Base_UV = base_uv_imp, UV_Source = uv_source_imp, UV_Rule = uv_rule_imp,
                           Base_Yrs_Own = base_imp_yrs_valid,
                           N_Imputed = n_imp_imputed, N_Direct = n_imp_direct)],
  Export = exp_uv_base[, .(Area_M49 = geographicAreaM49, Item_CPC = measuredItemCPC,
                           Base_UV = base_uv_exp, UV_Source = uv_source_exp, UV_Rule = uv_rule_exp,
                           Base_Yrs_Own = base_exp_yrs_valid,
                           N_Imputed = n_exp_imputed, N_Direct = n_exp_direct)]
), idcol = "Flow")
base_uv_chk <- keep_diss(base_uv_chk, "Area_M49")
item_names_all <- unique(agg_table[var_type == "item", .(Item_CPC = gsub("'", "", member_code), Item_Name = member_name)],
                         by = "Item_CPC")
if (file.exists("ItemCodes.csv")) {
  ic_all <- fread("ItemCodes.csv", colClasses = "character", encoding = "UTF-8")
  item_names_all <- unique(rbind(item_names_all, ic_all[, .(Item_CPC = gsub("'", "", `CPC Code`), Item_Name = Item)]), by = "Item_CPC")
  rm(ic_all)
}
base_uv_chk[, Item_Name := item_names_all$Item_Name[match(gsub("'", "", Item_CPC), item_names_all$Item_CPC)]]
base_uv_chk[, Area_Name := area_name_of(Area_M49)]
setcolorder(base_uv_chk, c("Area_M49", "Area_Name", "Item_CPC", "Item_Name", "Flow"))
base_uv_chk <- base_uv_chk[order(as.integer(Area_M49), Item_CPC, flow_ord(Flow))]
fwrite(base_uv_chk, "results/checks/base_unit_values.csv")
cat(sprintf("Saved: checks/base_unit_values.csv  (%s rows)\n",
            formatC(nrow(base_uv_chk), format = "d", big.mark = ",")))
rm(base_uv_chk)

cat("\n--- Step 5 outputs complete. Proceed to Step 6 (aggregations) when ready. ---\n")

# STEP 6 — Aggregations

cat("\n=== STEP 6: AGGREGATIONS ===\n")

# --- 6A: Item group mapping

item_map <- agg_table[var_type == "item", .(
  item_group_code = group_code,
  item_group_name = group_name,
  measuredItemCPC = member_code,
  item_factor     = as.numeric(factor)
)]

# Only item groups with dissemination == TRUE in DisseminationList.csv are
# calculated; the other groups of the aggregation table are dropped here.
grp_diss <- intersect(unique(item_map$item_group_code), disseminated_codes)
if (length(grp_diss)) {
  grp_drop <- unique(item_map[!item_group_code %in% grp_diss, .(item_group_code, item_group_name)])
  item_map <- item_map[item_group_code %in% grp_diss]
  if (nrow(grp_drop)) cat(sprintf("Item groups not in DisseminationList.csv (dropped): %s\n",
                                  paste(grp_drop$item_group_name, collapse = " | ")))
}

# Item groups excluded from the calculations (excluded_groups, Step 3) are dropped
# here; their items stay in every other group.
grp_excl <- unique(item_map[item_group_code %in% excluded_groups$code, .(item_group_code, item_group_name)])
item_map <- item_map[!item_group_code %in% excluded_groups$code]
cat(sprintf("Item groups excluded from the calculations (%d): %s\n", nrow(grp_excl),
            paste(grp_excl$item_group_name, collapse = " | ")))

cat(sprintf("Item map: %d groups, %d item-group pairs\n",
            uniqueN(item_map$item_group_code), nrow(item_map)))
cat(sprintf("Items appearing in >1 group: %d\n",
            item_map[, .N, by = measuredItemCPC][N > 1, .N]))

items_in_data <- unique(trade_calc$measuredItemCPC)
items_mapped  <- sum(items_in_data %in% item_map$measuredItemCPC)
cat(sprintf("Items in trade data: %d  |  Mapped to a group: %d  |  Unmapped: %d\n",
            length(items_in_data), items_mapped,
            length(items_in_data) - items_mapped))

# --- 6B: Area group mapping for aggregation

# "Excluding intra-trade" regions (codes with a dot, e.g. 002.03) are not published
# (decided 2026-10-27), so they are not built.
area_agg_map <- agg_table[var_type == "area" & as.numeric(factor) == 1 & !grepl("\\.", group_code), .(
  area_group_code   = group_code,
  area_group_name   = group_name,
  geographicAreaM49 = member_code
)]

# Aggregation control: keep only member countries and target regions
# flagged Aggregation=TRUE (no-op if the list is absent).
if (length(aggr_areas)) {
  area_agg_map <- area_agg_map[norm_area(geographicAreaM49) %in% aggr_areas &
                               norm_area(area_group_code)   %in% aggr_areas]
}

cat(sprintf("Area agg map: %d groups, %d country-group pairs\n",
            uniqueN(area_agg_map$area_group_code), nrow(area_agg_map)))

# --- 6C: Merge trade data with item groups

trade_for_agg <- trade_calc[, .(
  geographicAreaM49, measuredItemCPC, timePointYears,
  imp_v, imp_q, real_v_imp,
  exp_v, exp_q, real_v_exp
)]

trade_item_merge <- merge(
  trade_for_agg,
  item_map,
  by    = "measuredItemCPC",
  allow.cartesian = TRUE
)

cat(sprintf("\nRows after merging with item groups: %s\n",
            formatC(nrow(trade_item_merge), format = "d", big.mark = ",")))

# --- 6D: Aggregate to country × item_group × year

country_item_grp <- trade_item_merge[, .(
  agg_imp_v      = sum(imp_v,                    na.rm = TRUE),
  agg_imp_q      = sum(imp_q    * item_factor,   na.rm = TRUE),
  agg_real_v_imp = sum(real_v_imp,               na.rm = TRUE),  # no item factor: real value is already in base-year USD
  agg_exp_v      = sum(exp_v,                    na.rm = TRUE),
  agg_exp_q      = sum(exp_q    * item_factor,   na.rm = TRUE),
  agg_real_v_exp = sum(real_v_exp,               na.rm = TRUE),
  n_v_imp   = sum(!is.na(imp_v)),
  n_q_imp   = sum(!is.na(imp_q)),
  n_rv_imp  = sum(!is.na(real_v_imp)),
  n_v_exp   = sum(!is.na(exp_v)),
  n_q_exp   = sum(!is.na(exp_q)),
  n_rv_exp  = sum(!is.na(real_v_exp))
), by = .(geographicAreaM49, item_group_code, item_group_name, timePointYears)]

country_item_grp[n_v_imp  == 0, agg_imp_v      := NA_real_]
country_item_grp[n_q_imp  == 0, agg_imp_q      := NA_real_]
country_item_grp[n_rv_imp == 0, agg_real_v_imp := NA_real_]
country_item_grp[n_v_exp  == 0, agg_exp_v      := NA_real_]
country_item_grp[n_q_exp  == 0, agg_exp_q      := NA_real_]
country_item_grp[n_rv_exp == 0, agg_real_v_exp := NA_real_]
country_item_grp[, c("n_v_imp","n_q_imp","n_rv_imp","n_v_exp","n_q_exp","n_rv_exp") := NULL]

cat(sprintf("Country × item_group × year rows: %s\n",
            formatC(nrow(country_item_grp), format = "d", big.mark = ",")))

# --- 6E: Indices for country × item_group (value and quantity)
# Base period means computed from the aggregated series directly. The aggregate
# real value is an output in its own right; no real value index or unit value
# index is produced.

cig_base <- country_item_grp[timePointYears %in% base_period, .(
  base_mean_imp_v  = mean(agg_imp_v,      na.rm = TRUE),
  base_mean_imp_q  = mean(agg_imp_q,      na.rm = TRUE),
  base_mean_exp_v  = mean(agg_exp_v,      na.rm = TRUE),
  base_mean_exp_q  = mean(agg_exp_q,      na.rm = TRUE)
), by = .(geographicAreaM49, item_group_code)]

for (col in c("base_mean_imp_v","base_mean_imp_q",
              "base_mean_exp_v","base_mean_exp_q")) {
  cig_base[is.nan(get(col)), (col) := NA_real_]
}

country_item_grp <- merge(country_item_grp, cig_base,
                           by = c("geographicAreaM49", "item_group_code"), all.x = TRUE)

for (col in c("idx_v_imp","idx_q_imp","idx_v_exp","idx_q_exp")) {
  country_item_grp[, (col) := NA_real_]
}

country_item_grp[!is.na(agg_imp_v)      & !is.na(base_mean_imp_v)  & base_mean_imp_v  > 0,
                  idx_v_imp  := (agg_imp_v      / base_mean_imp_v)  * 100]
country_item_grp[!is.na(agg_imp_q)      & !is.na(base_mean_imp_q)  & base_mean_imp_q  > 0,
                  idx_q_imp  := (agg_imp_q      / base_mean_imp_q)  * 100]
country_item_grp[!is.na(agg_exp_v)      & !is.na(base_mean_exp_v)  & base_mean_exp_v  > 0,
                  idx_v_exp  := (agg_exp_v      / base_mean_exp_v)  * 100]
country_item_grp[!is.na(agg_exp_q)      & !is.na(base_mean_exp_q)  & base_mean_exp_q  > 0,
                  idx_q_exp  := (agg_exp_q      / base_mean_exp_q)  * 100]

country_item_grp[, c("base_mean_imp_v","base_mean_imp_q",
                      "base_mean_exp_v","base_mean_exp_q") := NULL]

cat("\nCountry-item group index coverage (non-NA rows):\n")
cat("  Import value index    :", formatC(country_item_grp[!is.na(idx_v_imp),  .N], format="d", big.mark=","), "\n")
cat("  Import quantity index :", formatC(country_item_grp[!is.na(idx_q_imp),  .N], format="d", big.mark=","), "\n")
cat("  Export value index    :", formatC(country_item_grp[!is.na(idx_v_exp),  .N], format="d", big.mark=","), "\n")
cat("  Export quantity index :", formatC(country_item_grp[!is.na(idx_q_exp),  .N], format="d", big.mark=","), "\n")

# Base period sanity check for aggregate indices
cat("\nBase period (2014-2016) mean of aggregate indices — should be near 100:\n")
bp <- country_item_grp[timePointYears %in% base_period]
cat("  Import value index    :", round(bp[, mean(idx_v_imp,  na.rm=TRUE)], 2), "\n")
cat("  Import quantity index :", round(bp[, mean(idx_q_imp,  na.rm=TRUE)], 2), "\n")
cat("  Export value index    :", round(bp[, mean(idx_v_exp,  na.rm=TRUE)], 2), "\n")
cat("  Export quantity index :", round(bp[, mean(idx_q_exp,  na.rm=TRUE)], 2), "\n")

# --- 6F: Area-level aggregation (area_group × item_group × year)

area_item_merge <- merge(
  country_item_grp[, .(geographicAreaM49, item_group_code, item_group_name, timePointYears,
                        agg_imp_v, agg_imp_q, agg_real_v_imp,
                        agg_exp_v, agg_exp_q, agg_real_v_exp)],
  area_agg_map,
  by              = "geographicAreaM49",
  allow.cartesian = TRUE
)

area_item_grp <- area_item_merge[, .(
  agg_imp_v      = sum(agg_imp_v,      na.rm = TRUE),
  agg_imp_q      = sum(agg_imp_q,      na.rm = TRUE),
  agg_real_v_imp = sum(agg_real_v_imp, na.rm = TRUE),
  agg_exp_v      = sum(agg_exp_v,      na.rm = TRUE),
  agg_exp_q      = sum(agg_exp_q,      na.rm = TRUE),
  agg_real_v_exp = sum(agg_real_v_exp, na.rm = TRUE),
  n_v_imp   = sum(!is.na(agg_imp_v)),
  n_q_imp   = sum(!is.na(agg_imp_q)),
  n_rv_imp  = sum(!is.na(agg_real_v_imp)),
  n_v_exp   = sum(!is.na(agg_exp_v)),
  n_q_exp   = sum(!is.na(agg_exp_q)),
  n_rv_exp  = sum(!is.na(agg_real_v_exp))
), by = .(area_group_code, area_group_name, item_group_code, item_group_name, timePointYears)]

area_item_grp[n_v_imp  == 0, agg_imp_v      := NA_real_]
area_item_grp[n_q_imp  == 0, agg_imp_q      := NA_real_]
area_item_grp[n_rv_imp == 0, agg_real_v_imp := NA_real_]
area_item_grp[n_v_exp  == 0, agg_exp_v      := NA_real_]
area_item_grp[n_q_exp  == 0, agg_exp_q      := NA_real_]
area_item_grp[n_rv_exp == 0, agg_real_v_exp := NA_real_]
area_item_grp[, c("n_v_imp","n_q_imp","n_rv_imp","n_v_exp","n_q_exp","n_rv_exp") := NULL]

cat(sprintf("\nArea × item_group × year rows: %s\n",
            formatC(nrow(area_item_grp), format = "d", big.mark = ",")))

# --- 6G: Indices for area × item groups (value and quantity)

aig_base <- area_item_grp[timePointYears %in% base_period, .(
  base_mean_imp_v  = mean(agg_imp_v,      na.rm = TRUE),
  base_mean_imp_q  = mean(agg_imp_q,      na.rm = TRUE),
  base_mean_exp_v  = mean(agg_exp_v,      na.rm = TRUE),
  base_mean_exp_q  = mean(agg_exp_q,      na.rm = TRUE)
), by = .(area_group_code, item_group_code)]

for (col in c("base_mean_imp_v","base_mean_imp_q",
              "base_mean_exp_v","base_mean_exp_q")) {
  aig_base[is.nan(get(col)), (col) := NA_real_]
}

area_item_grp <- merge(area_item_grp, aig_base,
                        by = c("area_group_code", "item_group_code"), all.x = TRUE)

for (col in c("idx_v_imp","idx_q_imp","idx_v_exp","idx_q_exp")) {
  area_item_grp[, (col) := NA_real_]
}

area_item_grp[!is.na(agg_imp_v)      & !is.na(base_mean_imp_v)  & base_mean_imp_v  > 0,
               idx_v_imp  := (agg_imp_v      / base_mean_imp_v)  * 100]
area_item_grp[!is.na(agg_imp_q)      & !is.na(base_mean_imp_q)  & base_mean_imp_q  > 0,
               idx_q_imp  := (agg_imp_q      / base_mean_imp_q)  * 100]
area_item_grp[!is.na(agg_exp_v)      & !is.na(base_mean_exp_v)  & base_mean_exp_v  > 0,
               idx_v_exp  := (agg_exp_v      / base_mean_exp_v)  * 100]
area_item_grp[!is.na(agg_exp_q)      & !is.na(base_mean_exp_q)  & base_mean_exp_q  > 0,
               idx_q_exp  := (agg_exp_q      / base_mean_exp_q)  * 100]

area_item_grp[, c("base_mean_imp_v","base_mean_imp_q",
                   "base_mean_exp_v","base_mean_exp_q") := NULL]

cat("\nArea-item group index coverage (non-NA rows):\n")
cat("  Import value index    :", formatC(area_item_grp[!is.na(idx_v_imp),  .N], format="d", big.mark=","), "\n")
cat("  Import quantity index :", formatC(area_item_grp[!is.na(idx_q_imp),  .N], format="d", big.mark=","), "\n")
cat("  Export value index    :", formatC(area_item_grp[!is.na(idx_v_exp),  .N], format="d", big.mark=","), "\n")
cat("  Export quantity index :", formatC(area_item_grp[!is.na(idx_q_exp),  .N], format="d", big.mark=","), "\n")

# --- 6H: Write the aggregate outputs

cig_out <- copy(country_item_grp)
setnames(cig_out,
  old = c("geographicAreaM49","item_group_code","item_group_name","timePointYears",
          "agg_imp_v","agg_imp_q","agg_real_v_imp",
          "agg_exp_v","agg_exp_q","agg_real_v_exp",
          "idx_v_imp","idx_q_imp",
          "idx_v_exp","idx_q_exp"),
  new = c("Area_M49","Item_Group_Code","Item_Group_Name","Year",
          "Import_Value_1000USD","Import_Quantity","Import_RealValue_1000USD",
          "Export_Value_1000USD","Export_Quantity","Export_RealValue_1000USD",
          "Import_Value_Index","Import_Qty_Index",
          "Export_Value_Index","Export_Qty_Index"))
cig_out <- cig_out[order(Area_M49, Item_Group_Code, Year)]

aig_out <- copy(area_item_grp)
setnames(aig_out,
  old = c("area_group_code","area_group_name","item_group_code","item_group_name","timePointYears",
          "agg_imp_v","agg_imp_q","agg_real_v_imp",
          "agg_exp_v","agg_exp_q","agg_real_v_exp",
          "idx_v_imp","idx_q_imp",
          "idx_v_exp","idx_q_exp"),
  new = c("Area_Group_Code","Area_Group_Name","Item_Group_Code","Item_Group_Name","Year",
          "Import_Value_1000USD","Import_Quantity","Import_RealValue_1000USD",
          "Export_Value_1000USD","Export_Quantity","Export_RealValue_1000USD",
          "Import_Value_Index","Import_Qty_Index",
          "Export_Value_Index","Export_Qty_Index"))
aig_out <- aig_out[order(Area_Group_Code, Item_Group_Code, Year)]

# --- Country and region aggregates in one table (Level column),
# one row per area x item group x year, import and export columns side by side
agg_level <- function(dt, level, code_col) {
  dt  <- keep_diss(dt, code_col)
  col <- function(n) if (n %in% names(dt)) dt[[n]] else NA_real_
  out <- data.table(
    Level             = level,
    `Area Code (M49)` = as.character(dt[[code_col]]),
    Area              = if ("Area_Group_Name" %in% names(dt)) dt$Area_Group_Name else area_name_of(dt[[code_col]]),
    `Item Group Code` = dt$Item_Group_Code,
    `Item Group`      = dt$Item_Group_Name,
    Year              = as.character(dt$Year))
  for (f in c("Import", "Export")) {
    out[, paste(f, "Quantity (t)")          := col(paste0(f, "_Quantity"))]
    out[, paste(f, "Value (1000 USD)")      := col(paste0(f, "_Value_1000USD"))]
    out[, paste(f, "Real Value (1000 USD)") := col(paste0(f, "_RealValue_1000USD"))]
    out[, paste(f, "Value Index")           := col(paste0(f, "_Value_Index"))]
    out[, paste(f, "Quantity Index")        := col(paste0(f, "_Qty_Index"))]
  }
  out
}
agg_all <- rbind(agg_level(cig_out, "Country", "Area_M49"),
                 agg_level(aig_out, "Region",  "Area_Group_Code"))
agg_all <- agg_all[order(match(Level, c("Country", "Region")),
                         suppressWarnings(as.integer(`Area Code (M49)`)), `Item Group Code`, Year)]

agg_id   <- c("Level", "Area Code (M49)", "Area", "Item Group Code", "Item Group", "Year")
rv_cols  <- grep("Real Value \\(1000 USD\\)", names(agg_all), value = TRUE)   # import and export real value only
idx_cols <- grep("Index", names(agg_all), value = TRUE)

rv_agg  <- agg_all[, c(agg_id, rv_cols), with = FALSE]
rv_agg  <- rv_agg[rowSums(!is.na(rv_agg[, ..rv_cols])) > 0]
idx_agg <- agg_all[, c(agg_id, idx_cols), with = FALSE]
idx_agg <- idx_agg[rowSums(!is.na(idx_agg[, ..idx_cols])) > 0]

fwrite(rv_agg,  "results/real_values_aggregates.csv")
fwrite(idx_agg, "results/indices_aggregates.csv")
cat(sprintf("\nSaved: real_values_aggregates.csv  (%s rows)\n",
            formatC(nrow(rv_agg), format = "d", big.mark = ",")))
cat(sprintf("Saved: indices_aggregates.csv     (%s rows)\n",
            formatC(nrow(idx_agg), format = "d", big.mark = ",")))
rm(agg_all, rv_agg, idx_agg)

cat("\n--- Step 6 complete ---\n")

# STEP 6.5 — Outlier detection on aggregate indices (value and quantity)

cat("\n=== STEP 6.5: OUTLIER DETECTION ON AGGREGATE INDICES ===\n")

abs_high_agg <- 5000

cat(sprintf("Threshold: index > %g (50x base period level)\n", abs_high_agg))

# --- Country × item group outliers

setorder(country_item_grp, geographicAreaM49, item_group_code, timePointYears)

country_item_grp[, yoy_v_imp  := idx_v_imp  / shift(idx_v_imp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, yoy_q_imp  := idx_q_imp  / shift(idx_q_imp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, yoy_v_exp  := idx_v_exp  / shift(idx_v_exp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, yoy_q_exp  := idx_q_exp  / shift(idx_q_exp),  by = .(geographicAreaM49, item_group_code)]

country_item_grp[, prev_v_imp  := shift(idx_v_imp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, prev_q_imp  := shift(idx_q_imp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, prev_v_exp  := shift(idx_v_exp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, prev_q_exp  := shift(idx_q_exp),  by = .(geographicAreaM49, item_group_code)]

build_agg_outlier_cig <- function(dt, idx_col, prev_col, yoy_col, flow, index_label) {
  rows <- dt[!is.na(get(idx_col)) & get(idx_col) > abs_high_agg]
  if (nrow(rows) == 0) return(data.table())
  rows[, .(
    Level        = "Country_ItemGroup",
    Flow         = flow,
    Index        = index_label,
    Area         = geographicAreaM49,
    Item_Group   = item_group_code,
    Item_Group_Name = item_group_name,
    Year         = timePointYears,
    Index_Value  = get(idx_col),
    Prev_Value   = round(get(prev_col), 2),
    YoY_Ratio    = round(get(yoy_col), 3),
    Flag_Reason  = paste0("Level > ", abs_high_agg)
  )]
}

cig_outliers <- rbind(
  build_agg_outlier_cig(country_item_grp, "idx_v_imp",  "prev_v_imp",  "yoy_v_imp",  "Import", "Value"),
  build_agg_outlier_cig(country_item_grp, "idx_q_imp",  "prev_q_imp",  "yoy_q_imp",  "Import", "Quantity"),
  build_agg_outlier_cig(country_item_grp, "idx_v_exp",  "prev_v_exp",  "yoy_v_exp",  "Export", "Value"),
  build_agg_outlier_cig(country_item_grp, "idx_q_exp",  "prev_q_exp",  "yoy_q_exp",  "Export", "Quantity")
)

country_item_grp[, c("yoy_v_imp","yoy_q_imp","yoy_v_exp","yoy_q_exp",
                      "prev_v_imp","prev_q_imp","prev_v_exp","prev_q_exp") := NULL]

# --- Area × item group outliers

setorder(area_item_grp, area_group_code, item_group_code, timePointYears)

area_item_grp[, yoy_v_imp  := idx_v_imp  / shift(idx_v_imp),  by = .(area_group_code, item_group_code)]
area_item_grp[, yoy_q_imp  := idx_q_imp  / shift(idx_q_imp),  by = .(area_group_code, item_group_code)]
area_item_grp[, yoy_v_exp  := idx_v_exp  / shift(idx_v_exp),  by = .(area_group_code, item_group_code)]
area_item_grp[, yoy_q_exp  := idx_q_exp  / shift(idx_q_exp),  by = .(area_group_code, item_group_code)]

area_item_grp[, prev_v_imp  := shift(idx_v_imp),  by = .(area_group_code, item_group_code)]
area_item_grp[, prev_q_imp  := shift(idx_q_imp),  by = .(area_group_code, item_group_code)]
area_item_grp[, prev_v_exp  := shift(idx_v_exp),  by = .(area_group_code, item_group_code)]
area_item_grp[, prev_q_exp  := shift(idx_q_exp),  by = .(area_group_code, item_group_code)]

build_agg_outlier_aig <- function(dt, idx_col, prev_col, yoy_col, flow, index_label) {
  rows <- dt[!is.na(get(idx_col)) & get(idx_col) > abs_high_agg]
  if (nrow(rows) == 0) return(data.table())
  rows[, .(
    Level           = "Area_ItemGroup",
    Flow            = flow,
    Index           = index_label,
    Area            = area_group_code,
    Area_Name       = area_group_name,
    Item_Group      = item_group_code,
    Item_Group_Name = item_group_name,
    Year            = timePointYears,
    Index_Value     = get(idx_col),
    Prev_Value      = round(get(prev_col), 2),
    YoY_Ratio       = round(get(yoy_col), 3),
    Flag_Reason     = paste0("Level > ", abs_high_agg)
  )]
}

aig_outliers <- rbind(
  build_agg_outlier_aig(area_item_grp, "idx_v_imp",  "prev_v_imp",  "yoy_v_imp",  "Import", "Value"),
  build_agg_outlier_aig(area_item_grp, "idx_q_imp",  "prev_q_imp",  "yoy_q_imp",  "Import", "Quantity"),
  build_agg_outlier_aig(area_item_grp, "idx_v_exp",  "prev_v_exp",  "yoy_v_exp",  "Export", "Value"),
  build_agg_outlier_aig(area_item_grp, "idx_q_exp",  "prev_q_exp",  "yoy_q_exp",  "Export", "Quantity")
)

area_item_grp[, c("yoy_v_imp","yoy_q_imp","yoy_v_exp","yoy_q_exp",
                   "prev_v_imp","prev_q_imp","prev_v_exp","prev_q_exp") := NULL]

# --- Combine and write

agg_outliers_all <- rbind(cig_outliers, aig_outliers, fill = TRUE)
setorder(agg_outliers_all, -Index_Value)

cat(sprintf("\nTotal flagged aggregate rows: %s\n",
            formatC(nrow(agg_outliers_all), format = "d", big.mark = ",")))

if (nrow(agg_outliers_all) > 0) {
  cat("\nBreakdown by level and index:\n")
  print(agg_outliers_all[, .N, by = .(Level, Flow, Index)][order(Level, Flow, Index)])
} else {
  cat("No aggregate indices exceed the threshold — no outliers found.\n")
}

fwrite(agg_outliers_all, "results/checks/outlier_check_aggregates.csv")
cat("Saved: checks/outlier_check_aggregates.csv\n")

# STEP 6.5B — Underlying data for top 50 flagged aggregates

cat("\n=== STEP 6.5B: DRILL-DOWN FOR TOP 50 FLAGGED AGGREGATES ===\n")

if (nrow(agg_outliers_all) == 0) {

  cat("No flagged aggregates — drill-down file not written.\n")

} else {

  top50 <- head(agg_outliers_all, 50)
  cat(sprintf("Pulling underlying data for %d flagged rows...\n", nrow(top50)))

  drilldown_list <- vector("list", nrow(top50))

  for (i in seq_len(nrow(top50))) {

    row      <- top50[i]
    yr       <- row$Year
    flow     <- row$Flow
    idx_type <- row$Index
    level    <- row$Level

    q_col  <- if (flow == "Import") "imp_q"      else "exp_q"
    v_col  <- if (flow == "Import") "imp_v"      else "exp_v"
    rv_col <- if (flow == "Import") "real_v_imp" else "real_v_exp"

    if (level == "Country_ItemGroup") {

      # --- Country-level: drill down by item within the group
      area_code  <- row$Area
      grp_code   <- row$Item_Group

      items_in_grp <- item_map[item_group_code == grp_code, measuredItemCPC]

      underlying <- trade_calc[
        geographicAreaM49 == area_code &
        timePointYears    == yr        &
        measuredItemCPC   %in% items_in_grp,
        .(
          Flagged_Area        = area_code,
          Flagged_Item_Group  = grp_code,
          Flagged_Index_Value = row$Index_Value,
          Level               = level,
          Flow                = flow,
          Index               = idx_type,
          Year                = yr,
          Country             = geographicAreaM49,
          Item_CPC            = measuredItemCPC,
          Quantity            = get(q_col),
          Trade_Value_1000USD = get(v_col),
          Real_Value_1000USD  = get(rv_col)
        )
      ]

    } else {

      # --- Area-level: drill down by country AND item within group
      area_code <- row$Area
      grp_code  <- row$Item_Group

      countries_in_area <- area_agg_map[area_group_code == area_code, geographicAreaM49]

      items_in_grp <- item_map[item_group_code == grp_code, measuredItemCPC]

      underlying <- trade_calc[
        geographicAreaM49 %in% countries_in_area &
        timePointYears    == yr                  &
        measuredItemCPC   %in% items_in_grp,
        .(
          Flagged_Area        = area_code,
          Flagged_Area_Name   = row$Area_Name,
          Flagged_Item_Group  = grp_code,
          Flagged_Index_Value = row$Index_Value,
          Level               = level,
          Flow                = flow,
          Index               = idx_type,
          Year                = yr,
          Country             = geographicAreaM49,
          Item_CPC            = measuredItemCPC,
          Quantity            = get(q_col),
          Trade_Value_1000USD = get(v_col),
          Real_Value_1000USD  = get(rv_col)
        )
      ]
    }

    if (idx_type == "Quantity")  underlying <- underlying[!is.na(Quantity)]
    if (idx_type == "Value")     underlying <- underlying[!is.na(Trade_Value_1000USD)]

    drilldown_list[[i]] <- underlying
  }

  drilldown_all <- rbindlist(drilldown_list, fill = TRUE)
  setorder(drilldown_all, -Flagged_Index_Value, Country, Item_CPC)

  cat(sprintf("Drill-down rows written: %s\n",
              formatC(nrow(drilldown_all), format = "d", big.mark = ",")))

  fwrite(drilldown_all, "results/checks/outlier_drilldown_aggregates.csv")
  cat("Saved: checks/outlier_drilldown_aggregates.csv\n")
}

cat("\n--- Step 6.5 complete ---\n")

cat("\nAll output files written (results/):\n")
cat("  real_values_aggregates.csv               — aggregate real values (import, export)\n")
cat("  indices_aggregates.csv                   — aggregate value and quantity indices\n")
cat("  checks/base_unit_values.csv              — base unit value behind every real value\n")
cat("  checks/outlier_check_aggregates.csv      — aggregate index values above 5000\n")
cat("  checks/outlier_drilldown_aggregates.csv  — underlying data for the top 50 of them\n")

gs <- function(k) { v <- RUN_STATS[[k]]; if (is.null(v)) NA_integer_ else v }
fm <- function(x) if (length(x) == 0 || is.na(x)) "n/a" else
                    formatC(as.numeric(x), format = "d", big.mark = ",")
pc <- function(a, b) if (is.na(a) || is.na(b) || b == 0) "" else
                       sprintf(" (%.2f%%)", 100 * a / b)

cat("\n\n")
cat("=================================================================\n")
cat("                          RUN SUMMARY                            \n")
cat("=================================================================\n")

cat("\n-- 1. STARTING DATA ---------------------------------------------\n")
cat(sprintf("Rows, long format (country-item-element-year) : %s\n", fm(gs("rows_long"))))
cat(sprintf("Rows, wide format (country-item-year)         : %s\n", fm(gs("rows_wide"))))
cat(sprintf("Countries / items                             : %s / %s\n",
            fm(gs("countries_start")), fm(gs("items_start"))))
cat(sprintf("Year range                                    : %s - %s\n",
            gs("year_min"), gs("year_max")))
cat(sprintf("Potential data points (wide rows x 4 measures): %s\n", fm(gs("cells_total"))))

cat("\n-- 2. NAs IN THE RAW WIDE PANEL ---------------------------------\n")
cat(sprintf("imp_q NA: %s    imp_v NA: %s\n",
            fm(gs("na_imp_q_start")), fm(gs("na_imp_v_start"))))
cat(sprintf("exp_q NA: %s    exp_v NA: %s\n",
            fm(gs("na_exp_q_start")), fm(gs("na_exp_v_start"))))
cat(sprintf("Total NA data points: %s of %s%s\n",
            fm(gs("na_cells_start")), fm(gs("cells_total")),
            pc(gs("na_cells_start"), gs("cells_total"))))

cat("\n-- 3. LIVE ANIMALS EXCLUDED (CPC 021) ---------------------------\n")
cat(sprintf("Rows removed : %s\n", fm(gs("rows_dropped_live_animals"))))
cat(sprintf("Items removed: %s\n", fm(gs("items_dropped_live_animals"))))
cat(sprintf("Rows left    : %s\n", fm(gs("rows_after_live_animals"))))

cat("\n-- 4. ASYMMETRIC Q/V PAIRS SET TO NA ----------------------------\n")
cat(sprintf("Import  Q=0 & V>0  -> imp_q set NA : %s\n", fm(gs("asym_imp_q_to_na"))))
cat(sprintf("Import  V=0 & Q>0  -> imp_v set NA : %s\n", fm(gs("asym_imp_v_to_na"))))
cat(sprintf("Export  Q=0 & V>0  -> exp_q set NA : %s\n", fm(gs("asym_exp_q_to_na"))))
cat(sprintf("Export  V=0 & Q>0  -> exp_v set NA : %s\n", fm(gs("asym_exp_v_to_na"))))
cat(sprintf("TOTAL data points set to NA        : %s\n", fm(gs("asym_total_to_na"))))

cat("\n-- 5. NEGLIGIBLE OBSERVATIONS (Q<10 OR V<10) — NOT REMOVED ------\n")
cat(sprintf("Usable observations (Q>0 & V>0)   Import: %s  | Export: %s\n",
            fm(gs("obs_usable_imp")), fm(gs("obs_usable_exp"))))
cat(sprintf("Negligible (Q<10 or V<10)         Import: %s%s | Export: %s%s\n",
            fm(gs("negligible_imp")), pc(gs("negligible_imp"), gs("obs_usable_imp")),
            fm(gs("negligible_exp")), pc(gs("negligible_exp"), gs("obs_usable_exp"))))
cat(sprintf("   of which near-zero Q (0<Q<1)   Import: %s  | Export: %s\n",
            fm(gs("negl_nearq_imp")), fm(gs("negl_nearq_exp"))))
cat(sprintf("   of which small Q (1<=Q<10)     Import: %s  | Export: %s\n",
            fm(gs("negl_q1to10_imp")), fm(gs("negl_q1to10_exp"))))
cat(sprintf("   of which small V (V<10)        Import: %s  | Export: %s\n",
            fm(gs("negl_smallv_imp")), fm(gs("negl_smallv_exp"))))
cat("   NOTE: these are KEPT — the figures show what a filter would cost.\n")

if (exists("imp_uv_base") && exists("exp_uv_base")) {
  cat("\n-- 6. HOW BASE UVs WERE OBTAINED --------------------------------\n")
  uvline <- function(b, uvc, srcc, yrs, impc, dirc, label) {
    cat(sprintf("%s: %s flows\n", label, fm(nrow(b))))
    cat(sprintf("   all-3-own (country)  : %s\n", fm(b[get(srcc) == "country", .N])))
    cat(sprintf("   partial (imputed)    : %s\n", fm(b[get(srcc) == "imputed", .N])))
    cat(sprintf("   direct hierarchy     : %s\n",
                fm(b[!is.na(get(uvc)) & !get(srcc) %in% c("country", "imputed"), .N])))
    cat(sprintf("   no base UV at all    : %s\n", fm(b[is.na(get(uvc)), .N])))
    cat(sprintf("   base-years filled -> own: %s | imputed: %s | direct: %s\n",
                fm(b[, sum(get(yrs))]), fm(b[, sum(get(impc))]), fm(b[, sum(get(dirc))])))
  }
  uvline(imp_uv_base, "base_uv_imp", "uv_source_imp",
         "base_imp_yrs_valid", "n_imp_imputed", "n_imp_direct", "Import")
  uvline(exp_uv_base, "base_uv_exp", "uv_source_exp",
         "base_exp_yrs_valid", "n_exp_imputed", "n_exp_direct", "Export")
  cat("   3x rule applied (Import | Export):\n")
  for (r in names(UV_RULE_LAB))
    cat(sprintf("     %-58s: %s | %s\n", UV_RULE_LAB[[r]],
                fm(imp_uv_base[uv_rule_imp == r, .N]), fm(exp_uv_base[uv_rule_exp == r, .N])))

  RUN_STATS$uv_imp_country  <- imp_uv_base[uv_source_imp == "country", .N]
  RUN_STATS$uv_imp_imputed  <- imp_uv_base[uv_source_imp == "imputed", .N]
  RUN_STATS$uv_imp_direct   <- imp_uv_base[!is.na(base_uv_imp) &
                                 !uv_source_imp %in% c("country", "imputed"), .N]
  RUN_STATS$uv_imp_none     <- imp_uv_base[is.na(base_uv_imp), .N]
  RUN_STATS$uv_exp_country  <- exp_uv_base[uv_source_exp == "country", .N]
  RUN_STATS$uv_exp_imputed  <- exp_uv_base[uv_source_exp == "imputed", .N]
  RUN_STATS$uv_exp_direct   <- exp_uv_base[!is.na(base_uv_exp) &
                                 !uv_source_exp %in% c("country", "imputed"), .N]
  RUN_STATS$uv_exp_none     <- exp_uv_base[is.na(base_uv_exp), .N]
  RUN_STATS$uv_imp_yrs_own      <- imp_uv_base[, sum(base_imp_yrs_valid)]
  RUN_STATS$uv_imp_yrs_imputed  <- imp_uv_base[, sum(n_imp_imputed)]
  RUN_STATS$uv_imp_yrs_direct   <- imp_uv_base[, sum(n_imp_direct)]
  RUN_STATS$uv_exp_yrs_own      <- exp_uv_base[, sum(base_exp_yrs_valid)]
  RUN_STATS$uv_exp_yrs_imputed  <- exp_uv_base[, sum(n_exp_imputed)]
  RUN_STATS$uv_exp_yrs_direct   <- exp_uv_base[, sum(n_exp_direct)]
}

cat("\n=================================================================\n")
cat("                       END OF RUN SUMMARY                        \n")
cat("=================================================================\n")


# STEP 8 — Diagnostics dashboard (aggregate indices, aggregate real values,
# base unit values, dissemination groups)
# Rebuilds results/diagnostics_dashboard.html and the results/checks/diagnostics_*.csv
# files from this run's objects.
# Runs in its own environment so it does not overwrite any objects above.
# A failure here is reported but does not affect the outputs already written.

cat("\n=== STEP 8: DIAGNOSTICS DASHBOARD ===\n")
if (file.exists("diagnostics/build_dashboard.R")) {
  invisible(gc())
  tryCatch(
    source("diagnostics/build_dashboard.R", local = new.env()),
    error = function(e) cat("Dashboard not built:", conditionMessage(e), "\n")
  )
} else {
  cat("diagnostics/build_dashboard.R not found — dashboard skipped.\n")
}

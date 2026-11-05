# Trade Indices (value and quantity indices of item groups)
# Base period: 2014-2016 = 100
#
# Indices-only version of the Jul-Oct 2026 pipeline (archived in
# _ARCHIVE_Our_Work_Jul-Oct_2026/01_Pipeline/trade_calculations.R). Real values,
# base unit values and every calculation behind them were removed on 5 Nov 2026:
# real values are now produced on SWS with the new methodology. The indices do not
# use real values or base unit values, so they are unchanged.
#
# Outputs: aggregates only (item groups, for countries and regions): value index and
# quantity index. No item-level indices, no unit value index, no real value index.
# No data are filtered out: small, unusual or flagged records are kept as reported
# (only live animals are excluded and one-sided zeros are set to missing, Step 1).
# Item groups in excluded_groups (Step 3) are not calculated at all; their items
# still count in every other group they belong to.
#
# Run from this folder (Indices/). Input files are read from this folder if present,
# otherwise from the parent project folder.

library(data.table)
library(openxlsx)

# All generated outputs are written to the results/ subfolder.
dir.create("results", showWarnings = FALSE)
dir.create("results/checks", showWarnings = FALSE)   # QA / diagnostic files

base_period <- c("2014", "2015", "2016")

# Input files: this folder first, otherwise the parent (project) folder
inp <- function(f) if (file.exists(f) || !file.exists(file.path("..", f))) f else file.path("..", f)

RUN_STATS <- list()

# STEP 1 — Load data and reshape to wide format

cat("\n=== STEP 1: LOAD AND RESHAPE ===\n")

trade <- readRDS(inp("TradeData.rds"))
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

agg_table <- setDT(read.xlsx(inp("ti_aggregation_table.xlsx")))
names(agg_table) <- c("var_type", "group_code", "member_code",
                      "factor", "group_name", "member_name")

# --- Area dissemination / aggregation control
# disseminationDataset.xlsx: Dissemination decides which areas appear in
# outputs; Aggregation decides which areas feed regional aggregates. Areas
# absent from the file are excluded from both. Codes are normalised (leading
# zeros stripped) to match the trade data and the aggregation table.
norm_area <- function(x) sub("^0+(?=\\d)", "", gsub("'", "", as.character(x)), perl = TRUE)
diss_areas <- character(0); aggr_areas <- character(0)
if (file.exists(inp("disseminationDataset.xlsx"))) {
  dset <- setDT(read.xlsx(inp("disseminationDataset.xlsx")))
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
if (file.exists(inp("DisseminationList.csv"))) {
  diss <- fread(inp("DisseminationList.csv"), colClasses = "character")
  disseminated_codes <- unique(norm_cpc(diss[tolower(dissemination) == "true", code]))
  cat(sprintf("Dissemination list: %d codes with dissemination==TRUE\n",
              length(disseminated_codes)))
} else {
  cat("NOTE: DisseminationList.csv not found — disseminated breakdown skipped.\n")
}

# --- Item groups excluded from the calculations (decided 2026-10-20)
# These groups are in DisseminationList.csv but are not calculated: no indices,
# in any output. Their items are still calculated and
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
if (file.exists(inp("CountryCode.csv"))) {
  cc <- fread(inp("CountryCode.csv"), colClasses = "character")
  area_names <- unique(cc[, .(Area_Code = norm_area(`M49 Code`), Area_Name = Area)]
                       [!is.na(Area_Code) & Area_Code != ""], by = "Area_Code")
  rm(cc)
}
area_name_of <- function(code) area_names$Area_Name[match(norm_area(code), area_names$Area_Code)]

# Flow order used when sorting stacked Import/Export tables
flow_ord <- function(f) match(f, c("Import", "Export"))

cat("\n--- Step 3 complete (reference tables loaded; no rows removed). ---\n")

# STEP 4 — Aggregations and indices
# Item records go straight into the aggregates (no real values, no base unit values).

cat("\n=== STEP 4: AGGREGATIONS AND INDICES ===\n")

trade_calc <- trade_wide

# --- 4A: Item group mapping

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

# --- 4B: Area group mapping for aggregation

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

# --- 4C: Merge trade data with item groups

trade_for_agg <- trade_calc[, .(
  geographicAreaM49, measuredItemCPC, timePointYears,
  imp_v, imp_q,
  exp_v, exp_q
)]

trade_item_merge <- merge(
  trade_for_agg,
  item_map,
  by    = "measuredItemCPC",
  allow.cartesian = TRUE
)

cat(sprintf("\nRows after merging with item groups: %s\n",
            formatC(nrow(trade_item_merge), format = "d", big.mark = ",")))

# --- 4D: Aggregate to country × item_group × year

country_item_grp <- trade_item_merge[, .(
  agg_imp_v      = sum(imp_v,                    na.rm = TRUE),
  agg_imp_q      = sum(imp_q    * item_factor,   na.rm = TRUE),
  agg_exp_v      = sum(exp_v,                    na.rm = TRUE),
  agg_exp_q      = sum(exp_q    * item_factor,   na.rm = TRUE),
  n_v_imp   = sum(!is.na(imp_v)),
  n_q_imp   = sum(!is.na(imp_q)),
  n_v_exp   = sum(!is.na(exp_v)),
  n_q_exp   = sum(!is.na(exp_q))
), by = .(geographicAreaM49, item_group_code, item_group_name, timePointYears)]

country_item_grp[n_v_imp  == 0, agg_imp_v      := NA_real_]
country_item_grp[n_q_imp  == 0, agg_imp_q      := NA_real_]
country_item_grp[n_v_exp  == 0, agg_exp_v      := NA_real_]
country_item_grp[n_q_exp  == 0, agg_exp_q      := NA_real_]
country_item_grp[, c("n_v_imp","n_q_imp","n_v_exp","n_q_exp") := NULL]

cat(sprintf("Country × item_group × year rows: %s\n",
            formatC(nrow(country_item_grp), format = "d", big.mark = ",")))

# --- 4E: Indices for country × item_group (value and quantity)
# Base period means computed from the aggregated series directly. No unit value
# index or real value index is produced.

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

# --- 4F: Area-level aggregation (area_group × item_group × year)

area_item_merge <- merge(
  country_item_grp[, .(geographicAreaM49, item_group_code, item_group_name, timePointYears,
                        agg_imp_v, agg_imp_q,
                        agg_exp_v, agg_exp_q)],
  area_agg_map,
  by              = "geographicAreaM49",
  allow.cartesian = TRUE
)

area_item_grp <- area_item_merge[, .(
  agg_imp_v      = sum(agg_imp_v,      na.rm = TRUE),
  agg_imp_q      = sum(agg_imp_q,      na.rm = TRUE),
  agg_exp_v      = sum(agg_exp_v,      na.rm = TRUE),
  agg_exp_q      = sum(agg_exp_q,      na.rm = TRUE),
  n_v_imp   = sum(!is.na(agg_imp_v)),
  n_q_imp   = sum(!is.na(agg_imp_q)),
  n_v_exp   = sum(!is.na(agg_exp_v)),
  n_q_exp   = sum(!is.na(agg_exp_q))
), by = .(area_group_code, area_group_name, item_group_code, item_group_name, timePointYears)]

area_item_grp[n_v_imp  == 0, agg_imp_v      := NA_real_]
area_item_grp[n_q_imp  == 0, agg_imp_q      := NA_real_]
area_item_grp[n_v_exp  == 0, agg_exp_v      := NA_real_]
area_item_grp[n_q_exp  == 0, agg_exp_q      := NA_real_]
area_item_grp[, c("n_v_imp","n_q_imp","n_v_exp","n_q_exp") := NULL]

cat(sprintf("\nArea × item_group × year rows: %s\n",
            formatC(nrow(area_item_grp), format = "d", big.mark = ",")))

# --- 4G: Indices for area × item groups (value and quantity)

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

# --- 4H: Write the aggregate outputs

cig_out <- copy(country_item_grp)
setnames(cig_out,
  old = c("geographicAreaM49","item_group_code","item_group_name","timePointYears",
          "agg_imp_v","agg_imp_q",
          "agg_exp_v","agg_exp_q",
          "idx_v_imp","idx_q_imp",
          "idx_v_exp","idx_q_exp"),
  new = c("Area_M49","Item_Group_Code","Item_Group_Name","Year",
          "Import_Value_1000USD","Import_Quantity",
          "Export_Value_1000USD","Export_Quantity",
          "Import_Value_Index","Import_Qty_Index",
          "Export_Value_Index","Export_Qty_Index"))
cig_out <- cig_out[order(Area_M49, Item_Group_Code, Year)]

aig_out <- copy(area_item_grp)
setnames(aig_out,
  old = c("area_group_code","area_group_name","item_group_code","item_group_name","timePointYears",
          "agg_imp_v","agg_imp_q",
          "agg_exp_v","agg_exp_q",
          "idx_v_imp","idx_q_imp",
          "idx_v_exp","idx_q_exp"),
  new = c("Area_Group_Code","Area_Group_Name","Item_Group_Code","Item_Group_Name","Year",
          "Import_Value_1000USD","Import_Quantity",
          "Export_Value_1000USD","Export_Quantity",
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
idx_cols <- grep("Index", names(agg_all), value = TRUE)

idx_agg <- agg_all[, c(agg_id, idx_cols), with = FALSE]
idx_agg <- idx_agg[rowSums(!is.na(idx_agg[, ..idx_cols])) > 0]

fwrite(idx_agg, "results/indices_aggregates.csv")
cat(sprintf("\nSaved: indices_aggregates.csv     (%s rows)\n",
            formatC(nrow(idx_agg), format = "d", big.mark = ",")))
rm(agg_all, idx_agg)

cat("\n--- Step 4 complete ---\n")

# STEP 5 — Outlier detection on aggregate indices (value and quantity)

cat("\n=== STEP 5: OUTLIER DETECTION ON AGGREGATE INDICES ===\n")

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

# STEP 5B — Underlying data for top 50 flagged aggregates

cat("\n=== STEP 5B: DRILL-DOWN FOR TOP 50 FLAGGED AGGREGATES ===\n")

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
          Trade_Value_1000USD = get(v_col)
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
          Trade_Value_1000USD = get(v_col)
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

cat("\n--- Step 5 complete ---\n")

cat("\nAll output files written (results/):\n")
cat("  indices_aggregates.csv                   — aggregate value and quantity indices\n")
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

cat("\n=================================================================\n")
cat("                       END OF RUN SUMMARY                        \n")
cat("=================================================================\n")


# STEP 6 — Diagnostics dashboard (aggregate indices, dissemination groups)
# Rebuilds results/diagnostics_dashboard.html and the results/checks/diagnostics_*.csv
# files from this run's objects.
# Runs in its own environment so it does not overwrite any objects above.
# A failure here is reported but does not affect the outputs already written.

cat("\n=== STEP 6: DIAGNOSTICS DASHBOARD ===\n")
if (file.exists("diagnostics/build_dashboard.R")) {
  invisible(gc())
  tryCatch(
    source("diagnostics/build_dashboard.R", local = new.env()),
    error = function(e) cat("Dashboard not built:", conditionMessage(e), "\n")
  )
} else {
  cat("diagnostics/build_dashboard.R not found — dashboard skipped.\n")
}

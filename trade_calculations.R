# Trade Real Values and Indices
# Base period: 2014-2016 = 100

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

cat("\nNo records removed — every observation continues to Step 3.\n")

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

# --- Item output list
# ItemList.xlsx decides which items appear in the item-level outputs (real
# values, indices, unit values, item outlier check, FAOSTAT comparison).
# Every item is still calculated and still feeds the item-group and regional
# aggregates; only the item-level output files are restricted to this list.
item_list <- setDT(read.xlsx("ItemList.xlsx"))
setnames(item_list, 1:2, c("Item_CPC", "Item_Name"))
item_list <- item_list[, .(Item_CPC  = trimws(gsub("'", "", as.character(Item_CPC))),
                           Item_Name = trimws(as.character(Item_Name)))]
item_list <- unique(item_list[!is.na(Item_CPC) & Item_CPC != ""], by = "Item_CPC")
list_codes <- item_list$Item_CPC
unmatched_list <- setdiff(list_codes, unique(trade_wide$measuredItemCPC))
cat(sprintf("Item list: %d codes in ItemList.xlsx, %d found in the trade data\n",
            length(list_codes), length(list_codes) - length(unmatched_list)))
if (length(unmatched_list) > 0)
  cat("  WARNING - not in trade data:", paste(unmatched_list, collapse = ", "), "\n")
RUN_STATS$items_listed  <- length(list_codes)
RUN_STATS$items_matched <- length(list_codes) - length(unmatched_list)

# Keep only listed items and attach Item_Name right after the item code column.
keep_items <- function(dt, col = "Item_CPC") {
  if (!nrow(dt)) return(dt)
  out <- dt[get(col) %in% list_codes]
  out[, Item_Name := item_list$Item_Name[match(get(col), item_list$Item_CPC)]]
  setcolorder(out, append(setdiff(names(out), "Item_Name"), "Item_Name",
                          after = match(col, names(out))))
  out
}

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
  own <- dcast(base_sub, geographicAreaM49 + measuredItemCPC ~ timePointYears,
               value.var = "uv")
  for (y in c("2014", "2015", "2016")) if (!y %in% names(own)) own[, (y) := NA_real_]
  setnames(own, c("2014", "2015", "2016"), c("o14", "o15", "o16"))
  own <- own[, .(geographicAreaM49, measuredItemCPC, o14, o15, o16)]

  bg <- merge(base_sub, area_map[, .(member_code, group_code, group_name)],
              by.x = "geographicAreaM49", by.y = "member_code", allow.cartesian = TRUE)
  grp <- bg[, .(uv = sum(v) / sum(q)),
            by = .(group_code, group_name, measuredItemCPC, timePointYears)]
  grp_w <- dcast(grp, group_code + group_name + measuredItemCPC ~ timePointYears,
                 value.var = "uv")
  for (y in c("2014", "2015", "2016")) if (!y %in% names(grp_w)) grp_w[, (y) := NA_real_]
  setnames(grp_w, c("2014", "2015", "2016"), c("g14", "g15", "g16"))
  grp_w[, gb := rowMeans(.SD, na.rm = TRUE), .SDcols = c("g14", "g15", "g16")]
  grp_w[is.nan(gb), gb := NA_real_]

  gl <- base_sub[, .(uv = sum(v) / sum(q)), by = .(measuredItemCPC, timePointYears)]
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

  res[, h14 := !is.na(o14)]
  res[, h15 := !is.na(o15)]
  res[, h16 := !is.na(o16)]
  res[, n_own := as.integer(h14) + as.integer(h15) + as.integer(h16)]

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
        n_own > 0 & !h15, "direct",
        default = NA_character_)]
  res[, s14 := fcase(
        h14, "own",
        n_own > 0 & !h14 & !is.na(u15) & !is.na(ratio_14_15), "imputed",
        n_own > 0 & !h14, "direct",
        default = NA_character_)]
  res[, s16 := fcase(
        h16, "own",
        n_own > 0 & !h16 & !is.na(u15) & !is.na(ratio_16_15), "imputed",
        n_own > 0 & !h16, "direct",
        default = NA_character_)]

  res[, n_imputed := (s14 == "imputed" & !is.na(s14)) +
                     (s15 == "imputed" & !is.na(s15)) +
                     (s16 == "imputed" & !is.na(s16))]
  res[, n_direct  := (s14 == "direct"  & !is.na(s14)) +
                     (s15 == "direct"  & !is.na(s15)) +
                     (s16 == "direct"  & !is.na(s16))]

  res[, base_uv := fifelse(n_own == 0, group_base_co, (u14 + u15 + u16) / 3)]
  res[, uv_source := fcase(
        n_own == 3, "country",
        n_own == 0, group_base_src,
        default    = "imputed")]
  res[is.na(base_uv), uv_source := NA_character_]

  list(
    base = res[, .(geographicAreaM49, measuredItemCPC, base_uv, uv_source,
                   n_own, n_imputed, n_direct)],
    base_sub = base_sub[, .(geographicAreaM49, measuredItemCPC, timePointYears, q, v)]
  )
}

imp_built <- build_base_uv("imp_q", "imp_v")
exp_built <- build_base_uv("exp_q", "exp_v")

imp_uv_base <- copy(imp_built$base)
setnames(imp_uv_base,
         c("base_uv", "uv_source", "n_own", "n_imputed", "n_direct"),
         c("base_uv_imp", "uv_source_imp", "base_imp_yrs_valid", "n_imp_imputed", "n_imp_direct"))
exp_uv_base <- copy(exp_built$base)
setnames(exp_uv_base,
         c("base_uv", "uv_source", "n_own", "n_imputed", "n_direct"),
         c("base_uv_exp", "uv_source_exp", "base_exp_yrs_valid", "n_exp_imputed", "n_exp_direct"))

imp_base <- copy(imp_built$base_sub); setnames(imp_base, c("q", "v"), c("imp_q", "imp_v"))
exp_base <- copy(exp_built$base_sub); setnames(exp_base, c("q", "v"), c("exp_q", "exp_v"))

# --- 4C: Reporting
cat("\n-- Import base-UV source breakdown --\n")
print(imp_uv_base[, .N, by = uv_source_imp][order(-N)])
cat("\n-- Export base-UV source breakdown --\n")
print(exp_uv_base[, .N, by = uv_source_exp][order(-N)])

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

# STEP 5 — Real values and individual item indices
# Base period means for value and quantity indices always come

cat("\n=== STEP 5: REAL VALUES AND ITEM-LEVEL INDICES ===\n")

# --- 5A: Base period means for value and quantity indices

imp_base_means <- imp_base[, .(
  base_mean_v_imp = mean(imp_v, na.rm = TRUE),
  base_mean_q_imp = mean(imp_q, na.rm = TRUE),
  base_n_yrs_imp  = .N
), by = .(geographicAreaM49, measuredItemCPC)]

exp_base_means <- exp_base[, .(
  base_mean_v_exp = mean(exp_v, na.rm = TRUE),
  base_mean_q_exp = mean(exp_q, na.rm = TRUE),
  base_n_yrs_exp  = .N
), by = .(geographicAreaM49, measuredItemCPC)]

# --- 5B: Merge everything onto trade_wide

trade_calc <- copy(trade_wide)

trade_calc <- merge(trade_calc,
  imp_uv_base[, .(geographicAreaM49, measuredItemCPC, base_uv_imp, uv_source_imp)],
  by = c("geographicAreaM49", "measuredItemCPC"), all.x = TRUE)

trade_calc <- merge(trade_calc,
  exp_uv_base[, .(geographicAreaM49, measuredItemCPC, base_uv_exp, uv_source_exp)],
  by = c("geographicAreaM49", "measuredItemCPC"), all.x = TRUE)

trade_calc <- merge(trade_calc, imp_base_means,
                    by = c("geographicAreaM49", "measuredItemCPC"), all.x = TRUE)
trade_calc <- merge(trade_calc, exp_base_means,
                    by = c("geographicAreaM49", "measuredItemCPC"), all.x = TRUE)

# --- 5C: Real values

trade_calc[!is.na(imp_q) & !is.na(base_uv_imp),
           real_v_imp := imp_q * base_uv_imp]

trade_calc[!is.na(exp_q) & !is.na(base_uv_exp),
           real_v_exp := exp_q * base_uv_exp]

# --- 5D: Indices

trade_calc[!is.na(imp_v) & !is.na(base_mean_v_imp) & base_mean_v_imp > 0,
           idx_v_imp := (imp_v / base_mean_v_imp) * 100]

trade_calc[!is.na(exp_v) & !is.na(base_mean_v_exp) & base_mean_v_exp > 0,
           idx_v_exp := (exp_v / base_mean_v_exp) * 100]

trade_calc[!is.na(imp_q) & !is.na(base_mean_q_imp) & base_mean_q_imp > 0,
           idx_q_imp := (imp_q / base_mean_q_imp) * 100]

trade_calc[!is.na(exp_q) & !is.na(base_mean_q_exp) & base_mean_q_exp > 0,
           idx_q_exp := (exp_q / base_mean_q_exp) * 100]

trade_calc[!is.na(imp_q) & imp_q > 0 &
           !is.na(imp_v) &
           !is.na(base_uv_imp) & base_uv_imp > 0,
           idx_uv_imp := ((imp_v / imp_q) / base_uv_imp) * 100]

trade_calc[!is.na(exp_q) & exp_q > 0 &
           !is.na(exp_v) &
           !is.na(base_uv_exp) & base_uv_exp > 0,
           idx_uv_exp := ((exp_v / exp_q) / base_uv_exp) * 100]

# --- 5E: Reporting

cat("Real value coverage (non-NA rows):\n")
cat("  Import real value:", formatC(trade_calc[!is.na(real_v_imp), .N], format="d", big.mark=","), "\n")
cat("  Export real value:", formatC(trade_calc[!is.na(real_v_exp), .N], format="d", big.mark=","), "\n")

cat("\nIndex coverage (non-NA rows):\n")
cat("  Import value index    :", formatC(trade_calc[!is.na(idx_v_imp), .N], format="d", big.mark=","), "\n")
cat("  Import quantity index :", formatC(trade_calc[!is.na(idx_q_imp), .N], format="d", big.mark=","), "\n")
cat("  Import UV index       :", formatC(trade_calc[!is.na(idx_uv_imp), .N], format="d", big.mark=","), "\n")
cat("  Export value index    :", formatC(trade_calc[!is.na(idx_v_exp), .N], format="d", big.mark=","), "\n")
cat("  Export quantity index :", formatC(trade_calc[!is.na(idx_q_exp), .N], format="d", big.mark=","), "\n")
cat("  Export UV index       :", formatC(trade_calc[!is.na(idx_uv_exp), .N], format="d", big.mark=","), "\n")

cat("\nBase period (2014-2016) mean of indices — should be near 100:\n")
cat("  Import value index    :", round(trade_calc[timePointYears %in% base_period, mean(idx_v_imp,   na.rm=TRUE)], 2), "\n")
cat("  Import quantity index :", round(trade_calc[timePointYears %in% base_period, mean(idx_q_imp,   na.rm=TRUE)], 2), "\n")
cat("  Import UV index       :", round(trade_calc[timePointYears %in% base_period, mean(idx_uv_imp,  na.rm=TRUE)], 2), "\n")
cat("  Export value index    :", round(trade_calc[timePointYears %in% base_period, mean(idx_v_exp,   na.rm=TRUE)], 2), "\n")
cat("  Export quantity index :", round(trade_calc[timePointYears %in% base_period, mean(idx_q_exp,   na.rm=TRUE)], 2), "\n")
cat("  Export UV index       :", round(trade_calc[timePointYears %in% base_period, mean(idx_uv_exp,  na.rm=TRUE)], 2), "\n")

cat("\n--- Step 5 complete. Object: trade_calc ---\n")
cat("Proceed to Step 6 (aggregations) when ready.\n")

# STEP 5 — OUTPUT: Write real values and indices to Excel

cat("\n=== STEP 5 OUTPUT: WRITING FILES ===\n")

# --- Load FAOSTAT published indices for comparison
fao_file <- "Faostat_TradeIndices.csv"
fao_idx  <- NULL
if (file.exists(fao_file)) {
  cat("\nLoading FAOSTAT reference indices for comparison...\n")
  fao_raw <- fread(fao_file,
                   select = c("Area Code (M49)", "Item Code (CPC)",
                              "Element Code", "Year", "Value"),
                   colClasses = list(character = c("Area Code (M49)", "Item Code (CPC)",
                                                   "Element Code", "Year")))
  setnames(fao_raw,
           c("Area Code (M49)", "Item Code (CPC)", "Element Code", "Year", "Value"),
           c("m49_raw", "cpc_raw", "elem", "Year", "fao_val"))
  fao_raw[, Area_M49 := as.character(as.integer(gsub("'", "", m49_raw)))]
  fao_raw[, Item_CPC := gsub("'", "", cpc_raw)]
  fao_raw[, Year     := as.character(as.integer(Year))]
  fao_raw[, fao_val  := as.numeric(fao_val)]

  elem_map <- c("462" = "FAO_Import_Value_Index",     "465" = "FAO_Import_Quantity_Index",
                "464" = "FAO_Import_UnitValue_Index", "492" = "FAO_Export_Value_Index",
                "495" = "FAO_Export_Quantity_Index",  "494" = "FAO_Export_UnitValue_Index")
  fao_raw <- fao_raw[elem %in% names(elem_map)]
  fao_raw[, col := elem_map[elem]]

  fao_idx <- dcast(fao_raw, Area_M49 + Item_CPC + Year ~ col,
                   value.var = "fao_val", fun.aggregate = function(x) x[1])

  n_areas <- uniqueN(fao_raw$Area_M49)
  cat(sprintf("  FAOSTAT rows used: %s | distinct areas: %d | years: %s-%s\n",
              formatC(nrow(fao_raw), format = "d", big.mark = ","),
              n_areas, min(fao_raw$Year), max(fao_raw$Year)))
  if (nrow(fread(fao_file, select = 1L)) >= 1048574) {
    cat("  *** WARNING: FAOSTAT file is at the ~1,048,575-row Excel limit and is almost\n")
    cat(sprintf("      certainly TRUNCATED — this extract covers only %d areas. Re-export the\n", n_areas))
    cat("      full dataset as CSV directly from FAOSTAT to compare all countries. ***\n")
  }
} else {
  cat("\nNOTE: Faostat_TradeIndices.csv not found — FAO comparison columns will be omitted.\n")
}

# --- Real values files

rv_imp <- trade_calc[
  (!is.na(imp_q) & imp_q > 0) | (!is.na(imp_v) & imp_v > 0) | !is.na(real_v_imp),
  .(geographicAreaM49, measuredItemCPC, timePointYears,
    imp_q, imp_v, real_v_imp, uv_source_imp)]
setnames(rv_imp, c("Area_M49", "Item_CPC", "Year",
                   "Import_Quantity", "Import_Value_1000USD",
                   "Import_RealValue_1000USD", "UV_Source"))

rv_exp <- trade_calc[
  (!is.na(exp_q) & exp_q > 0) | (!is.na(exp_v) & exp_v > 0) | !is.na(real_v_exp),
  .(geographicAreaM49, measuredItemCPC, timePointYears,
    exp_q, exp_v, real_v_exp, uv_source_exp)]
setnames(rv_exp, c("Area_M49", "Item_CPC", "Year",
                   "Export_Quantity", "Export_Value_1000USD",
                   "Export_RealValue_1000USD", "UV_Source"))

# Item-level output: disseminated areas x ItemList items only
rv_imp <- keep_items(keep_diss(rv_imp, "Area_M49"))
rv_exp <- keep_items(keep_diss(rv_exp, "Area_M49"))
# One file: one row per area x item x year, import and export columns side by side
rv_items <- merge(
  rv_imp[, .(Area_M49, Item_CPC, Item_Name, Year,
             `Import Quantity (t)`           = Import_Quantity,
             `Import Value (1000 USD)`       = Import_Value_1000USD,
             `Import Real Value (1000 USD)`  = Import_RealValue_1000USD)],
  rv_exp[, .(Area_M49, Item_CPC, Item_Name, Year,
             `Export Quantity (t)`           = Export_Quantity,
             `Export Value (1000 USD)`       = Export_Value_1000USD,
             `Export Real Value (1000 USD)`  = Export_RealValue_1000USD)],
  by = c("Area_M49", "Item_CPC", "Item_Name", "Year"), all = TRUE)
rv_items[, Area_Name := area_name_of(Area_M49)]
rv_items <- rv_items[order(as.integer(Area_M49), Item_CPC, Year)]
setnames(rv_items, c("Area_M49", "Area_Name", "Item_CPC", "Item_Name"),
                   c("Area Code (M49)", "Area", "Item Code (CPC)", "Item"))
setcolorder(rv_items, c("Area Code (M49)", "Area", "Item Code (CPC)", "Item", "Year"))
fwrite(rv_items, "results/real_values_items.csv")
cat(sprintf("Saved: real_values_items.csv  (%s rows)\n",
            formatC(nrow(rv_items), format = "d", big.mark = ",")))
rm(rv_items)

# --- Indices files

idx_imp <- trade_calc[
  !is.na(idx_v_imp) | !is.na(idx_q_imp) | !is.na(idx_uv_imp),
  .(geographicAreaM49, measuredItemCPC, timePointYears,
    idx_v_imp, idx_q_imp, idx_uv_imp, uv_source_imp)]
setnames(idx_imp, c("Area_M49", "Item_CPC", "Year",
                    "Value_Index", "Quantity_Index", "UnitValue_Index", "UV_Source"))

idx_exp <- trade_calc[
  !is.na(idx_v_exp) | !is.na(idx_q_exp) | !is.na(idx_uv_exp),
  .(geographicAreaM49, measuredItemCPC, timePointYears,
    idx_v_exp, idx_q_exp, idx_uv_exp, uv_source_exp)]
setnames(idx_exp, c("Area_M49", "Item_CPC", "Year",
                    "Value_Index", "Quantity_Index", "UnitValue_Index", "UV_Source"))

# --- Attach FAOSTAT comparison columns
if (!is.null(fao_idx)) {

  idx_imp[, Area_M49 := as.character(as.integer(Area_M49))]
  idx_exp[, Area_M49 := as.character(as.integer(Area_M49))]
  idx_imp[, Year := as.character(Year)]
  idx_exp[, Year := as.character(Year)]

  fao_imp <- fao_idx[, .(Area_M49, Item_CPC, Year,
                         FAO_Value_Index     = FAO_Import_Value_Index,
                         FAO_Quantity_Index  = FAO_Import_Quantity_Index,
                         FAO_UnitValue_Index = FAO_Import_UnitValue_Index)]
  fao_exp <- fao_idx[, .(Area_M49, Item_CPC, Year,
                         FAO_Value_Index     = FAO_Export_Value_Index,
                         FAO_Quantity_Index  = FAO_Export_Quantity_Index,
                         FAO_UnitValue_Index = FAO_Export_UnitValue_Index)]

  idx_imp <- merge(idx_imp, fao_imp, by = c("Area_M49", "Item_CPC", "Year"), all.x = TRUE)
  idx_exp <- merge(idx_exp, fao_exp, by = c("Area_M49", "Item_CPC", "Year"), all.x = TRUE)

  ord <- c("Area_M49", "Item_CPC", "Year",
           "Value_Index", "FAO_Value_Index",
           "Quantity_Index", "FAO_Quantity_Index",
           "UnitValue_Index", "FAO_UnitValue_Index", "UV_Source")
  setcolorder(idx_imp, ord)
  setcolorder(idx_exp, ord)

  n_imp_fao <- idx_imp[!is.na(FAO_Value_Index) | !is.na(FAO_Quantity_Index) | !is.na(FAO_UnitValue_Index), .N]
  n_exp_fao <- idx_exp[!is.na(FAO_Value_Index) | !is.na(FAO_Quantity_Index) | !is.na(FAO_UnitValue_Index), .N]
  cat(sprintf("  FAO match — import: %s / %s rows | export: %s / %s rows\n",
              formatC(n_imp_fao, format = "d", big.mark = ","),
              formatC(nrow(idx_imp), format = "d", big.mark = ","),
              formatC(n_exp_fao, format = "d", big.mark = ","),
              formatC(nrow(idx_exp), format = "d", big.mark = ",")))
}

# Item-level output: ItemList items only. The item filter is applied in memory
# so the Step 7 FAOSTAT comparison also covers only the listed items; the area
# (dissemination) filter is applied on write only, as before.
idx_imp <- keep_items(idx_imp)
idx_exp <- keep_items(idx_exp)
# One file: one row per area x item x year, import and export indices side by side
idx_label <- c(Value_Index         = "Value Index",
               Quantity_Index      = "Quantity Index",
               UnitValue_Index     = "Unit Value Index",
               FAO_Value_Index     = "Value Index (FAOSTAT)",
               FAO_Quantity_Index  = "Quantity Index (FAOSTAT)",
               FAO_UnitValue_Index = "Unit Value Index (FAOSTAT)")
flow_idx <- function(dt, flow) {
  x <- keep_diss(dt, "Area_M49")
  cols <- intersect(names(idx_label), names(x))
  x <- x[, c("Area_M49", "Item_CPC", "Item_Name", "Year", cols), with = FALSE]
  setnames(x, cols, paste(flow, idx_label[cols]))
  x
}
idx_items <- merge(flow_idx(idx_imp, "Import"), flow_idx(idx_exp, "Export"),
                   by = c("Area_M49", "Item_CPC", "Item_Name", "Year"), all = TRUE)
idx_items[, Area_Name := area_name_of(Area_M49)]
idx_items <- idx_items[order(as.integer(Area_M49), Item_CPC, Year)]
setnames(idx_items, c("Area_M49", "Area_Name", "Item_CPC", "Item_Name"),
                    c("Area Code (M49)", "Area", "Item Code (CPC)", "Item"))
setcolorder(idx_items, c("Area Code (M49)", "Area", "Item Code (CPC)", "Item", "Year"))
fwrite(idx_items, "results/indices_items.csv")
cat(sprintf("Saved: indices_items.csv  (%s rows)\n",
            formatC(nrow(idx_items), format = "d", big.mark = ",")))
rm(idx_items)

# --- Base unit values (QA file): the 2014-2016 base UV behind every real value
# and index, and how it was derived. ItemList items, disseminated areas.
base_uv_chk <- rbindlist(list(
  Import = imp_uv_base[, .(Area_M49 = geographicAreaM49, Item_CPC = measuredItemCPC,
                           Base_UV = base_uv_imp, UV_Source = uv_source_imp,
                           Base_Yrs_Own = base_imp_yrs_valid,
                           N_Imputed = n_imp_imputed, N_Direct = n_imp_direct)],
  Export = exp_uv_base[, .(Area_M49 = geographicAreaM49, Item_CPC = measuredItemCPC,
                           Base_UV = base_uv_exp, UV_Source = uv_source_exp,
                           Base_Yrs_Own = base_exp_yrs_valid,
                           N_Imputed = n_exp_imputed, N_Direct = n_exp_direct)]
), idcol = "Flow")
base_uv_chk <- keep_items(keep_diss(base_uv_chk, "Area_M49"))
base_uv_chk[, Area_Name := area_name_of(Area_M49)]
setcolorder(base_uv_chk, c("Area_M49", "Area_Name", "Item_CPC", "Item_Name", "Flow"))
base_uv_chk <- base_uv_chk[order(as.integer(Area_M49), Item_CPC, flow_ord(Flow))]
fwrite(base_uv_chk, "results/checks/base_unit_values.csv")
cat(sprintf("Saved: checks/base_unit_values.csv  (%s rows)\n",
            formatC(nrow(base_uv_chk), format = "d", big.mark = ",")))
rm(base_uv_chk)

cat("\n--- Step 5 outputs complete. Proceed to Step 6 (aggregations) when ready. ---\n")

# STEP 5.5 — Outlier detection on indices

cat("\n=== STEP 5.5: OUTLIER DETECTION ON INDICES ===\n")

# --- Thresholds — adjust these to tune the flag count
abs_high <- 5000

cat(sprintf("Threshold: abs_high=%g (index > %g flagged)\n", abs_high, abs_high))

# --- Year-over-year ratios
setorder(trade_calc, geographicAreaM49, measuredItemCPC, timePointYears)

trade_calc[, yoy_v_imp  := idx_v_imp  / shift(idx_v_imp),  by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, yoy_q_imp  := idx_q_imp  / shift(idx_q_imp),  by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, yoy_uv_imp := idx_uv_imp / shift(idx_uv_imp), by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, yoy_v_exp  := idx_v_exp  / shift(idx_v_exp),  by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, yoy_q_exp  := idx_q_exp  / shift(idx_q_exp),  by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, yoy_uv_exp := idx_uv_exp / shift(idx_uv_exp), by = .(geographicAreaM49, measuredItemCPC)]

# --- Previous-year values (used for Prev_Value in outlier output)
trade_calc[, prev_v_imp  := shift(idx_v_imp),  by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, prev_q_imp  := shift(idx_q_imp),  by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, prev_uv_imp := shift(idx_uv_imp), by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, prev_v_exp  := shift(idx_v_exp),  by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, prev_q_exp  := shift(idx_q_exp),  by = .(geographicAreaM49, measuredItemCPC)]
trade_calc[, prev_uv_exp := shift(idx_uv_exp), by = .(geographicAreaM49, measuredItemCPC)]

# --- Flag function
flag_extreme <- function(idx) {
  !is.na(idx) & idx > abs_high
}

trade_calc[, flag_v_imp  := flag_extreme(idx_v_imp)]
trade_calc[, flag_q_imp  := flag_extreme(idx_q_imp)]
trade_calc[, flag_uv_imp := flag_extreme(idx_uv_imp)]
trade_calc[, flag_v_exp  := flag_extreme(idx_v_exp)]
trade_calc[, flag_q_exp  := flag_extreme(idx_q_exp)]
trade_calc[, flag_uv_exp := flag_extreme(idx_uv_exp)]

cat("\nFlagged observations:\n")
cat("  Import value index    :", trade_calc[flag_v_imp  == TRUE, .N], "\n")
cat("  Import quantity index :", trade_calc[flag_q_imp  == TRUE, .N], "\n")
cat("  Import UV index       :", trade_calc[flag_uv_imp == TRUE, .N], "\n")
cat("  Export value index    :", trade_calc[flag_v_exp  == TRUE, .N], "\n")
cat("  Export quantity index :", trade_calc[flag_q_exp  == TRUE, .N], "\n")
cat("  Export UV index       :", trade_calc[flag_uv_exp == TRUE, .N], "\n")

# --- Build combined output table

build_outlier_table <- function(dt, idx_col, prev_col, yoy_col, flag_col, flow, index_label) {
  rows <- dt[get(flag_col) == TRUE & !is.na(get(idx_col))]
  if (nrow(rows) == 0) return(data.table())
  flagged <- rows[, .(
    geographicAreaM49, measuredItemCPC, timePointYears,
    Index_Value = get(idx_col),
    Prev_Value  = round(get(prev_col), 2),
    YoY_Ratio   = round(get(yoy_col), 3)
  )]
  flagged[, Flow  := flow]
  flagged[, Index := index_label]
  flagged[, Flag_Reason := paste0("Level > ", abs_high)]
  setnames(flagged, c("geographicAreaM49","measuredItemCPC","timePointYears"),
                    c("Area_M49","Item_CPC","Year"))
  setcolorder(flagged, c("Flow","Index","Area_M49","Item_CPC","Year",
                         "Index_Value","Prev_Value","YoY_Ratio","Flag_Reason"))
  flagged
}

out_all <- rbind(
  build_outlier_table(trade_calc,"idx_v_imp", "prev_v_imp", "yoy_v_imp", "flag_v_imp", "Import","Value"),
  build_outlier_table(trade_calc,"idx_q_imp", "prev_q_imp", "yoy_q_imp", "flag_q_imp", "Import","Quantity"),
  build_outlier_table(trade_calc,"idx_uv_imp","prev_uv_imp","yoy_uv_imp","flag_uv_imp","Import","UnitValue"),
  build_outlier_table(trade_calc,"idx_v_exp", "prev_v_exp", "yoy_v_exp", "flag_v_exp", "Export","Value"),
  build_outlier_table(trade_calc,"idx_q_exp", "prev_q_exp", "yoy_q_exp", "flag_q_exp", "Export","Quantity"),
  build_outlier_table(trade_calc,"idx_uv_exp","prev_uv_exp","yoy_uv_exp","flag_uv_exp","Export","UnitValue")
)
setorder(out_all, -Index_Value)

cat(sprintf("\nTotal flagged rows (all indices): %s\n",
            formatC(nrow(out_all), format = "d", big.mark = ",")))
cat("Breakdown by reason:\n")
print(out_all[, .N, by = Flag_Reason][order(-N)])

# Written for ItemList items only (all areas, as before - QA file)
out_items <- keep_items(out_all)
fwrite(out_items, "results/checks/outlier_check_items.csv")
cat(sprintf("Saved: checks/outlier_check_items.csv  (%s rows, ItemList items only)\n",
            formatC(nrow(out_items), format = "d", big.mark = ",")))
rm(out_items)

trade_calc[, c("flag_v_imp","flag_q_imp","flag_uv_imp",
               "flag_v_exp","flag_q_exp","flag_uv_exp",
               "yoy_v_imp","yoy_q_imp","yoy_uv_imp",
               "yoy_v_exp","yoy_q_exp","yoy_uv_exp") := NULL]

cat("\n--- Step 5.5 complete. ---\n")
cat("Proceed to Step 6 (aggregations) when ready.\n")

# STEP 6 — Aggregations

cat("\n=== STEP 6: AGGREGATIONS ===\n")

# --- 6A: Item group mapping

item_map <- agg_table[var_type == "item", .(
  item_group_code = group_code,
  item_group_name = group_name,
  measuredItemCPC = member_code,
  item_factor     = as.numeric(factor)
)]

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

area_agg_map <- agg_table[var_type == "area" & as.numeric(factor) == 1, .(
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

# --- 6E: Indices for country × item_group
# Base period means computed from the aggregated series directly.

cig_base <- country_item_grp[timePointYears %in% base_period, .(
  base_mean_imp_v  = mean(agg_imp_v,      na.rm = TRUE),
  base_mean_imp_q  = mean(agg_imp_q,      na.rm = TRUE),
  base_mean_rv_imp = mean(agg_real_v_imp, na.rm = TRUE),
  base_mean_exp_v  = mean(agg_exp_v,      na.rm = TRUE),
  base_mean_exp_q  = mean(agg_exp_q,      na.rm = TRUE),
  base_mean_rv_exp = mean(agg_real_v_exp, na.rm = TRUE)
), by = .(geographicAreaM49, item_group_code)]

for (col in c("base_mean_imp_v","base_mean_imp_q","base_mean_rv_imp",
              "base_mean_exp_v","base_mean_exp_q","base_mean_rv_exp")) {
  cig_base[is.nan(get(col)), (col) := NA_real_]
}

country_item_grp <- merge(country_item_grp, cig_base,
                           by = c("geographicAreaM49", "item_group_code"), all.x = TRUE)

for (col in c("idx_v_imp","idx_q_imp","idx_rv_imp","idx_v_exp","idx_q_exp","idx_rv_exp")) {
  country_item_grp[, (col) := NA_real_]
}

country_item_grp[!is.na(agg_imp_v)      & !is.na(base_mean_imp_v)  & base_mean_imp_v  > 0,
                  idx_v_imp  := (agg_imp_v      / base_mean_imp_v)  * 100]
country_item_grp[!is.na(agg_imp_q)      & !is.na(base_mean_imp_q)  & base_mean_imp_q  > 0,
                  idx_q_imp  := (agg_imp_q      / base_mean_imp_q)  * 100]
country_item_grp[!is.na(agg_real_v_imp) & !is.na(base_mean_rv_imp) & base_mean_rv_imp > 0,
                  idx_rv_imp := (agg_real_v_imp / base_mean_rv_imp) * 100]
country_item_grp[!is.na(agg_exp_v)      & !is.na(base_mean_exp_v)  & base_mean_exp_v  > 0,
                  idx_v_exp  := (agg_exp_v      / base_mean_exp_v)  * 100]
country_item_grp[!is.na(agg_exp_q)      & !is.na(base_mean_exp_q)  & base_mean_exp_q  > 0,
                  idx_q_exp  := (agg_exp_q      / base_mean_exp_q)  * 100]
country_item_grp[!is.na(agg_real_v_exp) & !is.na(base_mean_rv_exp) & base_mean_rv_exp > 0,
                  idx_rv_exp := (agg_real_v_exp / base_mean_rv_exp) * 100]

country_item_grp[, c("base_mean_imp_v","base_mean_imp_q","base_mean_rv_imp",
                      "base_mean_exp_v","base_mean_exp_q","base_mean_rv_exp") := NULL]

cat("\nCountry-item group index coverage (non-NA rows):\n")
cat("  Import value index    :", formatC(country_item_grp[!is.na(idx_v_imp),  .N], format="d", big.mark=","), "\n")
cat("  Import quantity index :", formatC(country_item_grp[!is.na(idx_q_imp),  .N], format="d", big.mark=","), "\n")
cat("  Import real val index :", formatC(country_item_grp[!is.na(idx_rv_imp), .N], format="d", big.mark=","), "\n")
cat("  Export value index    :", formatC(country_item_grp[!is.na(idx_v_exp),  .N], format="d", big.mark=","), "\n")
cat("  Export quantity index :", formatC(country_item_grp[!is.na(idx_q_exp),  .N], format="d", big.mark=","), "\n")
cat("  Export real val index :", formatC(country_item_grp[!is.na(idx_rv_exp), .N], format="d", big.mark=","), "\n")

# Base period sanity check for aggregate indices
cat("\nBase period (2014-2016) mean of aggregate indices — should be near 100:\n")
bp <- country_item_grp[timePointYears %in% base_period]
cat("  Import value index    :", round(bp[, mean(idx_v_imp,  na.rm=TRUE)], 2), "\n")
cat("  Import quantity index :", round(bp[, mean(idx_q_imp,  na.rm=TRUE)], 2), "\n")
cat("  Import real val index :", round(bp[, mean(idx_rv_imp, na.rm=TRUE)], 2), "\n")
cat("  Export value index    :", round(bp[, mean(idx_v_exp,  na.rm=TRUE)], 2), "\n")
cat("  Export quantity index :", round(bp[, mean(idx_q_exp,  na.rm=TRUE)], 2), "\n")
cat("  Export real val index :", round(bp[, mean(idx_rv_exp, na.rm=TRUE)], 2), "\n")

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

# --- 6G: Indices for area × item groups

aig_base <- area_item_grp[timePointYears %in% base_period, .(
  base_mean_imp_v  = mean(agg_imp_v,      na.rm = TRUE),
  base_mean_imp_q  = mean(agg_imp_q,      na.rm = TRUE),
  base_mean_rv_imp = mean(agg_real_v_imp, na.rm = TRUE),
  base_mean_exp_v  = mean(agg_exp_v,      na.rm = TRUE),
  base_mean_exp_q  = mean(agg_exp_q,      na.rm = TRUE),
  base_mean_rv_exp = mean(agg_real_v_exp, na.rm = TRUE)
), by = .(area_group_code, item_group_code)]

for (col in c("base_mean_imp_v","base_mean_imp_q","base_mean_rv_imp",
              "base_mean_exp_v","base_mean_exp_q","base_mean_rv_exp")) {
  aig_base[is.nan(get(col)), (col) := NA_real_]
}

area_item_grp <- merge(area_item_grp, aig_base,
                        by = c("area_group_code", "item_group_code"), all.x = TRUE)

for (col in c("idx_v_imp","idx_q_imp","idx_rv_imp","idx_v_exp","idx_q_exp","idx_rv_exp")) {
  area_item_grp[, (col) := NA_real_]
}

area_item_grp[!is.na(agg_imp_v)      & !is.na(base_mean_imp_v)  & base_mean_imp_v  > 0,
               idx_v_imp  := (agg_imp_v      / base_mean_imp_v)  * 100]
area_item_grp[!is.na(agg_imp_q)      & !is.na(base_mean_imp_q)  & base_mean_imp_q  > 0,
               idx_q_imp  := (agg_imp_q      / base_mean_imp_q)  * 100]
area_item_grp[!is.na(agg_real_v_imp) & !is.na(base_mean_rv_imp) & base_mean_rv_imp > 0,
               idx_rv_imp := (agg_real_v_imp / base_mean_rv_imp) * 100]
area_item_grp[!is.na(agg_exp_v)      & !is.na(base_mean_exp_v)  & base_mean_exp_v  > 0,
               idx_v_exp  := (agg_exp_v      / base_mean_exp_v)  * 100]
area_item_grp[!is.na(agg_exp_q)      & !is.na(base_mean_exp_q)  & base_mean_exp_q  > 0,
               idx_q_exp  := (agg_exp_q      / base_mean_exp_q)  * 100]
area_item_grp[!is.na(agg_real_v_exp) & !is.na(base_mean_rv_exp) & base_mean_rv_exp > 0,
               idx_rv_exp := (agg_real_v_exp / base_mean_rv_exp) * 100]

area_item_grp[, c("base_mean_imp_v","base_mean_imp_q","base_mean_rv_imp",
                   "base_mean_exp_v","base_mean_exp_q","base_mean_rv_exp") := NULL]

cat("\nArea-item group index coverage (non-NA rows):\n")
cat("  Import value index    :", formatC(area_item_grp[!is.na(idx_v_imp),  .N], format="d", big.mark=","), "\n")
cat("  Import quantity index :", formatC(area_item_grp[!is.na(idx_q_imp),  .N], format="d", big.mark=","), "\n")
cat("  Import real val index :", formatC(area_item_grp[!is.na(idx_rv_imp), .N], format="d", big.mark=","), "\n")
cat("  Export value index    :", formatC(area_item_grp[!is.na(idx_v_exp),  .N], format="d", big.mark=","), "\n")
cat("  Export quantity index :", formatC(area_item_grp[!is.na(idx_q_exp),  .N], format="d", big.mark=","), "\n")
cat("  Export real val index :", formatC(area_item_grp[!is.na(idx_rv_exp), .N], format="d", big.mark=","), "\n")

# --- 6H: Write aggregates.xlsx

cig_out <- copy(country_item_grp)
setnames(cig_out,
  old = c("geographicAreaM49","item_group_code","item_group_name","timePointYears",
          "agg_imp_v","agg_imp_q","agg_real_v_imp",
          "agg_exp_v","agg_exp_q","agg_real_v_exp",
          "idx_v_imp","idx_q_imp","idx_rv_imp",
          "idx_v_exp","idx_q_exp","idx_rv_exp"),
  new = c("Area_M49","Item_Group_Code","Item_Group_Name","Year",
          "Import_Value_1000USD","Import_Quantity","Import_RealValue_1000USD",
          "Export_Value_1000USD","Export_Quantity","Export_RealValue_1000USD",
          "Import_Value_Index","Import_Qty_Index","Import_RealVal_Index",
          "Export_Value_Index","Export_Qty_Index","Export_RealVal_Index"))
cig_out <- cig_out[order(Area_M49, Item_Group_Code, Year)]

aig_out <- copy(area_item_grp)
setnames(aig_out,
  old = c("area_group_code","area_group_name","item_group_code","item_group_name","timePointYears",
          "agg_imp_v","agg_imp_q","agg_real_v_imp",
          "agg_exp_v","agg_exp_q","agg_real_v_exp",
          "idx_v_imp","idx_q_imp","idx_rv_imp",
          "idx_v_exp","idx_q_exp","idx_rv_exp"),
  new = c("Area_Group_Code","Area_Group_Name","Item_Group_Code","Item_Group_Name","Year",
          "Import_Value_1000USD","Import_Quantity","Import_RealValue_1000USD",
          "Export_Value_1000USD","Export_Quantity","Export_RealValue_1000USD",
          "Import_Value_Index","Import_Qty_Index","Import_RealVal_Index",
          "Export_Value_Index","Export_Qty_Index","Export_RealVal_Index"))
aig_out <- aig_out[order(Area_Group_Code, Item_Group_Code, Year)]

# --- Attach FAOSTAT comparison columns to aggregates
if (exists("fao_idx") && !is.null(fao_idx)) {

  norm_m49 <- function(x) as.character(suppressWarnings(as.integer(as.character(x))))

  fao_lkp <- fao_idx[, .(
    .akey                     = Area_M49,
    Item_Group_Code           = Item_CPC,
    .ykey                     = Year,
    FAO_Import_Value_Index    = FAO_Import_Value_Index,
    FAO_Import_Quantity_Index = FAO_Import_Quantity_Index,
    FAO_Export_Value_Index    = FAO_Export_Value_Index,
    FAO_Export_Quantity_Index = FAO_Export_Quantity_Index)]

  cig_out[, `:=`(.akey = norm_m49(Area_M49), .ykey = as.character(Year))]
  cig_out <- merge(cig_out, fao_lkp,
                   by = c(".akey", "Item_Group_Code", ".ykey"),
                   all.x = TRUE, sort = FALSE)
  cig_out[, c(".akey", ".ykey") := NULL]

  aig_out[, `:=`(.akey = norm_m49(Area_Group_Code), .ykey = as.character(Year))]
  aig_out <- merge(aig_out, fao_lkp,
                   by = c(".akey", "Item_Group_Code", ".ykey"),
                   all.x = TRUE, sort = FALSE)
  aig_out[, c(".akey", ".ykey") := NULL]

  setcolorder(cig_out, c(
    "Area_M49", "Item_Group_Code", "Item_Group_Name", "Year",
    "Import_Value_1000USD", "Import_Quantity", "Import_RealValue_1000USD",
    "Export_Value_1000USD", "Export_Quantity", "Export_RealValue_1000USD",
    "Import_Value_Index", "FAO_Import_Value_Index",
    "Import_Qty_Index", "Import_RealVal_Index", "FAO_Import_Quantity_Index",
    "Export_Value_Index", "FAO_Export_Value_Index",
    "Export_Qty_Index", "Export_RealVal_Index", "FAO_Export_Quantity_Index"))
  setcolorder(aig_out, c(
    "Area_Group_Code", "Area_Group_Name", "Item_Group_Code", "Item_Group_Name", "Year",
    "Import_Value_1000USD", "Import_Quantity", "Import_RealValue_1000USD",
    "Export_Value_1000USD", "Export_Quantity", "Export_RealValue_1000USD",
    "Import_Value_Index", "FAO_Import_Value_Index",
    "Import_Qty_Index", "Import_RealVal_Index", "FAO_Import_Quantity_Index",
    "Export_Value_Index", "FAO_Export_Value_Index",
    "Export_Qty_Index", "Export_RealVal_Index", "FAO_Export_Quantity_Index"))

  cig_out <- cig_out[order(suppressWarnings(as.integer(Area_M49)), Item_Group_Code, Year)]
  aig_out <- aig_out[order(Area_Group_Code, Item_Group_Code, Year)]

  cat(sprintf("FAO aggregate match — country×group: %s rows | area×group: %s rows\n",
      formatC(cig_out[!is.na(FAO_Import_Value_Index) | !is.na(FAO_Export_Value_Index), .N], format = "d", big.mark = ","),
      formatC(aig_out[!is.na(FAO_Import_Value_Index) | !is.na(FAO_Export_Value_Index), .N], format = "d", big.mark = ",")))
}

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
    v <- col(paste0(f, "_Value_Index")); rv <- col(paste0(f, "_RealVal_Index"))
    out[, paste(f, "Quantity (t)")          := col(paste0(f, "_Quantity"))]
    out[, paste(f, "Value (1000 USD)")      := col(paste0(f, "_Value_1000USD"))]
    out[, paste(f, "Real Value (1000 USD)") := col(paste0(f, "_RealValue_1000USD"))]
    out[, paste(f, "Value Index")           := v]
    out[, paste(f, "Quantity Index")        := col(paste0(f, "_Qty_Index"))]
    out[, paste(f, "Real Value Index")      := rv]
    # Unit value index = aggregate UV / base aggregate UV x 100, where
    # aggregate UV = aggregate value / aggregate quantity and the base is the mean of the
    # yearly aggregate UVs over the 2014-16 years with both (as for the item base UV).
    # Open question: value index / real value index x 100 would hold the item mix fixed.
    av <- col(paste0(f, "_Value_1000USD")); aq <- col(paste0(f, "_Quantity"))
    out[, .auv := fifelse(!is.na(av) & !is.na(aq) & av > 0 & aq > 0, av / aq, NA_real_)]
    out[, .abuv := { b <- .auv[Year %in% base_period]; if (any(!is.na(b))) mean(b, na.rm = TRUE) else NA_real_ },
        by = .(`Area Code (M49)`, `Item Group Code`)]
    out[, paste(f, "Unit Value Index")      := fifelse(!is.na(.auv) & !is.na(.abuv) & .abuv > 0,
                                                       .auv / .abuv * 100, NA_real_)]
    out[, c(".auv", ".abuv") := NULL]
    if (!is.null(fao_idx)) {
      out[, paste(f, "Value Index (FAOSTAT)")    := col(paste0("FAO_", f, "_Value_Index"))]
      out[, paste(f, "Quantity Index (FAOSTAT)") := col(paste0("FAO_", f, "_Quantity_Index"))]
    }
  }
  out
}
agg_all <- rbind(agg_level(cig_out, "Country", "Area_M49"),
                 agg_level(aig_out, "Region",  "Area_Group_Code"))
agg_all <- agg_all[order(match(Level, c("Country", "Region")),
                         suppressWarnings(as.integer(`Area Code (M49)`)), `Item Group Code`, Year)]

agg_id   <- c("Level", "Area Code (M49)", "Area", "Item Group Code", "Item Group", "Year")
rv_cols  <- grep("Quantity \\(t\\)|Value \\(1000 USD\\)", names(agg_all), value = TRUE)
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

# STEP 6.5 — Outlier detection on aggregate indices

cat("\n=== STEP 6.5: OUTLIER DETECTION ON AGGREGATE INDICES ===\n")

abs_high_agg <- 5000

cat(sprintf("Threshold: index > %g (50x base period level)\n", abs_high_agg))

# --- Country × item group outliers

setorder(country_item_grp, geographicAreaM49, item_group_code, timePointYears)

country_item_grp[, yoy_v_imp  := idx_v_imp  / shift(idx_v_imp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, yoy_q_imp  := idx_q_imp  / shift(idx_q_imp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, yoy_rv_imp := idx_rv_imp / shift(idx_rv_imp), by = .(geographicAreaM49, item_group_code)]
country_item_grp[, yoy_v_exp  := idx_v_exp  / shift(idx_v_exp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, yoy_q_exp  := idx_q_exp  / shift(idx_q_exp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, yoy_rv_exp := idx_rv_exp / shift(idx_rv_exp), by = .(geographicAreaM49, item_group_code)]

country_item_grp[, prev_v_imp  := shift(idx_v_imp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, prev_q_imp  := shift(idx_q_imp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, prev_rv_imp := shift(idx_rv_imp), by = .(geographicAreaM49, item_group_code)]
country_item_grp[, prev_v_exp  := shift(idx_v_exp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, prev_q_exp  := shift(idx_q_exp),  by = .(geographicAreaM49, item_group_code)]
country_item_grp[, prev_rv_exp := shift(idx_rv_exp), by = .(geographicAreaM49, item_group_code)]

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
  build_agg_outlier_cig(country_item_grp, "idx_rv_imp", "prev_rv_imp", "yoy_rv_imp", "Import", "RealValue"),
  build_agg_outlier_cig(country_item_grp, "idx_v_exp",  "prev_v_exp",  "yoy_v_exp",  "Export", "Value"),
  build_agg_outlier_cig(country_item_grp, "idx_q_exp",  "prev_q_exp",  "yoy_q_exp",  "Export", "Quantity"),
  build_agg_outlier_cig(country_item_grp, "idx_rv_exp", "prev_rv_exp", "yoy_rv_exp", "Export", "RealValue")
)

country_item_grp[, c("yoy_v_imp","yoy_q_imp","yoy_rv_imp","yoy_v_exp","yoy_q_exp","yoy_rv_exp",
                      "prev_v_imp","prev_q_imp","prev_rv_imp","prev_v_exp","prev_q_exp","prev_rv_exp") := NULL]

# --- Area × item group outliers

setorder(area_item_grp, area_group_code, item_group_code, timePointYears)

area_item_grp[, yoy_v_imp  := idx_v_imp  / shift(idx_v_imp),  by = .(area_group_code, item_group_code)]
area_item_grp[, yoy_q_imp  := idx_q_imp  / shift(idx_q_imp),  by = .(area_group_code, item_group_code)]
area_item_grp[, yoy_rv_imp := idx_rv_imp / shift(idx_rv_imp), by = .(area_group_code, item_group_code)]
area_item_grp[, yoy_v_exp  := idx_v_exp  / shift(idx_v_exp),  by = .(area_group_code, item_group_code)]
area_item_grp[, yoy_q_exp  := idx_q_exp  / shift(idx_q_exp),  by = .(area_group_code, item_group_code)]
area_item_grp[, yoy_rv_exp := idx_rv_exp / shift(idx_rv_exp), by = .(area_group_code, item_group_code)]

area_item_grp[, prev_v_imp  := shift(idx_v_imp),  by = .(area_group_code, item_group_code)]
area_item_grp[, prev_q_imp  := shift(idx_q_imp),  by = .(area_group_code, item_group_code)]
area_item_grp[, prev_rv_imp := shift(idx_rv_imp), by = .(area_group_code, item_group_code)]
area_item_grp[, prev_v_exp  := shift(idx_v_exp),  by = .(area_group_code, item_group_code)]
area_item_grp[, prev_q_exp  := shift(idx_q_exp),  by = .(area_group_code, item_group_code)]
area_item_grp[, prev_rv_exp := shift(idx_rv_exp), by = .(area_group_code, item_group_code)]

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
  build_agg_outlier_aig(area_item_grp, "idx_rv_imp", "prev_rv_imp", "yoy_rv_imp", "Import", "RealValue"),
  build_agg_outlier_aig(area_item_grp, "idx_v_exp",  "prev_v_exp",  "yoy_v_exp",  "Export", "Value"),
  build_agg_outlier_aig(area_item_grp, "idx_q_exp",  "prev_q_exp",  "yoy_q_exp",  "Export", "Quantity"),
  build_agg_outlier_aig(area_item_grp, "idx_rv_exp", "prev_rv_exp", "yoy_rv_exp", "Export", "RealValue")
)

area_item_grp[, c("yoy_v_imp","yoy_q_imp","yoy_rv_imp","yoy_v_exp","yoy_q_exp","yoy_rv_exp",
                   "prev_v_imp","prev_q_imp","prev_rv_imp","prev_v_exp","prev_q_exp","prev_rv_exp") := NULL]

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

# --- UV-source provenance summary per flagged aggregate
if (nrow(agg_outliers_all) > 0) {

  prov_list <- vector("list", nrow(agg_outliers_all))

  for (i in seq_len(nrow(agg_outliers_all))) {
    row      <- agg_outliers_all[i]
    yr       <- row$Year
    flow     <- row$Flow
    idx_type <- row$Index
    grp_code <- row$Item_Group

    q_col     <- if (flow == "Import") "imp_q"         else "exp_q"
    v_col     <- if (flow == "Import") "imp_v"         else "exp_v"
    rv_col    <- if (flow == "Import") "real_v_imp"    else "real_v_exp"
    uvsrc_col <- if (flow == "Import") "uv_source_imp" else "uv_source_exp"

    meas_col <- if (idx_type == "Quantity") q_col else if (idx_type == "Value") v_col else rv_col

    items_in_grp <- item_map[item_group_code == grp_code, measuredItemCPC]

    countries <- if (row$Level == "Country_ItemGroup") {
      row$Area
    } else {
      area_agg_map[area_group_code == row$Area, geographicAreaM49]
    }

    u <- trade_calc[
      geographicAreaM49 %in% countries   &
      timePointYears    == yr            &
      measuredItemCPC   %in% items_in_grp &
      !is.na(get(meas_col)),
      .(src = get(uvsrc_col))
    ]

    n_tot     <- nrow(u)
    n_country <- u[src == "country", .N]
    n_global  <- u[src == "global",  .N]
    n_na      <- u[is.na(src),        .N]
    n_fall    <- n_tot - n_country - n_global - n_na

    detail <- if (n_tot == 0) NA_character_ else {
      tbl <- u[, .N, by = src][order(-N)]
      paste(sprintf("%s:%d", ifelse(is.na(tbl$src), "NA", tbl$src), tbl$N), collapse = "; ")
    }

    prov_list[[i]] <- data.table(
      N_Items          = n_tot,
      N_UV_Country     = n_country,
      N_UV_Fallback    = n_fall,
      N_UV_Global      = n_global,
      N_UV_NA          = n_na,
      Pct_UV_Country   = if (n_tot > 0) round(100 * n_country / n_tot, 1) else NA_real_,
      UV_Source_Detail = detail
    )
  }

  agg_outliers_all <- cbind(agg_outliers_all, rbindlist(prov_list))
}

# --- FAOSTAT value for each flagged aggregate
if (nrow(agg_outliers_all) > 0 && exists("fao_idx") && !is.null(fao_idx)) {

  norm_m49 <- function(x) as.character(suppressWarnings(as.integer(as.character(x))))

  fao_a <- fao_idx[, .(
    .akey = Area_M49, Item_Group = Item_CPC, .ykey = Year,
    FAO_Import_Value_Index, FAO_Import_Quantity_Index,
    FAO_Export_Value_Index, FAO_Export_Quantity_Index)]

  agg_outliers_all[, `:=`(.akey = norm_m49(Area), .ykey = as.character(Year))]
  agg_outliers_all <- merge(agg_outliers_all, fao_a,
                            by = c(".akey", "Item_Group", ".ykey"),
                            all.x = TRUE, sort = FALSE)

  agg_outliers_all[, FAO_Index_Value := fcase(
    Flow == "Import" & Index == "Value",                      FAO_Import_Value_Index,
    Flow == "Import" & Index == "Quantity", FAO_Import_Quantity_Index,
    Flow == "Export" & Index == "Value",                      FAO_Export_Value_Index,
    Flow == "Export" & Index == "Quantity", FAO_Export_Quantity_Index,
    default = NA_real_)]

  agg_outliers_all[, c(".akey", ".ykey",
                       "FAO_Import_Value_Index", "FAO_Import_Quantity_Index",
                       "FAO_Export_Value_Index", "FAO_Export_Quantity_Index") := NULL]

  cn <- names(agg_outliers_all)
  cn <- append(cn[cn != "FAO_Index_Value"], "FAO_Index_Value",
               after = which(cn[cn != "FAO_Index_Value"] == "Index_Value"))
  setcolorder(agg_outliers_all, cn)

  setorder(agg_outliers_all, -Index_Value)
}

fwrite(agg_outliers_all, "results/checks/outlier_check_aggregates.csv")
cat("Saved: _outlier_check_aggregates.csv\n")

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

    baseuv_col   <- if (flow == "Import") "base_uv_imp"     else "base_uv_exp"
    uvsrc_col    <- if (flow == "Import") "uv_source_imp"   else "uv_source_exp"
    basemq_col   <- if (flow == "Import") "base_mean_q_imp" else "base_mean_q_exp"
    basemv_col   <- if (flow == "Import") "base_mean_v_imp" else "base_mean_v_exp"
    basenyrs_col <- if (flow == "Import") "base_n_yrs_imp"  else "base_n_yrs_exp"

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
          Real_Value_1000USD  = get(rv_col),
          Base_UV_Used        = get(baseuv_col),
          UV_Source           = get(uvsrc_col),
          Base_Mean_Quantity  = get(basemq_col),
          Base_Mean_Value     = get(basemv_col),
          Base_Years_Used     = get(basenyrs_col)
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
          Real_Value_1000USD  = get(rv_col),
          Base_UV_Used        = get(baseuv_col),
          UV_Source           = get(uvsrc_col),
          Base_Mean_Quantity  = get(basemq_col),
          Base_Mean_Value     = get(basemv_col),
          Base_Years_Used     = get(basenyrs_col)
        )
      ]
    }

    if (idx_type == "Quantity")  underlying <- underlying[!is.na(Quantity)]
    if (idx_type == "Value")     underlying <- underlying[!is.na(Trade_Value_1000USD)]
    if (idx_type == "RealValue") underlying <- underlying[!is.na(Real_Value_1000USD)]

    drilldown_list[[i]] <- underlying
  }

  drilldown_all <- rbindlist(drilldown_list, fill = TRUE)
  setorder(drilldown_all, -Flagged_Index_Value, Country, Item_CPC)

  # --- Attach the matching FAOSTAT item-level index
  if (exists("fao_idx") && !is.null(fao_idx) && nrow(drilldown_all) > 0) {

    norm_m49 <- function(x) as.character(suppressWarnings(as.integer(as.character(x))))

    fao_item <- fao_idx[, .(
      .akey = Area_M49, Item_CPC = Item_CPC, .ykey = Year,
      FAO_Import_Value_Index, FAO_Import_Quantity_Index,
      FAO_Export_Value_Index, FAO_Export_Quantity_Index)]

    drilldown_all[, `:=`(.akey = norm_m49(Country), .ykey = as.character(Year))]
    drilldown_all <- merge(drilldown_all, fao_item,
                           by = c(".akey", "Item_CPC", ".ykey"),
                           all.x = TRUE, sort = FALSE)

    drilldown_all[, FAO_Item_Index := fcase(
      Flow == "Import" & Index == "Value",                          FAO_Import_Value_Index,
      Flow == "Import" & Index == "Quantity",     FAO_Import_Quantity_Index,
      Flow == "Export" & Index == "Value",                          FAO_Export_Value_Index,
      Flow == "Export" & Index == "Quantity",     FAO_Export_Quantity_Index,
      default = NA_real_
    )]
    drilldown_all[, c(".akey", ".ykey",
                      "FAO_Import_Value_Index", "FAO_Import_Quantity_Index",
                      "FAO_Export_Value_Index", "FAO_Export_Quantity_Index") := NULL]

    fao_agg <- fao_idx[, .(
      .akey = Area_M49, Flagged_Item_Group = Item_CPC, .ykey = Year,
      .fiv = FAO_Import_Value_Index, .fiq = FAO_Import_Quantity_Index,
      .fev = FAO_Export_Value_Index, .feq = FAO_Export_Quantity_Index)]
    drilldown_all[, `:=`(.akey = norm_m49(Flagged_Area), .ykey = as.character(Year))]
    drilldown_all <- merge(drilldown_all, fao_agg,
                           by = c(".akey", "Flagged_Item_Group", ".ykey"),
                           all.x = TRUE, sort = FALSE)
    drilldown_all[, Flagged_FAO_Index := fcase(
      Flow == "Import" & Index == "Value",                      .fiv,
      Flow == "Import" & Index == "Quantity", .fiq,
      Flow == "Export" & Index == "Value",                      .fev,
      Flow == "Export" & Index == "Quantity", .feq,
      default = NA_real_)]
    drilldown_all[, c(".akey", ".ykey", ".fiv", ".fiq", ".fev", ".feq") := NULL]

    setorder(drilldown_all, -Flagged_Index_Value, Country, Item_CPC)
  }

  cat(sprintf("Drill-down rows written: %s\n",
              formatC(nrow(drilldown_all), format = "d", big.mark = ",")))

  fwrite(drilldown_all, "results/checks/outlier_drilldown_aggregates.csv")
  cat("Saved: _outlier_check_aggregates_drilldown.csv\n")
}

cat("\n--- Step 6.5 complete ---\n")

cat("\nAll output files written:\n")
cat("  real_values_import.csv                        — individual items, all years\n")
cat("  real_values_export.csv                        — individual items, all years\n")
cat("  indices_import.csv                            — item-level indices, all years\n")
cat("  indices_export.csv                            — item-level indices, all years\n")
cat("  outlier_check.csv                             — flagged extreme item-level index values\n")
cat("  outlier_check_aggregates.csv                  — flagged extreme aggregate index values\n")
cat("  outlier_check_aggregates_drilldown.csv        — underlying data for top 50 flagged aggregates\n")
cat("  aggregates_country_item_groups.csv            — item group aggregates per country\n")
cat("  aggregates_area_item_groups.csv               — item group aggregates per area group\n")

# STEP 7 — FAOSTAT vs OURS: index comparison workbook
#

cat("\n=== STEP 7: FAOSTAT vs OURS INDEX COMPARISON ===\n")

if (!("FAO_Value_Index" %in% names(idx_imp))) {
  cat("FAO columns absent (fao_idx was NULL) — skipping comparison workbook.\n")
} else {

  SAME_TOL <- 0

  # ---- Name lookups (small reference files; keys normalised to idx form)
  cc_nm <- fread("CountryCode.csv", colClasses = "character")
  cc_nm[, Area_M49 := suppressWarnings(as.character(as.integer(gsub("'", "", `M49 Code`))))]
  area_nm <- cc_nm[!is.na(Area_M49) & Area_M49 != "", .(Area = Area[1L]), by = Area_M49]

  ic_nm <- fread("ItemCodes.csv", colClasses = "character")
  ic_nm[, Item_CPC := gsub("'", "", `CPC Code`)]
  item_nm <- ic_nm[!is.na(Item_CPC) & Item_CPC != "", .(Item = Item[1L]), by = Item_CPC]

  idx_pairs <- list(
    list(name = "Value",     ours = "Value_Index",     fao = "FAO_Value_Index"),
    list(name = "Quantity",  ours = "Quantity_Index",  fao = "FAO_Quantity_Index"),
    list(name = "UnitValue", ours = "UnitValue_Index", fao = "FAO_UnitValue_Index")
  )
  flows_cmp <- list(
    list(flow = "Import", dt = idx_imp),
    list(flow = "Export", dt = idx_exp)
  )

  summary_rows <- list()
  gap_sheets   <- list()

  for (fl in flows_cmp) {
    for (it in idx_pairs) {
      sub <- fl$dt[!is.na(get(it$ours)) & !is.na(get(it$fao)),
                   .(Area_M49, Item_CPC, Year,
                     Our_Index = get(it$ours), FAO_Index = get(it$fao))]
      sub[, Diff     := Our_Index - FAO_Index]
      sub[, Abs_Diff := abs(Diff)]
      sub[, Pct_Diff := fifelse(FAO_Index != 0,
                                100 * Diff / FAO_Index, NA_real_)]
      n <- nrow(sub)

      summary_rows[[length(summary_rows) + 1L]] <- data.table(
        Flow            = fl$flow,
        Index           = it$name,
        N_matched       = n,
        Correlation     = if (n > 1) round(cor(sub$Our_Index, sub$FAO_Index), 4) else NA_real_,
        Mean_abs_diff   = round(mean(sub$Abs_Diff), 3),
        Median_abs_diff = round(median(sub$Abs_Diff), 3),
        P95_abs_diff    = round(as.numeric(quantile(sub$Abs_Diff, 0.95)), 3),
        Max_abs_diff    = round(max(sub$Abs_Diff), 3),
        Pct_within_1pt  = round(100 * mean(sub$Abs_Diff <= 1), 2),
        Pct_within_5pt  = round(100 * mean(sub$Abs_Diff <= 5), 2)
      )

      top <- sub[Abs_Diff > SAME_TOL][order(-Abs_Diff)]
      top <- merge(top, area_nm, by = "Area_M49", all.x = TRUE, sort = FALSE)
      top <- merge(top, item_nm, by = "Item_CPC", all.x = TRUE, sort = FALSE)
      top <- top[order(-Abs_Diff)]
      setcolorder(top, c("Area_M49", "Area", "Item_CPC", "Item", "Year",
                         "Our_Index", "FAO_Index", "Diff", "Abs_Diff", "Pct_Diff"))
      top[, c("Our_Index", "FAO_Index", "Diff", "Abs_Diff", "Pct_Diff") :=
            lapply(.SD, round, 2),
          .SDcols = c("Our_Index", "FAO_Index", "Diff", "Abs_Diff", "Pct_Diff")]

      gap_sheets[[paste0(fl$flow, "_", it$name)]] <- top
    }
  }

  summary_dt <- rbindlist(summary_rows)

  # ---- Write workbook (openxlsx already loaded at top of script)
  wb  <- createWorkbook()
  hdr <- createStyle(textDecoration = "bold", fgFill = "#1F4E78",
                     fontColour = "white", halign = "center",
                     border = "TopBottomLeftRight")
  numS <- createStyle(numFmt = "#,##0.00")

  addWorksheet(wb, "Summary")
  writeData(wb, "Summary", summary_dt, headerStyle = hdr)
  freezePane(wb, "Summary", firstActiveRow = 2)
  setColWidths(wb, "Summary", cols = 1:ncol(summary_dt), widths = "auto")

  for (nm in names(gap_sheets)) {
    addWorksheet(wb, nm)
    writeData(wb, nm, gap_sheets[[nm]], headerStyle = hdr)
    freezePane(wb, nm, firstActiveRow = 2)
    setColWidths(wb, nm, cols = 1:ncol(gap_sheets[[nm]]), widths = "auto")
    addStyle(wb, nm, numS, rows = 2:(nrow(gap_sheets[[nm]]) + 1),
             cols = 6:10, gridExpand = TRUE)
  }

  saveWorkbook(wb, "results/checks/faostat_vs_ours_indices.xlsx", overwrite = TRUE)
  cat("Saved: _faostat_vs_ours_indices.xlsx\n")
  print(summary_dt)
}

cat("  _faostat_vs_ours_indices.xlsx                 — FAOSTAT vs ours index comparison\n")

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
cat(sprintf("Items in item-level outputs (ItemList.xlsx)   : %s listed / %s found in data\n",
            fm(gs("items_listed")), fm(gs("items_matched"))))
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


# STEP 8 — Diagnostics dashboard (item indices, aggregates, real values)
# Rebuilds results/diagnostics_dashboard.html and
# results/checks/diagnostics_item_indices.csv from this run's outputs.
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

# =============================================================================
# Trade Real Values & Indices — diagnostics dashboard
#
# Runs automatically as Step 8 of trade_calculations.R (needs the pipeline's
# objects in memory). Run on its own from the project folder
#   source("diagnostics/build_dashboard.R")
# it rebuilds the page from the last cached data (results/checks/diagnostics_cache.rds),
# which is useful after a change to the page template only.
#
# Sections of the dashboard
#   Indices           aggregate value and quantity indices: extreme values and their cause
#   Real values       aggregate real values that change 10x or more from the previous year,
#                     compared with what other countries did in the same year
#   Base unit values  base unit values far from other countries' or from the country's own years
#   Dissemination groups  size and reliability of every item group and area level, with a
#                     tick box per item group (pre-ticked from DisseminationList.csv) and
#                     each group's FAOSTAT level (faostat_product_group_levels.xlsx);
#                     plus a separate section for the item groups excluded from the
#                     calculations (excluded_groups in trade_calculations.R)
#
# Writes  results/diagnostics_dashboard.html
#         results/checks/diagnostics_aggregates.csv
#         results/checks/diagnostics_real_value_jumps.csv
#         results/checks/diagnostics_base_unit_values.csv
#         results/checks/diagnostics_dissemination_groups.csv
#         results/checks/diagnostics_excluded_groups.csv
#         results/checks/diagnostics_cache.rds
#
# Packages: data.table, jsonlite.
# =============================================================================

suppressMessages({ library(data.table); library(jsonlite) })

t0 <- Sys.time()
cat("\n=== DIAGNOSTICS DASHBOARD ===\n")

# -----------------------------------------------------------------------------
# Settings
# -----------------------------------------------------------------------------
CFG <- list(
  base_years = 2014:2016,
  crit_hi    = 5000,   # index: critical above this ...
  crit_lo    = 1,      # ... or below this
  warn_hi    = 1000,   # index: warning above this ...
  warn_lo    = 10,     # ... or below this
  yoy        = 10,     # warning: year-on-year change of 10x or more, either way
  jump_w     = 10,     # real value: warning when it changes 10x or more from the previous year
  jump_c     = 100,    # real value: critical when it changes 100x or more
  shared     = 0.15,   # real value jump shared by other countries: at least this share (and 3 countries) also jumped
  floor_q    = 10,     # negligible quantity (t)
  floor_v    = 10,     # negligible value (1000 USD)
  window     = 3,      # +/- years used for "surrounding years"
  size_years = 5,      # trade size = mean value of the latest N years with data
  dominant   = 0.5,    # one member explains at least this share of the change
  coverage   = 0.5,    # members absent in base / in the year explain at least this share
  uv_w       = 10,     # base unit value: warning when 10x off ...
  uv_c       = 100,    # ... critical when 100x off other countries' base unit value
  uv_peers   = 5,      # other countries' base UV needs at least this many countries with own data
  small      = 100     # dissemination groups: a series below this size (1000 USD / year) counts as small
)

script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) "diagnostics")
template   <- file.path(script_dir, "dashboard_template.html")
if (!file.exists(template)) template <- "diagnostics/dashboard_template.html"
stopifnot(file.exists(template))
CACHE <- "results/checks/diagnostics_cache.rds"

PIPE <- all(vapply(c("trade_calc", "item_map", "area_agg_map", "country_item_grp", "area_item_grp",
                     "imp_uv_base", "exp_uv_base", "agg_table"), exists, logical(1))) &&
        file.exists("results/real_values_aggregates.csv")

norm_a <- function(x) sub("^0+(?=\\d)", "", gsub("'", "", as.character(x)), perl = TRUE)
norm_i <- function(x) gsub("'", "", as.character(x))
med2   <- function(x) { x <- x[!is.na(x) & x > 0]; if (length(x) >= 2) median(x) else NA_real_ }
l10    <- function(a, b) fifelse(!is.na(a) & !is.na(b) & a > 0 & b > 0, log10(a / b), 0)
sg6    <- function(x) signif(x, 6)
win    <- c(min(CFG$base_years) - CFG$window:1, max(CFG$base_years) + 1:CFG$window)
offs   <- setdiff(-CFG$window:CFG$window, 0)

# median of positive values in the +/- window around each row (same series), excluding the row
nbr_med <- function(x, sid) {
  x[!is.na(x) & x <= 0] <- NA; n <- length(x)
  long <- rbindlist(lapply(offs, function(k) {
    j <- seq_len(n) + k; ok <- j >= 1 & j <= n; jj <- ifelse(ok, j, 1L)
    data.table(r = seq_len(n), val = fifelse(ok & sid[jj] == sid, x[jj], NA_real_))
  }))[!is.na(val)]
  m <- long[, .(med = if (.N >= 2) median(val) else NA_real_), by = r]
  out <- rep(NA_real_, n); out[m$r] <- m$med; out
}

# Severity + cause for a table of index values. Needs: x, px, cur, base, typ, nbm,
# pcur, pnbm, tiny_cur, tiny_prev, tiny_base. Adds sev (dropping 0), comps, cat.
classify <- function(P) {
  P[, yr   := fifelse(!is.na(px) & px > 0, x / px, NA_real_)]
  P[, lvl  := x > CFG$warn_hi | x < CFG$warn_lo]
  P[, jump := !is.na(yr) & (yr >= CFG$yoy | yr <= 1 / CFG$yoy)]
  P[, sev  := fifelse(x > CFG$crit_hi | x < CFG$crit_lo, 2L, fifelse(lvl | jump, 1L, 0L))]
  P <- P[sev > 0]
  P[, D  := fifelse(lvl, log10(x / 100), log10(yr))]
  P[, sg := sign(D)]
  # deviation split (log10): base = level around base vs base; one-off = year vs
  # surrounding years; sustained = surrounding years vs level around base
  P[, `:=`(cB = 0, cO = 0, cP = 0, cS = 0)]
  P[lvl == TRUE, `:=`(cB = l10(typ, base), cO = l10(cur, nbm), cS = l10(nbm, typ))]
  P[lvl == TRUE & is.na(nbm) & !is.na(typ), cS := l10(cur, typ)]
  P[lvl == TRUE & is.na(typ), `:=`(cB = 0, cS = 0)]
  P[lvl == FALSE, `:=`(cO = l10(cur, nbm), cP = -l10(pcur, pnbm))]
  P[lvl == FALSE, cS := D - cO - cP]
  P[, `:=`(aB = cB * sg, aO = pmax(cO * sg, cP * sg), aS = cS * sg)]
  P[, comp := fifelse(aB >= aO & aB >= aS & aB > 0, "B", fifelse(aO >= aS & aO > 0, "O", "S"))]
  P[lvl == TRUE & is.na(typ) & is.na(nbm), comp := fifelse(sg > 0 & tiny_base, "B", "S")]
  P[, odd_prev := !lvl & cP * sg > cO * sg]
  P[, cat := fcase(
    comp == "B" & sg > 0 & tiny_base, "tiny_base",
    comp == "B" & sg > 0, "low_base",
    comp == "B" & sg < 0, "high_base",
    tiny_cur | tiny_prev, "tiny_flow",
    comp == "O" & !odd_prev & sg > 0, "spike",
    comp == "O" & !odd_prev & sg < 0, "dip",
    comp == "O" & odd_prev & sg > 0, "dip",
    comp == "O" & odd_prev & sg < 0, "spike",
    default = "trend")]
  P
}

enc <- function(x) {
  js <- toJSON(x, dataframe = "columns", na = "null", digits = NA, auto_unbox = TRUE)
  b <- gsub("[\r\n]", "", base64_enc(memCompress(charToRaw(enc2utf8(as.character(js))), type = "gzip")))
  cat(sprintf("Payload: %.1f MB JSON -> %.1f MB embedded\n", nchar(js) / 1e6, nchar(b) / 1e6)); b
}
# FAOSTAT level of each item group (faostat_product_group_levels.xlsx: Level, Group name),
# matched by group name ("; " in the aggregation table = ", " in the levels file;
# Agricultural Products is called "Crops and livestock products" there)
LEVELS_FILE <- "faostat_product_group_levels.xlsx"
LEVEL_ALIAS <- c("agricultural products" = "crops and livestock products")
norm_g <- function(x) {
  x <- tolower(gsub("\\s+", " ", trimws(gsub(";", ",", x, fixed = TRUE))))
  fifelse(x %in% names(LEVEL_ALIAS), LEVEL_ALIAS[x], x)
}
grp_levels <- function(nm) {
  if (!file.exists(LEVELS_FILE) || !requireNamespace("openxlsx", quietly = TRUE)) {
    cat("Note:", LEVELS_FILE, "(or the openxlsx package) not found: item group levels left empty\n")
    return(rep(NA_integer_, length(nm)))
  }
  lv <- openxlsx::read.xlsx(LEVELS_FILE)
  out <- as.integer(lv[[1]])[match(norm_g(nm), norm_g(lv[[2]]))]
  if (any(is.na(out))) cat("Item groups not in", LEVELS_FILE, ":", paste(nm[is.na(out)], collapse = " | "), "\n")
  out
}
write_html <- function(payload, payload_agg) {
  pub <- as.data.table(payload_agg$pub)
  pub[, lvl := grp_levels(grp_name)]
  payload_agg$pub <- pub
  if (!is.null(payload_agg$pubx) && length(payload_agg$pubx$grp)) {
    pubx <- as.data.table(payload_agg$pubx)
    pubx[, lvl := grp_levels(grp_name)]
    payload_agg$pubx <- pubx
  }
  b64 <- enc(payload); b64a <- enc(payload_agg)
  html <- readChar(template, file.info(template)$size, useBytes = TRUE)
  p1 <- strsplit(html, "__PAYLOAD_AGG__", fixed = TRUE, useBytes = TRUE)[[1]]
  stopifnot(length(p1) == 2)
  p0 <- strsplit(p1[1], "__PAYLOAD__", fixed = TRUE, useBytes = TRUE)[[1]]
  stopifnot(length(p0) == 2)
  writeBin(c(charToRaw(p0[1]), charToRaw(b64), charToRaw(p0[2]), charToRaw(b64a), charToRaw(p1[2])),
           "results/diagnostics_dashboard.html")
  cat(sprintf("Saved: results/diagnostics_dashboard.html  (%.1f s)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

if (!PIPE) {
  if (!file.exists(CACHE)) stop("The pipeline's objects are not in memory and there is no ", CACHE,
                                ". Run trade_calculations.R once.")
  cat("Pipeline objects not in memory: rebuilding the page from", CACHE, "\n")
  cc <- readRDS(CACHE)
  write_html(cc$payload, cc$payload_agg)
} else {

# -----------------------------------------------------------------------------
# 1. Aggregate series (country and region × item group × flow × year)
# -----------------------------------------------------------------------------
cat("Aggregates: building series ...\n")
# the published aggregates: rows of either output file
scope <- unique(rbindlist(lapply(c("results/real_values_aggregates.csv", "results/indices_aggregates.csv"), function(f)
  if (file.exists(f)) unique(fread(f, encoding = "UTF-8", colClasses = "character",
                                   select = c("Level", "Area Code (M49)", "Area", "Item Group Code", "Item Group"))))))
setnames(scope, c("lvl", "area", "area_name", "grp", "grp_name"))
scope_s <- unique(scope[, .(lvl, area, grp)])
cig <- get("country_item_grp"); aig <- get("area_item_grp")
wide <- rbind(
  cig[, .(lvl = "Country", area = as.character(geographicAreaM49), grp = item_group_code, year = as.integer(timePointYears),
          iV = agg_imp_v, iQ = agg_imp_q, iR = agg_real_v_imp, eV = agg_exp_v, eQ = agg_exp_q, eR = agg_real_v_exp,
          xiv = idx_v_imp, xiq = idx_q_imp, xev = idx_v_exp, xeq = idx_q_exp)],
  aig[, .(lvl = "Region", area = as.character(area_group_code), grp = item_group_code, year = as.integer(timePointYears),
          iV = agg_imp_v, iQ = agg_imp_q, iR = agg_real_v_imp, eV = agg_exp_v, eQ = agg_exp_q, eR = agg_real_v_exp,
          xiv = idx_v_imp, xiq = idx_q_imp, xev = idx_v_exp, xeq = idx_q_exp)])
wide <- wide[scope_s, on = .(lvl, area, grp), nomatch = 0]
A <- rbind(wide[, .(lvl, area, grp, year, flow = "Import", V = iV, Q = iQ, R = iR, pv = xiv, pq = xiq)],
           wide[, .(lvl, area, grp, year, flow = "Export", V = eV, Q = eQ, R = eR, pv = xev, pq = xeq)])
rm(wide)
A <- A[, if (any(!is.na(V) & V != 0 | !is.na(R) & R != 0 | !is.na(Q) & Q != 0)) .SD, by = .(lvl, area, grp, flow)]
yrs <- 2000:max(A$year)
ggrid <- unique(A[, .(lvl, area, grp, flow)])[, .(year = yrs), by = .(lvl, area, grp, flow)]
A <- A[ggrid, on = .(lvl, area, grp, flow, year)]
setorder(A, lvl, area, grp, flow, year)
A[, gsid := .GRP, by = .(lvl, area, grp, flow)]

# base means and indices exactly as Step 6
AB <- A[year %in% CFG$base_years, .(BV = mean(V, na.rm = TRUE), BQ = mean(Q, na.rm = TRUE),
                                    nbV = sum(!is.na(V)), nbQ = sum(!is.na(Q))), by = gsid]
for (cc in c("BV", "BQ")) AB[is.nan(get(cc)), (cc) := NA_real_]
A <- merge(A, AB, by = "gsid", all.x = TRUE)
setorder(A, gsid, year)
A[, `:=`(xV = fifelse(!is.na(V) & BV > 0, V / BV * 100, NA_real_),
         xQ = fifelse(!is.na(Q) & BQ > 0, Q / BQ * 100, NA_real_))]
AVER <- rbindlist(lapply(list(c("Value", "xV", "pv"), c("Quantity", "xQ", "pq")), function(z) {
  a <- A[[z[3]]]; b <- A[[z[2]]]; ok <- !is.na(a) & !is.na(b)
  list(index = z[1], max_rel = if (any(ok)) max(abs(a[ok] - b[ok]) / pmax(abs(a[ok]), 1e-12)) else 0,
       na_mismatch = sum(is.na(a) != is.na(b)))
}))
print(AVER)

# reference levels
Tn <- A[year %in% win, .(tV = med2(V), tQ = med2(Q)), by = gsid]
Ta <- A[!(year %in% CFG$base_years), .(aV = med2(V), aQ = med2(Q)), by = gsid]
A <- merge(merge(A, Tn, by = "gsid", all.x = TRUE), Ta, by = "gsid", all.x = TRUE)
A[is.na(tV), tV := aV]; A[is.na(tQ), tQ := aQ]; A[, c("aV", "aQ") := NULL]
setorder(A, gsid, year)
A[, nV := nbr_med(V, gsid)]; A[, nQ := nbr_med(Q, gsid)]
A[, `:=`(pV = shift(V), pQ = shift(Q), pR = shift(R), pxV = shift(xV), pxQ = shift(xQ),
         pnV = shift(nV), pnQ = shift(nQ)), by = gsid]
ASZ <- A[!is.na(V) & V > 0][order(gsid, -year)][, .(size = mean(head(V, CFG$size_years))), by = gsid]
A <- merge(A, ASZ, by = "gsid", all.x = TRUE)
A[is.na(size), size := 0]
setorder(A, gsid, year)
SER <- unique(A[, .(gsid, lvl, area, grp, flow, size)])

# -----------------------------------------------------------------------------
# 2. Indices: flag and explain aggregate value and quantity index values
# -----------------------------------------------------------------------------
mka <- function(k) {
  d <- A[, .(gsid, year, type = k, x = get(paste0("x", k)), px = get(paste0("px", k)), cur = get(k),
             base = get(paste0("B", k)), typ = get(paste0("t", k)), nbm = get(paste0("n", k)),
             pcur = get(paste0("p", k)), pnbm = get(paste0("pn", k)))]
  d <- d[!is.na(x) & x > 0]
  fl <- if (k == "Q") CFG$floor_q else CFG$floor_v
  d[, `:=`(tiny_cur = cur < fl, tiny_prev = FALSE, tiny_base = base < fl)]
  d
}
PA <- classify(rbindlist(lapply(c("V", "Q"), mka)))
PA[!lvl & !is.na(pcur) & pcur < fifelse(type == "Q", CFG$floor_q, CFG$floor_v) & cat == "trend", cat := "tiny_flow"]
PA[, tiny_base_agg := tiny_base & sg > 0 & lvl]
cat("Indices: flagged aggregate index values:", format(nrow(PA), big.mark = ","), "\n")

# ---- members of every flagged aggregate series (processed in chunks to limit memory)
fl_s <- SER[gsid %in% unique(PA$gsid)]
tim <- if (exists("trade_item_merge")) get("trade_item_merge") else
  merge(get("trade_calc")[, .(geographicAreaM49, measuredItemCPC, timePointYears, imp_v, imp_q, real_v_imp, exp_v, exp_q, real_v_exp)],
        get("item_map"), by = "measuredItemCPC", allow.cartesian = TRUE)
TIM <- tim[, .(area = as.character(geographicAreaM49), grp = item_group_code, member = norm_i(measuredItemCPC),
               year = as.integer(timePointYears), f = item_factor, imp_v, imp_q, real_v_imp, exp_v, exp_q, real_v_exp)]
rm(tim)
setkey(TIM, area, grp)
amap <- unique(get("area_agg_map")[, .(area = as.character(area_group_code), member = as.character(geographicAreaM49))])
cig_m <- cig[, .(member = as.character(geographicAreaM49), grp = item_group_code, year = as.integer(timePointYears),
                 agg_imp_v, agg_imp_q, agg_real_v_imp, agg_exp_v, agg_exp_q, agg_real_v_exp)]
setkey(cig_m, grp)
AY  <- A[year %in% CFG$base_years, .(gsid, year, okV = !is.na(V), okQ = !is.na(Q), nbV, nbQ)]
ABB <- unique(A[, .(gsid, BV, BQ)], by = "gsid")
FPall <- unique(PA[, .(gsid, year, type)])
ids <- unique(fl_s$gsid)
chunks <- split(ids, ceiling(seq_along(ids) / 800))
XSL <- vector("list", length(chunks)); TOPL <- vector("list", length(chunks))
members_of <- function(fs) {
  cp <- unique(fs[lvl == "Country", .(area, grp)]); rp <- unique(fs[lvl == "Region", .(area, grp)])
  MC <- TIM[cp, nomatch = 0]
  MC <- rbind(MC[, .(lvl = rep("Country", .N), area, grp, member, year, flow = rep("Import", .N), mV = imp_v, mQ = imp_q * f, mR = real_v_imp)],
              MC[, .(lvl = rep("Country", .N), area, grp, member, year, flow = rep("Export", .N), mV = exp_v, mQ = exp_q * f, mR = real_v_exp)])
  MR <- merge(amap[area %in% rp$area], cig_m[grp %in% rp$grp], by = "member", allow.cartesian = TRUE)[rp, on = .(area, grp), nomatch = 0]
  MR <- rbind(MR[, .(lvl = rep("Region", .N), area, grp, member, year, flow = rep("Import", .N), mV = agg_imp_v, mQ = agg_imp_q, mR = agg_real_v_imp)],
              MR[, .(lvl = rep("Region", .N), area, grp, member, year, flow = rep("Export", .N), mV = agg_exp_v, mQ = agg_exp_q, mR = agg_real_v_exp)])
  M <- merge(rbind(MC, MR), fs[, .(gsid, lvl, area, grp, flow)], by = c("lvl", "area", "grp", "flow"))
  M[!(is.na(mV) & is.na(mQ) & is.na(mR)), .(gsid, member, year, mV, mQ, mR)]
}
for (k in seq_along(chunks)) {
  fs <- fl_s[gsid %in% chunks[[k]]]
  M <- members_of(fs)
  # member base contributions: base-year values over the base years where the aggregate exists
  MB <- merge(M[year %in% CFG$base_years], AY, by = c("gsid", "year"))
  MB <- MB[, .(bV = sum(fifelse(okV, fcoalesce(mV, 0), 0)) / nbV[1], bQ = sum(fifelse(okQ, fcoalesce(mQ, 0), 0)) / nbQ[1]),
           by = .(gsid, member)]
  for (cc in c("bV", "bQ")) MB[!is.finite(get(cc)), (cc) := 0]
  FP <- FPall[gsid %in% chunks[[k]]]
  MT <- unique(M[unique(FP[, .(gsid, year)]), on = .(gsid, year), nomatch = 0], by = c("gsid", "year", "member"))
  MT <- MT[, .(gsid, year, member, aV = mV, aQ = mQ)]
  ALLM <- unique(rbind(MB[, .(gsid, member)], MT[, .(gsid, member)]))
  X <- merge(FP, ALLM, by = "gsid", allow.cartesian = TRUE)
  X <- merge(X, MB, by = c("gsid", "member"), all.x = TRUE)
  X <- merge(X, MT, by = c("gsid", "year", "member"), all.x = TRUE)
  for (cc in c("bV", "bQ", "aV", "aQ")) set(X, which(is.na(X[[cc]])), cc, 0)
  X[, `:=`(b = fifelse(type == "V", bV, bQ), a = fifelse(type == "V", aV, aQ))]
  X[, d := a - b]
  X <- X[!(b == 0 & a == 0), .(gsid, year, type, member, a, b, d)]
  X[, Dsum := sum(d), by = .(gsid, year, type)]
  X[, sgn := fifelse(Dsum >= 0, 1, -1)]
  X[, ord := -d * sgn]
  setorder(X, gsid, year, type, ord)
  XSL[[k]] <- X[, .(n_mem = .N, top = member[1], top_share = if (Dsum[1] != 0) d[1] / Dsum[1] else 0,
                    share_new = if (sum(a) > 0) sum(a[b == 0]) / sum(a) else 0,
                    share_lost = if (sum(b) > 0) sum(b[a == 0 & b > 0]) / sum(b) else 0,
                    n_new = sum(b == 0 & a > 0), n_lost = sum(a == 0 & b > 0)), by = .(gsid, year, type)]
  X[, rk := rowid(gsid, year, type)]
  TOPL[[k]] <- X[rk <= 5, .(gsid, year, type, member, b, a, d, rk)]
  rm(X, MT, MB, M, ALLM, FP)
}
XS <- rbindlist(XSL); TOP <- rbindlist(TOPL); rm(XSL, TOPL)
PA <- merge(PA, XS, by = c("gsid", "year", "type"), all.x = TRUE)
PA <- merge(PA, SER[, .(gsid, glvl = lvl)], by = "gsid")
PA[is.na(n_mem), `:=`(n_mem = 0L, top_share = 0, share_new = 0, share_lost = 0, n_new = 0L, n_lost = 0L)]
PA[, acat := paste0("a_", cat)]
PA[top_share >= CFG$dominant & glvl == "Country", acat := "a_one_item"]
PA[top_share >= CFG$dominant & glvl == "Region", acat := "a_one_country"]
PA[lvl == TRUE & sg < 0 & share_lost >= CFG$coverage, acat := "a_lost"]
PA[lvl == TRUE & sg > 0 & share_new >= CFG$coverage, acat := "a_new"]
PA[tiny_base_agg == TRUE, acat := "a_tiny_base"]

# -----------------------------------------------------------------------------
# 3. Real values: aggregate real value 10x or more from the previous year,
#    compared with the other countries (country aggregates) or with the member
#    countries (region aggregates) in the same item group, flow and year
# -----------------------------------------------------------------------------
cat("Real values: year-on-year jumps ...\n")
A[, rr := fifelse(!is.na(R) & !is.na(pR) & R > 0 & pR > 0, R / pR, NA_real_)]
J <- A[!is.na(rr) & (rr >= CFG$jump_w | rr <= 1 / CFG$jump_w),
       .(gsid, lvl, area, grp, flow, year, R, pR, rr, V, pV, Q, pQ, size)]
J[, sev := fifelse(rr >= CFG$jump_c | rr <= 1 / CFG$jump_c, 2L, 1L)]
J[, up := rr > 1]
J[, `:=`(vr = fifelse(!is.na(V) & !is.na(pV) & V > 0 & pV > 0, V / pV, NA_real_),
         qr = fifelse(!is.na(Q) & !is.na(pQ) & Q > 0 & pQ > 0, Q / pQ, NA_real_))]
# country-level ratios of every series and year (the peers)
CR <- A[lvl == "Country" & !is.na(rr), .(area, grp, flow, year, rr, R, pR)]
bk <- function(r) findInterval(log10(r), c(-1, log10(0.5), log10(2), 1), left.open = FALSE)  # 0..4
CR[, b := pmin(pmax(bk(rr), 0L), 4L)]
CR[rr <= 1 / CFG$jump_w, b := 0L]; CR[rr >= CFG$jump_w, b := 4L]
peer_stats <- function(P, J1) {
  # P: peers (key = jump id k) with rr, b, R, pR, area; returns one row per k
  P[, .(np = .N, pmed = median(rr), psame_up = mean(rr >= CFG$jump_w), psame_dn = mean(rr <= 1 / CFG$jump_w),
        b0 = sum(b == 0L), b1 = sum(b == 1L), b2 = sum(b == 2L), b3 = sum(b == 3L), b4 = sum(b == 4L),
        tr = sum(R) / sum(pR)), by = k]
}
J[, k := .I]
# country jumps: all other countries
JC <- J[lvl == "Country", .(k, area, grp, flow, year)]
PC <- merge(JC[, .(k, sarea = area, grp, flow, year)], CR, by = c("grp", "flow", "year"), allow.cartesian = TRUE)[area != sarea]
# region jumps: the region's member countries
JR <- J[lvl == "Region", .(k, rarea = area, grp, flow, year)]
PR <- merge(merge(JR, amap, by.x = "rarea", by.y = "area", allow.cartesian = TRUE),
            CR, by.x = c("member", "grp", "flow", "year"), by.y = c("area", "grp", "flow", "year"))
PR[, area := member]
PS <- rbind(peer_stats(PC, J), peer_stats(PR, J))
J <- merge(J, PS, by = "k", all.x = TRUE)
J[is.na(np), `:=`(np = 0L, b0 = 0L, b1 = 0L, b2 = 0L, b3 = 0L, b4 = 0L)]
J[, psame := fifelse(up, psame_up, psame_dn)]
J[, shared := np >= 5 & ((psame >= CFG$shared & psame * np >= 3) | (up & pmed >= 3) | (!up & pmed <= 1 / 3))]
J[, jcat := fcase(
  pmin(R, pR) < CFG$floor_v, "j_tiny",
  shared == TRUE, "j_shared",
  !is.na(vr) & abs(log10(vr)) < 0.5, "j_rv_only",
  default = "j_both")]
# top peers to show: the largest other countries / members (by previous-year real value)
PP <- rbind(PC[, .(k, area, pR, R)], PR[, .(k, area, pR, R)])
setorder(PP, k, -pR)
PP <- PP[, head(.SD, 8), by = k]
# members behind the jump: items (country) or countries (region), largest change in real value
JK <- rbind(J[lvl == "Country", .(k, area, grp, flow, year, cur = TRUE)],
            J[lvl == "Country", .(k, area, grp, flow, year = year - 1L, cur = FALSE)])
JCm <- merge(JK, TIM, by = c("area", "grp", "year"), allow.cartesian = TRUE)
rm(JK)
JCm[, `:=`(mR = fifelse(flow == "Import", real_v_imp, real_v_exp), mQ = fifelse(flow == "Import", imp_q * f, exp_q * f),
           mV = fifelse(flow == "Import", imp_v, exp_v))]
JCm <- JCm[, .(pmR = sum(mR[!cur], na.rm = TRUE), mR = sum(mR[cur], na.rm = TRUE),
               pmQ = sum(mQ[!cur], na.rm = TRUE), mQ = sum(mQ[cur], na.rm = TRUE),
               pmV = sum(mV[!cur], na.rm = TRUE), mV = sum(mV[cur], na.rm = TRUE)),
           by = .(k, member, area, flow)]
JRm <- merge(J[lvl == "Region", .(k, rarea = area, grp, flow, year)], amap, by.x = "rarea", by.y = "area", allow.cartesian = TRUE)
JRm <- merge(JRm, A[lvl == "Country", .(member = area, grp, flow, year, mR = R, pmR = pR, mQ = Q, pmQ = pQ, mV = V, pmV = pV)],
             by = c("member", "grp", "flow", "year"))
JRm <- JRm[, .(k, member, area = member, flow, pmR = fcoalesce(pmR, 0), mR = fcoalesce(mR, 0), pmQ = fcoalesce(pmQ, 0),
               mQ = fcoalesce(mQ, 0), pmV = fcoalesce(pmV, 0), mV = fcoalesce(mV, 0))]
JM <- rbind(JCm, JRm)
JM <- JM[!(pmR == 0 & mR == 0)]
JM[, dR := mR - pmR]
JM <- merge(JM, J[, .(k, dtot = R - pR)], by = "k")
JM[, share := fifelse(dtot != 0, dR / dtot, 0)]
setorder(JM, k, -share)
JM[, rk := rowid(k)]
JM <- JM[rk <= 5]
J <- merge(J, JM[rk == 1, .(k, top_member = member, top_share = share)], by = "k", all.x = TRUE)
# a region jump that one member country explains is that country's problem, not a shared one
J[lvl == "Region" & jcat == "j_shared" & top_share >= CFG$dominant,
  jcat := fifelse(!is.na(vr) & abs(log10(vr)) < 0.5, "j_rv_only", "j_both")]
cat("Real values: jumps", format(nrow(J), big.mark = ","), "\n")
print(J[, .(jumps = .N, critical = sum(sev == 2)), by = .(lvl, jcat)][order(lvl, jcat)])

# -----------------------------------------------------------------------------
# 4. Base unit values: compared with other countries' and with the country's own years
# -----------------------------------------------------------------------------
cat("Base unit values ...\n")
BUV <- rbind(
  get("imp_uv_base")[, .(area = norm_a(geographicAreaM49), item = norm_i(measuredItemCPC), flow = "Import",
                         buv = base_uv_imp, src = uv_source_imp, own = base_imp_yrs_valid,
                         nimp = n_imp_imputed, ndir = n_imp_direct)],
  get("exp_uv_base")[, .(area = norm_a(geographicAreaM49), item = norm_i(measuredItemCPC), flow = "Export",
                         buv = base_uv_exp, src = uv_source_exp, own = base_exp_yrs_valid,
                         nimp = n_exp_imputed, ndir = n_exp_direct)])
BUV <- BUV[!is.na(buv)]
countries_in <- unique(c(norm_a(scope[lvl == "Country"]$area), norm_a(amap$member)))
TC <- get("trade_calc")[, .(area = norm_a(geographicAreaM49), item = norm_i(measuredItemCPC),
                            year = as.integer(timePointYears), imp_q, imp_v, exp_q, exp_v)]
TL <- rbind(TC[, .(area, item, year, flow = "Import", q = imp_q, v = imp_v)],
            TC[, .(area, item, year, flow = "Export", q = exp_q, v = exp_v)])
rm(TC)
TL <- TL[!is.na(q) | !is.na(v)]
TL[, uv := fifelse(!is.na(q) & !is.na(v) & q > 0 & v > 0, v / q, NA_real_)]
# other countries' unit value of the item each year (median, at least 5 non-negligible records)
G  <- TL[!is.na(uv) & q >= CFG$floor_q & v >= CFG$floor_v, .(G = median(uv), nG = .N), by = .(item, flow, year)][nG >= 5, .(item, flow, year, G)]
GB <- BUV[src %in% "country", .(gb = if (.N >= CFG$uv_peers) median(buv) else NA_real_, ngb = .N), by = .(item, flow)]
OWN <- TL[year %in% CFG$base_years & !is.na(uv),
          .(u_min = min(uv), u_max = max(uv), n_u = .N, tiny_b = any(q < CFG$floor_q | v < CFG$floor_v)), by = .(area, item, flow)]
OTH <- TL[!(year %in% CFG$base_years) & !is.na(uv) & q >= CFG$floor_q & v >= CFG$floor_v,
          .(ou = median(uv), n_o = .N), by = .(area, item, flow)]
QSZ <- TL[!is.na(q) & q > 0][order(area, item, flow, -year)][, .(qs = mean(head(q, CFG$size_years)),
          vs = mean(head(v[!is.na(v)], CFG$size_years))), by = .(area, item, flow)]
# only items that feed the published aggregates (items in an item group)
used_items <- unique(norm_i(get("item_map")$measuredItemCPC))
BU <- BUV[area %in% countries_in & item %in% used_items]
BU <- merge(BU, GB, by = c("item", "flow"), all.x = TRUE)
BU <- merge(BU, OWN, by = c("area", "item", "flow"), all.x = TRUE)
BU <- merge(BU, OTH, by = c("area", "item", "flow"), all.x = TRUE)
BU <- merge(BU, QSZ, by = c("area", "item", "flow"), all.x = TRUE)
BU[is.na(tiny_b), tiny_b := FALSE]
BU[, `:=`(rp = fifelse(!is.na(gb) & gb > 0, buv / gb, NA_real_),
          ro = fifelse(!is.na(ou) & ou > 0, buv / ou, NA_real_),
          disp = fifelse(!is.na(n_u) & n_u >= 2, u_max / u_min, NA_real_))]
BU[, `:=`(lp = fifelse(is.na(rp), 0, abs(log10(rp))), lo = fifelse(is.na(ro), 0, abs(log10(ro))),
          ld = fifelse(is.na(disp), 0, log10(disp)))]
BU[, eff := fifelse(!is.na(qs) & !is.na(gb), qs * (buv - gb), NA_real_)]   # real value effect, 1000 USD / year
BU[, off_p := lp >= log10(CFG$uv_w)]
BU[, off_o := lo >= log10(CFG$uv_w)]
BU[, off_d := ld >= log10(CFG$uv_w)]
BU[, bsev := fifelse(lp >= log10(CFG$uv_c), 2L, fifelse(off_p | off_o | off_d, 1L, 0L))]
BU[, borrowed := !(src %in% c("country", "imputed"))]
same_dir <- function(a, b) !is.na(a) & !is.na(b) & sign(log10(a)) == sign(log10(b))
BU[bsev > 0, bcat := fcase(
  off_p & borrowed, "b_borrowed",
  off_p & tiny_b, "b_tiny",
  off_p & off_d, "b_disagree",
  off_p & src == "imputed", "b_rebuilt",
  off_p & off_o & same_dir(rp, ro), "b_own_off",
  off_p, "b_consistent",
  off_d, "b_disagree",
  default = "b_own_only")]
BI <- BU[bsev > 0]
BI[, tier := match(bcat, c("b_borrowed", "b_tiny", "b_disagree", "b_rebuilt", "b_own_off", "b_consistent", "b_own_only"))]
BI[, aeff := fcoalesce(abs(eff), 0)]
setorder(BI, -bsev, -aeff, tier)
cat("Base unit values: checked", format(nrow(BU), big.mark = ","), "· flagged", format(nrow(BI), big.mark = ","), "\n")
print(BI[, .(series = .N, critical = sum(bsev == 2)), by = bcat][order(-series)])
# distribution of base UV ÷ other countries' base UV, by source
BU[, srcg := fifelse(src %in% "country", "Own data", fifelse(src %in% "imputed", "Partly rebuilt", "From a group"))]
BU[, rb := fifelse(is.na(rp), NA_integer_, as.integer(pmin(pmax(findInterval(log10(rp), c(-2, -1, -log10(3), log10(3), 1, 2)), 0L), 6L)))]
BDIST <- dcast(BU[!is.na(rb)], srcg ~ rb, value.var = "rb", fun.aggregate = length)
BSRC  <- BU[, .(n = .N, flagged = sum(bsev > 0), critical = sum(bsev == 2), no_peer = sum(is.na(gb))), by = srcg]

# -----------------------------------------------------------------------------
# 5. Item series shown in the dashboard: flagged base UVs + items behind a flagged aggregate
# -----------------------------------------------------------------------------
ACATS <- data.table(
  id = c("a_tiny_base", "a_low_base", "a_high_base", "a_new", "a_lost", "a_one_item", "a_one_country",
         "a_spike", "a_dip", "a_tiny_flow", "a_trend"),
  label = c("Negligible base", "Base unusually low", "Base unusually high", "Not traded in base", "Missing in year",
            "Driven by one item", "Driven by one country", "One-year spike", "One-year drop",
            "Negligible flow in year", "Sustained change"),
  driver = c("Base", "Base", "Base", "Coverage", "Coverage", "Member", "Member", "Year", "Year", "Year", "Level"),
  artefact = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
PA[, acat_i := match(acat, ACATS$id) - 1L]
stopifnot(!anyNA(PA$acat_i))
need_items <- unique(merge(PA[acat == "a_one_item", .(gsid, top)], SER, by = "gsid")[, .(area = norm_a(area), item = top, flow)])
keys <- unique(rbind(need_items, BI[, .(area, item, flow)]))
L <- TL[keys, on = .(area, item, flow), nomatch = 0]
grid <- keys[, .(year = yrs), by = .(area, item, flow)]
L <- L[grid, on = .(area, item, flow, year)]
setorder(L, area, item, flow, year)
L[, sid := .GRP, by = .(area, item, flow)]
# source flags
tr <- as.data.table(readRDS("TradeData.rds"))
tr[, `:=`(area = norm_a(geographicAreaM49), item = norm_i(measuredItemCPC))]
tr <- tr[unique(keys[, .(area, item)]), on = .(area, item), nomatch = 0]
tr[, el := c("5610" = "iq", "5622" = "iv", "5910" = "eq", "5922" = "ev")[as.character(measuredElementTrade)]]
tr <- tr[!is.na(el)]
tr[, `:=`(year = as.integer(timePointYears), flow = fifelse(el %in% c("iq", "iv"), "Import", "Export"),
          qv = fifelse(el %in% c("iq", "eq"), "q", "v"), flag = paste0(flagObservationStatus, "|", flagMethod),
          raw = Value)]
FL <- dcast(tr, area + item + flow + year ~ qv, value.var = c("flag", "raw"))
rm(tr)
for (cc in c("flag_q", "flag_v", "raw_q", "raw_v")) if (!cc %in% names(FL)) FL[, (cc) := NA]
L <- merge(L, FL[, .(area, item, flow, year, qf = flag_q, vf = flag_v, q0 = raw_q, v0 = raw_v)],
           by = c("area", "item", "flow", "year"), all.x = TRUE)
L <- merge(L, G, by = c("item", "flow", "year"), all.x = TRUE)
setorder(L, sid, year)

# item-level diagnosis of the items that drive an aggregate (shown as "its own data")
Bm <- L[year %in% CFG$base_years & q > 0 & v > 0, .(bq = mean(q), bv = mean(v)), by = sid]
L <- merge(L, Bm, by = "sid", all.x = TRUE)
setorder(L, sid, year)
L[, `:=`(xq = fifelse(bq > 0 & !is.na(q), q / bq * 100, NA_real_), xv = fifelse(bv > 0 & !is.na(v), v / bv * 100, NA_real_))]
Tn <- L[year %in% win, .(tq = med2(q), tv = med2(v)), by = sid]
Ta <- L[!(year %in% CFG$base_years), .(aq = med2(q), av = med2(v)), by = sid]
L  <- merge(merge(L, Tn, by = "sid", all.x = TRUE), Ta, by = "sid", all.x = TRUE)
L[is.na(tq), tq := aq]; L[is.na(tv), tv := av]; L[, c("aq", "av") := NULL]
setorder(L, sid, year)
L[, nq := nbr_med(q, sid)]; L[, nv := nbr_med(v, sid)]
L[, `:=`(pq = shift(q), pv = shift(v), pxq = shift(xq), pxv = shift(xv), pnq = shift(nq), pnv = shift(nv)), by = sid]
mk <- function(type, x, px, cur, base, typ, nbm, pcur, pnbm) {
  d <- L[, .(sid, year, type = type, x = get(x), px = get(px), cur = get(cur), base = get(base),
             typ = get(typ), nbm = get(nbm), pcur = get(pcur), pnbm = get(pnbm))]
  d <- d[!is.na(x) & x > 0]
  fl <- if (type == "Q") CFG$floor_q else CFG$floor_v
  d[, `:=`(tiny_cur = cur < fl, tiny_prev = !is.na(pcur) & pcur < fl, tiny_base = base < fl)]
  d
}
P <- rbind(mk("Q", "xq", "pxq", "q", "bq", "tq", "nq", "pq", "pnq"), mk("V", "xv", "pxv", "v", "bv", "tv", "nv", "pv", "pnv"))
P[, tiny_prev := tiny_prev & !(x > CFG$warn_hi | x < CFG$warn_lo)]
P <- classify(P)
CATS <- data.table(
  id = c("tiny_base", "low_base", "high_base", "spike", "dip", "tiny_flow", "trend"),
  label = c("Negligible base", "Base unusually low", "Base unusually high", "One-year spike", "One-year drop",
            "Negligible flow in year", "Sustained change"))

S <- unique(L[, .(sid, area, item, flow)], by = "sid")
S <- merge(S, BU[, .(area, item, flow, buv, src, own, nimp, ndir, gb, ou, u_min, u_max, n_u, qs, vs, rp, ro, disp)],
           by = c("area", "item", "flow"), all.x = TRUE)
SZ <- L[!is.na(v) & v > 0][order(sid, -year)][, .(size = mean(head(v, CFG$size_years))), by = sid]
S <- merge(S, SZ, by = "sid", all.x = TRUE)
setorder(S, sid)
# own base-year unit values (for the item view)
BYU <- dcast(L[year %in% CFG$base_years], sid ~ year, value.var = "uv")
setnames(BYU, as.character(CFG$base_years), paste0("u", CFG$base_years))
S <- merge(S, BYU, by = "sid", all.x = TRUE)
setorder(S, sid)
# how each base year was built (pipeline Step 4: own, rebuilt from an own year x group change, group's price)
HOW_ST <- c("own", "imputed", "direct", "group")
if (exists("imp_uv_how") && exists("exp_uv_how")) {
  HOW <- rbind(cbind(get("imp_uv_how"), flow = "Import"), cbind(get("exp_uv_how"), flow = "Export"))
  HOW[, `:=`(area = norm_a(geographicAreaM49), item = norm_i(measuredItemCPC))]
  HOW[, c("geographicAreaM49", "measuredItemCPC") := NULL]
  S <- merge(S, HOW, by = c("area", "item", "flow"), all.x = TRUE)
  setorder(S, sid)
  rm(HOW)
} else cat("Note: imp_uv_how / exp_uv_how not found - rerun trade_calculations.R to show how base unit values were built\n")
how_payload <- function() {
  if (!"st14" %in% names(S)) return(NULL)
  hg <- sort(unique(na.omit(c(S$grp14, S$grp15, S$grp16))))
  o <- list(grp = hg)
  for (k in 0:2) {
    y <- c("14", "15", "16")[k + 1]
    o[[paste0("s", k)]] <- match(S[[paste0("st", y)]], HOW_ST) - 1L
    o[[paste0("u", k)]] <- sg6(S[[paste0("u", y)]])
    o[[paste0("a", k)]] <- S[[paste0("anc", y)]]
    o[[paste0("g", k)]] <- match(S[[paste0("grp", y)]], hg) - 1L
    o[[paste0("m", k)]] <- sg6(S[[paste0("gm", y)]])
    o[[paste0("n", k)]] <- sg6(S[[paste0("ga", y)]])
    o[[paste0("h", k)]] <- signif(S[[paste0("sh", y)]], 4)
  }
  o
}

# names
at <- get("agg_table")
item_nm <- unique(at[var_type == "item", .(item = norm_i(member_code), item_name = member_name)], by = "item")
if (exists("item_list")) item_nm <- unique(rbind(get("item_list")[, .(item = Item_CPC, item_name = Item_Name)], item_nm), by = "item")
if (file.exists("ItemCodes.csv")) {
  ic <- fread("ItemCodes.csv", colClasses = "character", encoding = "UTF-8")
  item_nm <- unique(rbind(item_nm, ic[, .(item = norm_i(`CPC Code`), item_name = Item)]), by = "item")
}
area_nm <- unique(scope[, .(area = norm_a(area), area_name)], by = "area")
if (exists("area_names")) area_nm <- unique(rbind(area_nm, get("area_names")[, .(area = norm_a(Area_Code), area_name = Area_Name)]), by = "area")
S <- merge(S, area_nm, by = "area", all.x = TRUE)
S <- merge(S, item_nm, by = "item", all.x = TRUE)
S[is.na(area_name), area_name := area]; S[is.na(item_name), item_name := item]
setorder(S, sid)

# sub-cause of the dominant item at that point
ci <- merge(PA[acat == "a_one_item", .(gsid, year, type, top)], SER[, .(gsid, area, flow)], by = "gsid")
ci[, area := norm_a(area)]
ci <- merge(ci, S[, .(sid, area, item, flow)], by.x = c("area", "top", "flow"), by.y = c("area", "item", "flow"), all.x = TRUE)
ci <- merge(ci, P[, .(sid, year, type, icat = cat)], by = c("sid", "year", "type"), all.x = TRUE)
PA <- merge(PA, ci[, .(gsid, year, type, sub_item = icat)], by = c("gsid", "year", "type"), all.x = TRUE)
cc <- merge(PA[acat == "a_one_country", .(gsid, year, type, top)], SER[, .(gsid, grp, flow)], by = "gsid")
cc <- merge(cc, SER[lvl == "Country", .(mgsid = gsid, top = area, grp, flow)], by = c("top", "grp", "flow"), all.x = TRUE)
cc <- merge(cc, PA[, .(mgsid = gsid, year, type, ccat = acat)], by = c("mgsid", "year", "type"), all.x = TRUE)
PA <- merge(PA, cc[, .(gsid, year, type, sub_cty = ccat)], by = c("gsid", "year", "type"), all.x = TRUE)
PA[, sub_i := match(sub_item, CATS$id) - 1L]
PA[, sub_c := match(sub_cty, ACATS$id) - 1L]
cat("\nIndices — flagged aggregate values by cause:\n")
print(PA[, .(values = .N, critical = sum(sev == 2), series = uniqueN(gsid)), by = acat][order(-critical)])

# -----------------------------------------------------------------------------
# 6. Issue tables
# -----------------------------------------------------------------------------
GS <- unique(A[, .(gsid, lvl, area, grp, flow, BV, BQ, size, tV, tQ)], by = "gsid")
GS <- merge(GS, unique(scope[, .(lvl, area, area_name)], by = c("lvl", "area")), by = c("lvl", "area"), all.x = TRUE)
GS <- merge(GS, unique(scope[, .(grp, grp_name)], by = "grp"), by = "grp", all.x = TRUE)
setorder(GS, gsid)
PA[, mag := abs(log10(x / 100))]
AI <- PA[order(-sev, -mag), .(sev = max(sev), n = .N, n_crit = sum(sev == 2),
         types = paste(intersect(c("V", "Q"), unique(type)), collapse = " "),
         y0 = min(year), y1 = max(year), worst_type = type[1], worst_year = year[1], worst_x = x[1],
         years = paste(sort(unique(year)), collapse = " ")), by = .(gsid, acat, acat_i)]
AI <- merge(AI, GS[, .(gsid, lvl, area, area_name, grp, grp_name, flow, size)], by = "gsid")
AI[, tier := as.numeric(match(ACATS$driver[acat_i + 1L], c("Base", "Coverage", "Member", "Year", "Level")))]
AI[acat == "a_tiny_flow", tier := 4.5]
setorder(AI, -sev, tier, -size, gsid)
AI[, rank := .I]
REP <- unique(PA[order(-sev, -mag)], by = c("gsid", "acat"))[, .(gsid, acat, year, type)]
AI <- merge(AI, REP, by = c("gsid", "acat"))
setorder(AI, rank)
TOPI <- merge(REP, TOP, by = c("gsid", "year", "type"))
TOPI <- merge(TOPI, AI[, .(gsid, acat, rank)], by = c("gsid", "acat"))
setorder(TOPI, rank, rk)
fwrite(AI[, .(Rank = rank, Severity = fifelse(sev == 2, "Critical", "Warning"), Category = ACATS$label[acat_i + 1L],
              Level = lvl, `Area Code (M49)` = area, Area = area_name, `Item Group Code` = grp, `Item Group` = grp_name,
              Flow = flow, `Indices affected` = types, `Flagged values` = n, `Critical values` = n_crit,
              `First year` = y0, `Last year` = y1, Years = years, `Most extreme index` = paste(worst_type, worst_year),
              `Most extreme value` = signif(worst_x, 6), `Trade size (1000 USD)` = round(size, 1))],
       "results/checks/diagnostics_aggregates.csv")
cat("Saved: results/checks/diagnostics_aggregates.csv (", nrow(AI), "issues )\n")

JCATS <- data.table(
  id = c("j_rv_only", "j_both", "j_shared", "j_tiny"),
  label = c("Only real value jumped", "Value jumped too", "Other countries moved too", "From or to a negligible level"),
  short = c("Real value only", "Value too", "Shared", "Negligible"))
J[, jcat_i := match(jcat, JCATS$id) - 1L]
J[, tier := jcat_i]
setorder(J, -sev, tier, -size, gsid, year)
J[, rank := .I]
J <- merge(J, GS[, .(gsid, area_name, grp_name)], by = "gsid")
setorder(J, rank)
mem_nm <- function(lvl, code) fifelse(lvl == "Country", item_nm$item_name[match(code, item_nm$item)],
                                      area_nm$area_name[match(norm_a(code), area_nm$area)])
J[, top_name := mem_nm(lvl, top_member)]
fwrite(J[, .(Rank = rank, Severity = fifelse(sev == 2, "Critical", "Warning"), Category = JCATS$label[jcat_i + 1L],
             Level = lvl, `Area Code (M49)` = area, Area = area_name, `Item Group Code` = grp, `Item Group` = grp_name,
             Flow = flow, Year = year, `Real value, previous year (1000 USD)` = signif(pR, 6),
             `Real value (1000 USD)` = signif(R, 6), `Real value ratio` = signif(rr, 4), `Value ratio` = signif(vr, 4),
             `Quantity ratio` = signif(qr, 4), `Countries compared` = np,
             `Median ratio of the other countries` = signif(pmed, 4),
             `Share of the other countries that also jumped` = round(psame, 3),
             `All countries compared together, ratio` = signif(tr, 4),
             `Largest contributor` = top_name, `Its share of the change` = round(top_share, 3),
             `Trade size (1000 USD)` = round(size, 1))],
       "results/checks/diagnostics_real_value_jumps.csv")
cat("Saved: results/checks/diagnostics_real_value_jumps.csv (", nrow(J), "jumps )\n")

BCATS <- data.table(
  id = c("b_borrowed", "b_tiny", "b_disagree", "b_rebuilt", "b_own_off", "b_consistent", "b_own_only"),
  label = c("Taken from a group", "Negligible base-year record", "Base years disagree", "Rebuilt base year off",
            "Base period out of line", "Country always differs", "Off the country's own other years"))
BI <- merge(BI, area_nm, by = "area", all.x = TRUE)
BI <- merge(BI, item_nm, by = "item", all.x = TRUE)
BI[, bcat_i := match(bcat, BCATS$id) - 1L]
setorder(BI, -bsev, -aeff, tier)
BI[, rank := .I]
fwrite(BI[, .(Rank = rank, Severity = fifelse(bsev == 2, "Critical", "Warning"), Category = BCATS$label[bcat_i + 1L],
              `Area Code (M49)` = area, Area = area_name, `Item Code (CPC)` = item, Item = item_name, Flow = flow,
              `Base UV (1000 USD/t)` = signif(buv, 6), `Base UV source` = src,
              `Other countries' base UV (1000 USD/t)` = signif(gb, 6), `Base UV / other countries` = signif(rp, 4),
              `Own other years' UV (1000 USD/t)` = signif(ou, 6), `Base UV / own other years` = signif(ro, 4),
              `Base-year UVs, highest / lowest` = signif(disp, 4),
              `Quantity, latest years (t)` = signif(qs, 6),
              `Real value effect (1000 USD / year)` = signif(eff, 6))],
       "results/checks/diagnostics_base_unit_values.csv")
cat("Saved: results/checks/diagnostics_base_unit_values.csv (", nrow(BI), "series )\n")

# -----------------------------------------------------------------------------
# 7. Dissemination groups: item groups and area levels
# -----------------------------------------------------------------------------
reg_type <- function(code) {
  c0 <- norm_a(code)
  fcase(grepl("\\.", code), "Excluding intra-trade",
        c0 == "1", "World",
        c0 %in% c("2", "19", "142", "150", "9"), "Continents",
        c0 %in% c("97", "199", "432", "722", "901", "902"), "Special groupings",
        default = "Sub-regions")
}
GS[, ltype := fifelse(lvl == "Country", "Countries", reg_type(area))]
SF <- GS[, .(gsid, lvl, ltype, area, grp, flow, size)]
SF[, crit := gsid %in% AI[sev == 2]$gsid]
SF[, anyi := gsid %in% AI$gsid]
SF[, jump := gsid %in% J[jcat != "j_tiny"]$gsid]
SF[, jcrit := gsid %in% J[jcat != "j_tiny" & sev == 2]$gsid]
# real value coverage of each group's value, latest years (country members, scope countries)
lastY <- max(yrs) - CFG$size_years + 1L
cty_scope <- unique(scope[lvl == "Country"]$area)
CV <- TIM[area %in% cty_scope & year >= lastY]
BUs <- BUV[, .(area, item, flow, own_src = src %in% "country")]
CV <- rbind(CV[, .(area, grp, member, year, flow = "Import", v = imp_v, rv = real_v_imp)],
            CV[, .(area, grp, member, year, flow = "Export", v = exp_v, rv = real_v_exp)])[!is.na(v) | !is.na(rv)]
CV <- merge(CV, BUs, by.x = c("area", "member", "flow"), by.y = c("area", "item", "flow"), all.x = TRUE)
CVG <- CV[, .(v_tot = sum(v, na.rm = TRUE), v_rv = sum(v[!is.na(rv)], na.rm = TRUE),
              rv_tot = sum(rv, na.rm = TRUE), rv_borrowed = sum(rv[!(own_src %in% TRUE)], na.rm = TRUE)), by = grp]
rm(CV)
nitems <- get("item_map")[, .(n_items = uniqueN(measuredItemCPC)), by = .(grp = item_group_code)]
nitems_d <- unique(TIM[, .(grp, member)])[, .(n_items_data = .N), by = grp]
PUBG <- SF[lvl == "Country", .(n_series = sum(size > 0), n_countries = uniqueN(area[size > 0]),
                               world_value = sum(size), median_size = as.numeric(median(size[size > 0])),
                               n_small = sum(size > 0 & size < CFG$small),
                               p_crit = mean(crit[size > 0]), p_any = mean(anyi[size > 0]), p_jump = mean(jump[size > 0])),
           by = grp]
PUBR <- SF[lvl == "Region", .(r_series = sum(size > 0), r_crit = mean(crit[size > 0]), r_jump = mean(jump[size > 0])), by = grp]
PUBG <- merge(PUBG, PUBR, by = "grp", all.x = TRUE)
PUBG <- merge(PUBG, nitems, by = "grp", all.x = TRUE)
PUBG <- merge(PUBG, nitems_d, by = "grp", all.x = TRUE)
PUBG <- merge(PUBG, CVG, by = "grp", all.x = TRUE)
PUBG[, rv_cover := fifelse(v_tot > 0, v_rv / v_tot, NA_real_)]
PUBG[, rv_borrow := fifelse(rv_tot > 0, rv_borrowed / rv_tot, NA_real_)]
top_tot <- PUBG[grp == "F1882"]$world_value
if (!length(top_tot) || is.na(top_tot)) top_tot <- max(PUBG$world_value)
PUBG[, world_share := world_value / top_tot]
PUBG <- merge(PUBG, unique(scope[, .(grp, grp_name)], by = "grp"), by = "grp", all.x = TRUE)
# groups with members but no data at all (e.g. live animals, excluded in Step 1)
nod <- setdiff(unique(get("item_map")$item_group_code), PUBG$grp)
if (length(nod)) {
  gn <- unique(get("item_map")[item_group_code %in% nod, .(grp = item_group_code, grp_name = item_group_name)], by = "grp")
  PUBG <- rbind(PUBG, merge(gn, nitems, by = "grp", all.x = TRUE), fill = TRUE)
}
PUBG[is.na(world_value), `:=`(world_value = 0, world_share = 0, n_series = 0L, n_countries = 0L)]
# current dissemination flag of each item group (DisseminationList.csv), used to pre-tick the dashboard
PUBG[, diss := NA]
if (file.exists("DisseminationList.csv")) {
  dl <- fread("DisseminationList.csv", colClasses = "character", encoding = "UTF-8")
  dl[, code := gsub("'", "", code)]
  PUBG[, diss := as.logical(toupper(dl$dissemination[match(grp, dl$code)]))]
}
setorder(PUBG, -world_value)
PUBG[, lvl := grp_levels(grp_name)]
fwrite(PUBG[, .(`Item Group Code` = grp, `Item Group` = grp_name, `FAOSTAT level` = lvl, `In DisseminationList.csv` = diss, `Items in group` = n_items, `Items with data` = n_items_data,
                `Trade size, imports + exports, all countries (1000 USD / year)` = round(world_value), `Share of Agricultural Products` = round(world_share, 5),
                `Countries with trade` = n_countries, `Country series` = n_series,
                `Small country series (< 100 thousand USD / year)` = n_small, `Median country series size` = round(median_size, 1),
                `Country series with a critical index issue` = round(p_crit, 3), `Country series with any index issue` = round(p_any, 3),
                `Country series with a real value jump` = round(p_jump, 3), `Region series` = r_series,
                `Region series with a critical index issue` = round(r_crit, 3), `Region series with a real value jump` = round(r_jump, 3),
                `Value covered by real value` = round(rv_cover, 3), `Real value on a borrowed base UV` = round(rv_borrow, 3))],
       "results/checks/diagnostics_dissemination_groups.csv")
cat("Saved: results/checks/diagnostics_dissemination_groups.csv (", nrow(PUBG), "item groups )\n")

# item groups excluded from the calculations (excluded_groups in trade_calculations.R):
# no aggregates exist for them, so their size is measured on the item data directly
# (same definition as above: latest five years with data, country members in scope)
PUBX <- NULL
if (exists("excluded_groups")) PUBX <- tryCatch({
  xg <- unique(as.data.table(get("excluded_groups"))[, .(grp = norm_i(code), note = as.character(note))], by = "grp")
  at_x <- get("agg_table")[var_type == "item", .(grp = norm_i(group_code), grp_name = group_name, item = norm_i(member_code))]
  xm <- unique(at_x[grp %in% xg$grp, .(grp, item)])
  xn <- unique(at_x[grp %in% xg$grp, .(grp, grp_name)], by = "grp")
  tc <- get("trade_calc")[, .(area = as.character(geographicAreaM49), item = norm_i(measuredItemCPC),
                              year = as.integer(timePointYears), imp_v, exp_v)]
  tc <- tc[item %in% xm$item]
  if (length(cty_scope)) tc <- tc[area %in% cty_scope]
  xt <- merge(tc, xm, by = "item", allow.cartesian = TRUE)
  rm(tc)
  xt <- rbind(xt[, .(grp, area, item, year, flow = "Import", v = imp_v)],
              xt[, .(grp, area, item, year, flow = "Export", v = exp_v)])[!is.na(v)]
  xd <- xt[v > 0, .(n_items_data = uniqueN(item)), by = grp]
  xs <- xt[, .(v = sum(v)), by = .(grp, area, flow, year)][v > 0][order(grp, area, flow, -year)][
    , .(size = mean(head(v, CFG$size_years))), by = .(grp, area, flow)]
  xs <- xs[, .(world_value = sum(size), n_countries = uniqueN(area)), by = grp]
  X <- merge(xg, xn, by = "grp", all.x = TRUE)
  X[is.na(grp_name), grp_name := grp]
  X <- merge(X, xm[, .(n_items = .N), by = grp], by = "grp", all.x = TRUE)
  X <- merge(X, xd, by = "grp", all.x = TRUE)
  X <- merge(X, xs, by = "grp", all.x = TRUE)
  X[is.na(n_items), n_items := 0L]; X[is.na(n_items_data), n_items_data := 0L]
  X[is.na(world_value), `:=`(world_value = 0, n_countries = 0L)]
  X[, world_share := world_value / top_tot]
  setorder(X, -world_value)
  fwrite(X[, .(`Item Group Code` = grp, `Item Group` = grp_name, `FAOSTAT level` = grp_levels(grp_name), Note = note,
               `Items in group` = n_items, `Items with data` = n_items_data,
               `Trade size, imports + exports, all countries (1000 USD / year)` = round(world_value),
               `Share of Agricultural Products` = round(world_share, 5), `Countries with trade` = n_countries)],
         "results/checks/diagnostics_excluded_groups.csv")
  cat("Saved: results/checks/diagnostics_excluded_groups.csv (", nrow(X), "item groups excluded from the calculations )\n")
  X
}, error = function(e) { cat("Note: excluded item groups section skipped:", conditionMessage(e), "\n"); NULL })
PUBL <- SF[, .(n_areas = uniqueN(area[size > 0]), n_series = sum(size > 0),
               median_size = as.numeric(median(size[size > 0])), p_small = mean(size[size > 0] < CFG$small),
               p_crit = mean(crit[size > 0]), p_any = mean(anyi[size > 0]), p_jump = mean(jump[size > 0])), by = ltype]
PUBL <- PUBL[order(match(ltype, c("Countries", "World", "Continents", "Sub-regions", "Special groupings", "Excluding intra-trade")))]
print(PUBL)

# -----------------------------------------------------------------------------
# 8. Payloads
# -----------------------------------------------------------------------------
flags <- sort(unique(c(L$qf, L$vf))); flags <- flags[!is.na(flags)]
areas <- unique(S[, .(area, area_name)])[order(area_name)]
items <- unique(S[, .(item, item_name)])[order(item_name)]
srcs  <- sort(unique(S$src[!is.na(S$src)]))
setorder(L, sid, year)
ny <- length(yrs)
stopifnot(nrow(L) == nrow(S) * ny, all(L$year == rep(yrs, nrow(S))))
# parent group's unit value every year, for the item view: pooled sum(V) / sum(Q) of the OTHER member
# countries (leave-one-out, as in Step 4). Group = the one used to build the base unit value
# (2015's first), otherwise the country's smallest regional group (tier 1).
GU_NAMES <- character(0); L[, GU := NA_real_]; S[, gu_i := NA_integer_]
if (exists("country_region_map")) {
  crm <- unique(get("country_region_map")[, .(area = norm_a(member_code), gcode = norm_a(group_code), gname = group_name, tier)])
  gcol <- function(y) if (paste0("grp", y) %in% names(S)) S[[paste0("grp", y)]] else rep(NA_character_, nrow(S))
  SG <- S[, .(sid, area, item, flow)][, used := fcoalesce(gcol("15"), gcol("14"), gcol("16"))]
  SG <- merge(SG, crm[tier == 1, .(area, g1 = gcode, n1 = gname)], by = "area", all.x = TRUE)
  SG <- merge(SG, unique(crm[, .(area, used = gname, gu = gcode)], by = c("area", "used")), by = c("area", "used"), all.x = TRUE)
  SG[, gcode := fifelse(used %in% "All countries", "ALL", fcoalesce(gu, g1))]
  SG[, gname := fifelse(gcode %in% "ALL", "All countries", fifelse(!is.na(gu), used, n1))]
  TV <- TL[!is.na(uv) & item %in% unique(SG$item), .(area, item, flow, year, q, v)]
  MEM <- unique(crm[gcode %in% SG$gcode, .(gcode, area)])
  need <- unique(SG[!is.na(gcode), .(gcode, item, flow)])
  GT <- TV[MEM, on = .(area), allow.cartesian = TRUE, nomatch = 0][need, on = .(gcode, item, flow), nomatch = 0][
          , .(V = sum(v), Q = sum(q), N = .N), by = .(gcode, item, flow, year)]
  if (any(SG$gcode %in% "ALL"))
    GT <- rbind(GT, TV[item %in% SG[gcode %in% "ALL"]$item, .(V = sum(v), Q = sum(q), N = .N), by = .(item, flow, year)][, gcode := "ALL"], use.names = TRUE)
  L[SG, gcode := i.gcode, on = .(sid)]
  L[GT, `:=`(gV = i.V, gQ = i.Q, gN = i.N), on = .(gcode, item, flow, year)]
  L[, GU := fifelse(is.na(gN), NA_real_,
              fifelse(!is.na(uv), fifelse(gN > 1 & (gQ - q) > 0, (gV - v) / (gQ - q), NA_real_), gV / gQ))]
  L[, c("gcode", "gV", "gQ", "gN") := NULL]
  setorder(L, sid, year)
  GU_NAMES <- sort(unique(na.omit(SG$gname)))
  S[SG, gu_i := match(i.gname, GU_NAMES) - 1L, on = .(sid)]
  rm(crm, SG, TV, MEM, need, GT)
} else cat("Note: country_region_map not found - group unit values not shown in the item view\n")
BI <- merge(BI, S[, .(area, item, flow, sid)], by = c("area", "item", "flow"))
setorder(BI, rank)
payload <- list(
  years = yrs, base_years = CFG$base_years, cfg = CFG, cats = CATS, bcats = BCATS,
  areas = list(code = areas$area, name = areas$area_name),
  items = list(code = items$item, name = items$item_name),
  flags = flags, srcs = srcs,
  series = list(
    a = match(S$area, areas$area) - 1L, i = match(S$item, items$item) - 1L, f = as.integer(S$flow == "Export"),
    buv = sg6(S$buv), src = match(S$src, srcs) - 1L, own = S$own, nimp = S$nimp, ndir = S$ndir,
    gb = sg6(S$gb), ou = sg6(S$ou), u0 = sg6(S$u2014), u1 = sg6(S$u2015), u2 = sg6(S$u2016),
    qs = sg6(S$qs), size = signif(S$size, 4)),
  q = sg6(L$q0), v = sg6(L$v0), qf = match(L$qf, flags) - 1L, vf = match(L$vf, flags) - 1L, G = signif(L$G, 4),
  GU = signif(L$GU, 4), gu_names = GU_NAMES, gu_i = S$gu_i,
  bissues = list(s = BI$sid - 1L, c = BI$bcat_i, sev = BI$bsev, rp = signif(BI$rp, 4), ro = signif(BI$ro, 4),
                 d = signif(BI$disp, 4), eff = signif(BI$eff, 4)),
  bsrc = BSRC, bdist = BDIST, how = how_payload(),
  meta = list(generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
              outputs_date = format(file.mtime("results/real_values_aggregates.csv"), "%Y-%m-%d %H:%M"),
              n_buv = nrow(BU), n_items_series = nrow(S))
)

# aggregates
setorder(GS, gsid); setorder(A, gsid, year)
stopifnot(nrow(A) == nrow(GS) * ny, all(A$year == rep(yrs, nrow(GS))))
areasA <- unique(GS[, .(lvl, area, area_name, ltype)])[order(lvl, area_name)]
groups <- unique(GS[, .(grp, grp_name)])[order(grp_name)]
PA[, nbm_out := fifelse(odd_prev, pnbm, nbm)]
PA <- merge(PA, AI[, .(gsid, year, type, is_rep = TRUE)], by = c("gsid", "year", "type"), all.x = TRUE)
PA[is.na(is_rep), `:=`(cB = 0, cO = 0, cP = 0, cS = 0)]
setorder(PA, gsid, type, year)
TOPI[, mname := mem_nm(SER$lvl[match(gsid, SER$gsid)], member)]
TOPI[is.na(mname), mname := member]
TOPI <- merge(TOPI, SER[, .(gsid, lvl, area, grp, flow)], by = "gsid")
TOPI[, marea := norm_a(area)]
TOPI <- merge(TOPI, SER[lvl == "Country", .(m_gsid = gsid, member = area, grp, flow)], by = c("member", "grp", "flow"), all.x = TRUE)
TOPI[lvl == "Country", m_gsid := NA]
TOPI <- merge(TOPI, S[, .(m_sid = sid, marea = area, member = item, flow)], by = c("marea", "member", "flow"), all.x = TRUE)
TOPI[lvl == "Region", m_sid := NA]
setorder(TOPI, rank, rk)
# jump members and peers
JM <- merge(JM, J[, .(k, rank, lvl)], by = "k")
JM[, mname := mem_nm(lvl, member)]
JM[is.na(mname), mname := member]
JM[, marea := norm_a(area)]
JM <- merge(JM, BU[, .(marea = area, member = item, flow, mbuv = buv, msrc = src, mrp = rp)],
            by = c("marea", "member", "flow"), all.x = TRUE)
JM[lvl == "Region", `:=`(mbuv = NA_real_, msrc = NA_character_, mrp = NA_real_)]
JM <- merge(JM, S[, .(m_sid = sid, marea = area, member = item, flow)], by = c("marea", "member", "flow"), all.x = TRUE)
JM[lvl == "Region", m_sid := NA]
JM <- merge(JM, J[, .(k, grp)], by = "k")
JM <- merge(JM, SER[lvl == "Country", .(m_gsid = gsid, member = area, grp, flow)], by = c("member", "grp", "flow"), all.x = TRUE)
JM[lvl == "Country", m_gsid := NA]
setorder(JM, rank, rk)
PP <- merge(PP, J[, .(k, rank)], by = "k")
PP[, pa := match(paste("Country", PP$area), paste(areasA$lvl, areasA$area)) - 1L]
setorder(PP, rank, -pR)
mnames <- unique(c(TOPI$mname, JM$mname))
srcsA <- sort(unique(JM$msrc[!is.na(JM$msrc)]))
setorder(J, rank)
payload_agg <- list(
  years = yrs, base_years = CFG$base_years, cfg = CFG, mnames = mnames, srcs = srcsA,
  cats = ACATS, jcats = JCATS, verify = AVER, built = format(Sys.time(), "%Y-%m-%d %H:%M"),
  areas = list(lvl = areasA$lvl, code = areasA$area, name = areasA$area_name, type = areasA$ltype),
  groups = list(code = groups$grp, name = groups$grp_name),
  series = list(lv = as.integer(GS$lvl == "Region"), a = match(paste(GS$lvl, GS$area), paste(areasA$lvl, areasA$area)) - 1L,
                g = match(GS$grp, groups$grp) - 1L, f = as.integer(GS$flow == "Export"),
                bv = sg6(GS$BV), bq = sg6(GS$BQ), size = signif(GS$size, 4), tv = sg6(GS$tV), tq = sg6(GS$tQ)),
  V = sg6(A$V), Q = sg6(A$Q), R = sg6(A$R),
  cells = list(s = PA$gsid - 1L, t = match(PA$type, c("V", "Q")) - 1L, y = PA$year, sev = PA$sev, c = PA$acat_i,
               cb = round(PA$cB, 2), co = round(PA$cO, 2), cp = round(PA$cP, 2), cs = round(PA$cS, 2),
               nbm = signif(PA$nbm_out, 4), op = as.integer(PA$odd_prev),
               sh = round(PA$top_share, 3), sn = round(PA$share_new, 3), sl = round(PA$share_lost, 3),
               nn = PA$n_new, nl = PA$n_lost, si = PA$sub_i, sc = PA$sub_c),
  issues = list(s = AI$gsid - 1L, c = AI$acat_i, sev = AI$sev, n = AI$n, nc = AI$n_crit, ty = AI$types,
                y0 = AI$y0, y1 = AI$y1, wt = match(AI$worst_type, c("V", "Q")) - 1L, wy = AI$worst_year,
                wx = signif(AI$worst_x, 5), ry = AI$year, rt = match(AI$type, c("V", "Q")) - 1L),
  top = list(r = TOPI$rank - 1L, m = match(TOPI$mname, mnames) - 1L, b = sg6(TOPI$b), a = sg6(TOPI$a), d = sg6(TOPI$d),
             ms = TOPI$m_sid - 1L, mg = TOPI$m_gsid - 1L),
  jumps = list(s = J$gsid - 1L, y = J$year, c = J$jcat_i, sev = J$sev, r = signif(J$rr, 4), vr = signif(J$vr, 4),
               qr = signif(J$qr, 4), np = J$np, pm = signif(J$pmed, 4), ps = round(J$psame, 3), tr = signif(J$tr, 4),
               b0 = J$b0, b1 = J$b1, b2 = J$b2, b3 = J$b3, b4 = J$b4, sh = round(J$top_share, 3)),
  jtop = list(r = JM$rank - 1L, m = match(JM$mname, mnames) - 1L, pr = sg6(JM$pmR), cr = sg6(JM$mR),
              pq = sg6(JM$pmQ), cq = sg6(JM$mQ), pv = sg6(JM$pmV), cv = sg6(JM$mV), sh = round(JM$share, 3),
              buv = sg6(JM$mbuv), src = match(JM$msrc, srcsA) - 1L, rp = signif(JM$mrp, 3),
              ms = JM$m_sid - 1L, mg = JM$m_gsid - 1L),
  jpeer = list(r = PP$rank - 1L, a = PP$pa, pr = sg6(PP$pR), cr = sg6(PP$R)),
  pub = PUBG[, .(grp, grp_name, diss = as.integer(diss), n_items, n_items_data, world_value = round(world_value), world_share = signif(world_share, 4),
                 n_countries, n_series, n_small, median_size = signif(median_size, 4), p_crit = round(p_crit, 4),
                 p_any = round(p_any, 4), p_jump = round(p_jump, 4), r_series, r_crit = round(r_crit, 4),
                 r_jump = round(r_jump, 4), rv_cover = round(rv_cover, 4), rv_borrow = round(rv_borrow, 4))],
  pubx = if (is.null(PUBX)) list(grp = character(0)) else
    PUBX[, .(grp, grp_name, note, n_items, n_items_data, world_value = round(world_value),
             world_share = signif(world_share, 4), n_countries)],
  publ = PUBL[, .(ltype, n_areas, n_series, median_size = signif(median_size, 4), p_small = round(p_small, 4),
                  p_crit = round(p_crit, 4), p_any = round(p_any, 4), p_jump = round(p_jump, 4))],
  sflags = list(crit = as.integer(SF$crit), any = as.integer(SF$anyi), jump = as.integer(SF$jump), jcrit = as.integer(SF$jcrit))
)
saveRDS(list(payload = payload, payload_agg = payload_agg), CACHE)
write_html(payload, payload_agg)
}

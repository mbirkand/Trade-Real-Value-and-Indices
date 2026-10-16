# =============================================================================
# Trade Real Values & Indices — diagnostics dashboard
#
# Runs automatically as Step 8 of trade_calculations.R. It can also be run on
# its own from the project folder:   source("diagnostics/build_dashboard.R")
#
# Reads   TradeData.rds, results/indices_items.csv,
#         results/checks/base_unit_values.csv, results/real_values_aggregates.csv
#         + the pipeline's objects in memory when run as Step 8 (needed for the
#           Aggregates section; a standalone run reuses the last cached result)
# Writes  results/diagnostics_dashboard.html
#         results/checks/diagnostics_item_indices.csv
#         results/checks/diagnostics_aggregates.csv
#         results/checks/diagnostics_aggregates_cache.rds
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
  crit_hi    = 5000,   # critical: index above this ...
  crit_lo    = 1,      # ... or below this
  warn_hi    = 1000,   # warning: index above this ...
  warn_lo    = 10,     # ... or below this
  yoy        = 10,     # warning: year-on-year change of more than 10x either way
  floor_q    = 10,     # negligible quantity (t)
  floor_v    = 10,     # negligible value (1000 USD)
  window     = 3,      # +/- years used for "surrounding years"
  size_years = 5,      # trade size = mean value of the latest N years with data
  dominant   = 0.5,    # one member explains at least this share of the change
  coverage   = 0.5     # members absent in base / in the year explain at least this share
)

script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) "diagnostics")
template   <- file.path(script_dir, "dashboard_template.html")
if (!file.exists(template)) template <- "diagnostics/dashboard_template.html"
stopifnot(file.exists(template), file.exists("TradeData.rds"),
          file.exists("results/indices_items.csv"), file.exists("results/checks/base_unit_values.csv"))

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
# pcur, pnbm, is_uv, tiny_cur, tiny_prev, tiny_base. Adds sev (dropping 0), comps, cat.
classify <- function(P) {
  P[, yr   := fifelse(!is.na(px) & px > 0, x / px, NA_real_)]
  P[, lvl  := x > CFG$warn_hi | x < CFG$warn_lo]
  P[, jump := !is.na(yr) & (yr > CFG$yoy | yr < 1 / CFG$yoy)]
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
    comp == "B" & is_uv, "base_uv_off",
    comp == "B" & sg > 0 & tiny_base, "tiny_base",
    comp == "B" & sg > 0, "low_base",
    comp == "B" & sg < 0, "high_base",
    tiny_cur | tiny_prev, "tiny_flow",
    comp == "O" & is_uv, "qv_mismatch",
    comp == "O" & !odd_prev & sg > 0, "spike",
    comp == "O" & !odd_prev & sg < 0, "dip",
    comp == "O" & odd_prev & sg > 0, "dip",
    comp == "O" & odd_prev & sg < 0, "spike",
    default = "trend")]
  P
}

# -----------------------------------------------------------------------------
# 1. Inputs
# -----------------------------------------------------------------------------
idx <- fread("results/indices_items.csv", encoding = "UTF-8",
             colClasses = list(character = c("Area Code (M49)", "Item Code (CPC)")))
setnames(idx, 1:11, c("area", "area_name", "item", "item_name", "year", "iv", "iq", "iu", "ev", "eq", "eu"))
item_keys <- unique(idx[, .(area, item)])

PIPE <- all(vapply(c("trade_calc", "item_map", "area_agg_map", "country_item_grp", "area_item_grp",
                     "imp_uv_base", "exp_uv_base"), exists, logical(1))) &&
        file.exists("results/real_values_aggregates.csv")
cat(if (PIPE) "Pipeline objects found: item indices + aggregates.\n"
    else "Pipeline objects not in memory: item indices only (aggregates from the last cache, if any).\n")

CACHE <- "results/checks/diagnostics_aggregates_cache.rds"
agg_cache <- if (!PIPE && file.exists(CACHE)) readRDS(CACHE) else NULL

# base unit values: all items when the pipeline is in memory, ItemList items otherwise
if (PIPE) {
  BUV <- rbind(
    get("imp_uv_base")[, .(area = norm_a(geographicAreaM49), item = norm_i(measuredItemCPC), flow = "Import",
                           buv = base_uv_imp, src = uv_source_imp, own = base_imp_yrs_valid,
                           nimp = n_imp_imputed, ndir = n_imp_direct)],
    get("exp_uv_base")[, .(area = norm_a(geographicAreaM49), item = norm_i(measuredItemCPC), flow = "Export",
                           buv = base_uv_exp, src = uv_source_exp, own = base_exp_yrs_valid,
                           nimp = n_exp_imputed, ndir = n_exp_direct)])
} else {
  b0 <- fread("results/checks/base_unit_values.csv", encoding = "UTF-8", colClasses = list(character = c("Area_M49", "Item_CPC")))
  BUV <- b0[, .(area = Area_M49, item = Item_CPC, flow = Flow, buv = Base_UV, src = UV_Source,
                own = Base_Yrs_Own, nimp = N_Imputed, ndir = N_Direct)]
}

# cross-country unit value references (median over countries, >= 5 non-negligible records)
if (PIPE) {
  tcx <- get("trade_calc")[, .(area = norm_a(geographicAreaM49), item = norm_i(measuredItemCPC),
                               year = as.integer(timePointYears), imp_q, imp_v, exp_q, exp_v)]
  RU <- rbind(tcx[, .(item, year, flow = "Import", q = imp_q, v = imp_v)],
              tcx[, .(item, year, flow = "Export", q = exp_q, v = exp_v)])
  rm(tcx)
  G  <- RU[!is.na(q) & !is.na(v) & q >= CFG$floor_q & v >= CFG$floor_v,
           .(G = median(v / q), nG = .N), by = .(item, flow, year)][nG >= 5, .(item, flow, year, G)]
  rm(RU)
  GB <- BUV[!is.na(buv), .(gb = if (sum(src == "country", na.rm = TRUE) >= 5) median(buv[src %in% "country"])
                                else if (.N >= 5) median(buv) else NA_real_), by = .(item, flow)]
}

# -----------------------------------------------------------------------------
# 2. Aggregates (needs the pipeline in memory)
# -----------------------------------------------------------------------------
AGG <- NULL
if (PIPE) {
  cat("Aggregates: building series ...\n")
  scope <- unique(fread("results/real_values_aggregates.csv", encoding = "UTF-8",
                        select = c("Level", "Area Code (M49)", "Area", "Item Group Code", "Item Group"),
                        colClasses = "character"))
  setnames(scope, c("lvl", "area", "area_name", "grp", "grp_name"))
  scope_s <- unique(scope[, .(lvl, area, grp)])
  cig <- get("country_item_grp"); aig <- get("area_item_grp")
  wide <- rbind(
    cig[, .(lvl = "Country", area = as.character(geographicAreaM49), grp = item_group_code, year = as.integer(timePointYears),
            iV = agg_imp_v, iQ = agg_imp_q, iR = agg_real_v_imp, eV = agg_exp_v, eQ = agg_exp_q, eR = agg_real_v_exp,
            xiv = idx_v_imp, xiq = idx_q_imp, xir = idx_rv_imp, xev = idx_v_exp, xeq = idx_q_exp, xer = idx_rv_exp)],
    aig[, .(lvl = "Region", area = as.character(area_group_code), grp = item_group_code, year = as.integer(timePointYears),
            iV = agg_imp_v, iQ = agg_imp_q, iR = agg_real_v_imp, eV = agg_exp_v, eQ = agg_exp_q, eR = agg_real_v_exp,
            xiv = idx_v_imp, xiq = idx_q_imp, xir = idx_rv_imp, xev = idx_v_exp, xeq = idx_q_exp, xer = idx_rv_exp)])
  wide <- wide[scope_s, on = .(lvl, area, grp), nomatch = 0]
  A <- rbind(wide[, .(lvl, area, grp, year, flow = "Import", V = iV, Q = iQ, R = iR, pv = xiv, pq = xiq, pr = xir)],
             wide[, .(lvl, area, grp, year, flow = "Export", V = eV, Q = eQ, R = eR, pv = xev, pq = xeq, pr = xer)])
  rm(wide)
  A <- A[, if (any(!is.na(V) & V != 0 | !is.na(R) & R != 0 | !is.na(Q) & Q != 0)) .SD, by = .(lvl, area, grp, flow)]
  yrs <- 2000:max(A$year)
  ggrid <- unique(A[, .(lvl, area, grp, flow)])[, .(year = yrs), by = .(lvl, area, grp, flow)]
  A <- A[ggrid, on = .(lvl, area, grp, flow, year)]
  setorder(A, lvl, area, grp, flow, year)
  A[, gsid := .GRP, by = .(lvl, area, grp, flow)]

  # base means and indices exactly as Step 6
  AB <- A[year %in% CFG$base_years, .(BV = mean(V, na.rm = TRUE), BQ = mean(Q, na.rm = TRUE), BR = mean(R, na.rm = TRUE),
                                      nbV = sum(!is.na(V)), nbQ = sum(!is.na(Q)), nbR = sum(!is.na(R))), by = gsid]
  for (cc in c("BV", "BQ", "BR")) AB[is.nan(get(cc)), (cc) := NA_real_]
  A <- merge(A, AB, by = "gsid", all.x = TRUE)
  setorder(A, gsid, year)
  A[, `:=`(xV = fifelse(!is.na(V) & BV > 0, V / BV * 100, NA_real_),
           xQ = fifelse(!is.na(Q) & BQ > 0, Q / BQ * 100, NA_real_),
           xR = fifelse(!is.na(R) & BR > 0, R / BR * 100, NA_real_))]
  A[, xU := fifelse(!is.na(xV) & !is.na(xR) & xR > 0, xV / xR * 100, NA_real_)]
  A[, U  := fifelse(!is.na(V) & !is.na(R) & R > 0, V / R, NA_real_)]
  A[, BU := fifelse(BV > 0 & BR > 0, BV / BR, NA_real_)]
  AVER <- rbindlist(lapply(list(c("Value", "xV", "pv"), c("Quantity", "xQ", "pq"), c("Real value", "xR", "pr")), function(z) {
    a <- A[[z[3]]]; b <- A[[z[2]]]; ok <- !is.na(a) & !is.na(b)
    list(index = z[1], max_rel = if (any(ok)) max(abs(a[ok] - b[ok]) / pmax(abs(a[ok]), 1e-12)) else 0,
         na_mismatch = sum(is.na(a) != is.na(b)))
  }))
  print(AVER)

  # reference levels
  Tn <- A[year %in% win, .(tV = med2(V), tQ = med2(Q), tR = med2(R), tU = med2(U)), by = gsid]
  Ta <- A[!(year %in% CFG$base_years), .(aV = med2(V), aQ = med2(Q), aR = med2(R), aU = med2(U)), by = gsid]
  A <- merge(merge(A, Tn, by = "gsid", all.x = TRUE), Ta, by = "gsid", all.x = TRUE)
  for (k in c("V", "Q", "R", "U")) A[is.na(get(paste0("t", k))), (paste0("t", k)) := get(paste0("a", k))]
  A[, c("aV", "aQ", "aR", "aU") := NULL]
  setorder(A, gsid, year)
  for (k in c("V", "Q", "R", "U")) A[, (paste0("n", k)) := nbr_med(A[[k]], A$gsid)]
  A[, `:=`(pV = shift(V), pQ = shift(Q), pR = shift(R), pU = shift(U),
           pxV = shift(xV), pxQ = shift(xQ), pxR = shift(xR), pxU = shift(xU),
           pnV = shift(nV), pnQ = shift(nQ), pnR = shift(nR), pnU = shift(nU)), by = gsid]
  ASZ <- A[!is.na(V) & V > 0][order(gsid, -year)][, .(size = mean(head(V, CFG$size_years))), by = gsid]
  A <- merge(A, ASZ, by = "gsid", all.x = TRUE)
  setorder(A, gsid, year)

  mka <- function(k) {
    d <- A[, .(gsid, year, type = k, x = get(paste0("x", k)), px = get(paste0("px", k)), cur = get(k),
               base = get(paste0("B", k)), typ = get(paste0("t", k)), nbm = get(paste0("n", k)),
               pcur = get(paste0("p", k)), pnbm = get(paste0("pn", k)), V, R, BV, BR, is_uv = k == "U")]
    d <- d[!is.na(x) & x > 0]
    fl <- if (k == "Q") CFG$floor_q else CFG$floor_v
    if (k == "U") {
      d[, `:=`(tiny_cur = V < CFG$floor_v | R < CFG$floor_v, tiny_prev = FALSE, tiny_base = BV < CFG$floor_v | BR < CFG$floor_v)]
    } else {
      d[, `:=`(tiny_cur = cur < fl, tiny_prev = FALSE, tiny_base = base < fl)]
    }
    d
  }
  PA <- classify(rbindlist(lapply(c("V", "Q", "R", "U"), mka)))
  PA[!lvl & !is.na(pcur) & type != "U" & pcur < fifelse(type == "Q", CFG$floor_q, CFG$floor_v) & cat == "trend", cat := "tiny_flow"]
  PA[cat == "base_uv_off", cat := fifelse(sg > 0, "low_base", "high_base")]
  PA[cat == "qv_mismatch", cat := fifelse(sg > 0, "spike", "dip")]
  PA[, tiny_base_agg := (type != "U" & tiny_base & sg > 0 & lvl) | (type == "U" & tiny_base & lvl)]
  cat("Aggregates: flagged index values:", format(nrow(PA), big.mark = ","), "\n")

  # ---- members of every flagged aggregate series (processed in chunks to limit memory)
  SER <- unique(A[, .(gsid, lvl, area, grp, flow)])
  fl_s <- SER[gsid %in% unique(PA$gsid)]
  # country level: items (Q and real value carry the item factor)
  tim <- if (exists("trade_item_merge")) get("trade_item_merge") else
    merge(get("trade_calc")[, .(geographicAreaM49, measuredItemCPC, timePointYears, imp_v, imp_q, real_v_imp, exp_v, exp_q, real_v_exp)],
          get("item_map"), by = "measuredItemCPC", allow.cartesian = TRUE)
  cpairs <- unique(fl_s[lvl == "Country", .(area, grp)])
  MC_all <- tim[as.character(geographicAreaM49) %in% cpairs$area & item_group_code %in% cpairs$grp,
                .(area = as.character(geographicAreaM49), grp = item_group_code, member = norm_i(measuredItemCPC),
                  year = as.integer(timePointYears), f = item_factor, imp_v, imp_q, real_v_imp, exp_v, exp_q, real_v_exp)]
  MC_all <- MC_all[cpairs, on = .(area, grp), nomatch = 0]
  setkey(MC_all, area, grp)
  rm(tim)
  # region level: countries
  rpairs <- unique(fl_s[lvl == "Region", .(area, grp)])
  amap <- get("area_agg_map")[, .(area = as.character(area_group_code), member = as.character(geographicAreaM49))]
  amap <- unique(amap[area %in% rpairs$area])
  cig_m <- cig[item_group_code %in% rpairs$grp, .(member = as.character(geographicAreaM49), grp = item_group_code,
               year = as.integer(timePointYears), agg_imp_v, agg_imp_q, agg_real_v_imp, agg_exp_v, agg_exp_q, agg_real_v_exp)]
  AY  <- A[year %in% CFG$base_years, .(gsid, year, okV = !is.na(V), okQ = !is.na(Q), okR = !is.na(R), nbV, nbQ, nbR)]
  ABB <- unique(A[, .(gsid, BV, BQ, BR)], by = "gsid")
  FPall <- unique(PA[, .(gsid, year, type)])
  ids <- unique(fl_s$gsid)
  chunks <- split(ids, ceiling(seq_along(ids) / 800))
  XSL <- vector("list", length(chunks)); TOPL <- vector("list", length(chunks))
  for (k in seq_along(chunks)) {
    fs <- fl_s[gsid %in% chunks[[k]]]
    cp <- unique(fs[lvl == "Country", .(area, grp)]); rp <- unique(fs[lvl == "Region", .(area, grp)])
    MC <- MC_all[cp, nomatch = 0]
    if (nrow(MC) == 0) MC <- data.table(area = character(), grp = character(), member = character(), year = integer(), f = numeric(),
                                        imp_v = numeric(), imp_q = numeric(), real_v_imp = numeric(), exp_v = numeric(), exp_q = numeric(), real_v_exp = numeric())
    MC <- rbind(MC[, .(lvl = rep("Country", .N), area, grp, member, year, flow = rep("Import", .N), mV = imp_v, mQ = imp_q * f, mR = real_v_imp * f)],
                MC[, .(lvl = rep("Country", .N), area, grp, member, year, flow = rep("Export", .N), mV = exp_v, mQ = exp_q * f, mR = real_v_exp * f)])
    MR <- merge(amap[area %in% rp$area], cig_m[grp %in% rp$grp], by = "member", allow.cartesian = TRUE)[rp, on = .(area, grp), nomatch = 0]
    MR <- rbind(MR[, .(lvl = rep("Region", .N), area, grp, member, year, flow = rep("Import", .N), mV = agg_imp_v, mQ = agg_imp_q, mR = agg_real_v_imp)],
                MR[, .(lvl = rep("Region", .N), area, grp, member, year, flow = rep("Export", .N), mV = agg_exp_v, mQ = agg_exp_q, mR = agg_real_v_exp)])
    M <- merge(rbind(MC, MR), fs, by = c("lvl", "area", "grp", "flow"))
    rm(MC, MR)
    M <- M[!(is.na(mV) & is.na(mQ) & is.na(mR)), .(gsid, member, year, mV, mQ, mR)]
    # member base contributions: base-year values over the base years where the aggregate exists
    MB <- merge(M[year %in% CFG$base_years], AY, by = c("gsid", "year"))
    MB <- MB[, .(bV = sum(fifelse(okV, fcoalesce(mV, 0), 0)) / nbV[1], bQ = sum(fifelse(okQ, fcoalesce(mQ, 0), 0)) / nbQ[1],
                 bR = sum(fifelse(okR, fcoalesce(mR, 0), 0)) / nbR[1]), by = .(gsid, member)]
    for (cc in c("bV", "bQ", "bR")) MB[!is.finite(get(cc)), (cc) := 0]
    FP <- FPall[gsid %in% chunks[[k]]]
    MT <- unique(M[unique(FP[, .(gsid, year)]), on = .(gsid, year), nomatch = 0], by = c("gsid", "year", "member"))
    MT <- MT[, .(gsid, year, member, aV = mV, aQ = mQ, aR = mR)]
    ALLM <- unique(rbind(MB[, .(gsid, member)], MT[, .(gsid, member)]))
    X <- merge(FP, ALLM, by = "gsid", allow.cartesian = TRUE)
    X <- merge(X, MB, by = c("gsid", "member"), all.x = TRUE)
    X <- merge(X, MT, by = c("gsid", "year", "member"), all.x = TRUE)
    for (cc in c("bV", "bQ", "bR", "aV", "aQ", "aR")) set(X, which(is.na(X[[cc]])), cc, 0)
    X <- merge(X, ABB, by = "gsid")
    X[, `:=`(b = fcase(type == "V", bV, type == "Q", bQ, type == "R", bR, default = 0),
             a = fcase(type == "V", aV, type == "Q", aQ, type == "R", aR, default = 0))]
    X[, d := fifelse(type == "U", fifelse(BV > 0 & BR > 0, (aV - bV) / BV - (aR - bR) / BR, 0), a - b)]
    X <- X[!(b == 0 & a == 0 & d == 0), .(gsid, year, type, member, a, b, d)]
    X[, Dsum := sum(d), by = .(gsid, year, type)]
    X[, sgn := fifelse(Dsum >= 0, 1, -1)]
    X[, ord := -d * sgn]
    setorder(X, gsid, year, type, ord)
    XSL[[k]] <- X[, .(n_mem = .N, Dsum = Dsum[1], top = member[1], top_share = if (Dsum[1] != 0) d[1] / Dsum[1] else 0,
                      share_new = if (type[1] != "U" && sum(a) > 0) sum(a[b == 0]) / sum(a) else 0,
                      share_lost = if (type[1] != "U" && sum(b) > 0) sum(b[a == 0 & b > 0]) / sum(b) else 0,
                      n_new = sum(b == 0 & a > 0), n_lost = sum(a == 0 & b > 0)), by = .(gsid, year, type)]
    X[, rk := rowid(gsid, year, type)]
    TOPL[[k]] <- X[rk <= 5, .(gsid, year, type, member, b, a, d, rk)]
    rm(X, MT, MB, M, ALLM, FP)
  }
  XS <- rbindlist(XSL); TOP <- rbindlist(TOPL)
  rm(XSL, TOPL, MC_all, cig_m)
  PA <- merge(PA, XS, by = c("gsid", "year", "type"), all.x = TRUE)
  PA <- merge(PA, SER[, .(gsid, glvl = lvl)], by = "gsid")
  PA[is.na(n_mem), `:=`(n_mem = 0L, top_share = 0, share_new = 0, share_lost = 0, n_new = 0L, n_lost = 0L)]
  PA[, acat := paste0("a_", cat)]
  PA[top_share >= CFG$dominant & glvl == "Country", acat := "a_one_item"]
  PA[top_share >= CFG$dominant & glvl == "Region", acat := "a_one_country"]
  PA[lvl == TRUE & sg < 0 & share_lost >= CFG$coverage & type != "U", acat := "a_lost"]
  PA[lvl == TRUE & sg > 0 & share_new >= CFG$coverage & type != "U", acat := "a_new"]
  PA[tiny_base_agg == TRUE, acat := "a_tiny_base"]

  # members whose item-level cause is needed (country level, dominant item)
  need_items <- unique(merge(PA[acat == "a_one_item", .(gsid, top)], SER, by = "gsid")[, .(area = norm_a(area), item = top)])
  AGG <- list(A = A, PA = PA, SER = SER, TOP = TOP, scope = scope, VER = AVER, need_items = need_items)
}

# -----------------------------------------------------------------------------
# 3. Item engine (ItemList series + items that drive an aggregate)
# -----------------------------------------------------------------------------
need_items <- if (!is.null(AGG)) AGG$need_items else if (!is.null(agg_cache)) agg_cache$need_items else NULL
keys <- unique(rbind(item_keys[, .(area, item, inlist = TRUE)],
                     if (!is.null(need_items)) need_items[, .(area, item, inlist = FALSE)]), by = c("area", "item"))
tr <- as.data.table(readRDS("TradeData.rds"))
tr[, `:=`(area = norm_a(geographicAreaM49), item = norm_i(measuredItemCPC))]
tr <- tr[keys[, .(area, item)], on = .(area, item), nomatch = 0]
tr[, year := as.integer(timePointYears)]
tr[, flag := paste0(flagObservationStatus, "|", flagMethod)]
tr[, el := c("5610" = "iq", "5622" = "iv", "5910" = "eq", "5922" = "ev")[measuredElementTrade]]
tr <- tr[!is.na(el)]
W  <- dcast(tr, area + item + year ~ el, value.var = "Value")
WF <- dcast(tr, area + item + year ~ el, value.var = "flag")
rm(tr)
for (cc in c("iq", "iv", "eq", "ev")) { if (!cc %in% names(W)) W[, (cc) := NA_real_]; if (!cc %in% names(WF)) WF[, (cc) := NA_character_] }
setnames(WF, c("iq", "iv", "eq", "ev"), c("iq_f", "iv_f", "eq_f", "ev_f"))
W <- merge(W, WF, by = c("area", "item", "year"))
L <- rbind(W[, .(area, item, year, flow = "Import", q0 = iq, v0 = iv, qf = iq_f, vf = iv_f)],
           W[, .(area, item, year, flow = "Export", q0 = eq, v0 = ev, qf = eq_f, vf = ev_f)])
rm(W, WF)
L <- L[, if (any(!is.na(q0) | !is.na(v0))) .SD, by = .(area, item, flow)]
yrs <- 2000:max(L$year)
grid <- unique(L[, .(area, item, flow)])[, .(year = yrs), by = .(area, item, flow)]
L <- L[grid, on = .(area, item, flow, year)]
L <- merge(L, keys, by = c("area", "item"))
setorder(L, area, item, flow, year)
L[, sid := .GRP, by = .(area, item, flow)]

L[, `:=`(q = q0, v = v0)]
L[!is.na(q) & q == 0 & !is.na(v) & v > 0, q := NA]
L[!is.na(v) & v == 0 & !is.na(q) & q > 0, v := NA]
Bm <- L[year %in% CFG$base_years & q > 0 & v > 0, .(bq = mean(q), bv = mean(v), nb = .N), by = sid]
L <- merge(L, Bm, by = "sid", all.x = TRUE)
L <- merge(L, BUV, by = c("area", "item", "flow"), all.x = TRUE)
setorder(L, sid, year)
L[, uv := fifelse(!is.na(q) & q > 0 & !is.na(v), v / q, NA_real_)]
L[, `:=`(xq = fifelse(bq > 0, q / bq * 100, NA_real_), xv = fifelse(bv > 0, v / bv * 100, NA_real_),
         xu = fifelse(buv > 0, uv / buv * 100, NA_real_))]

chk <- merge(idx[, .(area, item, year, iv, iq, iu, ev, eq, eu)],
             dcast(L[inlist == TRUE], area + item + year ~ flow, value.var = c("xq", "xv", "xu")),
             by = c("area", "item", "year"), all.x = TRUE)
relchk <- function(a, b) { ok <- !is.na(a) & !is.na(b)
  list(max_rel = if (any(ok)) max(abs(a[ok] - b[ok]) / pmax(abs(a[ok]), 1e-12)) else 0, na_mismatch = sum(is.na(a) != is.na(b))) }
VER <- rbindlist(list(
  c(index = "Import Quantity", relchk(chk$iq, chk$xq_Import)), c(index = "Import Value", relchk(chk$iv, chk$xv_Import)),
  c(index = "Import Unit Value", relchk(chk$iu, chk$xu_Import)), c(index = "Export Quantity", relchk(chk$eq, chk$xq_Export)),
  c(index = "Export Value", relchk(chk$ev, chk$xv_Export)), c(index = "Export Unit Value", relchk(chk$eu, chk$xu_Export))))
print(VER)
rm(chk)

Tn <- L[year %in% win, .(tq = med2(q), tv = med2(v), tu = med2(uv)), by = sid]
Ta <- L[!(year %in% CFG$base_years), .(aq = med2(q), av = med2(v), au = med2(uv)), by = sid]
L  <- merge(merge(L, Tn, by = "sid", all.x = TRUE), Ta, by = "sid", all.x = TRUE)
L[is.na(tq), tq := aq]; L[is.na(tv), tv := av]; L[is.na(tu), tu := au]
L[, c("aq", "av", "au") := NULL]
setorder(L, sid, year)
L[, nq := nbr_med(q, sid)]; L[, nv := nbr_med(v, sid)]; L[, nu := nbr_med(uv, sid)]
L[, `:=`(pq = shift(q), pv = shift(v), pu = shift(uv), pxq = shift(xq), pxv = shift(xv), pxu = shift(xu),
         pnq = shift(nq), pnv = shift(nv), pnu = shift(nu)), by = sid]
if (!PIPE) {
  G  <- L[!is.na(uv) & q >= CFG$floor_q & v >= CFG$floor_v, .(G = median(uv), nG = .N), by = .(item, flow, year)][nG >= 5, .(item, flow, year, G)]
  GB <- unique(L[!is.na(buv), .(sid, item, flow, buv, src)])[,
          .(gb = if (sum(src == "country", na.rm = TRUE) >= 5) median(buv[src %in% "country"]) else if (.N >= 5) median(buv) else NA_real_), by = .(item, flow)]
}
L <- merge(L, G, by = c("item", "flow", "year"), all.x = TRUE)
L <- merge(L, GB, by = c("item", "flow"), all.x = TRUE)
setorder(L, sid, year)
L[, pG := shift(G), by = sid]
L[, wref := !is.na(G) & !is.na(gb)]
L[, `:=`(tu_r = fifelse(wref, gb, tu), nu_r = fifelse(wref, G, nu), pnu_r = fifelse(wref & !is.na(pG), pG, pnu))]
SZ <- L[!is.na(v) & v > 0][order(sid, -year)][, .(size = mean(head(v, CFG$size_years))), by = sid]
L  <- merge(L, SZ, by = "sid", all.x = TRUE)
setorder(L, sid, year)

mk <- function(type, x, px, cur, base, typ, nbm, pcur, pnbm) {
  d <- L[, .(sid, year, type = type, x = get(x), px = get(px), cur = get(cur), base = get(base),
             typ = get(typ), nbm = get(nbm), pcur = get(pcur), pnbm = get(pnbm), q, v, wref = type == "UV" & wref, is_uv = type == "UV")]
  d <- d[!is.na(x) & x > 0]
  fl <- if (type == "Q") CFG$floor_q else CFG$floor_v
  if (type == "UV") d[, `:=`(tiny_cur = q < CFG$floor_q | v < CFG$floor_v, tiny_prev = FALSE, tiny_base = FALSE)]
  else d[, `:=`(tiny_cur = cur < fl, tiny_prev = !is.na(pcur) & pcur < fl, tiny_base = base < fl)]
  d
}
P <- rbind(mk("Q", "xq", "pxq", "q", "bq", "tq", "nq", "pq", "pnq"),
           mk("V", "xv", "pxv", "v", "bv", "tv", "nv", "pv", "pnv"),
           mk("UV", "xu", "pxu", "uv", "buv", "tu_r", "nu_r", "pu", "pnu_r"))
P[, tiny_prev := tiny_prev & !(x > CFG$warn_hi | x < CFG$warn_lo)]
P <- classify(P)

CATS <- data.table(
  id = c("tiny_base", "low_base", "high_base", "base_uv_off", "spike", "dip", "qv_mismatch", "tiny_flow", "trend"),
  label = c("Negligible base", "Base unusually low", "Base unusually high", "Base unit value off",
            "One-year spike", "One-year drop", "Q/V mismatch in year", "Negligible flow in year", "Sustained change"),
  driver = c("Base", "Base", "Base", "Base", "Year", "Year", "Year", "Year", "Level"),
  artefact = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
P[, cat_i := match(cat, CATS$id) - 1L]
P <- merge(P, unique(L[, .(sid, inlist)]), by = "sid")
cat("\nItem indices — flagged values by cause (ItemList series):\n")
print(P[inlist == TRUE, .(values = .N, critical = sum(sev == 2), series = uniqueN(sid)), by = cat][order(-critical)])

# names
area_nm <- unique(idx[, .(area, area_name)], by = "area")
item_nm <- unique(idx[, .(item, item_name)], by = "item")
if (PIPE) {
  at <- get("agg_table")
  item_nm <- unique(rbind(item_nm, unique(at[var_type == "item", .(item = norm_i(member_code), item_name = member_name)], by = "item")), by = "item")
  if (exists("area_names")) area_nm <- unique(rbind(area_nm, get("area_names")[, .(area = norm_a(Area_Code), area_name = Area_Name)]), by = "area")
} else if (!is.null(agg_cache) && !is.null(agg_cache$item_names)) {
  item_nm <- unique(rbind(item_nm, agg_cache$item_names), by = "item")
}

S <- unique(L[, .(sid, area, item, flow, inlist, bq, bv, nb, buv, src, own, nimp, ndir, tq, tv, tu, gb, size)], by = "sid")
S <- merge(S, area_nm, by = "area", all.x = TRUE)
S <- merge(S, item_nm, by = "item", all.x = TRUE)
S[is.na(area_name), area_name := area]; S[is.na(item_name), item_name := item]
setorder(S, sid)

P[, mag := abs(log10(x / 100))]
I <- P[inlist == TRUE][order(-sev, -mag), .(
  sev = max(sev), n = .N, n_crit = sum(sev == 2),
  types = paste(intersect(c("Q", "V", "UV"), unique(type)), collapse = " "),
  y0 = min(year), y1 = max(year), worst_type = type[1], worst_year = year[1], worst_x = x[1],
  years = paste(sort(unique(year)), collapse = " ")), by = .(sid, cat, cat_i)]
I <- merge(I, S[, .(sid, area, area_name, item, item_name, flow, size)], by = "sid")
I[, tier := as.numeric(match(CATS$driver[cat_i + 1L], c("Base", "Year", "Level")))]
I[cat == "tiny_flow", tier := 2.5]
setorder(I, -sev, tier, -size, sid)
I[, rank := .I]
fwrite(I[, .(Rank = rank, Severity = fifelse(sev == 2, "Critical", "Warning"), Category = CATS$label[cat_i + 1L],
             `Area Code (M49)` = area, Area = area_name, `Item Code (CPC)` = item, Item = item_name, Flow = flow,
             `Indices affected` = types, `Flagged values` = n, `Critical values` = n_crit, `First year` = y0,
             `Last year` = y1, Years = years, `Most extreme index` = paste(worst_type, worst_year),
             `Most extreme value` = signif(worst_x, 6), `Trade size (1000 USD)` = round(size, 1))],
       "results/checks/diagnostics_item_indices.csv")
cat("Saved: results/checks/diagnostics_item_indices.csv (", nrow(I), "issues )\n")

# ---- real values: real value = quantity x base UV, so real value / value = base UV / UV.
# A flagged unit value index is the same thing seen from the real value side.
RI <- P[inlist == TRUE & type == "UV"][order(-sev, -mag), .(
  sev = max(sev), n = .N, n_crit = sum(sev == 2), y0 = min(year), y1 = max(year), wy = year[1],
  years = paste(sort(unique(year)), collapse = " ")), by = .(sid, cat, cat_i)]
RI <- merge(RI, L[, .(sid, wy = year, q, v, buv)], by = c("sid", "wy"))
RI[, `:=`(rv = q * buv, gap = q * buv - v, ratio = fifelse(v > 0, q * buv / v, NA_real_))]
RI <- merge(RI, S[, .(sid, area, area_name, item, item_name, flow, size, src)], by = "sid")
RI[, tier := match(cat, c("base_uv_off", "qv_mismatch", "tiny_flow", "trend"))]
RI[, absgap := -abs(gap)]
setorder(RI, -sev, tier, absgap, sid)
RI[, rank := .I]
fwrite(RI[, .(Rank = rank, Severity = fifelse(sev == 2, "Critical", "Warning"), Category = CATS$label[cat_i + 1L],
              `Area Code (M49)` = area, Area = area_name, `Item Code (CPC)` = item, Item = item_name, Flow = flow,
              `Flagged years` = n, Years = years, `Worst year` = wy, `Quantity (t)` = signif(q, 6),
              `Value (1000 USD)` = signif(v, 6), `Base UV (1000 USD/t)` = signif(buv, 6), `Base UV source` = src,
              `Real value (1000 USD)` = signif(rv, 6), `Real value - value` = signif(gap, 6),
              `Real value / value` = signif(ratio, 4))],
       "results/checks/diagnostics_real_values.csv")
cat("Saved: results/checks/diagnostics_real_values.csv (", nrow(RI), "issues )\n")
ly <- max(L$year)
RS <- L[inlist == TRUE & year == ly & !is.na(q) & !is.na(buv), .(rv = sum(q * buv)), by = .(own = src %in% "country")]
RVSTATS <- list(year = ly, rv_total = sum(RS$rv), rv_own = sum(RS[own == TRUE]$rv),
                n_rv = uniqueN(L[inlist == TRUE & !is.na(buv)]$sid),
                n_borrowed = uniqueN(L[inlist == TRUE & !is.na(buv) & !(src %in% "country")]$sid))

zl <- rbind(L[inlist == TRUE, .(x = xq, f = qf)], L[inlist == TRUE, .(x = xv, f = vf)])[!is.na(x) & x == 0]
zl[, st := sub("\\|.*", "", f)]
ZERO <- zl[, .N, by = st][order(-N)]
rm(zl)

# -----------------------------------------------------------------------------
# 4. Aggregates: member causes, issues, cache
# -----------------------------------------------------------------------------
ACATS <- data.table(
  id = c("a_tiny_base", "a_low_base", "a_high_base", "a_new", "a_lost", "a_one_item", "a_one_country",
         "a_spike", "a_dip", "a_tiny_flow", "a_trend"),
  label = c("Negligible base", "Base unusually low", "Base unusually high", "Not traded in base", "Missing in year",
            "Driven by one item", "Driven by one country", "One-year spike", "One-year drop",
            "Negligible flow in year", "Sustained change"),
  driver = c("Base", "Base", "Base", "Coverage", "Coverage", "Member", "Member", "Year", "Year", "Year", "Level"),
  artefact = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))

if (!is.null(AGG)) {
  PA <- AGG$PA; SER <- AGG$SER; A <- AGG$A
  PA[, acat_i := match(acat, ACATS$id) - 1L]
  stopifnot(!anyNA(PA$acat_i))
  # sub-cause of the dominant member at that point
  tmap <- c(V = "V", Q = "Q", R = "Q", U = "UV")
  ci <- merge(PA[acat == "a_one_item", .(gsid, year, type, top)], SER[, .(gsid, area, flow)], by = "gsid")
  ci[, `:=`(area = norm_a(area), itype = tmap[type])]
  ci <- merge(ci, S[, .(sid, area, item, flow, buv, gb)], by.x = c("area", "top", "flow"), by.y = c("area", "item", "flow"), all.x = TRUE)
  ci <- merge(ci, P[, .(sid, year, itype = type, icat = cat)], by = c("sid", "year", "itype"), all.x = TRUE)
  ci[, uvoff := !is.na(buv) & !is.na(gb) & gb > 0 & (buv / gb > 10 | buv / gb < 0.1)]
  ci[, sub := fifelse(type %in% c("R", "U") & uvoff, "base_uv_off", icat)]
  PA <- merge(PA, ci[, .(gsid, year, type, sub_item = sub, msid = sid)], by = c("gsid", "year", "type"), all.x = TRUE)
  cc <- merge(PA[acat == "a_one_country", .(gsid, year, type, top)], SER[, .(gsid, grp, flow)], by = "gsid")
  cc <- merge(cc, SER[lvl == "Country", .(mgsid = gsid, top = area, grp, flow)], by = c("top", "grp", "flow"), all.x = TRUE)
  cc <- merge(cc, PA[, .(mgsid = gsid, year, type, ccat = acat)], by = c("mgsid", "year", "type"), all.x = TRUE)
  PA <- merge(PA, cc[, .(gsid, year, type, sub_cty = ccat, mgsid)], by = c("gsid", "year", "type"), all.x = TRUE)
  PA[, sub_i := match(sub_item, CATS$id) - 1L]
  PA[, sub_c := match(sub_cty, ACATS$id) - 1L]

  cat("\nAggregates — flagged values by cause:\n")
  print(PA[, .(values = .N, critical = sum(sev == 2), series = uniqueN(gsid)), by = acat][order(-critical)])

  GS <- unique(A[, .(gsid, lvl, area, grp, flow, BV, BQ, BR, size, tV, tQ, tR, tU)], by = "gsid")
  GS <- merge(GS, unique(AGG$scope[, .(lvl, area, area_name)], by = c("lvl", "area")), by = c("lvl", "area"), all.x = TRUE)
  GS <- merge(GS, unique(AGG$scope[, .(grp, grp_name)], by = "grp"), by = "grp", all.x = TRUE)
  setorder(GS, gsid)
  PA[, mag := abs(log10(x / 100))]
  # Issues are kept per index family, as the dashboard shows them under two tabs:
  #   VQ = value and quantity indices (Item indices), RU = real value and unit value indices (Real values)
  PA[, fam := fifelse(type %in% c("V", "Q"), "VQ", "RU")]
  AI <- PA[order(-sev, -mag), .(sev = max(sev), n = .N, n_crit = sum(sev == 2),
           types = paste(intersect(c("V", "Q", "R", "U"), unique(type)), collapse = " "),
           y0 = min(year), y1 = max(year), worst_type = type[1], worst_year = year[1], worst_x = x[1],
           years = paste(sort(unique(year)), collapse = " ")), by = .(fam, gsid, acat, acat_i)]
  AI <- merge(AI, GS[, .(gsid, lvl, area, area_name, grp, grp_name, flow, size)], by = "gsid")
  AI[, tier := as.numeric(match(ACATS$driver[acat_i + 1L], c("Base", "Coverage", "Member", "Year", "Level")))]
  AI[acat == "a_tiny_flow", tier := 4.5]
  AI[, fam_o := match(fam, c("VQ", "RU"))]
  setorder(AI, fam_o, -sev, tier, -size, gsid)
  AI[, rank := rowid(fam)]          # rank within the family
  AI[, k := .I]                     # row of the issue in the payload
  # the representative point of every issue (worst value of its family) and its top members
  REP <- unique(PA[order(-sev, -mag)], by = c("fam", "gsid", "acat"))[, .(fam, gsid, acat, year, type)]
  AI <- merge(AI, REP, by = c("fam", "gsid", "acat"))
  setorder(AI, k)
  TOPI <- merge(REP, AGG$TOP, by = c("gsid", "year", "type"))
  TOPI <- merge(TOPI, AI[, .(fam, gsid, acat, k)], by = c("fam", "gsid", "acat"))
  setorder(TOPI, k, rk)
  fwrite(AI[, .(`Index family` = fifelse(fam == "VQ", "Value & quantity", "Real value & unit value"),
                Rank = rank, Severity = fifelse(sev == 2, "Critical", "Warning"), Category = ACATS$label[acat_i + 1L],
                Level = lvl, `Area Code (M49)` = area, Area = area_name, `Item Group Code` = grp, `Item Group` = grp_name,
                Flow = flow, `Indices affected` = gsub("R", "RV", gsub("U", "UV", types)), `Flagged values` = n,
                `Critical values` = n_crit, `First year` = y0, `Last year` = y1, Years = years,
                `Most extreme index` = paste(worst_type, worst_year), `Most extreme value` = signif(worst_x, 6),
                `Trade size (1000 USD)` = round(size, 1))],
         "results/checks/diagnostics_aggregates.csv")
  cat("Saved: results/checks/diagnostics_aggregates.csv (", nrow(AI), "issues )\n")

  # member display names
  mem_nm <- function(lvl, code) fifelse(lvl == "Country", item_nm$item_name[match(code, item_nm$item)],
                                        area_nm$area_name[match(norm_a(code), area_nm$area)])
  TOPI <- merge(TOPI, GS[, .(gsid, lvl)], by = "gsid")
  TOPI[, mname := mem_nm(lvl, member)]
  TOPI[is.na(mname), mname := member]
  # link targets: item series (country level) or country aggregate (region level)
  TOPI <- merge(TOPI, SER[, .(gsid, area, grp, flow)], by = "gsid")
  TOPI[, marea := norm_a(area)]
  TOPI <- merge(TOPI, SER[lvl == "Country", .(m_gsid = gsid, member = area, grp, flow)], by = c("member", "grp", "flow"), all.x = TRUE)
  TOPI[lvl == "Country", m_gsid := NA]
  setorder(TOPI, k, rk)

  agg_payload <- list(
    cats = ACATS,
    groups = unique(GS[, .(grp, grp_name)])[order(grp_name)],
    areas = unique(GS[, .(lvl, area, area_name)])[order(lvl, area_name)],
    GS = GS, A = A[, .(gsid, year, V, Q, R)],
    PA = PA[, .(gsid, year, type, sev, acat_i, cB, cO, cP, cS, nbm, pnbm, odd_prev, top_share, share_new, share_lost,
                n_new, n_lost, sub_i, sub_c)],
    AI = AI, TOPI = TOPI, VER = AGG$VER,
    need_items = AGG$need_items, item_names = item_nm[item %in% AGG$need_items$item],
    built = format(Sys.time(), "%Y-%m-%d %H:%M"))
  saveRDS(agg_payload, CACHE)
} else if (!is.null(agg_cache)) {
  agg_payload <- agg_cache
  cat("Aggregates: using the cache from", agg_payload$built, "\n")
} else agg_payload <- NULL

# -----------------------------------------------------------------------------
# 5. Payload for the HTML
# -----------------------------------------------------------------------------
flags <- sort(unique(c(L$qf, L$vf))); flags <- flags[!is.na(flags)]
areas <- unique(S[, .(area, area_name)])[order(area_name)]
items <- unique(S[, .(item, item_name)])[order(item_name)]
srcs  <- sort(unique(S$src[!is.na(S$src)]))
setorder(L, sid, year)
ny <- length(yrs)
stopifnot(nrow(L) == nrow(S) * ny)
P[, nbm_out := fifelse(odd_prev, pnbm, nbm)]
o <- order(-P$sev, -P$mag); keep_c <- logical(nrow(P)); keep_c[o] <- !duplicated(P[o, .(sid, cat)])
P[, keep_c := keep_c]
P[keep_c == FALSE, `:=`(cB = 0, cO = 0, cP = 0, cS = 0)]
setorder(P, sid, type, year)

payload <- list(
  years = yrs, base_years = CFG$base_years, cfg = CFG, cats = CATS,
  areas = list(code = areas$area, name = areas$area_name),
  items = list(code = items$item, name = items$item_name),
  flags = flags, srcs = srcs,
  series = list(
    a = match(S$area, areas$area) - 1L, i = match(S$item, items$item) - 1L, f = as.integer(S$flow == "Export"),
    l = as.integer(S$inlist),
    bq = sg6(S$bq), bv = sg6(S$bv), nb = S$nb, buv = sg6(S$buv),
    src = match(S$src, srcs) - 1L, own = S$own, nimp = S$nimp, ndir = S$ndir,
    tq = sg6(S$tq), tv = sg6(S$tv), tu = sg6(S$tu), gb = sg6(S$gb), size = signif(S$size, 4)),
  q = sg6(L$q0), v = sg6(L$v0), qf = match(L$qf, flags) - 1L, vf = match(L$vf, flags) - 1L,
  cells = list(s = P$sid - 1L, t = match(P$type, c("Q", "V", "UV")) - 1L, y = P$year, sev = P$sev, c = P$cat_i,
               cb = round(P$cB, 2), co = round(P$cO, 2), cp = round(P$cP, 2), cs = round(P$cS, 2),
               nbm = signif(P$nbm_out, 4), op = as.integer(P$odd_prev), w = as.integer(P$wref)),
  issues = list(s = I$sid - 1L, c = I$cat_i, sev = I$sev, n = I$n, nc = I$n_crit, ty = I$types, y0 = I$y0, y1 = I$y1,
                wt = match(I$worst_type, c("Q", "V", "UV")) - 1L, wy = I$worst_year, wx = signif(I$worst_x, 5)),
  verify = VER, zeros = ZERO, rvstats = RVSTATS,
  rissues = list(s = RI$sid - 1L, c = RI$cat_i, sev = RI$sev, n = RI$n, nc = RI$n_crit, y0 = RI$y0, y1 = RI$y1,
                 wy = RI$wy, gap = signif(RI$gap, 5), ratio = signif(RI$ratio, 4)),
  meta = list(generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
              outputs_date = format(file.mtime("results/indices_items.csv"), "%Y-%m-%d %H:%M"),
              n_series = nrow(S[inlist == TRUE]),
              n_values = L[inlist == TRUE, sum(!is.na(xq)) + sum(!is.na(xv)) + sum(!is.na(xu))],
              n_areas = uniqueN(S[inlist == TRUE]$area), n_items = uniqueN(S[inlist == TRUE]$item))
)

if (!is.null(agg_payload)) {
  ap <- agg_payload
  GS <- ap$GS; AA <- ap$A; PA <- ap$PA; AI <- copy(ap$AI); TOPI <- copy(ap$TOPI)
  # caches written before the index-family split: one issue list, the dashboard splits it
  if (!"k" %in% names(AI)) { AI[, k := rank]; TOPI[, k := rank] }
  setorder(AI, k)
  setorder(GS, gsid); setorder(AA, gsid, year)
  stopifnot(nrow(AA) == nrow(GS) * length(yrs), all(AA$year == rep(yrs, nrow(GS))))
  areasA <- ap$areas; groups <- ap$groups
  PA[, nbm_out := fifelse(odd_prev, pnbm, nbm)]
  PA <- merge(PA, AI[, .(gsid, year, type, is_rep = TRUE)], by = c("gsid", "year", "type"), all.x = TRUE)
  PA[is.na(is_rep), `:=`(cB = 0, cO = 0, cP = 0, cS = 0)]
  setorder(PA, gsid, type, year)
  mnames <- unique(TOPI$mname)
  # member links: item series in this run's S (country level)
  TOPI <- merge(TOPI, S[, .(m_sid = sid, marea = area, member = item, flow)], by = c("marea", "member", "flow"), all.x = TRUE)
  TOPI[lvl == "Region", m_sid := NA]
  setorder(TOPI, k, rk)
  payload_agg <- list(
    years = yrs, base_years = CFG$base_years, cfg = CFG, mnames = mnames,
    cats = ap$cats, verify = ap$VER, built = ap$built,
    areas = list(lvl = areasA$lvl, code = areasA$area, name = areasA$area_name),
    groups = list(code = groups$grp, name = groups$grp_name),
    series = list(lv = as.integer(GS$lvl == "Region"), a = match(paste(GS$lvl, GS$area), paste(areasA$lvl, areasA$area)) - 1L,
                  g = match(GS$grp, groups$grp) - 1L, f = as.integer(GS$flow == "Export"),
                  bv = sg6(GS$BV), bq = sg6(GS$BQ), br = sg6(GS$BR), size = signif(GS$size, 4),
                  tv = sg6(GS$tV), tq = sg6(GS$tQ), tr = sg6(GS$tR), tu = sg6(GS$tU)),
    V = sg6(AA$V), Q = sg6(AA$Q), R = sg6(AA$R),
    cells = list(s = PA$gsid - 1L, t = match(PA$type, c("V", "Q", "R", "U")) - 1L, y = PA$year, sev = PA$sev, c = PA$acat_i,
                 cb = round(PA$cB, 2), co = round(PA$cO, 2), cp = round(PA$cP, 2), cs = round(PA$cS, 2),
                 nbm = signif(PA$nbm_out, 4), op = as.integer(PA$odd_prev),
                 sh = round(PA$top_share, 3), sn = round(PA$share_new, 3), sl = round(PA$share_lost, 3),
                 nn = PA$n_new, nl = PA$n_lost, si = PA$sub_i, sc = PA$sub_c),
    issues = c(list(s = AI$gsid - 1L, c = AI$acat_i, sev = AI$sev, n = AI$n, nc = AI$n_crit, ty = AI$types,
                  y0 = AI$y0, y1 = AI$y1, wt = match(AI$worst_type, c("V", "Q", "R", "U")) - 1L, wy = AI$worst_year,
                  wx = signif(AI$worst_x, 5), ry = AI$year, rt = match(AI$type, c("V", "Q", "R", "U")) - 1L),
               if ("fam" %in% names(AI)) list(fm = tolower(AI$fam))),
    top = list(r = TOPI$k - 1L, m = match(TOPI$mname, mnames) - 1L, b = sg6(TOPI$b), a = sg6(TOPI$a), d = sg6(TOPI$d),
               ms = TOPI$m_sid - 1L, mg = TOPI$m_gsid - 1L)
  )
} else payload_agg <- list(none = TRUE)

enc <- function(x) {
  js <- toJSON(x, dataframe = "columns", na = "null", digits = NA, auto_unbox = TRUE)
  b <- gsub("[\r\n]", "", base64_enc(memCompress(charToRaw(enc2utf8(as.character(js))), type = "gzip")))
  cat(sprintf("Payload: %.1f MB JSON -> %.1f MB embedded\n", nchar(js) / 1e6, nchar(b) / 1e6)); b
}
b64  <- enc(payload)
b64a <- enc(payload_agg)
html  <- readChar(template, file.info(template)$size, useBytes = TRUE)
p1 <- strsplit(html, "__PAYLOAD_AGG__", fixed = TRUE, useBytes = TRUE)[[1]]
stopifnot(length(p1) == 2)
p0 <- strsplit(p1[1], "__PAYLOAD__", fixed = TRUE, useBytes = TRUE)[[1]]
stopifnot(length(p0) == 2)
writeBin(c(charToRaw(p0[1]), charToRaw(b64), charToRaw(p0[2]), charToRaw(b64a), charToRaw(p1[2])),
         "results/diagnostics_dashboard.html")
cat(sprintf("Saved: results/diagnostics_dashboard.html  (%.1f s)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

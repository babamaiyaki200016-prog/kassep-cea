## ============================================================================
##  KASSEP cost-effectiveness analysis
##  01_model.R  --  parameters, decision model, one-way DSA, probabilistic SA
##
##  Cost-effectiveness of a community-based sample registration system with
##  verbal and social autopsy for maternal mortality surveillance in Kano
##  State, Nigeria.
##
##  All epidemiological inputs are UNWEIGHTED.
##  Run with:  Rscript R/01_model.R
## ============================================================================

set.seed(20260712)          # fixed so the PSA is exactly reproducible
N_PSA <- 10000L

## ---------------------------------------------------------------- parameters
## Every value below is read from data/parameters.json -- that file, not this
## script, is the single source of truth. (Household size SD, 3.66, appears
## only in prose elsewhere and is not a model input, so it is not read here.)
raw <- jsonlite::fromJSON("data/parameters.json")

params <- list(

  ## --- currency -------------------------------------------------------------
  fx           = raw$exchange_rate_ngn_per_usd,   # NGN per US$ (CBN NFEM, July 2026)
  gdp_pc       = raw$gdp_per_capita_usd,          # Nigeria GDP per capita, current US$, 2025
  wtp_multiple = raw$wtp_multiple,                # willingness to pay = 1 x GDP per capita

  ## --- annual recurrent OPERATING cost to Kano State (NGN) -------------------
  cost = unlist(raw$costs_ngn_per_year),

  ## --- capital and establishment --------------------------------------------
  vehicle      = raw$vehicle_capital_ngn,         # one-off vehicle purchase (NGN) -- NOT fuel
  veh_life     = raw$vehicle_useful_life_years,   # years
  estab_usd    = raw$establishment_investment_usd,# net establishment investment, actual (US$)
  transition   = raw$transition_cost_ngn,         # one-off State transition cost (NGN)
  estab_life   = raw$establishment_useful_life_years,  # years
  disc         = raw$discount_rate,               # discount rate

  ## --- epidemiology (UNWEIGHTED) --------------------------------------------
  households   = raw$epidemiology_unweighted$households_enumerated,  # baseline census
  hh_size      = raw$epidemiology_unweighted$mean_household_size,    # mean household size (SD 3.66)
  deliveries   = raw$epidemiology_unweighted$deliveries,
  stillbirths  = raw$epidemiology_unweighted$stillbirths,            # counted, but NOT cause-assigned by VASA
  mat_deaths   = raw$epidemiology_unweighted$maternal_deaths_cause_assigned,
  lgas         = raw$epidemiology_unweighted$lgas,

  ## --- ascertainment probabilities ------------------------------------------
  p_vasa       = raw$p_cause_assigned_kassep,      # P(cause assigned | maternal death), KASSEP
  p_crvs       = raw$p_cause_assigned_status_quo   # P(cause assigned | maternal death), status quo
)

## derived quantities
params$population   <- round(params$households * params$hh_size)   # 251,847
params$live_births  <- params$deliveries - params$stillbirths      #   9,928
params$vital_events <- params$deliveries + params$mat_deaths       #  11,199
params$mmr          <- params$mat_deaths / params$live_births * 1e5
params$wtp          <- params$wtp_multiple * params$gdp_pc         # US$1,223

## ---------------------------------------------------------------- helpers
annuity <- function(rate, n) {
  if (rate <= 0) return(as.numeric(n))
  (1 - (1 + rate)^(-n)) / rate
}
eac <- function(pv, rate, n) pv / annuity(rate, n)

## ---------------------------------------------------------------- base case
##  Single-cycle decision tree. Unit of analysis: one live birth in the
##  surveillance population. Costs attach at the strategy level, effects at
##  the terminal nodes.
base_case <- function(p = params,
                      cost       = NULL,
                      vehicle    = NULL,
                      veh_life   = NULL,
                      disc       = NULL,
                      estab_usd  = NULL,
                      fx         = NULL,
                      deaths     = NULL,
                      p_vasa     = NULL,
                      p_crvs     = NULL) {

  cost      <- if (is.null(cost))      p$cost      else cost
  vehicle   <- if (is.null(vehicle))   p$vehicle   else vehicle
  veh_life  <- if (is.null(veh_life))  p$veh_life  else veh_life
  disc      <- if (is.null(disc))      p$disc      else disc
  estab_usd <- if (is.null(estab_usd)) p$estab_usd else estab_usd
  fx        <- if (is.null(fx))        p$fx        else fx
  deaths    <- if (is.null(deaths))    p$mat_deaths else deaths
  p_vasa    <- if (is.null(p_vasa))    p$p_vasa    else p_vasa
  p_crvs    <- if (is.null(p_crvs))    p$p_crvs    else p_crvs

  operating <- sum(cost)
  veh_eac   <- eac(vehicle, disc, veh_life)
  govt      <- operating + veh_eac                       # NGN per year

  estab_ngn <- estab_usd * fx + p$transition
  estab_eac <- eac(estab_ngn, disc, p$estab_life)
  total     <- govt + estab_eac                          # NGN per year

  eff_kassep <- deaths * p_vasa      # deaths cause-assigned, KASSEP
  eff_status <- deaths * p_crvs      # deaths cause-assigned, status quo
  d_eff      <- eff_kassep - eff_status

  list(
    operating     = operating,
    veh_eac       = veh_eac,
    govt          = govt,
    estab_eac     = estab_eac,
    total         = total,
    eff_kassep    = eff_kassep,
    eff_status    = eff_status,
    d_eff         = d_eff,
    icer_govt_ngn = govt  / d_eff,
    icer_govt_usd = govt  / d_eff / fx,
    icer_tot_ngn  = total / d_eff,
    icer_tot_usd  = total / d_eff / fx,
    fx            = fx
  )
}

BC <- base_case()

## ---------------------------------------------------------------- DSA
icer <- function(...) base_case(...)$icer_govt_usd

scale_one <- function(key, f) { z <- params$cost; z[[key]] <- z[[key]] * f; z }

dsa <- data.frame(
  parameter = c(
    "Maternal deaths cause-assigned per year",
    "Personnel and field workforce cost",
    "Exchange rate",
    "Completeness of cause assignment, KASSEP",
    "Cause assignment under status-quo CRVS",
    "Vehicle useful life",
    "Field operations and VASA logistics cost",
    "Vehicle capital cost",
    "Discount rate",
    "Training and capacity building cost",
    "Technology and data infrastructure cost"
  ),
  low_label = c("45", "-25%", "N1,600", "0.80", "0.10",
                "8 yr", "-25%", "N60m", "0%", "-25%", "-25%"),
  high_label = c("150", "+25%", "N1,200", "1.00", "0",
                 "3 yr", "+25%", "N100m", "10%", "+25%", "+25%"),
  low = c(
    icer(deaths = 150),
    icer(cost = scale_one("Personnel and field workforce", 0.75)),
    icer(fx = 1600),
    icer(p_vasa = 1.00),
    icer(p_crvs = 0.00),
    icer(veh_life = 8),
    icer(cost = scale_one("Field operations and VASA logistics", 0.75)),
    icer(vehicle = 60000000),
    icer(disc = 1e-9),
    icer(cost = scale_one("Training and capacity building", 0.75)),
    icer(cost = scale_one("Technology and data infrastructure", 0.75))
  ),
  high = c(
    icer(deaths = 45),
    icer(cost = scale_one("Personnel and field workforce", 1.25)),
    icer(fx = 1200),
    icer(p_vasa = 0.80),
    icer(p_crvs = 0.10),
    icer(veh_life = 3),
    icer(cost = scale_one("Field operations and VASA logistics", 1.25)),
    icer(vehicle = 100000000),
    icer(disc = 0.10),
    icer(cost = scale_one("Training and capacity building", 1.25)),
    icer(cost = scale_one("Technology and data infrastructure", 1.25))
  ),
  stringsAsFactors = FALSE
)
dsa$width <- abs(dsa$high - dsa$low)
dsa <- dsa[order(-dsa$width), ]

## ---------------------------------------------------------------- PSA
## Costs      ~ Gamma, coefficient of variation 0.20 (strictly positive, skewed)
## Deaths     ~ Poisson(93)
## p_vasa     ~ Beta(94, 1)   : posterior for 93/93 successes, uniform prior
## p_crvs     ~ Beta(1, 199)  : mean 0.005
## Exchange   ~ lognormal, SD 10% on the log scale
rgamma_cv <- function(n, mean, cv = 0.20) {
  shape <- 1 / cv^2
  rgamma(n, shape = shape, scale = mean / shape)
}

psa_cost      <- sapply(params$cost, function(m) rgamma_cv(N_PSA, m))
psa_operating <- rowSums(psa_cost)
psa_vehicle   <- rgamma_cv(N_PSA, params$vehicle)
psa_veh_eac   <- psa_vehicle / annuity(params$disc, params$veh_life)
psa_govt      <- psa_operating + psa_veh_eac                       # NGN

psa_estab_ngn <- rgamma_cv(N_PSA, params$estab_usd * params$fx + params$transition)
psa_estab_eac <- psa_estab_ngn / annuity(params$disc, params$estab_life)
psa_total     <- psa_govt + psa_estab_eac

psa_deaths <- rpois(N_PSA, params$mat_deaths)
psa_pvasa  <- rbeta(N_PSA, params$mat_deaths + 1, 1)
psa_pcrvs  <- rbeta(N_PSA, 1, 199)
psa_deff   <- psa_deaths * psa_pvasa - psa_deaths * psa_pcrvs

psa_fx         <- rlnorm(N_PSA, meanlog = log(params$fx), sdlog = 0.10)
psa_cost_govt  <- psa_govt  / psa_fx                               # US$
psa_cost_total <- psa_total / psa_fx

psa_icer_govt  <- psa_cost_govt  / psa_deff
psa_icer_total <- psa_cost_total / psa_deff

## CEAC: net monetary benefit at willingness to pay lambda
wtp_grid   <- seq(0, 8000, by = 50)
ceac_govt  <- vapply(wtp_grid,
                     function(w) mean(psa_deff * w - psa_cost_govt  > 0), numeric(1))
ceac_total <- vapply(wtp_grid,
                     function(w) mean(psa_deff * w - psa_cost_total > 0), numeric(1))

p_ce_govt  <- mean(psa_deff * params$wtp - psa_cost_govt  > 0)
p_ce_total <- mean(psa_deff * params$wtp - psa_cost_total > 0)
inmb_govt  <- mean(psa_deff * params$wtp - psa_cost_govt)
inmb_total <- mean(psa_deff * params$wtp - psa_cost_total)

## Since the willingness-to-pay convention itself spans 1x-3x GDP per capita
## (see Methods), report probability-cost-effective at each exact multiple,
## not just the primary (1x) threshold -- so the reader sees the full range
## the literature actually contemplates, not just the conservative end.
gdp_multiples   <- c(1, 2, 3)
wtp_by_multiple <- gdp_multiples * params$gdp_pc
p_ce_govt_by_multiple  <- vapply(wtp_by_multiple,
  function(w) mean(psa_deff * w - psa_cost_govt  > 0), numeric(1))
p_ce_total_by_multiple <- vapply(wtp_by_multiple,
  function(w) mean(psa_deff * w - psa_cost_total > 0), numeric(1))
names(p_ce_govt_by_multiple)  <- paste0(gdp_multiples, "x")
names(p_ce_total_by_multiple) <- paste0(gdp_multiples, "x")

psa <- data.frame(
  d_eff      = psa_deff,
  cost_govt  = psa_cost_govt,
  cost_total = psa_cost_total,
  icer_govt  = psa_icer_govt,
  icer_total = psa_icer_total
)

## ---------------------------------------------------------------- break-even
## The ICER exceeds 1 x GDP per capita. These analyses answer: what would have
## to be true for it not to?

## (a) How many maternal deaths would KASSEP have to detect?
##     Only the VASA field-transport line (NGN 50,000 per event) varies with the
##     event count; everything else is fixed.
per_event_var <- 50000
fixed_cost    <- BC$govt - params$mat_deaths * per_event_var
breakeven_deaths <- fixed_cost / (params$wtp * params$fx - per_event_var)

## (b) How far would the annual cost have to fall?
breakeven_cost <- params$wtp * params$fx * params$mat_deaths
cost_cut_pct   <- 100 * (1 - breakeven_cost / BC$govt)

## (c) At what willingness to pay does KASSEP become cost-effective with
##     probability >= 50%?
wtp_50 <- wtp_grid[which.max(ceac_govt >= 0.5)]

## (d) ILLUSTRATIVE ONLY -- not a result of the primary analysis.
##     If acting on the surveillance data averted maternal deaths, how many
##     would have to be averted for the cost per DALY averted to fall below the
##     threshold? Assumes a fixed number of DALYs per maternal death averted.
dalys_per_death   <- c(25, 30, 35)
deaths_to_avert   <- (BC$govt / params$fx) / (params$wtp * dalys_per_death)
illustrative <- data.frame(
  dalys_per_death_averted = dalys_per_death,
  deaths_to_avert_per_year = round(deaths_to_avert, 1),
  pct_of_detected = round(100 * deaths_to_avert / params$mat_deaths, 1)
)

## ---------------------------------------------------------------- results
ci <- function(x) stats::quantile(x, c(0.025, 0.975), names = FALSE)

denoms <- c(
  "Maternal death cause-assigned" = params$mat_deaths,
  "Vital event registered"        = params$vital_events,
  "Live birth"                    = params$live_births,
  "Household covered"             = params$households,
  "Person covered"                = params$population,
  "LGA with local MMR"            = params$lgas
)

if (sys.nframe() == 0L || identical(environment(), globalenv())) {

  cat("\n================ COSTS (per year) ================\n")
  cat(sprintf("Recurrent operating      NGN %15s   US$%12s\n",
              format(round(BC$operating), big.mark = ","),
              format(round(BC$operating / params$fx), big.mark = ",")))
  cat(sprintf("Annualised vehicle       NGN %15s   US$%12s\n",
              format(round(BC$veh_eac), big.mark = ","),
              format(round(BC$veh_eac / params$fx), big.mark = ",")))
  cat(sprintf("TOTAL COST TO THE STATE  NGN %15s   US$%12s\n",
              format(round(BC$govt), big.mark = ","),
              format(round(BC$govt / params$fx), big.mark = ",")))
  cat(sprintf("Annualised establishment NGN %15s   US$%12s\n",
              format(round(BC$estab_eac), big.mark = ","),
              format(round(BC$estab_eac / params$fx), big.mark = ",")))
  cat(sprintf("TOTAL ECONOMIC COST      NGN %15s   US$%12s\n",
              format(round(BC$total), big.mark = ","),
              format(round(BC$total / params$fx), big.mark = ",")))

  cat("\n================ BASE CASE ================\n")
  cat(sprintf("ICER, State perspective     NGN %12s   US$%8s\n",
              format(round(BC$icer_govt_ngn), big.mark = ","),
              format(round(BC$icer_govt_usd), big.mark = ",")))
  cat(sprintf("ICER, economic perspective  NGN %12s   US$%8s\n",
              format(round(BC$icer_tot_ngn), big.mark = ","),
              format(round(BC$icer_tot_usd), big.mark = ",")))

  cat("\n---- cost per unit, other denominators (State perspective) ----\n")
  for (nm in names(denoms)) {
    n <- denoms[[nm]]
    cat(sprintf("  %-32s NGN %11s   US$%9.2f\n", nm,
                format(round(BC$govt / n), big.mark = ","),
                BC$govt / n / params$fx))
  }

  cat("\n================ ONE-WAY SENSITIVITY (tornado) ================\n")
  cat(sprintf("base case = US$%s\n\n", format(round(BC$icer_govt_usd), big.mark = ",")))
  for (i in seq_len(nrow(dsa))) {
    cat(sprintf("  %-42s %8s .. %8s\n", dsa$parameter[i],
                format(round(dsa$low[i]),  big.mark = ","),
                format(round(dsa$high[i]), big.mark = ",")))
  }

  cat("\n================ PROBABILISTIC SENSITIVITY ================\n")
  cat(sprintf("  simulations: %s\n", format(N_PSA, big.mark = ",")))
  cat(sprintf("  Incremental cost, State (US$/yr)   mean %s   95%% CrI %s to %s\n",
              format(round(mean(psa_cost_govt)), big.mark = ","),
              format(round(ci(psa_cost_govt)[1]), big.mark = ","),
              format(round(ci(psa_cost_govt)[2]), big.mark = ",")))
  cat(sprintf("  Incremental cost, econ  (US$/yr)   mean %s   95%% CrI %s to %s\n",
              format(round(mean(psa_cost_total)), big.mark = ","),
              format(round(ci(psa_cost_total)[1]), big.mark = ","),
              format(round(ci(psa_cost_total)[2]), big.mark = ",")))
  cat(sprintf("  Incremental effect (deaths/yr)     mean %.1f   95%% CrI %.1f to %.1f\n",
              mean(psa_deff), ci(psa_deff)[1], ci(psa_deff)[2]))
  cat(sprintf("  ICER, State (US$)   mean %s  median %s  95%% CrI %s to %s\n",
              format(round(mean(psa_icer_govt)), big.mark = ","),
              format(round(median(psa_icer_govt)), big.mark = ","),
              format(round(ci(psa_icer_govt)[1]), big.mark = ","),
              format(round(ci(psa_icer_govt)[2]), big.mark = ",")))
  cat(sprintf("  ICER, econ  (US$)   mean %s  median %s  95%% CrI %s to %s\n",
              format(round(mean(psa_icer_total)), big.mark = ","),
              format(round(median(psa_icer_total)), big.mark = ","),
              format(round(ci(psa_icer_total)[1]), big.mark = ","),
              format(round(ci(psa_icer_total)[2]), big.mark = ",")))

  cat(sprintf("\n  Willingness to pay = %g x GDP per capita = US$%s\n",
              params$wtp_multiple, format(params$wtp, big.mark = ",")))
  cat(sprintf("    P(cost-effective), State       %.1f%%\n", 100 * p_ce_govt))
  cat(sprintf("    P(cost-effective), economic    %.1f%%\n", 100 * p_ce_total))
  cat(sprintf("    INMB, State      US$%s per year\n",
              format(round(inmb_govt), big.mark = ",")))
  cat(sprintf("    INMB, economic   US$%s per year\n",
              format(round(inmb_total), big.mark = ",")))

  cat("\n  P(cost-effective) across the full 1x-3x GDP per capita convention:\n")
  for (i in seq_along(gdp_multiples)) {
    cat(sprintf("    %dx GDP (US$%s)   State %5.1f%%   economic %5.1f%%\n",
                gdp_multiples[i], format(round(wtp_by_multiple[i]), big.mark = ","),
                100 * p_ce_govt_by_multiple[i], 100 * p_ce_total_by_multiple[i]))
  }

  cat("\n================ BREAK-EVEN ANALYSIS ================\n")
  cat(sprintf("  The base-case ICER (US$%s) EXCEEDS the threshold (US$%s).\n",
              format(round(BC$icer_govt_usd), big.mark = ","),
              format(params$wtp, big.mark = ",")))
  cat(sprintf("  (a) Detection required : %.0f maternal deaths/yr (vs %d observed; %.2f-fold)\n",
              breakeven_deaths, params$mat_deaths, breakeven_deaths / params$mat_deaths))
  cat(sprintf("  (b) Cost reduction req : NGN %s (vs NGN %s) -- a %.0f%% cut\n",
              format(round(breakeven_cost), big.mark = ","),
              format(round(BC$govt), big.mark = ","), cost_cut_pct))
  cat(sprintf("  (c) WTP at which P(cost-effective) reaches 50%%: US$%s\n",
              format(wtp_50, big.mark = ",")))
  cat("  (d) ILLUSTRATIVE -- deaths that would need to be AVERTED for the cost\n")
  cat("      per DALY averted to fall below the threshold:\n")
  print(illustrative, row.names = FALSE)
  cat("\n")
}

## ---------------------------------------------------------------- save
if (!dir.exists("output")) dir.create("output", recursive = TRUE)
saveRDS(list(params = params, base = BC, dsa = dsa, psa = psa,
             wtp_grid = wtp_grid, ceac_govt = ceac_govt, ceac_total = ceac_total,
             p_ce_govt = p_ce_govt, p_ce_total = p_ce_total,
             inmb_govt = inmb_govt, inmb_total = inmb_total,
             wtp_by_multiple = wtp_by_multiple,
             p_ce_govt_by_multiple = p_ce_govt_by_multiple,
             p_ce_total_by_multiple = p_ce_total_by_multiple,
             breakeven_deaths = breakeven_deaths, breakeven_cost = breakeven_cost,
             cost_cut_pct = cost_cut_pct, wtp_50 = wtp_50,
             illustrative = illustrative),
        file = file.path("output", "results.rds"))
write.csv(dsa, file.path("output", "dsa_results.csv"), row.names = FALSE)

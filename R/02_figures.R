## ============================================================================
##  KASSEP cost-effectiveness analysis
##  02_figures.R  --  Figure 1 (decision tree), Figure 2 (tornado),
##                    Figure A1 (cost-effectiveness plane), Figure A2 (CEAC)
##
##  Figures carry no embedded titles: captions belong in the manuscript.
##  Run with:  Rscript R/02_figures.R   (after 01_model.R)
## ============================================================================

suppressPackageStartupMessages(library(ggplot2))

if (!exists("BC")) source(file.path("R", "01_model.R"))
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

INK   <- "#1A1A1A"; TEAL <- "#1F4E5F"; RUST <- "#B85C38"
GREY  <- "#7A7A7A"; PALE <- "#DDE7EB"; GREEN <- "#3A6247"

theme_paper <- function(base = 9) {
  theme_minimal(base_size = base, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = PALE, linewidth = 0.3),
      axis.line   = element_line(colour = INK, linewidth = 0.3),
      axis.ticks  = element_line(colour = INK, linewidth = 0.3),
      axis.title  = element_text(colour = INK, size = base - 0.4),
      axis.text   = element_text(colour = INK),
      legend.title = element_blank(),
      legend.key.height = unit(0.9, "lines"),
      legend.text = element_text(size = base - 1.4),
      plot.margin = margin(6, 10, 6, 6)
    )
}

p_death     <- params$mat_deaths / params$live_births
cost_per_lb <- BC$govt / params$fx / params$live_births
WTP         <- params$wtp

## ------------------------------------------------------------ FIGURE 1: TREE
fig1 <- function() {

  seg <- data.frame(x = numeric(), y = numeric(),
                    xend = numeric(), yend = numeric())
  add_seg <- function(x, y, xend, yend) {
    seg <<- rbind(seg, data.frame(x = x, y = y, xend = xend, yend = yend))
  }
  ## TreeAge-style bracket: stub, vertical spine, horizontal arms
  elbow <- function(x0, y0, x1, ys) {
    xm <- x0 + (x1 - x0) * 0.30
    add_seg(x0, y0, xm, y0)
    add_seg(xm, min(ys), xm, max(ys))
    for (y in ys) add_seg(xm, y, x1, y)
    xm
  }

  X0 <- 4; X1 <- 22; X2 <- 45; X3 <- 66
  nodes <- data.frame(x = numeric(), y = numeric(), type = character())
  terms <- data.frame(x = numeric(), y = numeric(),
                      lab = character(), eff = numeric())
  labs  <- data.frame(x = numeric(), y = numeric(), lab = character(),
                      col = character(), sz = numeric(), face = character(),
                      hj = numeric())

  addlab <- function(x, y, lab, col = INK, sz = 2.6,
                     face = "plain", hj = 0.5) {
    labs <<- rbind(labs, data.frame(x = x, y = y, lab = lab, col = col,
                                    sz = sz, face = face, hj = hj))
  }

  xm0 <- elbow(X0 + 1.4, 50, X1 - 1.3, c(74, 26))
  nodes <- rbind(nodes, data.frame(x = X0, y = 50, type = "decision"))
  addlab(X0, 44.5, "Strategy", INK, 2.9, "bold")

  arms <- list(
    list(top = 74, dth = 88, srv = 60, nm = "KASSEP sample registration system",
         cost = cost_per_lb, pa = params$p_vasa, up = TRUE,
         la = "Identified; VASA completed", ln = "Not ascertained"),
    list(top = 26, dth = 40, srv = 12, nm = "Status quo: existing CRVS",
         cost = 0, pa = params$p_crvs, up = FALSE,
         la = "Registered with cause", ln = "No cause assigned")
  )

  for (a in arms) {
    addlab((xm0 + X1) / 2 - 1, a$top + 2.6, a$nm,
           if (a$up) TEAL else GREY, 2.85, "bold")
    addlab((xm0 + X1) / 2 - 1, a$top - 3.8,
           sprintf("cost = US$%.2f per live birth", a$cost), INK, 2.5, "italic")
    nodes <- rbind(nodes, data.frame(x = X1, y = a$top, type = "chance"))

    xm1 <- elbow(X1 + 1.4, a$top, X2 - 1.3, c(a$dth, a$srv))
    addlab((xm1 + X2) / 2, a$dth + 1.6, "Maternal death", INK, 2.6)
    addlab((xm1 + X2) / 2, a$dth - 2.6, sprintf("p = %.4f", p_death), TEAL, 2.4, "italic")
    addlab((xm1 + X2) / 2, a$srv + 1.6, "No maternal death", GREY, 2.6)
    addlab((xm1 + X2) / 2, a$srv - 2.6, sprintf("p = %.4f", 1 - p_death), GREY, 2.4, "italic")

    nodes <- rbind(nodes, data.frame(x = X2, y = a$dth, type = "chance"))
    terms <- rbind(terms, data.frame(x = X2 + 1.5, y = a$srv,
                                     lab = "Woman survives", eff = 0))

    ya <- a$dth + 6.5; yn <- a$dth - 6.5
    xm2 <- elbow(X2 + 1.4, a$dth, X3 - 0.2, c(ya, yn))
    addlab((xm2 + X3) / 2, ya + 1.6, a$la, INK, 2.6)
    addlab((xm2 + X3) / 2, ya - 2.6, sprintf("p = %.2f", a$pa), TEAL, 2.4, "italic")
    addlab((xm2 + X3) / 2, yn + 1.6, a$ln, GREY, 2.6)
    addlab((xm2 + X3) / 2, yn - 2.6, sprintf("p = %.2f", 1 - a$pa), GREY, 2.4, "italic")

    terms <- rbind(terms,
      data.frame(x = X3, y = ya, lab = "Cause of death assigned",  eff = 1),
      data.frame(x = X3, y = yn, lab = "Cause of death unknown",   eff = 0))
  }

  tri <- do.call(rbind, lapply(seq_len(nrow(terms)), function(i) {
    data.frame(id = i,
               x = c(terms$x[i], terms$x[i] + 2.0, terms$x[i] + 2.0),
               y = c(terms$y[i], terms$y[i] + 1.4, terms$y[i] - 1.4),
               eff = terms$eff[i])
  }))

  ggplot() +
    geom_segment(data = seg, aes(x, y, xend = xend, yend = yend),
                 colour = GREY, linewidth = 0.35, lineend = "round") +
    ## expected-value box
    annotate("rect", xmin = 66, xmax = 98, ymin = 2, ymax = 19,
             fill = PALE, colour = TEAL, linewidth = 0.3) +
    annotate("text", x = 68, y = 16.2, hjust = 0, size = 2.9, fontface = "bold",
             colour = TEAL, label = "Expected values per live birth") +
    annotate("text", x = 68, y = 12.8, hjust = 0, size = 2.7, family = "mono",
             colour = INK,
             label = sprintf("KASSEP       US$%5.2f    %.5f",
                             cost_per_lb, p_death * params$p_vasa)) +
    annotate("text", x = 68, y = 10.0, hjust = 0, size = 2.7, family = "mono",
             colour = INK,
             label = sprintf("Status quo   US$ 0.00    %.5f",
                             p_death * params$p_crvs)) +
    annotate("segment", x = 68, xend = 96, y = 8, yend = 8,
             colour = TEAL, linewidth = 0.25) +
    annotate("text", x = 68, y = 5.0, hjust = 0, size = 3.3, fontface = "bold",
             colour = RUST,
             label = sprintf("ICER  US$%s",
                             format(round(BC$icer_govt_usd), big.mark = ","))) +
    annotate("text", x = 68, y = 2.8, hjust = 0, size = 2.5, fontface = "italic",
             colour = GREY, label = "per maternal death cause-assigned") +
    ## nodes
    geom_tile(data = subset(nodes, type == "decision"), aes(x, y),
              width = 2.6, height = 4.2, fill = "white", colour = INK, linewidth = 0.5) +
    geom_point(data = subset(nodes, type == "chance"), aes(x, y),
               shape = 21, size = 3.4, fill = "white", colour = TEAL, stroke = 0.5) +
    geom_polygon(data = tri, aes(x, y, group = id, fill = factor(eff)),
                 colour = RUST, linewidth = 0.4) +
    scale_fill_manual(values = c("0" = "white", "1" = RUST), guide = "none") +
    geom_text(data = terms, aes(x = x + 2.9, y = y + 1.0, label = lab),
              hjust = 0, size = 2.7, colour = INK) +
    geom_text(data = terms, aes(x = x + 2.9, y = y - 1.5,
                                label = paste0("effect = ", eff)),
              hjust = 0, size = 2.4, fontface = "italic", colour = GREY) +
    ## branch labels
    geom_text(data = labs, aes(x, y, label = lab, colour = I(col),
                               size = I(sz), fontface = face, hjust = hj)) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 100), expand = FALSE) +
    theme_void()
}
ggsave("figures/fig1_decision_tree.png", fig1(),
       width = 11, height = 6.4, dpi = 400, bg = "white")

## --------------------------------------------------------- FIGURE 2: TORNADO
fig2 <- function() {
  d <- dsa
  d$parameter <- factor(d$parameter, levels = rev(d$parameter))
  base <- BC$icer_govt_usd

  ## `low`/`high` are aligned to low_label/high_label (the parameter's own
  ## stated low/high value), NOT to which side of the base case is cheaper
  ## -- some parameters (detection rate, ascertainment completeness) have
  ## an inverse relationship with cost, so their low value can produce the
  ## *costlier* ICER. Determine the cheap/costly side per row from the
  ## actual computed ICERs, carrying the correct label along with each.
  d$cheap_icer   <- pmin(d$low, d$high)
  d$costly_icer  <- pmax(d$low, d$high)
  d$cheap_label  <- ifelse(d$low <= d$high, d$low_label, d$high_label)
  d$costly_label <- ifelse(d$low <= d$high, d$high_label, d$low_label)

  ggplot(d) +
    geom_rect(aes(ymin = as.numeric(parameter) - 0.32,
                  ymax = as.numeric(parameter) + 0.32,
                  xmin = cheap_icer, xmax = base),
              fill = TEAL, colour = "white", linewidth = 0.25) +
    geom_rect(aes(ymin = as.numeric(parameter) - 0.32,
                  ymax = as.numeric(parameter) + 0.32,
                  xmin = base, xmax = costly_icer),
              fill = RUST, colour = "white", linewidth = 0.25) +
    geom_text(aes(x = cheap_icer - 55, y = as.numeric(parameter),
                  label = cheap_label),
              hjust = 1, size = 2.4, colour = TEAL) +
    geom_text(aes(x = costly_icer + 55, y = as.numeric(parameter),
                  label = costly_label),
              hjust = 0, size = 2.4, colour = RUST) +
    geom_vline(xintercept = base, linetype = "dashed",
               colour = INK, linewidth = 0.4) +
    geom_vline(xintercept = WTP, colour = GREEN, linewidth = 0.5) +
    ## Labels are stacked vertically (not offset left/right of their line) so
    ## they never collide regardless of how close WTP and the base case sit
    ## to one another on the x-axis.
    annotate("text", x = base, y = nrow(d) + 1.35, hjust = 0.5, size = 2.6,
             fontface = "bold", colour = INK,
             label = sprintf("base case US$%s", format(round(base), big.mark = ","))) +
    annotate("text", x = WTP + 40, y = nrow(d) + 0.55, hjust = 0, size = 2.6,
             fontface = "bold", colour = GREEN,
             label = sprintf("willingness to pay = 1 x GDP per capita = US$%s",
                             format(WTP, big.mark = ","))) +
    scale_y_continuous(breaks = seq_len(nrow(d)), labels = levels(d$parameter),
                       expand = expansion(add = c(0.6, 2.0))) +
    scale_x_continuous(labels = scales::comma, expand = expansion(mult = 0.01)) +
    coord_cartesian(xlim = c(900, 4100)) +
    labs(x = "Cost per maternal death detected and cause-assigned (2026 US$)",
         y = NULL) +
    theme_paper() +
    theme(panel.grid.major.y = element_blank())
}
ggsave("figures/fig2_tornado.png", fig2(),
       width = 9.2, height = 5.0, dpi = 400, bg = "white")

## -------------------------------------------------------- FIGURE A1: CE PLANE
fig3 <- function() {
  ggplot(psa, aes(d_eff, cost_govt)) +
    geom_point(alpha = 0.15, size = 0.45, colour = TEAL) +
    geom_abline(aes(slope = WTP, intercept = 0, colour = "wtp"), linewidth = 0.55) +
    geom_abline(aes(slope = BC$icer_govt_usd, intercept = 0, colour = "icer"),
                linetype = "dashed", linewidth = 0.4) +
    geom_point(data = data.frame(x = BC$d_eff, y = BC$govt / params$fx),
               aes(x, y), shape = 23, size = 3, fill = RUST,
               colour = "white", stroke = 0.6) +
    scale_colour_manual(
      values = c(wtp = GREEN, icer = INK),
      labels = c(
        icer = sprintf("Base-case ICER = US$%s",
                       format(round(BC$icer_govt_usd), big.mark = ",")),
        wtp  = sprintf("Willingness to pay = US$%s (1 x GDP per capita)",
                       format(WTP, big.mark = ","))),
      breaks = c("wtp", "icer")) +
    scale_x_continuous(limits = c(0, 150), expand = expansion(mult = c(0, 0.01))) +
    scale_y_continuous(limits = c(0, 480000), labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    labs(x = "Incremental effect: maternal deaths cause-assigned per year",
         y = "Incremental cost (2026 US$ per year)") +
    theme_paper() +
    theme(legend.position = c(0.03, 0.97), legend.justification = c(0, 1))
}
ggsave("figures/fig3_ce_plane.png", fig3(),
       width = 7.0, height = 5.8, dpi = 400, bg = "white")

## ------------------------------------------------------------ FIGURE A2: CEAC
fig4 <- function() {
  ce <- rbind(
    data.frame(wtp = wtp_grid, p = ceac_govt,
               perspective = "Government (Kano State) perspective"),
    data.frame(wtp = wtp_grid, p = ceac_total,
               perspective = "Full economic perspective"))
  ce$perspective <- factor(ce$perspective,
    levels = c("Government (Kano State) perspective", "Full economic perspective"))

  ggplot(ce, aes(wtp, p, colour = perspective, linetype = perspective)) +
    geom_vline(xintercept = WTP, colour = GREEN, linewidth = 0.5) +
    geom_line(linewidth = 0.8) +
    annotate("point", x = WTP, y = p_ce_govt,  size = 1.8, colour = TEAL) +
    annotate("point", x = WTP, y = p_ce_total, size = 1.8, colour = RUST) +
    annotate("text", x = WTP + 250, y = p_ce_govt + 0.08, hjust = 0, size = 2.8,
             fontface = "bold", colour = TEAL,
             label = sprintf("%.1f%%", 100 * p_ce_govt)) +
    annotate("text", x = WTP + 250, y = p_ce_total + 0.20, hjust = 0, size = 2.8,
             fontface = "bold", colour = RUST,
             label = sprintf("%.1f%%", 100 * p_ce_total)) +
    annotate("text", x = WTP + 100, y = 0.55, hjust = 0, size = 2.7,
             fontface = "bold", colour = GREEN,
             label = sprintf("1 x GDP per capita\nUS$%s", format(WTP, big.mark = ","))) +
    scale_colour_manual(values = c(TEAL, RUST)) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_x_continuous(limits = c(0, 8000), labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_y_continuous(limits = c(0, 1.02), expand = expansion(mult = c(0, 0))) +
    labs(x = "Willingness to pay per maternal death cause-assigned (2026 US$)",
         y = "Probability cost-effective") +
    theme_paper() +
    theme(legend.position = c(0.97, 0.5), legend.justification = c(1, 0.5))
}
ggsave("figures/fig4_ceac.png", fig4(),
       width = 7.6, height = 4.9, dpi = 400, bg = "white")

cat("Figures written to figures/\n")

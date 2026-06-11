###########################################################################
# Mixed-design ANOVA re-analysis (response to reviewer)
#
# Reviewer asked: "Why t-tests instead of mixed design ANOVA with follow-up?"
#
# Design: 2 (Time: pre [A] / post [B], within-subjects)
#       x 4 (Session Group: 0/1/4/8 sessions, between-subjects)
# Outcomes: Total reading-comprehension score (7-35) + each item RC1-RC7 (1-5)
#
# Base R only. With a 2-level within factor, sphericity is automatically
# satisfied, so aov() with an Error(Subject/Time) term gives the exact
# mixed-ANOVA F-tests (no Greenhouse-Geisser correction needed).
#
# Run from the repository root:  Rscript ANOVA/mixed_anova_analysis.R
###########################################################################

results_dir <- "ANOVA/results"

d <- read.csv(
  "ANOVA Student Clinician Results Sept 2024/student-clinician-all-cleaned.csv",
  check.names = FALSE
)
names(d)[names(d) == "Session Group"] <- "Group"
d$Subject <- factor(d$Code)
d$Group   <- factor(d$Group, levels = c("0", "1", "4", "8"))

# ---- helper: reshape one outcome (pre col, post col) to long ----
to_long <- function(d, pre, post) {
  long <- rbind(
    data.frame(Subject = d$Subject, Group = d$Group, Time = "pre",  Score = d[[pre]]),
    data.frame(Subject = d$Subject, Group = d$Group, Time = "post", Score = d[[post]])
  )
  long$Time <- factor(long$Time, levels = c("pre", "post"))
  long
}

# ---- helper: run the 2x4 mixed ANOVA + generalized eta^2 ----
mixed_anova <- function(long, label) {
  fit <- aov(Score ~ Group * Time + Error(Subject / Time), data = long)
  s   <- summary(fit)

  # between-subjects stratum: Group
  btw <- s[["Error: Subject"]][[1]]
  # within-subjects stratum: Time, Group:Time
  wit <- s[["Error: Subject:Time"]][[1]]

  grab <- function(tab, row) {
    i <- grep(paste0("^", row, "$"), trimws(rownames(tab)))
    if (length(i) == 0) i <- grep(row, rownames(tab))[1]
    c(df1 = tab[i, "Df"], df2 = tab[nrow(tab), "Df"],
      F = tab[i, "F value"], p = tab[i, "Pr(>F)"],
      SS = tab[i, "Sum Sq"])
  }
  g_grp  <- grab(btw, "Group")
  g_time <- grab(wit, "Time")
  g_int  <- grab(wit, "Group:Time")

  # generalized eta^2 = SS_effect / (SS_effect + SS_error_terms)
  ss_err_btw <- btw[nrow(btw), "Sum Sq"]
  ss_err_wit <- wit[nrow(wit), "Sum Sq"]
  ges <- function(ss) ss / (ss + ss_err_btw + ss_err_wit)

  data.frame(
    Outcome = label,
    Effect  = c("Group (between)", "Time (within)", "Group x Time"),
    df1 = c(g_grp["df1"], g_time["df1"], g_int["df1"]),
    df2 = c(g_grp["df2"], g_time["df2"], g_int["df2"]),
    F   = round(c(g_grp["F"], g_time["F"], g_int["F"]), 3),
    p   = signif(c(g_grp["p"], g_time["p"], g_int["p"]), 4),
    ges = round(c(ges(g_grp["SS"]), ges(g_time["SS"]), ges(g_int["SS"])), 4),
    row.names = NULL
  )
}

# ---- run for Total + each item ----
outcomes <- c(
  list(c("TotalA", "TotalB")),
  lapply(1:7, function(i) c(paste0("RC", i, "A"), paste0("RC", i, "B")))
)
labels <- c("Total", paste("Survey Item", 1:7))   # RC1-RC7 displayed as Survey Items

omnibus <- do.call(rbind, Map(function(o, l) mixed_anova(to_long(d, o[1], o[2]), l),
                              outcomes, labels))
cat("=========== OMNIBUS 2x4 MIXED ANOVA ===========\n")
print(omnibus, row.names = FALSE)
write.csv(omnibus, file.path(results_dir, "mixed_anova_omnibus.csv"), row.names = FALSE)

###########################################################################
# FOLLOW-UP TESTS (recommended structure)
#
# The hypothesis-relevant term is the Group x Time interaction: does the
# pre->post gain depend on number of sessions? Decompose it two ways:
#
#  (1) Simple effect of Time within each Group  -> paired t (pre vs post),
#      Holm-corrected across the 4 groups. = "who improved?"
#  (2) The dose question: is the GAIN (post-pre) monotonically related to
#      session count? Test a linear trend across ordered 0<1<4<8 using the
#      actual (unequally spaced) session counts as the predictor of gain.
###########################################################################

session_n <- c("0" = 0, "1" = 1, "4" = 4, "8" = 8)

followups <- function(d, pre, post, label) {
  gain <- d[[post]] - d[[pre]]

  # (1) simple effect of Time within each group (paired t)
  st <- do.call(rbind, lapply(levels(d$Group), function(g) {
    idx <- d$Group == g
    tt  <- t.test(d[[post]][idx], d[[pre]][idx], paired = TRUE)
    data.frame(Group = g, n = sum(idx),
               mean_gain = round(mean(gain[idx]), 3),
               t = round(unname(tt$statistic), 3),
               df = unname(tt$parameter),
               p_raw = signif(tt$p.value, 4))
  }))
  st$p_holm <- signif(p.adjust(st$p_raw, "holm"), 4)

  # (2) linear trend of gain on session count (dose-response)
  dose <- session_n[as.character(d$Group)]
  trend <- summary(lm(gain ~ dose))$coefficients["dose", ]

  list(
    simple = data.frame(Outcome = label, st, row.names = NULL),
    trend  = data.frame(Outcome = label,
                        slope_per_session = round(trend["Estimate"], 4),
                        t = round(trend["t value"], 3),
                        p = signif(trend["Pr(>|t|)"], 4),
                        row.names = NULL)
  )
}

fu <- Map(function(o, l) followups(d, o[1], o[2], l), outcomes, labels)
simple_tab <- do.call(rbind, lapply(fu, `[[`, "simple"))
trend_tab  <- do.call(rbind, lapply(fu, `[[`, "trend"))

cat("\n=========== FOLLOW-UP 1: Time within each Group (paired t, Holm) ===========\n")
print(simple_tab, row.names = FALSE)
cat("\n=========== FOLLOW-UP 2: Dose-response (linear trend of gain on #sessions) ===========\n")
print(trend_tab, row.names = FALSE)

write.csv(simple_tab, file.path(results_dir, "followup_simple_effects.csv"), row.names = FALSE)
write.csv(trend_tab,  file.path(results_dir, "followup_dose_trend.csv"),     row.names = FALSE)

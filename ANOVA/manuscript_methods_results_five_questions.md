# Method and Results

## Method

### Statistical Analysis

Reading-comprehension performance was analyzed with a 2 (Time: pre-session, post-session; within-subjects) × 4 (Session Group: 0, 1, 4, or 8 sessions; between-subjects) mixed-design analysis of variance (ANOVA). The comprehension measure comprised five survey items (Survey Items 1–4 and 7); Survey Items 5 and 6 were excluded from all analyses [reason for exclusion to be supplied by the authors]. The primary analysis was conducted on the total comprehension score (the sum of the five items; range 5–25); parallel analyses were conducted on each individual item (Survey Items 1–4 and 7; range 1–5) to characterize item-level effects. Because the within-subjects factor (Time) comprised only two levels, the sphericity assumption was satisfied by definition, and no correction to the degrees of freedom was applied. Effect sizes are reported as generalized eta-squared (η²G), the recommended index for designs that combine within- and between-subjects factors.

Significant Group × Time interactions were decomposed with two sets of follow-up tests. First, the simple effect of Time was examined within each session group using paired-samples t-tests, with Holm correction applied across the four groups. Second, the relationship between intervention dose and improvement was evaluated by testing the linear trend of pre-to-post gain (post − pre) across the ordered session counts, complemented by Tukey-HSD pairwise comparisons of the gain scores. The session groups did not differ at baseline (pre-session total score: F(3,78) = 0.90, p = .45); follow-up comparisons were therefore computed on gain scores, which directly index the differential change captured by the Group × Time interaction. All analyses were conducted in R (version 4.5), with α = .05.

## Results

### Total Comprehension Score

The mixed-design ANOVA revealed a large main effect of Time, F(1,78) = 147.13, p \< .001, η²G = .401, reflecting an increase in comprehension scores from pre- to post-session across the sample. This effect was qualified by a significant Group × Time interaction, F(3,78) = 2.89, p = .041, η²G = .038. The main effect of Session Group did not reach significance, F(3,78) = 2.70, p = .051, η²G = .063. The interaction is displayed in Figure 1.

Follow-up tests localized the interaction. The simple effect of Time was significant within every session group (all Holm-corrected p \< .01), indicating reliable pre-to-post improvement across conditions. The magnitude of that improvement increased with dose: mean total gains were 3.79, 3.15, 4.00, and 6.00 points for the 0-, 1-, 4-, and 8-session groups, respectively. Consistent with a graded dose–response relationship, the linear trend across session counts was significant, with each additional session associated with a 0.33-point increase in total gain, t(80) = 2.73, p = .008 (Figure 2).

Tukey-HSD comparisons of the gain scores indicated that no single pairwise contrast between groups survived correction for multiple comparisons (all p \> .05), even though the 8-session group showed the numerically largest gain. The dose effect is therefore best characterized as a gradual, monotonic increase in improvement with additional sessions rather than as a discrete difference between specific groups, with the strongest gains observed in the 8-session condition.

### Item-Level Analyses

The main effect of Time was significant for all five items (all p \< .001), indicating improvement on every component of the measure. The Group × Time interaction was significant for Survey Item 1, F(3,78) = 3.39, p = .022, and Survey Item 4, F(3,78) = 3.84, p = .013, and non-significant for Survey Items 2, 3, and 7. The dose effect was most pronounced for Survey Item 4, for which gain increased by 0.13 points per session (p = .001); Survey Item 4 also showed a significant main effect of Session Group, F(3,78) = 8.78, p \< .001. Item-level interactions are shown in Figure 3, and full ANOVA results are reported in Table 1.

------------------------------------------------------------------------

**Table 1.** *Mixed-design ANOVA results for total and item-level comprehension scores.*

| Outcome | Time *F*(1,78) | *p* | η²G | Group *F*(3,78) | *p* | η²G | Group × Time *F*(3,78) | *p* | η²G |
|----|----|----|----|----|----|----|----|----|----|
| Total | 147.13 | \< .001 | .401 | 2.70 | .051 | .063 | 2.89 | .041 | .038 |
| Survey Item 1 | 52.44 | \< .001 | .217 | 0.71 | .548 | .016 | 3.39 | .022 | .051 |
| Survey Item 2 | 184.22 | \< .001 | .454 | 0.35 | .789 | .009 | 0.68 | .568 | .009 |
| Survey Item 3 | 65.53 | \< .001 | .249 | 1.86 | .143 | .042 | 1.24 | .302 | .018 |
| Survey Item 4 | 72.82 | \< .001 | .302 | 8.78 | \< .001 | .153 | 3.84 | .013 | .064 |
| Survey Item 7 | 54.47 | \< .001 | .215 | 2.84 | .043 | .062 | 1.61 | .195 | .024 |

*Note.* η²G = generalized eta-squared. Total = sum of Survey Items 1–4 and 7 (range 5–25).

**Figure 1.** Mean total comprehension score at pre- and post-session for each session group. **Figure 2.** Mean pre-to-post gain in total comprehension score as a function of number of sessions. **Figure 3.** Pre- to post-session change by session group for the total score and each individual survey item (Survey Items 1–4 and 7).

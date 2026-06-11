# Method and Results

## Method

### Statistical Analysis

Reading-comprehension performance was analyzed with a 2 (Time: pre-session,
post-session; within-subjects) × 4 (Session Group: 0, 1, 4, or 8 sessions;
between-subjects) mixed-design analysis of variance (ANOVA). The primary analysis was
conducted on the total comprehension score (range 7–35); parallel analyses were conducted
on each of the seven individual survey items (Survey Items 1–7; range 1–5) to characterize
item-level effects. Because the within-subjects factor (Time) comprised only two levels, the
sphericity assumption was satisfied by definition, and no correction to the degrees of
freedom was applied. Effect sizes are reported as generalized eta-squared (η²G), the
recommended index for designs that combine within- and between-subjects factors.

Significant Group × Time interactions were decomposed with two sets of follow-up tests.
First, the simple effect of Time was examined within each session group using
paired-samples t-tests, with Holm correction applied across the four groups. Second, the
relationship between intervention dose and improvement was evaluated by testing the linear
trend of pre-to-post gain (post − pre) across the ordered session counts, complemented by
Tukey-HSD pairwise comparisons of the gain scores. Follow-up comparisons were computed on
gain scores rather than post-session scores because the session groups differed at
baseline (pre-session total score: F(3,78) = 4.70, p = .005); using change scores removes
this baseline difference from the comparison. All analyses were conducted in R (version
4.5.0), with α = .05.

## Results

### Total Comprehension Score

The mixed-design ANOVA revealed a large main effect of Time, F(1,78) = 249.85, p < .001,
η²G = .518, reflecting an increase in comprehension scores from pre- to post-session across
the sample. This effect was qualified by a significant Group × Time interaction,
F(3,78) = 7.47, p < .001, η²G = .088, and accompanied by a main effect of Session Group,
F(3,78) = 4.40, p = .007, η²G = .101. The interaction is displayed in Figure 1.

Follow-up tests localized the interaction. The simple effect of Time was significant within
every session group (all Holm-corrected p < .01), indicating reliable pre-to-post
improvement across conditions. The magnitude of that improvement, however, varied
systematically with dose: mean total gains were 4.71, 4.15, 9.14, and 9.58 points for the
0-, 1-, 4-, and 8-session groups, respectively. The linear trend was significant, with each
additional session associated with a 0.66-point increase in total gain, t(80) = 4.02,
p < .001 (Figure 2).

Tukey-HSD comparisons of the gain scores clarified the form of this dose–response
relationship. The 4- and 8-session groups each improved significantly more than the 0- and
1-session groups (all p < .02), whereas neither the 0- versus 1-session contrast nor the
4- versus 8-session contrast was significant. This pattern is consistent with a threshold
effect: approximately four sessions were required to produce gains beyond those associated
with no or minimal intervention, with little additional benefit from further sessions.

### Item-Level Analyses

The main effect of Time was significant for all seven items (all p < .05), indicating
improvement on every component of the measure. The Group × Time interaction was significant
for Survey Item 1, F(3,78) = 3.39, p = .022; Survey Item 4, F(3,78) = 3.84, p = .013; and
Survey Item 6, F(3,78) = 2.82, p = .044; and non-significant for Survey Items 2, 3, 5, and
7. The dose effect was most pronounced for Survey Item 4, for which gain increased by 0.13
points per session (p = .001). Survey Item 5 showed reliable improvement only in the
8-session group. Item-level interactions are shown in Figure 3, and full ANOVA results are
reported in Table 1.

---

**Table 1.** *Mixed-design ANOVA results for total and item-level comprehension scores.*

| Outcome | Time *F*(1,78) | *p* | η²G | Group *F*(3,78) | *p* | η²G | Group × Time *F*(3,78) | *p* | η²G |
|---|---|---|---|---|---|---|---|---|---|
| Total | 249.85 | < .001 | .518 | 4.40 | .007 | .101 | 7.47 | < .001 | .088 |
| Survey Item 1 | 52.44 | < .001 | .217 | 0.71 | .548 | .016 | 3.39 | .022 | .051 |
| Survey Item 2 | 184.22 | < .001 | .454 | 0.35 | .789 | .009 | 0.68 | .568 | .009 |
| Survey Item 3 | 65.53 | < .001 | .249 | 1.86 | .143 | .042 | 1.24 | .302 | .018 |
| Survey Item 4 | 72.82 | < .001 | .302 | 8.78 | < .001 | .153 | 3.84 | .013 | .064 |
| Survey Item 5 | 4.37 | .040 | .021 | 6.83 | < .001 | .139 | 2.02 | .118 | .029 |
| Survey Item 6 | 71.51 | < .001 | .280 | 1.20 | .314 | .026 | 2.82 | .044 | .044 |
| Survey Item 7 | 54.47 | < .001 | .215 | 2.84 | .043 | .062 | 1.61 | .195 | .024 |

*Note.* η²G = generalized eta-squared.

**Figure 1.** Mean total comprehension score at pre- and post-session for each session group.
**Figure 2.** Mean pre-to-post gain in total comprehension score as a function of number of sessions.
**Figure 3.** Pre- to post-session change by session group for each individual survey item (Survey Items 1–7).

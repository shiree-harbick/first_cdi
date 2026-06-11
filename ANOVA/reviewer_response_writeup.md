# Mixed-design ANOVA re-analysis — Methods & Results text

## Methods (revised statistical analysis paragraph)

Reading-comprehension performance was analyzed with a 2 (Time: pre-session, post-session;
within-subjects) × 4 (Session Group: 0, 1, 4, or 8 sessions; between-subjects)
mixed-design analysis of variance (ANOVA), conducted on the total score (range 7–35) and,
secondarily, on each of the seven individual items (RC1–RC7; range 1–5). Because the
within-subjects factor had only two levels, the sphericity assumption is satisfied by
definition and no Greenhouse–Geisser correction was required. Effect sizes are reported as
generalized eta-squared (η²G), the recommended index for mixed designs. A significant
Group × Time interaction was decomposed with two planned follow-up analyses: (a) the simple
effect of Time within each session group (paired-samples t-tests, Holm-corrected across the
four groups), and (b) a test of the dose–response relationship between the number of
sessions and the magnitude of pre-to-post gain (linear trend across the ordered session
counts), supplemented by Tukey-HSD pairwise comparisons of the gain scores. Pairwise
comparisons were performed on change (post − pre) scores rather than post-session scores
because the session groups differed at baseline (pre-session total: F(3,78) = 4.70,
p = .005); change scores remove this baseline confound. Analyses were performed in R 4.5.0.

## Results (revised paragraph)

The mixed-design ANOVA on total reading-comprehension scores yielded a large main effect of
Time, F(1,78) = 249.85, p < .001, η²G = .518: scores increased from pre- to post-session
across the sample. This was qualified by a significant Group × Time interaction,
F(3,78) = 7.47, p < .001, η²G = .088, together with a main effect of Session Group,
F(3,78) = 4.40, p = .007, η²G = .101. (The Group × Time interaction is algebraically
equivalent to the one-way ANOVA on change scores reported previously, F(3,78) = 7.47,
p < .001, confirming that the original and revised analyses agree.)

Follow-up tests localized the interaction. The simple effect of Time was significant in every
group (Holm-corrected p < .01), indicating reliable pre-to-post improvement regardless of
condition; however, the magnitude of improvement increased with the number of sessions
(mean total gain = 4.71, 4.15, 9.14, and 9.58 points for the 0-, 1-, 4-, and 8-session
groups, respectively). The dose–response trend was significant, with each additional session
associated with a 0.66-point increase in total gain, t(80) = 4.02, p < .001. Tukey-HSD
comparisons of the gain scores showed that the 4- and 8-session groups each improved
significantly more than the 0- and 1-session groups (all p < .02), whereas the 0- vs. 1-session
and 4- vs. 8-session contrasts were not significant. This pattern indicates a threshold
effect: a minimum of approximately four sessions was required to produce gains beyond those
observed with no or minimal intervention.

At the item level, the main effect of Time was significant for all seven items (all p < .05),
confirming improvement on every component of the measure. The Group × Time interaction was
significant for RC1 (F(3,78) = 3.39, p = .022), RC4 (F(3,78) = 3.84, p = .013), and RC6
(F(3,78) = 2.82, p = .044), and non-significant for RC2, RC3, RC5, and RC7. The dose effect
was strongest for RC4 (0.13 points per session, p = .001), consistent with the large
between-group differences on this item reported previously. RC5 improved reliably only in
the 8-session group.

## Omnibus table (for Table X)

| Outcome | Time F(1,78) | Time p | η²G | Group F(3,78) | Group p | η²G | Group×Time F(3,78) | G×T p | η²G |
|---|---|---|---|---|---|---|---|---|---|
| Total | 249.85 | <.001 | .518 | 4.40 | .007 | .101 | 7.47 | <.001 | .088 |
| RC1 | 52.44 | <.001 | .217 | 0.71 | .548 | .016 | 3.39 | .022 | .051 |
| RC2 | 184.22 | <.001 | .454 | 0.35 | .789 | .009 | 0.68 | .568 | .009 |
| RC3 | 65.53 | <.001 | .249 | 1.86 | .143 | .042 | 1.24 | .302 | .018 |
| RC4 | 72.82 | <.001 | .302 | 8.78 | <.001 | .153 | 3.84 | .013 | .064 |
| RC5 | 4.37 | .040 | .021 | 6.83 | <.001 | .139 | 2.02 | .118 | .029 |
| RC6 | 71.51 | <.001 | .280 | 1.20 | .314 | .026 | 2.82 | .044 | .044 |
| RC7 | 54.47 | <.001 | .215 | 2.84 | .043 | .062 | 1.61 | .195 | .024 |

η²G = generalized eta-squared.

## ESMARConf2023
## Tutorial on R package netmeta
## Guido Schwarzer, Freiburg

##
## (1) Make R package netmeta available
##

## install.packages("netmeta") # official CRAN version
## remotes::install_github("guido-s/netmeta", ref = "develop")
library("netmeta")
help("netmeta-package")
## vignette("netmeta") # not yet available


##
## (2) Data from network meta-analysis on antithrombotics to prevent
##     strokes
##
## P: patients with non-valvular atrial fibrillation
## I: apixaban, aspirin, aspirin+clopidogtrel, dabigatran 110mg,
##    dabigatran 150mg, rivaroxaban, vitamin K antagonists (VKAs),
##    placebo/control
## O: stroke
##

data("Dogliotti2014")
subset(Dogliotti2014, study %in% c("AFASAK-I 1989", "BAATAF 1990"))


##
## (3) Transform data from long arm-based format to contrast-based
##     format
##

## Mandatory arguments:
## - 'treat', 'n' and 'event' (for binary outcomes)
## - 'studlab' (for long arm-based format)
## 
pw1 <- pairwise(treat = treatment, n = total, event = stroke,
  studlab = study, data = Dogliotti2014, sm = "OR")

suppressMessages(library("dplyr"))
## TE = log odds ratio, seTE = SE(log odds ratio)
subset(pw1, study %in% c("AFASAK-I 1989", "BAATAF 1990")) %>%
  select(studlab, TE, seTE, treat1, event1, n1, treat2, event2, n2)

## Two-arm study: 1 pairwise comparison 
## Three-arm study: 3 pairwise comparisons
## ...
data.frame(n.arms = 2:6, n.comps = choose(2:6, 2))

## Keep only comparisons to the study-specific reference ('basic parameters')
## Here: Placebo/Control if available
pw1.drp <- pairwise(treat = treatment, n = total, event = stroke,
  studlab = study, data = Dogliotti2014, sm = "OR",
  reference = "plac", keep.all.comparisons = FALSE)
subset(pw1.drp, study %in% c("AFASAK-I 1989", "BAATAF 1990")) %>%
  select(studlab, TE, seTE, treat1, treat2)


##
## (4) Conduct random effects network meta-analysis
##

## Print estimates and confidence limits with two digits
settings.meta(digits = 2)

## Network meta-analysis
## - only consider random effect model (argument 'common')
## - define Placebo/Control as reference (argument 'reference.group')
## - number of strokes should be small (argument 'small.values'),
##   needed for ranking of treatments
##
net1 <- netmeta(pw1, common = FALSE, ref = "plac",
  small.values = "desirable")

## Print data for WASPO study
subset(pw1, study == "WASPO, 2007") %>%
  select(studlab, TE, seTE, event1, n1, event2, n2)

## Print results of network meta-analysis
net1
forest(net1)


##
## (5) Network graph
##

netgraph(net1)
##
netgraph(net1, seq = "optimal")
##
netgraph(net1, seq = "optimal", number.of.studies = TRUE)
##
netgraph(net1, seq = "optimal", number.of.studies = TRUE,
  plastic = FALSE, cex.points = n.trts,
  labels = paste0(trts, "\n(n=", round(n.trts), ")"))
##
netgraph(net1, seq = "optimal", number.of.studies = TRUE,
  plastic = FALSE, cex.points = n.trts,
  labels = paste0(trts, "\n(n=", round(n.trts), ")"),
  offset = ifelse(trts == "VKAs", 0.08, 0.05))

## Modify current setting for position of treatment labels
## (argument 'offset')
tmp <- netgraph(net1)
names(tmp)
names(tmp$nodes)
tmp$nodes$offset.x
netgraph(net1,
  offset =
    cbind(tmp$nodes$offset.x, tmp$nodes$offset.y + 0.1))
netgraph(net1,
  offset =
    cbind(tmp$nodes$offset.x + 0.1, tmp$nodes$offset.y))


##
## (6) Ranking of treatments
##

set.seed(1909) # reproducible results
rankogram(net1) # value for argument 'small.values' defined in net1

## Cumulative ranking probabilities
set.seed(1909)
rg1 <- rankogram(net1, cumulative = TRUE)
rg1
plot(rg1)

## Surface under the cumulative ranking curve (SUCRA)
netrank(rg1) # same result: netrank(net1, method = "SUCRA")

## P-score (ranking without resampling, same interpretation as SUCRA)
netrank(net1)

## Forest plot sorted by SUCRA
set.seed(1909)
forest(net1,
  sortvar = -sucra, digits.prop = 4,
  rightcols = c("effect", "ci", "sucra"),
  reference = "Dabigatran 150mg", baseline = FALSE,
  drop.reference.group = FALSE,
  smlab = "Odds ratio for stroke\n(Dabigatran 150mg vs other)",
  label.left = "Favours Dabigatran 150mg",
  label.right = "Favours other treatment",
  col.label.left = "green", col.label.right = "red")


##
## (7) League table
##

## Save league table as Excel file
netleague(net1, path = "league1.xlsx")

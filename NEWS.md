---
title: "NEWS"
output: html_document
---

## incidentally 1.0.3

* fixed bug in `incidence.from.congress()` when specifying policy areas, h/t Kevin Reuning 
* fixed links in vignettes, h/t Kevin Reuning 
* added `space()` to examine the space of binary matrices with fixed marginals
* `incidence_from_congress()` adds a node color attribute to legislator nodes corresponding to party affiliation
* fixed XML parsing errors in `incidence_from_congress()`
* corrected usage of \link{} in documentation

## incidentally 1.0.2

* added reference to Neal (2022) for incidence.from.congress()
* added reference to Neal (2023) for incidence.from.adjacency()
* `incidence.from.congress()` includes bill attributes indicating the number of (co-)sponsors from each party
* fixed minor bugs introduced by `igraph 1.4.0`

## incidentally 1.0.1

* fixed bug in `incidence.from.congress()` when bills do not have a sponsor
* `incidence.from.congress()` excludes non-voting members by default
* `incidence.from.congress()` includes a bill attribute indicating the bill's sponsor's party
* renamed `group' model as `club` model, and renamed `blau` model as `organizations` model in `incidence.from.adjacency()`

## incidentally 1.0.0

* all functions revised, all documentation updated
* added `incidence.from.congress()` to generate incidence matrices of bill sponsorship in the US Congress

## incidentally 0.9.0

* initial release
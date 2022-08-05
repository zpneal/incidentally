---
title: "NEWS"
output: html_document
---

## incidentally 1.0.1

* fixed bug in `incidence.from.congress()` when bills do not have a sponsor
* `incidence.from.congress()` excludes non-voting members by default
* `incidence.from.congress()` includes a bill attribute indicating the bill's sponsor's party
* renamed `group' model as `club' model, and renamed `blau' model as `organizations' model in `incidence.from.adjacency()`

## incidentally 1.0.0

* all functions revised, all documentation updated
* added `incidence.from.congress()` to generate incidence matrices of bill sponsorship in the US Congress

## incidentally 0.9.0

* initial release
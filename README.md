# EpiStats
Package R for epidemiologists

## 2023-10-25 CRAN Release 1.6-2

- Adding references in the Description file
- Fixing GitHub issues 4 to 10:
    + CC and CS now recognised variable names when inserted in the form of an object
    + CCTable and CSTable have now more appropriate columnnames in the table output
    + CC, CS, CCInter, CSInter, CCTable, CSTable now works or at least return a detailed error message if
      - variables are not binary
      - include only 1s or 0s
      - return zero cells in the two-by-two epi tables

## 2023-09-19 CRAN Release 1.6-1

- Fixing previous version of EpiStats to make it available on the CRAN again

## 2021-06-07 CRAN Release 1.5-1

- Was archived on 2023-01-09 as issues were not corrected in time

## 2020-04-15 CRAN Release 1.4-1

## 2019-05-25 CRAN Release 1.3-1

## 2018-09-18 Release 1.2

- Function crossTable added

## 2018-06-05 Release 1.1

- Fixed bug for CSInter (no output)
- Option "table" for CCInter and CSInter

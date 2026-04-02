# COBOL Record Validator

A small practice COBOL assignment inspired by a Durham College course exercise. This version has been redesigned and rewritten with permission to protect academic integrity while providing a safe, public example of structured COBOL programming. Assignement has been altered using Microsoft Copilot

---

## Overview

This COBOL program simulates a classic mainframe EDIT/validation routine. It reads fixed‑length input records, validates each field, calculates an average score for valid records, and routes invalid data to a separate exception file.

The project demonstrates core batch‑processing concepts: file handling, field‑level validation, derived‑value calculation, and summary reporting.

---

## Input Layout (15 bytes)

| Field     | Columns | Type    |
|-----------|---------|---------|
| ID        | 1–5     | Numeric |
| SCORE-1   | 6–8     | Numeric |
| SCORE-2   | 9–11    | Numeric |
| SCORE-3   | 12–14   | Numeric |
| CATEGORY  | 15      | A/B/C   |

---

## Validation Rules

A record is valid only if:

- ID is numeric  
- All scores are numeric and between 0–100  
- CATEGORY is A, B, or C  

Invalid records are written to **ERROR-OUT** with an error code:

| Code | Meaning          |
|------|------------------|
| 01   | Invalid ID       |
| 02   | Invalid score    |
| 03   | Invalid category |

---

## Processing

For valid records:

- Convert scores to numeric  
- Compute the average (rounded)  
- Write formatted output to **VALID-OUT**  

Example valid output:  
`12345A085`

Example error output:  
`1234X090080A03`

---

## Summary Output

At end of run, the program prints:

- Total records  
- Valid records  
- Invalid records  

---

## Files in This Repo

- `record-validator.cbl` — main COBOL program  
- `input.txt` — sample input  
- `valid-out.txt` — sample valid output  
- `error-out.txt` — sample error output  
- `README.md` — project documentation

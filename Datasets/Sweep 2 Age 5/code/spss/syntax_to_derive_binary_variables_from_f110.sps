compute Remarks=-3.
if (f110 ge 1)Remarks=0.
if (f110=1 or f110=3 or f110=5 or f110=7 or f110=9 or f110=11 or f110=13 or f110=15 or f110=17 or f110=19 or f110=21 or f110=23 or f110=27 or f110=31 or f110=33 or f110=35 or f110=37 or f110=39 or f110=41 or f110=43 or f110=45 or f110=47 or f110=49)Remarks=1. 
compute Encourag=-3.
if (f110 ge 1)Encourag=0.
if (f110=2 or f110=3 or f110=6 or f110=7 or f110=10 or f110=11 or f110=14 or f110=15 or f110=18 or f110=19 or f110=22 or f110=23 or f110=26 or f110=27 or f110=30 or f110=31 or f110=34 or f110=35 or f110=38 or f110=39 or f110=42 or f110=43 or f110=46 or f110=47 or f110=50)Encourag=1.
compute Interup=-3.
if (f110 ge 1)Interup=0.
if (f110=4 or f110=5 or f110=6 or f110=7 or f110=12 or f110=13 or f110=14 or f110=15 or f110=20 or f110=21 or f110=22 or f110=23 or f110=28 or f110=30 or f110=31 or f110=36 or f110=37 or f110=38 or f110=39 or f110=44 or f110=45 or f110=46 or f110=47 or f110=52)Interup=1.
compute Othnoise=-3.
if (f110 ge 1)Othnoise=0.
if (f110=8 or f110=9 or f110=10 or f110=11 or f110=12 or f110=13 or f110=14 or f110=15 or f110=24 or f110=26 or f110=27 or f110=28 or f110=30 or f110=31 or f110=40 or f110=41 or f110=42 or f110=43 or f110=44 or f110=45 or f110=46 or f110=47 or f110=56)Othnoise=1.
compute Goodcond=-3.
if (f110 ge 1)Goodcond=0.
if (f110=16 or f110=17 or f110=18 or f110=19 or f110=20 or f110=21 or f110=22 or f110=23 or f110=24 or f110=26 or f110=27 or f110=28 or f110=30 or f110=31 or f110=48 or f110=49 or f110=50 or f110=52 or f110=56)Goodcond=1.
compute Othsitn=-3.
if (f110 ge 1)Othsitn=0.
if (f110 ge 32)Othsitn=1.

Variable labels Remarks 'Any distractions etc during test: Yes - remarks'.
Variable labels Encourag 'Any distractions etc during test: Yes - encouragement'.
Variable labels Interup 'Any distractions etc during test: Yes - interruptions'.
Variable labels Othnoise 'Any distractions etc during test: Yes - other noise'.
Variable labels Goodcond 'Any distractions etc during test: No - good conditions'.
Variable labels Othsitn 'Any distractions etc during test: Other situation'.

Value labels Remarks Encourag Othnoise Interup Goodcond Othsitn
-3 'Test not attempted'
0 'Does not apply'
1 'Does apply'.

Missing values Remarks Encourag Interup Othnoise Goodcond Othsitn (-3).
format Remarks Encourag Interup Othnoise Goodcond Othsitn (f3.0).

fre Remarks Encourag Interup Othnoise Goodcond Othsitn.

cro f110 by Remarks/f110 by Encourag/f110 by Interup/f110 by Othnoise/f110 by Goodcond/f110 by Othsitn.
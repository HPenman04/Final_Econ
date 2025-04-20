value labels f110
-3 'Not attempted'
1  "1_____ ‘Yes: remarks’"
2  "_2____ ‘Yes: encouragement’"
3  "12____ ‘Yes: remarks’ & ‘Yes: encouragement’"
4  "__3___ ‘Yes: interruptions’"
5  "1_3___ ‘Yes: remarks’ & ‘Yes: interruptions’"
6  "_23___ ‘Yes: encouragement’ & ‘Yes: interruptions’"
7  "123___ ‘Yes: remarks’ & ‘Yes: encouragement’ & ‘Yes: interruptions’"
8  "___4__ ‘Yes: other noise’"
9  "1__4__ ‘Yes: remarks’ & ‘Yes: other noise’"
10 "_2_4__ ‘Yes: encouragement’ & ‘Yes: other noise’"
11 "12_4__ ‘Yes: remarks’ & ‘Yes: encouragement’ & ‘Yes: other noise’"
12 "__34__ ‘Yes: interruptions’ & ‘Yes: other noise’"
13 "1_34__ ‘Yes: remarks’ & ‘Yes: interruptions’ & ‘Yes: other noise’"
14 "_234__ ‘Yes: encouragement’ & ‘Yes: interruptions’ & ‘Yes: other noise’"
15 "1234__ ‘Yes: remarks’ & ‘Yes: encouragement’ & ‘Yes: interruptions’ & ‘Yes: other noise’"
16 "____5_ ‘No: good conditions’"
17 "1___5_ ‘Yes: remarks’ & ‘“No: good conditions’"
18 "_2__5_ ‘Yes: encouragement’ & ‘No: good conditions’"
19 "12__5_ ‘Yes: remarks’ & ‘Yes: encouragement’ & ‘No: good conditions’"
20 "__3_5_ ‘Yes: interruptions’ & ‘No: good conditions’"
21 "1_3_5_ ‘Yes: remarks’ & ‘Yes: interruptions’ & ‘No: good conditions’"
22 "_23_5_ ‘Yes: encouragement’ & ‘Yes: interruptions’ & ‘No: good conditions’"
23 "123_5_ ‘Yes: remarks’ & ‘Yes: encouragement’ & ‘Yes: interruptions’ & ‘No: good conditions’"
24 "___45_ ‘Yes: other noise’ & ‘No: good conditions’"
26 "_2_45_ ‘Yes: encouragement’ & ‘Yes: other noise’ & ‘No: good conditions’"
27 "12_45_ 'Yes: remarks' & ‘Yes: encouragement’ & ‘Yes: other noise’ & ‘No: good conditions’"
28 "__345_ ‘Yes: interruptions’ & ‘Yes: other noise’ & ‘No: good conditions’"
30 "_2345_ ‘Yes: encouragement’ & ‘Yes: interruptions’ & ‘Yes: other noise’ & ‘No: good conditions’"
31 "12345_ ‘Yes: remarks’ & ‘Yes: encouragement’ & ‘Yes: interruptions’ & ‘Yes: other noise’ & ‘No: good conditions’"
32 "_____6 ‘Other situation'"
33 "1____6 ‘Yes: remarks' & 'Other situation'"
34 "_2___6 ‘Yes: encouragement’ & 'Other situation'"
35 "12___6 ‘Yes: remarks' & ‘Yes: encouragement’ & 'Other situation'"
36 "__3__6 ‘Yes: interruptions’ & 'Other situation'"
37 "1_3__6 'Yes: remarks' & ‘Yes: interruptions’ & 'Other situation'"
38 "_23__6 ‘Yes: encouragement’ & ‘Yes: interruptions’ & 'Other situation'"
39 "123__6 'Yes: remarks' & ‘Yes: encouragement’ & ‘Yes: interruptions’ & 'Other situation'"
40 "___4_6 ‘Yes: other noise’ & 'Other situation'"
41 "1__4_6 ‘Yes: remarks' & ‘Yes: other noise’ & 'Other situation'"
42 "_2_4_6 ‘Yes: encouragement’ & ‘Yes: other noise’ & 'Other situation'"
43 "12_4_6 ‘Yes: remarks' & ‘Yes: encouragement’ & ‘Yes: other noise’ & 'Other situation'"
44 "__34_6 ‘Yes: interruptions’ & 'Yes: other noise’ & 'Other situation'"
45 "1_34_6 'Yes: remarks' & ‘Yes: interruptions’ & 'Yes: other noise’ & 'Other situation'"
46 "_234_6 ‘Yes: encouragement’ & 'Yes: interruptions’ & 'Yes: other noise’ & 'Other situation'"
47 "1234_6 ‘Yes: remarks' & ‘Yes: encouragement’ & 'Yes: interruptions’ & 'Yes: other noise’ & 'Other situation'"
48 "____56 ‘No: good conditions’ & 'Other situation'"
49 "1___56 ‘Yes: remarks' & ‘No: good conditions’ & 'Other situation'"
50 "_2__56 ‘Yes: encouragement’ & ‘No: good conditions’ & 'Other situation'"
52 "__3_56 ‘Yes: interruptions’ & ‘No: good conditions’ & 'Other situation'"
56 "___456 ‘Yes: other noise’ & ‘No: good conditions’ & 'Other situation'".

fre f110.

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
if (f110=16 or f110=17 or f110=18 or f110=19 or f110=20 or f110=21 or f110=22 or f110=23 or f110=24 or f110=26 or f110=28 or f110=30 or f110=31 or f110=48 or f110=50 or f110=52 or f110=56)Goodcond=1.
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
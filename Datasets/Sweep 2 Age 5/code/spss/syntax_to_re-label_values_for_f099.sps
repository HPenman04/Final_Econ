value labels f099
-3 'Not attempted'
1  "1______ ‘No’"
2  "_2_____ ‘Yes: some letters’"
3  "12_____ ‘No’ & ‘Yes: some letters’"
4  "__3____ ‘Yes: some words’"
5  "1_3____ ‘No’ & ‘Yes: some words’"
6  "_23____ ‘Yes: some letters’ & ‘Yes: some words’"
7  "123____ ‘No’ & ‘Yes: some letters’ & ‘Yes: some words’"
8  "___4___ ‘Yes: simple sentences’"
9  "1__4___ ‘No’ & ‘Yes: simple sentences’"
10 "_2_4___ ‘Yes: some letters’ & ‘Yes: simple sentences’"
12 "__34___ ‘Yes: some words’ & ‘Yes: simple sentences’"
14 "_234___ ‘Yes: some letters’ & ‘Yes: some words’ & ‘Yes: simple sentences’"
15 "1234___ ‘No’ & ‘Yes: some letters’ & ‘Yes: some words’ & ‘Yes: simple sentences’"
16 "____5__  ‘Cannot say’"
17 "1___5__ ‘No’ & ‘“Cannot say’"
18 "_2__5__ ‘Yes: some letters’ & ‘Cannot say’"
19 "12__5__ ‘No’ & ‘Yes: some letters’ & ‘Cannot say’"
20 "__3_5__ ‘Yes: some words’ & ‘Cannot say’"
22 "_23_5__ ‘Yes: some letters’ & ‘Yes: some words’ & ‘Cannot say’"
32 "_____6_ 'Other reply'"
33 "1____6_ 'No' & 'Other reply'"
34 "_2___6_ ‘Yes: some letters’ & 'Other reply'"
35 "12___6_ 'No' & ‘Yes: some letters’ & 'Other reply'"
36 "__3__6_ ‘Yes: some words’ & 'Other reply'"
38 "_23__6_ ‘Yes: some letters’ & ‘Yes: some words’ & 'Other reply'"
40 "___4_6_ 'Yes: simple sentences’ & 'Other reply'"
44 "__34_6_ 'Yes: some words’ & 'Yes: simple sentences’ & 'Other reply'"
46 "_234_6_ 'Yes: some letters’ & 'Yes: some words’ & 'Yes: simple sentences’ & 'Other reply'"
48 "____56_ ‘Cannot say’ & 'Other reply'"
64 "______7 'ITA'"
65 "1_____7 'No' & 'ITA'"
66 "_2____7 'Yes: some letters' & ITA"
67 "12____7 'No' & 'Yes: some letters' & ITA"
68 "__3___7 ‘Yes: some words’ & 'ITA'"
70 "_23___7 ‘Yes: some letters’ & ‘Yes: some words’ & 'ITA'"
72 "___4__7 ‘Yes: simple sentences’ & 'ITA'"
74 "_2_4__7 ‘Yes: some letters’ & 'Yes: simple sentences’ & 'ITA'"
76 "__34__7 ‘Yes: some words’ & 'Yes: simple sentences’ & 'ITA'"
78 "_234__7 ‘Yes: some letters’ & ‘Yes: some words’ & 'Yes: simple sentences’ & 'ITA'"
80 "____5_7 'Cannot say' & 'ITA'"
96 "_____67 'Other reply' & 'ITA'".

fre f099.

compute Noread=-3.
if (f099 ge 1)Noread=0.
if (f099=1 or f099=3 or f099=5 or f099=7 or f099=9 or f099=15 or f099=17 or f099=19 or f099=33 or f099=35 or f099=65 or f099=67)Noread=1. 
compute Yeslet=-3.
if (f099 ge 1)Yeslet=0.
if (f099=2 or f099=3 or f099=6 or f099=7 or f099=10 or f099=14 or f099=15 or f099=18 or f099=19 or f099=22 or f099=34 or f099=35 or f099=38 or f099=46 or f099=66 or f099=67 or f099=70 or f099=74 or f099=78)Yeslet=1.
compute Yesword=-3.
if (f099 ge 1)Yesword=0.
if (f099=4 or f099=5 or f099=6 or f099=7 or f099=12 or f099=14 or f099=15 or f099=20 or f099=22 or f099=36 or f099=38 or f099=44 or f099=46 or f099=68 or f099=70 or f099=76 or f099=78)Yesword=1.
compute Yessent=-3.
if (f099 ge 1)Yessent=0.
if (f099=8 or f099=9 or f099=10 or f099=12 or f099=14 or f099=15 or f099=40 or f099=44 or f099=46 or f099=72 or f099=74 or f099=76 or f099=78)Yessent=1.
compute Cantsay=-3.
if (f099 ge 1)Cantsay=0.
if (f099=16 or f099=17 or f099=18 or f099=19 or f099=20 or f099=22 or f099=48 or f099=80)Cantsay=1.
compute Othreply=-3.
if (f099 ge 1)Othreply=0.
if (f099=32 or f099=33 or f099=34 or f099=35 or f099=36 or f099=38 or f099=40 or f099=44 or f099=46 or f099=48 or f099=96)Othreply=1.
compute ITA=-3.
if (f099 ge 1)ITA=0.
if (f099 ge 64)ITA=1.

Variable labels Noread 'Schonell reading test: Mother thinks No - child can read nothing'.
Variable labels Yeslet 'Schonell reading test: Mother thinks Yes - child can read some letters'.
Variable labels Yesword 'Schonell reading test: Mother thinks Yes - child can read some words'.
Variable labels Yessent 'Schonell reading test: Mother thinks Yes - child can read simple sentences'.
Variable labels Cantsay 'Schonell reading test: Mother cannot say if child can read'.
Variable labels Othreply 'Schonell reading test: Mother gives other reply'.
Variable labels ITA 'Schonell reading test: Mother says Initial Teaching Alphabet used'.

Value labels Noread Yeslet Yessent Yesword Cantsay Othreply ITA
-3 'Test not attempted'
0 'Does not apply'
1 'Does apply'.

Missing values Noread Yeslet Yessent Yesword Cantsay Othreply ITA (-3).
format Noread Yeslet Yesword Yessent Cantsay Othreply ITA (f3.0).

fre Noread Yeslet Yesword Yessent Cantsay Othreply ITA.

cro f099 by Noread/f099 by Yeslet/f099 by Yesword/f099 by Yessent/f099 by Cantsay/f099 by Othreply/f099 by ITA.
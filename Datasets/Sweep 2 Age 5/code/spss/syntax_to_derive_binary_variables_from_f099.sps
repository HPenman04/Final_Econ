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


/*5-3*/
DATA na;
	INPUT med $ X Y @@;
	CARDS;
		A 11 6 A 8 0 A 5 2 A 14 8 A 19 11
		A 6 4 A 10 13 A 6 1 A 11 8 A 3 0
		B 6 0 B 6 2 B 7 3 B 8 1 B 18 18
		B 8 4 B 19 14 B 8 9 B 5 1 B 15 9
		C 16 13 C 13 10 C 11 18 C 9 5 C 21 23
		C 16 12 C 12 5 C 12 16 C 7 1 C 12 20
;

PROC GLM DATA=na;
	CLASS med;
	MODEL Y = med X X*med /SOLUTION;
	LSMEANS med/TDIFF;
RUN; 

PROC GLM DATA=na;
	CLASS med;
	MODEL Y = med X/SOLUTION;
	LSMEANS med/TDIFF;
RUN; 

PROC MEANS DATA=na;
	VAR X;
RUN;

/*5-5*/
DATA baka;
 INPUT temp water fert growth @@;
 CARDS;
 	10 23.5 15 5 10 24 15.5 4.5 10 23 14 5 10 24.5 13.5 4.5 10 22.5 13.5 5.5 10 24.5 13.5 3
	10 25.5 13 5 10 25 14 5 10 23 14.5 5.5 10 23 14 4.5 10 22.5 14 6.5 10 23 15 5.5
	15 28.5 16 7 15 26 15.5 9 15 28.5 17 8.5 15 26.5 16 8.5 15 27 18.5 6 15 28 14.5 8
	15 26.5 17.5 7 15 29 13.5 7 15 27 16.5 7 15 27 16.5 11 15 27 17.5 9 15 27 18.5 8
	20 25.6 12 6.6 20 23.1 11.6 9.2 20 20.8 18.2 5.6 20 19.2 11.9 4.2 20 21.3 20.2 4.6 20 26.6 13.1 4.5
	20 25.5 16.2 4.5 20 18.9 11.9 6.1 20 29.3 10.5 6.3 20 30.2 11.3 8.1 20 18.9 10.9 4.9 20 25.3 12.2 6.2
;

PROC GLM DATA = baka;
	CLASS temp;
	MODEL growth = temp water fert water*temp fert*temp;
RUN;

PROC GLM DATA = baka;
	CLASS temp;
	MODEL growth = temp water fert/SOLUTION;
	LSMEANS temp/TDIFF;
RUN;

PROC MEANS DATA=baka;
	VAR water fert;
RUN;

/*5-7*/
DATA study;
 INPUT meth $ inte mot ach @@;
 	CARDS;
	A 99 12 58 A 95 11 55 A 99 13 59 A 99 13 55 A 102 12 60 A 101 12 57
	A 102 12 57 A 103 13 61 A 102 13 58 A 103 14 59 A 102 14 62 A 104 13 59
	A 106 15 60 A 106 13 62 A 107 13 60 A 108 12 62 A 109 13 64 A 111 13 63
	A 110 13 59 A 113 13 63
	B 95 12 58 B 97 12 55 B 100 13 60 B 100 13 55 B 102 13 60 B 105 12 58
	B 102 13 57 B 102 14 62 B 105 12 58 B 105 13 61 B 106 14 63 B 109 13 59
	B 107 14 61 B 108 14 64 B 111 14 61 B 108 13 63 B 111 13 65 B 113 14 64
	B 114 13 62 B 114 14 60
	C 94 12 59 C 95 12 56 C 99 13 62 C 99 11 57 C 99 14 61 C 102 12 60 C 99 13 58
	C 101 13 63 C 104 13 60 C 101 14 63 C 104 15 65 C 107 13 61 C 107 13 62
	C 106 14 64 C 109 14 61 C 107 14 65 C 109 14 63 C 111 14 66 C 111 14 62
	C 112 13 64
	;

PROC GLM DATA=study;
	CLASS meth;
	MODEL ach = meth inte mot meth*inte meth*mot;
	MODEL ach = meth inte mot/SOLUTION;
RUN;
PROC GLM DATA=study;
	CLASS meth;
	MODEL ach = meth inte mot/SOLUTION;
	LSMEANS meth/TDIFF;
RUN;


proc export data=na
   outfile='C:\Users\pkmon\Desktop\积苞单 苞力3\na.csv'
   dbms=csv
   replace;
run;


proc export data=baka
   outfile='C:\Users\pkmon\Desktop\积苞单 苞力3\baka.csv'
   dbms=csv
   replace;
run;


proc export data=study
   outfile='C:\Users\pkmon\Desktop\积苞单 苞力3\study.csv'
   dbms=csv
   replace;
run;

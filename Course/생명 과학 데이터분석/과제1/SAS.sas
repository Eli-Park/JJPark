/*1¹ø*/
DATA B;
INPUT group x @@;
CARDS;
1 48.2 2 52.3
1 54.6 2 57.4
1 58.3 2 55.6
1 47.8 2 53.2
1 51.4 2 61.3
1 52.0 2 58.0
1 55.2 2 59.8
1 49.1 2 54.8
1 49.9 2 51.2
1 52.6 2 46.2
;
PROC TTEST DATA=B;
	CLASS group;
	VAR x;
RUN;




/*7¹ø*/

DATA diet;
	DO exercise = 'A', 'B', 'C', 'D', 'E';
	DO meal = '1', '2', '3', '4';
	INPUT kg @@;
	OUTPUT;
	END;
	END;
CARDS;
7.0 5.3 4.9 8.8
9.9 5.7 7.6 8.9
8.5 4.7 5.5 8.1
5.1 3.5 2.8 3.3
10.3 7.7 8.4 9.1
;
PROC ANOVA DATA=diet;
	CLASS exercise meal;
	MODEL kg = exercise meal;
	MEANS exercise meal/ SCHEFFE;
RUN;

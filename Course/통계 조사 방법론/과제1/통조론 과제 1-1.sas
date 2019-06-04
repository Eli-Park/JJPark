FILENAME agpop 'C:\Users\pkmon\Desktop\Sampling Theory\agpop.dat';
DATA frame;
   INFILE agpop;
   INPUT county $ state $ acres92 acres87 acres82 farms92 farms87
         farms82 largef92 largef87 largef82 smallf92 smallf87
		 smallf82 region $;

RUN;

DATA frame1;
   SET frame;
   IF largef92 ge smallf92 THEN farmtype = 0;
      ELSE farmtype = 1;
   WHERE acres92 <> -99 AND acres87 <> -99 AND acres82 <> -99
            AND farms92 <> -99 AND farms87 <> -99 AND farms82 <> -99
            AND largef92 <> -99 AND largef87 <> -99 AND largef82 <> -99
            AND smallf92 <> -99 AND smallf87 <> -99 AND smallf82 <> -99; 
RUN;


PROC SURVEYSELECT SAMPSIZE = 100 SEED=2014150137 OUTSIZE METHOD=SRS DATA=frame1 OUT=SRSam;
	TITLE '100 Simple Random Samples';
RUN;

PROC PRINT DATA=SRSam;
RUN;

PROC SURVEYMEANS MEAN SUM DATA=SRSam TOTAL=3041;
   VAR acres82 acres87 acres92 ;
   STRATA county;
   TITLE 'Estimate Stratum Means and Totals Acres of Farms by county';
RUN;


PROC SURVEYMEANS DATA=SRSam TOTAL=3041;
CLASS farmtype;
VAR farmtype;
TITLE 'Estimate Proportion of County with more Largef in 1992';
RUN;

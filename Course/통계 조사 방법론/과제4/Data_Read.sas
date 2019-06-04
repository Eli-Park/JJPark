FILENAME HOTDOG 'C:\Users\pkmon\Desktop\TonjoRon4\hotdog.dat'; 
FILENAME PPS 'C:\Users\pkmon\Desktop\TonjoRon4\Cs1_pps_sample.dat'; 
FILENAME SRS 'C:\Users\pkmon\Desktop\TonjoRon4\Cs1_srs_sample.dat'; 

DATA HOTDOG;
 INFILE HOTDOG;
 INPUT ID dwellers hotdogs;
RUN;

PROC SURVEYMEANS DATA=HOTDOG;
 CLUSTER ID;
 VAR dwellers hotdogs;
 RATIO  hotdogs / dwellers;
RUN;

DATA SRS;
 INFILE SRS;
 INPUT cluster element y1 y2 y3 ;
 SamplingWeight = 250/25;
RUN;
 
PROC SURVEYSELECT DATA=SRS OUT=SRSS METHOD=SRS SAMPSIZE=20 SEED=2014150137;
  STRATA cluster;
  TITLE "Two stage simple random cluster sampling";
RUN; 

DATA SRSS;
	SET SRSS;
	SW = SamplingWeight / SelectionProb;
RUN;

/*Ratio 통계량 구함*/

PROC SURVEYMEANS SUM MEAN VAR DATA=SRSS TOTAL=28462;
	VAR y1 y2 y3;
	CLUSTER cluster;
	WEIGHT SW;
RUN;

/*Unbiased 통계량 구함*/

DATA SUNB;
 INPUT total1 total2 total3 se1 se2 se3 K;
 y1ub     = total1/K;
 y2ub     = total2/K;
 y3ub     = total3/K;
 se1_ub_mean  = se1/K;
 se2_ub_mean  = se2/K;
 se3_ub_mean  = se3/K; 
cards;
2833049 190927 16776 191270 15454 1341.426316 28642 
;

PROC PRINT DATA=SUNB;
RUN;

DATA PPS;
 INFILE PPS;
 INPUT cluster element y1 y2 y3 UnitSize NumberHits ExpectedHits SamplingWeight;
RUN;

/* 2번 뽑힌 cluster를 독립적으로 2번 추출하기 위해 분리*/

DATA PPS_t;
RUN;

DATA PPS_t;
	SET PPS;
	if NumberHits = 2 ;
RUN;

/*원래 cluster 갯수가 250이므로 겹치지 않게 250을 더해줌*/

DATA PPS_t;
	SET PPS_t;
	 cluster=cluster+250;
RUN;

DATA PPS;
	SET PPS PPS_t;
RUN;

PROC SURVEYSELECT DATA=PPS OUT=PPPS METHOD=SRS SAMPSIZE=20 SEED=2014150137;
 STRATA cluster;
 TITLE "Two stage pps cluster sampling";
RUN; 

DATA PPPS;
	SET PPPS;
	SW = SamplingWeight / SelectionProb;
RUN;


/*Ratio 통계량 구함*/

PROC SURVEYMEANS SUM MEAN DATA=PPPS TOTAL=28462;
 VAR y1 y2 y3;
 WEIGHT SW;
 CLUSTER cluster;
RUN;

/*Unbiased 통계량 구함*/

DATA PUNB;
 INPUT total1 total2 total3 se1 se2 se3 K;
 y1ub     = total1/K;
 y2ub     = total2/K;
 y3ub     = total3/K;
 se1_ub_mean  = se1/K;
 se2_ub_mean  = se2/K;
 se3_ub_mean  = se3/K; 
cards;
2838615 196787 17191 10439 7144.798595 603.057766 28642 
;

PROC PRINT DATA=PUNB;
RUN;


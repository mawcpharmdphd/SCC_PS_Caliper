/*Testing matching*/
LIBNAME res 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Uniform_C3';
OPTIONS NOSOURCE NONOTES;

ODS NORESULTS;
ODS GRAPHICS OFF;
ODS EXCLUDE ALL;

%macro pnu_match(inDS=_predicted, replace=N, matchVar=pred, caliper=, matchMethod=exact,
                 outds=_PNUmatched,order=firstlast);

   %LET matchMethod = %SYSFUNC(lowcase(&matchMethod));

   /* Prep for matching: Split into exposed and unexposed datasets */
   /*   Sort exposed in chronological order and unexposed based on PS */
   data _exp_ds_  (rename=(i=expID   matchVar=expMatchVar   starttime=daysOn_exposure))
        _unexp_ds_(rename=(i=unexpID matchVar=unexpMatchVar starttime=daysOn_comparator dummytime=unExpdate));

      set &inDS; 
      if exp=1 then output _exp_ds_; else if exp=0 then output _unexp_ds_;
	dummytime = starttime;
      keep i exp &matchVar starttime dummytime;
      rename &matchVar = matchVar;
   run;

   %IF &order = firstlast %THEN %DO;
		proc sort data=_exp_ds_; by daysOn_exposure; run;
	%END;
	%IF &order = lastfirst %THEN %DO;
		proc sort data=_exp_ds_; by DESCENDING daysOn_exposure; run;
	%END;
	%IF &order = rand %THEN %DO;
		DATA _exp_ds_;
			SET _exp_ds_;
			CALL STREAMINIT (&seed+10);
			order=RAND("UNIFORM");
		RUN;
		PROC SORT DATA= _exp_ds_;
			BY order;
		RUN;
	%END;

   proc sort data=_unexp_ds_; by unexpMatchVar; run;

   /* Perform matching */
   data _matched_nn_ _nomatch_(keep=expID expMatchVar match_ID match_date match_matchVar daysOn_exposure daysOn_comparator);
      *Set exposed and define hash object used to check for matches;
      set _exp_ds_;
      %IF &replace=N %THEN %DO;
         if _N_=1 then do;
            declare hash umatch();
            umatch.definekey('unexpID');
            umatch.definedone();
         end; 
      %END;

      length lastID match_ID $15;

      posFlag=0;
      matchFlag=0;
      i=1;

      *STEP 1: Process unexposed until the first match is found;
      do while (matchFlag=0 and i<=n_unexp);
         set _unexp_ds_ nobs=n_unexp point=i;
      
         *If unexposed falls into exposed exposure group...;
         if       %IF &matchMethod = exact %THEN daysOn_exposure = daysOn_comparator;
            %ELSE %IF &matchMethod = window %THEN daysOn_exposure-&window<=daysOn_comparator<=daysOn_exposure+&window;
            %ELSE %IF &matchMethod = threshold %THEN daysOn_exposure>&threshold AND daysOn_comparator>&threshold;
         then do;
            *...update positivity flag;
            if unexpMatchVar < expMatchVar and posFlag=0 then posFlag=1; 
            if expMatchVar = unexpMatchVar then posFlag=1.5;
            unexp_pointer_1stmatch=i;
           
            *...if matching without replacement, check whether unexposed has already been matched;
            %IF &replace=N %THEN if umatch.find() ne 0 then do;;

            *...if eligible match, hold on to their information and move to next step;
               lastMatchVar=unexpMatchVar;
               lastID=unexpID; lastDate=unexpDate;
               matchFlag=1; 

            %IF &replace=N %THEN end;;*end unmatched;
         end;*end exposure window;

         *If first match not found, move to next unexposed to continue searching;
         i+1;
      end; *end do while;


      *STEP 2: After 1st match is found, continue processing through unexposed;
      do while (i<=n_unexp and matchFlag=1);
         set _unexp_ds_ point=i;
         unexp_pointer_lastmatch=i;

         *...if matching without replacement, check whether unexposed has already been matched;
         %IF &replace=N %THEN %DO;
            if umatch.find() = 0 then do;
               *Update positivity flag and move to next step;
               if unexpMatchVar=expMatchVar then posFlag=1.5;
               if unexpMatchVar>expMatchVar and posFlag=1 then posFlag=2;
               if unexpMatchVar>=expMatchVar and posFlag=0 then goto nextExp; else goto nextUnexp;
            end;
         %END;

         *STEP 2A: Process all unexposed with unexp_prob < exp_prob;
         if unexpMatchVar < expMatchVar then do; 

            *...check whether unexposed falls into exposed exposure group;
            if       %IF &matchMethod = exact %THEN daysOn_exposure = daysOn_comparator;
               %ELSE %IF &matchMethod = window %THEN daysOn_exposure-&window<=daysOn_comparator<=daysOn_exposure+&window;
               %ELSE %IF &matchMethod = threshold %THEN daysOn_exposure>&threshold AND daysOn_comparator>&threshold;
            then do;
               *If eligible match, hold on to their information and move to next unexposed;
               lastDiff = expMatchVar - unexpMatchVar;
               lastID = unexpID; lastDate = unexpDate; lastMatchVar = unexpMatchVar;
               matchFlag=1; 
               goto nextUnexp;
            end;
         end; *end STEP 2A (unexposed < exposed);

         *STEP 2B: Process unexposed with unexp_prob >= exp_prob until 1st eligible match is found;
         else if unexpMatchVar >= expMatchVar then do;
            *...update positivity flag (move to next exposed if positivity not met);
            if posFlag=0 then goto nextExp;
            if unexpMatchVar=expMatchVar then posFlag=1.5;
            if unexpMatchVar>expMatchVar and posFlag=1 then posFlag=2;

            *...check whether unexposed falls into exposed exposure group;
            if       %IF &matchMethod = exact %THEN daysOn_exposure = daysOn_comparator;
               %ELSE %IF &matchMethod = window %THEN daysOn_exposure-&window<=daysOn_comparator<=daysOn_exposure+&window;
               %ELSE %IF &matchMethod = threshold %THEN daysOn_exposure>&threshold AND daysOn_comparator>&threshold;
            then do;
               *If eligible match, hold on to their information;
               thisDiff = unexpMatchVar - expMatchVar;
               if matchFlag=1 then matchFlag=2; 

               *Compare the two matches to find the nearest neighbor;
               minDiff = min(lastDiff, thisDIff);
               *If using a max caliper, check that nearest neighbor is within caliper;
               if minDiff > &caliper then goto nextExp;;
               *if THIS record is a nearer neighbor than the LAST record, output THIS record;
               if minDiff = thisDiff then do;
                  match_ID = unexpID; match_matchVar = unexpMatchVar; match_date = unexpDate;
                  outputflag=1; output _matched_nn_;
                  %IF &replace=N %THEN umatch.add();;
               end; 
               *if LAST record is a nearer neighbor then THIS record, output LAST record;
               else if minDiff = lastDiff then do;
                  match_ID = lastID; match_matchVar = lastMatchVar; match_date = lastDate;
                  outputflag=1; output _matched_nn_;
                  %IF &replace=N %THEN %DO; unexpID=lastID; umatch.add(); %END;
               end;
                  
               goto nextExp;
            end;*end exposure window match;
            else goto nextUnexp;
         end;*end STEP 2B (unexp_prob>=exp_prob);

         nextUnexp:
         i+1;*Move to next unexposed;

      end;*end STEP 2;

      nextExp:
      if outputflag ne 1 then output _nomatch_;
   run;


   data &outds;
      set _matched_nn_;
      retain exposureGroup 0;
      exposureGroup+1;

      bene_id=expID; exposure=1; DaysOn=daysOn_exposure; ps_&matchVar=expMatchVar; output;
      bene_id=match_id; exposure=0; DaysOn=daysOn_comparator; ps_&matchVar=match_matchVar; output;

      keep bene_id exposure exposureGroup daysOn ps_&matchVar posFlag;
   run;

   proc datasets lib=work nolist nodetails; delete _matched_nn_; run;quit;
%mend;

%macro createSCCsim(name=,n=,repstart=,repend=,seed=, /*This row encodes core simulation aspects; name of the data set, sample size (n), start and end of the number of replicates in this code cunk, and seed*/
					INPUTC1=, /*First, the "input" variables are specified*/
					INPUTC2=,
					INPUTC3=,
					INPUTX=,
					INPUTY=,
					PC1C2=,PC1X=,PC1Y=, /*These are causal partners related to C1's DAG edges, including its overall probability (PC1)*/
					PC2X=,PC2Y=, /*Similar, but for C2*/
					PXY=, /*Similar to the above, but for X*/
					MEANPC3X=, DEVPC3X=, MEANPC3Y=, DEVPC3Y=, /*Our continuous partners for C3*/
					MEAN1C3=, MEAN2C3=, DEV1C3=, DEV2C3=, /*Means and standard deviations of C3, depending on INPC3*/
					SCCC1=,/*Definition of complete pies for C1 (in this case, just INPUTC1=1)*/
					SCCC2=,/*Definition of complete pies for C2*/
					SCCC3=, /*Definition of pies that result in the "MEAN1/DEV1" distribution of C3, rather than "MEAN2/DEV2"*/
					SCCX=, /*Definition of complete pies for X*/
					SCCY= /*Definition of complete pies for Y*/);

DATA &name;
	DO replicate=&repstart to &repend ; /*Creating a set number of replicates*/
		CALL STREAMINIT(&seed + replicate); /*Creating a new random seed for each replicate*/
		DO ID=1 to &n; /*Creating a DO LOOP of the population size specified*/
			INPUTC1=RAND('BERNOULLI',&INPUTC1); /*Create all the partner variables*/
			PC1C2=RAND('BERNOULLI',&PC1C2);
			PC1X=RAND('BERNOULLI',&PC1X);
			PC1Y=RAND('BERNOULLI',&PC1Y);
			INPUTC2=RAND('BERNOULLI',&INPUTC2);
			PC2X=RAND('BERNOULLI',&PC2X);
			PC2Y=RAND('BERNOULLI',&PC2Y);
			INPUTX=RAND('BERNOULLI',&INPUTX);
			PXY=RAND('BERNOULLI',&PXY);
			INPUTY=RAND('BERNOULLI',&INPUTY);
			INPUTC3=RAND('BERNOULLI',&INPUTC3);
			PC3X=RAND('NORMAL',&MEANPC3X,&DEVPC3X);
			PC3Y=RAND('NORMAL',&MEANPC3Y,&DEVPC3Y);/*That's all of the partners*/

			&SCCC1 THEN C1 = 1; /*Create all the "DAG" variables, referencing each of their SCC*/
			ELSE C1 = 0;

			&SCCC2 THEN C2 = 1;
			ELSE C2 = 0;

			&SCCC3 THEN C3 = RAND('UNIFORM')*4;
			ELSE C3 = RAND('UNIFORM')*4;

			/*Getting potential outcome if X=1*/
			X=1;
			&SCCY THEN X1Y=1;
			ELSE X1Y=0;

			/*Getting potential outcome if X=0*/
			X=0;
			&SCCY THEN X0Y=1;
			ELSE X0Y=0;

			/*Getting factual outcome*/
			&SCCX THEN X=1;
			ELSE X = 0;

			&SCCY THEN Y=1;
			ELSE Y=0;


			OUTPUT; /*output each individual*/

		END; /*End individual loop*/
	END; /*End replicate loop*/
RUN; /*Complete data step*/
%mend;



%macro loopit (startloop=,endloop=);

%DO icon = &startloop %TO &endloop;

%createSCCsim(name=trialrun,n=450,repstart=&icon,repend=&icon,seed=1250+&icon, /*We'll create 1 dataset named "trialrun" with 100,000 people, with 1 replicate and a seed of 1250*/
					INPUTC1=0.5,PC1C2=0.4,PC1X=0.1,PC1Y=0.1,
					INPUTC2=0.1,PC2X=0.2,PC2Y=0.1,
					INPUTX=0.15,PXY=0.1,
					INPUTY=0.05,
					MEANPC3X=2,
					DEVPC3X=1,
					MEANPC3Y=2,
					DEVPC3Y=0.5,
					MEAN1C3=0,
					MEAN2C3=0,
					DEV1C3=1,
					DEV2C3=1,
		 			SCCC1=%QUOTE(IF INPUTC1 = 1), /*This %QUOTE is very important when creating the pies; otherwise the macro will terminate early*/
					SCCC2=%QUOTE(IF (C1 = 1 AND PC1C2 = 1) OR (INPUTC2 = 1) ), /*This means that C2 can equal 1 if C1 and PC1C2 have opposite values*/
					SCCC3=%QUOTE(IF INPUTC3 = 1),
					SCCX=%QUOTE(IF (C1 = 1 AND PC1X = 1) OR (C2 = 1 AND PC2X = 1) OR (INPUTX = 1) OR ( (PC3X + C3) > 3) ) ,
					SCCY=%QUOTE(IF (C1 = 1 AND PC1Y = 1) OR (C2 = 1 AND PC2Y = 1) OR ( (PC3Y * C3) > 6) OR (X=1 AND PXY=1) OR (INPUTY = 1) ),
					INPUTC3=0.5);

PROC GENMOD DATA=trialrun DESCENDING;
	MODEL X = C1 C2 C3 / link=logit dist=binomial;
	OUTPUT OUT=withpreds pred=pred;
RUN;

PROC MEANS DATA=withpreds;
	VAR pred;
RUN;

PROC MEANS DATA=withpreds NOPRINT;
	VAR pred;
	OUTPUT OUT=_predstddev STDDEV=stddev;
RUN;

DATA _null_;
	SET _predstddev;
	point001stddev=stddev*0.001;
	CALL SYMPUT('calip001',point001stddev);
	point005stddev=stddev*0.005;
	CALL SYMPUT('calip005',point005stddev);
	point01stddev=stddev*0.01;
	CALL SYMPUT('calip01',point01stddev);
	point02stddev=stddev*0.02;
	CALL SYMPUT('calip02',point02stddev);
	point05stddev=stddev*0.05;
	CALL SYMPUT('calip05',point05stddev);
	point1stddev=stddev*0.1;
	CALL SYMPUT('calip1',point1stddev);
	CALL SYMPUT('calip',stddev);
RUN;


DATA formatformatch;
	SET withpreds;
	i = ID;
	exp = X;
	starttime = 0;
	dummytime = 0;
RUN;

%pnu_match(inDS=formatformatch, replace=N, matchVar=pred, caliper=0.001, matchMethod=exact,
                 outds=matched0_001,order=firstlast);

%pnu_match(inDS=formatformatch, replace=N, matchVar=pred, caliper=0.01, matchMethod=exact,
                 outds=matched0_01,order=firstlast);

%pnu_match(inDS=formatformatch, replace=N, matchVar=pred, caliper=&calip001, matchMethod=exact,
                 outds=_matched0_001SD,order=firstlast);

%pnu_match(inDS=formatformatch, replace=N, matchVar=pred, caliper=&calip005, matchMethod=exact,
                 outds=_matched0_005SD,order=firstlast);

%pnu_match(inDS=formatformatch, replace=N, matchVar=pred, caliper=&calip01, matchMethod=exact,
                 outds=_matched0_01SD,order=firstlast);

%pnu_match(inDS=formatformatch, replace=N, matchVar=pred, caliper=&calip02, matchMethod=exact,
                 outds=_matched0_02SD,order=firstlast);

%pnu_match(inDS=formatformatch, replace=N, matchVar=pred, caliper=&calip05, matchMethod=exact,
                 outds=_matched0_05SD,order=firstlast);

%pnu_match(inDS=formatformatch, replace=N, matchVar=pred, caliper=&calip1, matchMethod=exact,
                 outds=_matched0_1SD,order=firstlast);

%pnu_match(inDS=formatformatch, replace=N, matchVar=pred, caliper=&calip, matchMethod=exact,
                 outds=_matchedSD,order=firstlast);

PROC MEANS DATA=matched0_01 NOPRINT MEAN;
	VAR ps_pred;
	CLASS exposure;
	OUTPUT OUT=pred0_01 MEAN=MEAN N=N;
RUN;

PROC MEANS DATA=matched0_01 NOPRINT STDDEV;
	VAR ps_pred;
	OUTPUT OUT=stdpred0_01 STDDEV=STDDEV;
RUN;

PROC TRANSPOSE DATA=pred0_01 OUT=tposedpred0_01;
	ID exposure;
	VAR mean;
	WHERE exposure ^= .;
RUN;

DATA mergedpred0_01;
	MERGE stdpred0_01 tposedpred0_01;
	SMD=(_1 - _0) / STDDEV;
	replicate=&icon;
RUN;

DATA withreppred0_01;
	SET pred0_01;
	replicate =&icon;
RUN;

PROC DATASETS library = res nolist;
	append base=res.withpred0_01 data=work.withreppred0_01;
QUIT;

PROC DATASETS library = res nolist;
	append base=res.mergedpred0_01 data=work.mergedpred0_01;
QUIT;

PROC SQL;
	create table withcov0_01 as
		select a.*, b.C1, b.C2, b.C3, b.Y from
			matched0_01 as a inner join trialrun as b
				on a.bene_ID = b.id;
QUIT;

PROC MEANS DATA=withcov0_01 NOPRINT MEAN;
	VAR c3;
	CLASS exposure;
	OUTPUT OUT=c30_01 MEAN=MEAN;
RUN;

PROC MEANS DATA=withcov0_01 NOPRINT STDDEV;
	VAR c3;
	OUTPUT OUT=stdc30_01 STDDEV=STDDEV;
RUN;

PROC TRANSPOSE DATA=c30_01 OUT=tposedc30_01;
	ID exposure;
	VAR mean;
	WHERE exposure ^= .;
RUN;

DATA mergedc30_01;
	MERGE stdc30_01 tposedc30_01;
	SMD=(_1 - _0) / STDDEV;
	replicate=&icon;
RUN;

PROC DATASETS library = res nolist;
	append base=res.mergedc30_01 data=work.mergedc30_01;
RUN;

PROC MEANS DATA=withcov0_01;
	CLASS exposure;
	VAR Y;
	OUTPUT OUT=meanvals0_01 MEAN=mean;
RUN;

DATA withall0_01;
	SET meanvals0_01;
	replicate = &icon;
RUN;

PROC DATASETS library = res nolist;
	append base=res.withall0_01 data=work.withall0_01;
QUIT;

PROC MEANS DATA=matched0_001 NOPRINT MEAN;
	VAR ps_pred;
	CLASS exposure;
	OUTPUT OUT=pred0_001 MEAN=MEAN N=N;
RUN;

PROC MEANS DATA=matched0_001 NOPRINT STDDEV;
	VAR ps_pred;
	OUTPUT OUT=stdpred0_001 STDDEV=STDDEV;
RUN;

PROC TRANSPOSE DATA=pred0_001 OUT=tposedpred0_001;
	ID exposure;
	VAR mean;
	WHERE exposure ^= .;
RUN;

DATA mergedpred0_001;
	MERGE stdpred0_001 tposedpred0_001;
	SMD=(_1 - _0) / STDDEV;
	replicate=&icon;
RUN;

DATA withreppred0_001;
	SET pred0_001;
	replicate =&icon;
RUN;

PROC DATASETS library = res nolist;
	append base=res.withpred0_001 data=work.withreppred0_001;
RUN;

PROC DATASETS library = res nolist;
	append base=res.mergedpred0_001 data=work.mergedpred0_001;
RUN;

PROC SQL;
	create table withcov0_001 as
		select a.*, b.C1, b.C2, b.C3, b.Y from
			matched0_001 as a inner join trialrun as b
				on a.bene_ID = b.id;
QUIT;

PROC MEANS DATA=withcov0_001 NOPRINT MEAN;
	VAR c3;
	CLASS exposure;
	OUTPUT OUT=c30_001 MEAN=MEAN;
RUN;

PROC MEANS DATA=withcov0_001 NOPRINT STDDEV;
	VAR c3;
	OUTPUT OUT=stdc30_001 STDDEV=STDDEV;
RUN;

PROC TRANSPOSE DATA=c30_001 OUT=tposedc30_001;
	ID exposure;
	VAR mean;
	WHERE exposure ^= .;
RUN;

DATA mergedc30_001;
	MERGE stdc30_001 tposedc30_001;
	SMD=(_1 - _0) / STDDEV;
	replicate=&icon;
RUN;

PROC DATASETS library = res nolist;
	append base=res.mergedc30_001 data=work.mergedc30_001;
RUN;


PROC MEANS DATA=withcov0_001;
	CLASS exposure;
	VAR Y;
	OUTPUT OUT=meanvals0_001 MEAN=mean;
RUN;

DATA withall0_001;
	SET meanvals0_001;
	replicate = &icon;
RUN;

PROC DATASETS library = res nolist;
	append base=res.withall0_001 data=work.withall0_001;
QUIT;

%macro gettablevals (caliper=);

PROC MEANS DATA=_matched&caliper NOPRINT MEAN;
	VAR ps_pred;
	CLASS exposure;
	OUTPUT OUT=pred&caliper MEAN=MEAN N=N;
RUN;

PROC MEANS DATA=_matched&caliper NOPRINT STDDEV;
	VAR ps_pred;
	OUTPUT OUT=stdpred&caliper STDDEV=STDDEV;
RUN;

PROC TRANSPOSE DATA=pred&caliper OUT=tposedpred&caliper;
	ID exposure;
	VAR mean;
	WHERE exposure ^= .;
RUN;

DATA mergedpred&caliper;
	MERGE stdpred&caliper tposedpred&caliper;
	SMD=(_1 - _0) / STDDEV;
	replicate=&icon;
RUN;

DATA withreppred&caliper;
	SET pred&caliper;
	replicate =&icon;
RUN;

PROC DATASETS library = res nolist;
	append base=res.withpred&caliper data=work.withreppred&caliper;
QUIT;

PROC DATASETS library = res nolist;
	append base=res.mergedpred&caliper data=work.mergedpred&caliper;
RUN;


PROC SQL;
	create table withcov&caliper as
		select a.*, b.C1, b.C2, b.C3, b.Y from
			_matched&caliper as a inner join trialrun as b
				on a.bene_ID = b.id;
QUIT;

PROC MEANS DATA=withcov&caliper NOPRINT MEAN;
	VAR c3;
	CLASS exposure;
	OUTPUT OUT=c3&caliper MEAN=MEAN;
RUN;

PROC MEANS DATA=withcov&caliper NOPRINT STDDEV;
	VAR c3;
	OUTPUT OUT=stdc3&caliper STDDEV=STDDEV;
RUN;

PROC TRANSPOSE DATA=c3&caliper OUT=tposedc3&caliper;
	ID exposure;
	VAR mean;
	WHERE exposure ^= .;
RUN;

DATA mergedc3&caliper;
	MERGE stdc3&caliper tposedc3&caliper;
	SMD=(_1 - _0) / STDDEV;
	replicate=&icon;
RUN;

PROC DATASETS library = res nolist;
	append base=res.mergedc3&caliper data=work.mergedc3&caliper;
RUN;

PROC MEANS DATA=withcov&caliper;
	CLASS exposure;
	VAR Y;
	OUTPUT OUT=meanvals&caliper MEAN=mean;
RUN;

DATA withall&caliper;
	SET meanvals&caliper;
	replicate = &icon;
RUN;

PROC DATASETS library = res nolist;
	append base=res.withall&caliper data=work.withall&caliper;
QUIT;

%mend;
	

%gettablevals(caliper=0_001SD);

%gettablevals(caliper=0_005SD);

%gettablevals(caliper=0_01SD);

%gettablevals(caliper=0_02SD);

%gettablevals(caliper=0_05SD);

%gettablevals(caliper=0_1SD);

%gettablevals(caliper=SD);

proc datasets library=WORK nolist kill; run; quit;

%END;

%mend;

%loopit(startloop=1,endloop=2000);

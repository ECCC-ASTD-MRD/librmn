#ifdef AIX
/*============================================================================
  prints the wallclock time used by steps under GANG scheduler
   
   Usage "wall_clock_time stepid1 stepid2 stepid3"
============================================================================ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "llapi.h"

static void get_master_machine_name( char *stepid, char **mtaskhname);
static void get_wall_clock_used(char *stepid,char **hostname,int *wu,int *wh,int *ws);
char* get_scheduler_type(void);

#ifdef DEBUG
int  main(int argc, char **argv)
{	
	char *hostname, *step_name, *stepid, *schedulertype;
	int wused, whard, wsoft;
	int rc, i, step_count;
	step_count = argc - 1;

	/* check the scheduler type and if not GANG exit */
	schedulertype = get_scheduler_type();

	if ( strlen (schedulertype) <= 0 ) {
		printf("SchedulerType could not be obtained. Make sure LoadLeveler is running and try again\n");
		exit(1);
	}

	if ( strcmp (schedulertype,"GANG") != 0 ) {
		printf(" SchedulerType is %s, currently this feature is available only on GANG scheduler\n",schedulertype);
		exit(1);
	}

	printf("%25s %15s \n"," ","WallClockTime");
	printf("%-10s %-15s %-5s %-5s %-5s \n","Hostname","StepId", "Used", "Hard", "Soft");

	/* loop through the steplist  to find wallclock time 
	   used for each step */

	for ( i = 0; i < step_count; i++ ) {
		stepid = argv[i+1];
		hostname=NULL;
		get_wall_clock_used(stepid, &hostname, &wused, &whard, &wsoft);
		printf ("%-10s %-15s %-5d %-5d %-5d \n", strtok(hostname,"."), stepid, wused, whard, wsoft);
	}	
}
#endif


/* 
  This function finds the hostname of the machine where a steps
	master task runs.
		params          type         descr
    step_id         char*     step id of the step whose master node to be found
    mtaskhname      char**    on return mtaskhname will have the hostname for
                              machine where master task for this step runs
*/

static void get_master_machine_name( char *stepid, char **mtaskhname)
{
	LL_element *queryObject = NULL, *job = NULL, *step = NULL,  *machine = NULL;
	char  *hname;
	char *steplist[2];
	char *step_name;
	int obj_count, err_code, rc;

	/* for the step list from step id */
	steplist[0] = stepid;
	steplist[1] = NULL;
	
	/* set up the query element */
	queryObject = ll_query(JOBS);
	if (!queryObject) { printf("Query JOBS: ll_query() returns NULL.\n"); exit(1); }
	rc = ll_set_request(queryObject, QUERY_STEPID, steplist, ALL_DATA);
	if (rc) { printf("ll_set_request() - QUERY_STEPID -  RC = %d\n", rc); exit(1); }
	/* Get the requested job objects from the cm daemon. */
	job = ll_get_objs(queryObject, LL_CM,
		NULL, &obj_count, &err_code);
	if ( job ) {
		ll_get_data(job, LL_JobGetFirstStep, &step);
		if (step ) {
			ll_get_data(step, LL_StepGetFirstMachine, &machine);
			if ( machine ) {
				ll_get_data(machine, LL_MachineName, &hname);
			} else { 
				hname = (char *)strdup("Idle");
			}
		} else {
			hname = (char *) strdup("Not Found");
		}
	} else {
			hname = (char *)strdup("Not Found");
	}
	*mtaskhname = hname;

}	
/* 
  This function finds the actual wall clock time used by a running step
  excluding the time spent during suspended state under GANG scheduler

		params          type         descr
    stepid         char*     step id of the step whose master node to be found
    hostname       char**    on return on hostname will have the hostname for
                              machine where master task for this step runs
	  wu             int*      on return wallclock used
	  wh             int*      on return wallclock  hard limit
	  ws             int*      on return wallclock  soft limit

*/
static void get_wall_clock_used(char *stepid, char **hostname, int *wu, int *wh, int *ws )
{
	LL_element *queryObject, *job = NULL, *step = NULL;
	int rc, err_code ;
	int  obj_count ;
	char *step_name;	
	char *steplist[2];
	steplist[0] = stepid;
	steplist[1] = NULL;
	
  /* get the master host name for this step */

	get_master_machine_name(stepid,hostname);
	 
	if ( ( strcmp( *hostname, "Not Found") == 0) || ( strcmp( *hostname, "Idle") == 0 ) ) {
  /* The step is either Idle or not in queue */
		*wu = *wh = *ws = 0; return ;
	}
		
	/* Initialize the query: Job query  */
	queryObject = ll_query(JOBS);
	if (!queryObject) { printf("Query JOBS: ll_query() returns NULL.\n"); exit(1); }

	/* Request information of job steps. */
	rc = ll_set_request(queryObject, QUERY_STEPID, steplist, ALL_DATA);
	if (rc) { printf("ll_set_request() - QUERY_STEPID -  RC = %d\n", rc); exit(1); }

	/* Get the requested job objects from the startd daemon. */
	job = ll_get_objs(queryObject, LL_STARTD,
		*hostname, &obj_count, &err_code);
	if (!job) { *wu= *wh= *ws= 0; *hostname = "Not Found"; return;   }  

	/* Loop through the job step objects. */
	ll_get_data(job, LL_JobGetFirstStep, &step);
		ll_get_data(step, LL_StepID, &step_name);
		ll_get_data(step, LL_StepWallClockUsed, wu);
		ll_get_data(step, LL_StepWallClockLimitHard, wh);
		ll_get_data(step, LL_StepWallClockLimitSoft, ws);
		step = NULL;
}
/* get scheduler type
*/
static char* get_scheduler_type( )
{
	LL_element *queryObject, *cluster = NULL;
	char *schedtype = NULL;
	int obj_count, err_code, rc;

	/* set up the query element */
	queryObject = ll_query(CLUSTERS);
	if (!queryObject) { printf("Query JOBS: ll_query() returns NULL.\n"); exit(1); }
	rc = ll_set_request(queryObject, QUERY_ALL, NULL, ALL_DATA);
	if (rc) { printf("ll_set_request() - QUERY_STEPID -  RC = %d\n", rc); exit(1); }
	/* Get the requested job objects from the cm daemon. */
	cluster = ll_get_objs(queryObject, LL_CM,
		NULL, &obj_count, &err_code);
	if ( cluster ) {
		ll_get_data(cluster, LL_ClusterSchedulerType, &schedtype);
	}
	return schedtype;
}
#endif	
void c_get_my_resident_time(int *wu, int *wh, int *ws )
{
 char *hostname;
 *wu=0 ; *wh=1800 ; *ws=1800 ;
#ifdef AIX
 get_wall_clock_used(getenv("LOADL_STEP_ID"), &hostname, wu, wh, ws);
#endif
}
#include <rpnmacros.h>
void f77name(f_get_my_resident_time)(int *wu, int *wh, int *ws )
{
 char *hostname;
 *wu=0 ; *wh=1800 ; *ws=1800 ;
#ifdef AIX
 get_wall_clock_used(getenv("LOADL_STEP_ID"), &hostname, wu, wh, ws);
#endif
}


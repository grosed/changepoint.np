#include <R.h>
#include <Rmath.h>
#include <Rinternals.h> // RK addition
#include <R_ext/RS.h>  // RK addition
#include <R_ext/Lapack.h> // RK addition
#include <R_ext/BLAS.h> // RK addition
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
//#include "costfunctions.c"
// #include "costfunctions.h"

namespace pelt
{

#define SWAP(a,b)   { int t; t=a; a=b; b=t; }  // Macro for swapping

// Cost functions
double costfunction(double sumstatout[], int tstar, int checklist, int *nquantiles, int *n){
  double Fkl;
  double temp_cost;
  double cost;
  int nseg, isum;

  cost = 0;
  temp_cost = 0;
  nseg = tstar - checklist;

  for(isum = 0; isum < *nquantiles; isum++){
    Fkl = (sumstatout[isum])/(nseg);
    temp_cost = (tstar-checklist)*(Fkl*log(Fkl)+(1-Fkl)*log(1-Fkl));
    if(!isnan(temp_cost)){
      cost = cost + temp_cost;
    }
    //else{
    //  cost = cost;
    //}
  }
  cost = -2*(log(2**n-1))*cost/(*nquantiles);
  return(cost);
}

double mll_nonparametric_ed_mbic(double sumstatout[], int tstar, int checklist, int *nquantiles, int *n){
  double Fkl;
  double temp_cost;
  double cost;
  int nseg, isum;

  cost = 0;
  temp_cost = 0;
  nseg = tstar - checklist;

  for(isum = 0; isum < *nquantiles; isum++){
    Fkl = (sumstatout[isum])/(nseg);
    temp_cost = (tstar-checklist)*(Fkl*log(Fkl)+(1-Fkl)*log(1-Fkl));
    if(!isnan(temp_cost)){
      cost = cost + temp_cost;
    }
    //else{
    //  cost = cost;
    //}
  }
  cost = -2*(log(2**n-1))*cost/(*nquantiles);
  return(cost);
}




void order_vec( int a[], int n ){
  int i, j;
  for(i = 0; i < n; i++){         // Make a pass through the array for each element
    for(j = 1; j < (n-i); j++){  		// Go through the array beginning to end
      if(a[j-1] > a[j])       // If the the first number is greater, swap it
        SWAP(a[j-1],a[j]);
    }
  }
}


static int *checklist;
static double *tmplike;

void FreePELT(int* error)
  {
    if(*error==0){
      free((void *)checklist);
      free((void *)tmplike);
    }
  }


void min_which(double *array,int n,double *minout,int *whichout){
  // Function to find minimum of an array with n elements that is put in min
  *minout=*array;
  *whichout=0;
  int i;
  for(i=1;i<n;i++){
    if(*(array+i)< *minout){
      *minout= *(array+i);
      *whichout=i;
    }
  }
}


  void pelt(char** cost_func, double* sumstat,int* n,double* pen,int* cptsout,int* error,int* minseglen,int* nquantiles, double* lastchangelike, int* lastchangecpts, int* numchangecpts)
  {

    // double (*costfunction)();
    double mll_nonparametric_ed();
    double mll_nonparametric_ed_mbic();

    //if (strcmp(*cost_func,"nonparametric.ed")==0){
    // costfunction = &mll_nonparametric_ed;
      //}
      //else if (strcmp(*cost_func,"nonparametric.ed.mbic")==0){
      //costfunction = &mll_nonparametric_ed_mbic;
      //}

    int *checklist;
    checklist = (int *)calloc(*n+1,sizeof(int));
    if (checklist==NULL)   {
      *error = 1;
      return;
      //goto err1;
    }

    int nchecklist;
    double minout;

    double *tmplike;
    tmplike = (double *)calloc(*n+1,sizeof(double));
    if (tmplike==NULL)   {
      *error = 2;
      // goto err2;
      free(checklist);
      return;
    }

    int tstar,i,whichout,nchecktmp;


    // void min_which();

    lastchangelike[0]= -*pen;
    lastchangecpts[0]=0;
    numchangecpts[0]=0;
    int j;
    int isum;
    double *sumstatout;
    sumstatout = (double *)calloc(*nquantiles,sizeof(double));
     for(j=*minseglen;j<(2*(*minseglen));j++){
       for(isum = 0; isum <*nquantiles; isum++){
         *(sumstatout+isum) = *(sumstat+isum+(*nquantiles*(j))) - *(sumstat+isum+(*nquantiles*(0)));
       }
        lastchangelike[j] = costfunction(sumstatout,j,0, nquantiles,n);
     }


    for(j=*minseglen;j<(2*(*minseglen));j++){
      lastchangecpts[j] = 0;
    }
    for(j=*minseglen;j<(2*(*minseglen));j++){
      numchangecpts[j] =1;
    }

    nchecklist=2;
    checklist[0]=0;
    checklist[1]=*minseglen;



    for(tstar=2*(*minseglen);tstar<(*n+1);tstar++){
      for(i=0;i<(nchecklist);i++){
        for(isum = 0; isum <*nquantiles; isum++){
          *(sumstatout+isum) = *(sumstat+isum+(*nquantiles*(tstar))) - *(sumstat+isum+(*nquantiles*(checklist[i])));
        }
        tmplike[i] = lastchangelike[checklist[i]] + costfunction(sumstatout, tstar, checklist[i], nquantiles, n) + *pen;
      }

      min_which(tmplike,nchecklist,&minout,&whichout); /*updates minout and whichout with min and which element */
      lastchangelike[tstar]=minout;
      lastchangecpts[tstar]=checklist[whichout];
      numchangecpts[tstar]=numchangecpts[lastchangecpts[tstar]]+1;
      /* Update checklist for next iteration, first element is next tau */
      nchecktmp=0;
      for(i=0;i<nchecklist;i++){
        if(tmplike[i]<= (lastchangelike[tstar]+*pen)){
          *(checklist+nchecktmp)=checklist[i];
          nchecktmp+=1;
        }
      }
      nchecklist = nchecktmp;
      *(checklist+nchecklist)=tstar-(*minseglen-1);// atleast 1 obs per seg
      nchecklist+=1;
    } // end taustar

    // put final set of changepoints together
    int ncpts=0;
    int last=*n;
    while(last!=0){
      *(cptsout + ncpts) = last;
      last=lastchangecpts[last];
      ncpts+=1;
    }
    // err2:  free(checklist);
    // err1:  return;
  }

} //namespace pelt
  

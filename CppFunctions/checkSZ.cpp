#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;
using namespace std;

#define HEAD 1
#define SPOUSE 2
#define CHILD 3
#define CHILDINLAW 4
#define PARENT 5
#define PARENTINLAW 6
#define SIBLING 7
#define SIBLINGINLAW 8
#define GRANDCHILD 9

#define GENDER 0
#define AGE 3
#define RELATE 4

inline int GetHead(NumericMatrix hh_to_check, int h) {
  for(int kk = 0; kk < h; kk++){
    if (hh_to_check(kk,RELATE) == HEAD) {
      return kk;
    }
  }
  return -1;
}

inline bool IsHead(double relate, double age) {
  return (relate == HEAD && age >=15);
}

inline bool MoreThanOneHead(NumericMatrix hh_to_check, int h) {
  int nhead = 0;
  for(int kk = 0; kk < h; kk++){
    if (hh_to_check(kk,RELATE) == HEAD) {
      nhead++;
    }
  }
  return (nhead >1);
}

inline int GetValidSpouse(NumericMatrix hh_to_check, int h) {
  int nspouse = 0;
  int spouse = -1;
  for(int kk = 0; kk < h; kk++){
    if (hh_to_check(kk,RELATE) == SPOUSE) {
      nspouse++;
      spouse = kk;
    }
  }
  if (nspouse > 1) {return -2;} //too many spouse
  if (nspouse == 0) { return -1;} //no spouse
  if (hh_to_check(spouse,AGE)<15) {return -2;} //spouse is under-age
  return spouse;
}

inline bool IsValidCouple(NumericMatrix hh_to_check, int spouse, int head) {
  if (spouse == -2) { //bad spouse or too many spouses
    return false;
  } else { //valid spouse or no spouse
    if (spouse >= 0) {//the only spouse, so check sex, and age difference
      if (hh_to_check(head,GENDER) == hh_to_check(spouse,GENDER)) {return false;}
      if (std::abs(hh_to_check(head,AGE) - hh_to_check(spouse,AGE)) > 52) {return false;}
    }
  }
  return true;
}

//return -1 if no child
//return the record index of the oldest child otherwise
inline int GetOldestChild(NumericMatrix hh_to_check, int h) {
  double age = -1;
  int child = -1;  //no childen
  for (int i = 0; i < h; i++) {
    if (hh_to_check(i,RELATE)==CHILD) {
      if (hh_to_check(i,AGE) > age) {
        age = hh_to_check(i,AGE);
        child = i;
      }
    }
  }
  return child;
}

inline bool IsValidChild(NumericMatrix hh_to_check, int child, int head) {
  if (child>=0) {//get a child, check age difference
    if (hh_to_check(head,AGE) - hh_to_check(child,AGE) <12) {return false;}
  }
  return true;
}

//return -1 if no childinlaw
//return the record index of the oldest childinlaw otherwise
inline int GetOldestChildInLaw(NumericMatrix hh_to_check, int h) {
  double age = -1;
  int child = -1;  //no childen
  for (int i = 0; i < h; i++) {
    if (hh_to_check(i,RELATE)==CHILDINLAW) {
      if (hh_to_check(i,AGE) > age) {
        age = hh_to_check(i,AGE);
        child = i;
      }
    }
  }
  return child;
}

inline bool IsValidChildInLaw(NumericMatrix hh_to_check, int childinlaw, int head) {
  if (childinlaw >= 0) {//get a child, check age difference
    if (hh_to_check(head,AGE) - hh_to_check(childinlaw,AGE) <7) {return false;}
  }
  return true;
}

//return -1 if no parent
//return the record index of the youngest parent otherwise
inline int GetYoungestParent(NumericMatrix hh_to_check, int h) {
  double age = 1000;
  int parent = -1;  //no parent
  for (int i = 0; i < h; i++) {
    if (hh_to_check(i,RELATE)==PARENT) {
      if (hh_to_check(i,AGE) < age) {
        age = hh_to_check(i,AGE);
        parent = i;
      }
    }
  }
  return parent;
}

inline bool IsValidParent(NumericMatrix hh_to_check, int parent, int head) {
  if (parent >= 0) {//get a child, check age difference
    if (hh_to_check(parent,AGE) - hh_to_check(head,AGE) < 13) {return false;}
  }
  return true;
}

//return -1 if no parentinlaw
//return the record index of the youngest parentinlaw otherwise
inline int GetYoungestParentInLaw(NumericMatrix hh_to_check, int h) {
  double age = 1000;
  int parent = -1;  //no parent
  for (int i = 0; i < h; i++) {
    if (hh_to_check(i,RELATE)==PARENTINLAW) {
      if (hh_to_check(i,AGE) < age) {
        age = hh_to_check(i,AGE);
        parent = i;
      }
    }
  }
  return parent;
}

inline bool IsValidParentInLaw(NumericMatrix hh_to_check, int parentinlaw, int head) {
  if (parentinlaw >= 0) {//get a child, check age difference
    if (hh_to_check(parentinlaw,AGE) - hh_to_check(head,AGE) < 9) {return false;}
  }
  return true;
}


inline bool IsValidSiblingOrSiblingInLaw(NumericMatrix hh_to_check, int h, int head) {
  for (int i = 0; i < h; i++) {
    if (hh_to_check(i,RELATE) == SIBLING || hh_to_check(i,RELATE) == SIBLINGINLAW) {
      if (std::abs(hh_to_check(i,AGE) - hh_to_check(head,AGE)) > 35) {return false;}
    }
  }
  return true;
}

inline bool IsValidGrandChild(NumericMatrix hh_to_check, int h, int spouse, int head) {
  for (int i = 0; i < h; i++) {
    if (hh_to_check(i,RELATE)== GRANDCHILD) {
      if (hh_to_check(head,AGE) < 33) {return false;} //too young to be grand parent for the HEAD
      if (spouse >= 0) { //make sure the spouse(if any) is not too young
        if (hh_to_check(spouse,AGE) < 33) {return false;}
      }
      if (hh_to_check(head,AGE) - hh_to_check(i,AGE) < 30) {return false;}
    }
  }
  return true;
}

inline int isValid(NumericMatrix hh_to_check, int h) {
  
  int head = GetHead(hh_to_check,h);
  if (head < 0) {return 0;}
  
  if (!IsHead(hh_to_check(head,RELATE), hh_to_check(head,AGE))) {return 0;}
  if (MoreThanOneHead(hh_to_check,h)) {return 0;}
  
  int spouse = GetValidSpouse(hh_to_check,h);
  if (!IsValidCouple(hh_to_check,spouse, head)) {return 0;}
  
  int oldestChild = GetOldestChild(hh_to_check,h);
  if (!IsValidChild(hh_to_check,oldestChild,head)) {return 0;}
  
  int oldestChildInLaw = GetOldestChildInLaw(hh_to_check,h);
  if (!IsValidChildInLaw(hh_to_check,oldestChildInLaw,head)) {return 0;}
  
  int youngestParent = GetYoungestParent(hh_to_check,h);
  if (!IsValidParent(hh_to_check,youngestParent,head)) {return 0;}
  
  int youngestParentInLaw = GetYoungestParentInLaw(hh_to_check,h);
  if (!IsValidParentInLaw(hh_to_check,youngestParentInLaw,head)) {return 0;}
  
  if (!IsValidSiblingOrSiblingInLaw(hh_to_check,h,head)) {return 0;}
  
  if (!IsValidGrandChild(hh_to_check,h,spouse,head)) {return 0;}
  
  return 1;
}

// [[Rcpp::export]]
NumericVector checkSZ(NumericMatrix Data_to_check, int h){
  int n0 = Data_to_check.nrow();
  int p = Data_to_check.ncol()/h;
  
  NumericVector Data_checked(n0);
  
  for (int i = 0; i < n0; i++) {
    NumericMatrix hh_to_check(h, p);
    for(int j = 0; j < p; j++){
      for(int k = 0; k < h; k++){
        hh_to_check(k,j) = Data_to_check(i,j+(k*p));
      }
    }
    Data_checked[i] = isValid(hh_to_check,h);
  }
  return Data_checked;
}





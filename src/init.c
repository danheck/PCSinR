#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _PCSinR_activation_function(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _PCSinR_check_convergence(SEXP, SEXP, SEXP, SEXP);
extern SEXP _PCSinR_convergence_floor(SEXP, SEXP, SEXP);
extern SEXP _PCSinR_pcs_matrix_cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _PCSinR_update_activation(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_PCSinR_activation_function", (DL_FUNC) &_PCSinR_activation_function, 5},
    {"_PCSinR_check_convergence",   (DL_FUNC) &_PCSinR_check_convergence,   4},
    {"_PCSinR_convergence_floor",   (DL_FUNC) &_PCSinR_convergence_floor,   3},
    {"_PCSinR_pcs_matrix_cpp",      (DL_FUNC) &_PCSinR_pcs_matrix_cpp,      8},
    {"_PCSinR_update_activation",   (DL_FUNC) &_PCSinR_update_activation,   5},
    {NULL, NULL, 0}
};

void R_init_PCSinR(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

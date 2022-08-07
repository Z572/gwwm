#include <libguile.h>
#define REF(A, B) (scm_c_public_ref(A, B))
#define FROM_P(P) (scm_from_pointer(P, NULL))
#define TO_P(P) (scm_to_pointer(P))
#define REF_CALL_1(M, N, ARG1) (scm_call_1(REF(M, N), ARG1))
#define REF_CALL_2(M, N, ARG1, ARG2) (scm_call_2(REF(M, N), ARG1, ARG2))
#define REF_CALL_3(M, N, ARG1, ARG2 ,ARG3) (scm_call_3(REF(M, N), ARG1, ARG2,ARG3))
#define SCM_LOOKUP_REF(name)    (scm_variable_ref(scm_c_lookup(name)))




static SCM client_type;
void
init_client_type (void)
{
  SCM name, slots;
  scm_t_struct_finalize finalizer;

  name = scm_from_utf8_symbol ("gwwm-client");
  slots = scm_list_1 (scm_from_utf8_symbol ("data"));
  finalizer = NULL;

  client_type =
    scm_make_foreign_object_type (name, slots, finalizer);
}

void init_scm() {
  init_client_type();
  /* init_scm_snarf(); */
  scm_primitive_load(scm_sys_search_load_path(scm_from_utf8_string("gwwm.scm")));
  scm_primitive_load(scm_sys_search_load_path(scm_from_utf8_string("gwwm/startup.scm")));
  scm_call_0(SCM_LOOKUP_REF("parse-command-line"));
}

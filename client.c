#include "libguile/boolean.h"
#include "libguile/eval.h"
#include "libguile/goops.h"
#include "libguile/gsubr.h"
#include "libguile/list.h"
#include "libguile/numbers.h"
#include "libguile/scm.h"
#include "libguile/symbols.h"
#include "libguile/values.h"
#include "string.h"
#include <stdbool.h>
#include <stdint.h>
#include <wlr/types/wlr_scene.h>
#include "util.h"
#include "client.h"
#include "gwwm.h"
#include "wayland-util.h"
#include <wlr/types/wlr_layer_shell_v1.h>
SCM find_client(Client *c) {
  return (c) ? c->scm : SCM_BOOL_F;
}

Client*
unwrap_client_1(SCM o)
{
  if (scm_is_false(o)) {
    return NULL;
  }
  SCM a=scm_slot_ref(o, scm_from_utf8_symbol("data"));
  if (scm_to_bool(scm_zero_p(a))) {
    scm_error(scm_misc_error_key,"unwrap-client","client is delated" ,SCM_EOL,SCM_EOL);
    return NULL;
  }
  return (TO_P(MAKE_P(a)));
}

void register_client(Client *c, enum gwwm_client_type type) {
  PRINT_FUNCTION;
  char *tp = "<gwwm-xdg-client>";
  switch (type) {
  case GWWM_LAYER_CLIENT_TYPE:
    tp = "<gwwm-layer-client>";
    break;
  case GWWM_XDG_CLIENT_TYPE:
    tp = "<gwwm-xdg-client>";
    break;
  case GWWM_X_CLIENT_TYPE:
    tp = "<gwwm-x-client>";
    break;
  }
  SCM sc=(scm_call_3(REF("oop goops", "make"), REF("gwwm client", tp),
                              scm_from_utf8_keyword("data"),
                     scm_pointer_address(FROM_P(c))));
  c->scm=sc;
}

void
logout_client(Client *c){
  PRINT_FUNCTION;
  SCM sc=WRAP_CLIENT(c);
  scm_call_1(REFP("gwwm client","logout-client") ,sc);
  c->scm=NULL;
  /* free(c); */
}

bool
client_is_x11(Client *c)
{
  return (scm_to_bool(REF_CALL_1("gwwm client","client-is-x11?", WRAP_CLIENT(c))));
}

void
client_for_each_surface(Client *c, wlr_surface_iterator_func_t fn, void *data)
{
	wlr_surface_for_each_surface(CLIENT_SURFACE(c), fn, data);
#ifdef XWAYLAND
	if (client_is_x11(c))
		return;
#endif
	wlr_xdg_surface_for_each_popup_surface(wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c)), fn, data);
}

const char *
client_get_appid(Client *c)
{
  return scm_to_utf8_string(REF_CALL_1("gwwm client", "client-get-appid",(WRAP_CLIENT(c))));
}

int
client_tags(Client *c) {
  return exp2(scm_to_int(scm_slot_ref(WRAP_CLIENT(c),scm_from_utf8_symbol("tags"))));
}
void set_client_tags(Client *c,int tags) {
  scm_slot_set_x(WRAP_CLIENT(c), scm_from_utf8_symbol("tags"), scm_from_int(log2(tags)));
  /* c->tags=tags; */
}

void set_client_geom(Client *c , struct wlr_box* box)
{
  PRINT_FUNCTION;
  SCM sc=WRAP_CLIENT(c);
  scm_slot_set_x(sc,scm_from_utf8_symbol("geom"),
                          (box) ? SHALLOW_CLONE(WRAP_WLR_BOX(box)) : SCM_BOOL_F);
}


const char *
client_get_title(Client *c)
{
  return (scm_to_utf8_string(REF_CALL_1("gwwm client","client-get-title", WRAP_CLIENT(c))));
}

bool
client_is_float_type(Client *c)
{
	struct wlr_box min = {0}, max={0};
    SCM values=REF_CALL_1("gwwm client","client-get-size-hints" , WRAP_CLIENT(c));
    max= *(UNWRAP_WLR_BOX(scm_c_value_ref(values, 0)));
    min= *(UNWRAP_WLR_BOX(scm_c_value_ref(values, 1)));

#ifdef XWAYLAND
	if (client_is_x11(c)) {
		struct wlr_xwayland_surface *surface = wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c));
		if (surface->modal)
			return 1;

		for (size_t i = 0; i < surface->window_type_len; i++)
          if (surface->window_type[i] == get_netatom_n(NetWMWindowTypeDialog)
              || surface->window_type[i] == get_netatom_n(NetWMWindowTypeSplash)
              || surface->window_type[i] == get_netatom_n(NetWMWindowTypeToolbar)
              || surface->window_type[i] == get_netatom_n(NetWMWindowTypeUtility))
				return 1;

		return ((min.width > 0 || min.height > 0 || max.width > 0 || max.height > 0)
			&& (min.width == max.width || min.height == max.height))
			|| wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->parent;
	}
#endif

	return ((min.width > 0 || min.height > 0 || max.width > 0 || max.height > 0)
		&& (min.width == max.width || min.height == max.height))
		|| wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->toplevel->parent;
}

SCM_DEFINE (gwwm_client_is_float_type_p,"client-is-float-type?",1,0,0,
            (SCM c),"")
#define FUNC_NAME s_gwwm_client_is_float_type_p
{
  GWWM_ASSERT_CLIENT_OR_FALSE(c ,1);
  return scm_from_bool(client_is_float_type(UNWRAP_CLIENT(c)));
}
#undef FUNC_NAME

bool
client_is_unmanaged(Client *c)
{
  return (scm_to_bool(REF_CALL_1("gwwm client","client-is-unmanaged?", WRAP_CLIENT(c))));
}

void
client_notify_enter(struct wlr_surface *s, struct wlr_keyboard *kb)
{
	if (kb)
      wlr_seat_keyboard_notify_enter(get_gloabl_seat(), s, kb->keycodes,
				kb->num_keycodes, &kb->modifiers);
	else
      wlr_seat_keyboard_notify_enter(get_gloabl_seat(), s, NULL, 0, NULL);
}

struct wlr_surface *
client_surface_at(Client *c, double cx, double cy, double *sx, double *sy)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		return wlr_surface_surface_at(CLIENT_SURFACE(c),
				cx, cy, sx, sy);
#endif
	return wlr_xdg_surface_surface_at(wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c)), cx, cy, sx, sy);
}

SCM_DEFINE_PUBLIC(gwwm_client_from_popup,"client-from-popup",1,0,0,(SCM spopup),"" ){
  struct wlr_xdg_popup *popup=UNWRAP_WLR_XDG_POPUP(spopup);
  struct wlr_xdg_surface *surface = popup->base;

  while (true) {
    switch (surface->role) {
    case WLR_XDG_SURFACE_ROLE_POPUP:
      if (wlr_surface_is_layer_surface(surface->popup->parent))
        return (wlr_layer_surface_v1_from_wlr_surface(surface->popup->parent)->data);
      else if (!wlr_surface_is_xdg_surface(surface->popup->parent))
        return NULL;

      surface = wlr_xdg_surface_from_wlr_surface(surface->popup->parent);
      break;
    case WLR_XDG_SURFACE_ROLE_TOPLEVEL:
      return (surface->data);
    case WLR_XDG_SURFACE_ROLE_NONE:
      return SCM_BOOL_F;
    }
  }
  return SCM_BOOL_F;
}

SCM_DEFINE_PUBLIC (gwwm_client_from_wlr_surface ,"client-from-wlr-surface" ,1,0,0,(SCM ss),"")
{
  struct wlr_surface *s=UNWRAP_WLR_SURFACE(ss);
  struct wlr_xdg_surface *surface;
  if (s) {
  if (wlr_surface_is_xdg_surface(s)
      && (surface = wlr_xdg_surface_from_wlr_surface(s))
      && surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL)
    return surface->data;
  struct wlr_xwayland_surface *xsurface;
  if (wlr_surface_is_xwayland_surface(s)
      && (xsurface = wlr_xwayland_surface_from_wlr_surface(s)))
    return xsurface->data;
  if (wlr_surface_is_subsurface(s))
    return gwwm_client_from_wlr_surface(WRAP_WLR_SURFACE(wlr_surface_get_root_surface(s)));
  }
  return SCM_BOOL_F;
}

SCM_DEFINE_PUBLIC (gwwm_client_from_list,"gwwm-client-from-link",1,0,0,(SCM slink),""){
  struct wl_list *link=UNWRAP_WL_LIST(slink);
  Client *c;
  return WRAP_CLIENT(wl_container_of(link->next, c, link));
}

void
scm_init_gwwm_client(void)
{
#ifndef SCM_MAGIC_SNARFER
#include "client.x"
#endif
}

#include <wlr/types/wlr_scene.h>
#include "util.h"
#include "client.h"
#include <wlr/types/wlr_layer_shell_v1.h>
SCM
find_client(void *c) {
  SCM p =(scm_pointer_address(scm_from_pointer(c ,NULL)));
  return scm_hashq_ref(INNER_CLIENTS_HASH_TABLE, p ,
                       scm_hashq_ref(REFP("gwwm client", "%layer-clients"),
                                     p ,SCM_BOOL_F));
}

Client*
unwrap_client_1(SCM o)
{
  SCM a=scm_call_1(REFP("gwwm client",".data"),o);
  if (scm_is_false(a)) {
    scm_error(scm_misc_error_key,"unwrap-client","client is delated" ,SCM_EOL,SCM_EOL);
    return NULL;
  }
  return (TO_P(MAKE_P(a)));
}

void
register_client(void *c, char *type) {
  scm_hashq_set_x((type=="<gwwm-layer-client>"
                   ? REFP("gwwm client", "%layer-clients")
                   : INNER_CLIENTS_HASH_TABLE),
                  (scm_pointer_address(FROM_P(c))),
                  (scm_call_3(REF("oop goops","make"),
                              REF("gwwm client",type),
                              scm_from_utf8_keyword("data"),
                              scm_pointer_address(FROM_P(c)))));
}

void
logout_client(void *c){
  scm_call_1(REFP("gwwm client","logout-client") ,WRAP_CLIENT(c));
  /* scm_hashq_remove_x(((scm_to_utf8_string(gwwm_client_type(WRAP_CLIENT(c)))== "LayerShell") */
  /*                     ? REFP("gwwm client", "%layer-clients") */
  /*                     : INNER_CLIENTS_HASH_TABLE), */
  /*                    scm_pointer_address(scm_from_pointer(c ,NULL))); */
    /* scm_slot_set_x(WRAP_CLIENT(c),scm_from_utf8_symbol("data"),SCM_BOOL_F); */
    free(c);
}
struct wlr_scene_rect *
client_border_n(Client *c, int n)
{
  /* return c->border[n]; */
  return (UNWRAP_WLR_SCENE_RECT(scm_list_ref(scm_slot_ref(WRAP_CLIENT(c), scm_from_utf8_symbol("borders")),scm_from_int(n))));
}
void
client_init_border(Client *c)
{
  PRINT_FUNCTION;
  SCM list =scm_make_list(scm_from_int(0), SCM_UNSPECIFIED);
  for (int i = 0; i < 4; i++) {
    struct wlr_scene_rect *rect= wlr_scene_rect_create(CLIENT_SCENE(c), 0, 0, GWWM_BORDERCOLOR());
    rect->node.data = c;
    wlr_scene_rect_set_color(rect, GWWM_BORDERCOLOR());
    list=scm_cons(WRAP_WLR_SCENE_RECT(rect), list);
  }
  scm_slot_set_x(WRAP_CLIENT(c), scm_from_utf8_symbol("borders"), list);
}

int
client_is_x11(Client *c)
{
  return (scm_to_bool(REF_CALL_1("gwwm client","client-is-x11?", WRAP_CLIENT(c))));
}

Client *
client_from_wlr_surface(struct wlr_surface *s)
{
	struct wlr_xdg_surface *surface;
	struct wlr_surface *parent;

#ifdef XWAYLAND
	struct wlr_xwayland_surface *xsurface;
	if (s && wlr_surface_is_xwayland_surface(s)
			&& (xsurface = wlr_xwayland_surface_from_wlr_surface(s)))
		return xsurface->data;
#endif
	if (s && wlr_surface_is_xdg_surface(s)
			&& (surface = wlr_xdg_surface_from_wlr_surface(s))
			&& surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL)
		return surface->data;

	if (s && wlr_surface_is_subsurface(s))
		return client_from_wlr_surface(wlr_surface_get_root_surface(s));
	return NULL;
}

/* The others */
void
client_activate_surface(struct wlr_surface *s, int activated)
{
	struct wlr_xdg_surface *surface;
#ifdef XWAYLAND
	struct wlr_xwayland_surface *xsurface;
	if (wlr_surface_is_xwayland_surface(s)
			&& (xsurface = wlr_xwayland_surface_from_wlr_surface(s))) {
		wlr_xwayland_surface_activate(xsurface, activated);
		return;
	}
#endif
	if (wlr_surface_is_xdg_surface(s)
			&& (surface = wlr_xdg_surface_from_wlr_surface(s))
			&& surface->role == WLR_XDG_SURFACE_ROLE_TOPLEVEL)
		wlr_xdg_toplevel_set_activated(surface, activated);
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

struct wlr_scene_node *
client_scene_surface(Client *c, struct wlr_scene_node *surface) {
  SCM s;
  SCM scm_c= WRAP_CLIENT(c);
  if (surface) {
    s=WRAP_WLR_SCENE_NODE(surface);
    scm_slot_set_x(scm_c,scm_from_utf8_symbol("scene-surface"),s);
    return surface;
  } else {
    s=scm_slot_ref(scm_c,scm_from_utf8_symbol("scene-surface"));
    return scm_is_false(s) ? NULL : UNWRAP_WLR_SCENE_NODE(s);
}
}

struct wlr_box*
client_get_geometry(Client *c)
{
  struct wlr_box *geom=ecalloc(1,sizeof(*geom));
#ifdef XWAYLAND
	if (client_is_x11(c)) {
      geom->x = wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->x;
		geom->y = wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->y;
		geom->width = wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->width;
		geom->height = wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->height;
		return geom;
	}
#endif
	wlr_xdg_surface_get_geometry(wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c)), geom);
    return geom;
}

void
client_get_size_hints(Client *c, struct wlr_box *max, struct wlr_box *min)
{
	struct wlr_xdg_toplevel *toplevel;
	struct wlr_xdg_toplevel_state *state;
#ifdef XWAYLAND
	if (client_is_x11(c)) {
		struct wlr_xwayland_surface_size_hints *size_hints;
		size_hints = wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->size_hints;
		if (size_hints) {
			max->width = size_hints->max_width;
			max->height = size_hints->max_height;
			min->width = size_hints->min_width;
			min->height = size_hints->min_height;
		}
		return;
	}
#endif
	toplevel = wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->toplevel;
	state = &toplevel->current;
	max->width = state->max_width;
	max->height = state->max_height;
	min->width = state->min_width;
	min->height = state->min_height;
}

SCM_DEFINE (gwwm_client_get_size_hints,"client-get-size-hints",1,0,0,
            (SCM c),"")
#define FUNC_NAME s_gwwm_client_get_size_hints
{
  GWWM_ASSERT_CLIENT_OR_FALSE(c ,1);
  struct wlr_box min = {0}, max = {0};
  Client *cl =UNWRAP_CLIENT(c);
  client_get_size_hints(cl, &max, &min);
  return scm_cons(WRAP_WLR_BOX(&max),WRAP_WLR_BOX(&min));
}
#undef FUNC_NAME

const char *
client_get_title(Client *c)
{
  return (scm_to_utf8_string(REF_CALL_1("gwwm client","client-get-title", WRAP_CLIENT(c))));
}

Client *
client_get_parent(Client *c)
{
  PRINT_FUNCTION;
	Client *p;
#ifdef XWAYLAND
	if (client_is_x11(c) && wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->parent){
      return client_from_wlr_surface(wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->parent->surface);
    }
#endif
    PRINT_FUNCTION;
	if (wlr_surface_is_xdg_surface(CLIENT_SURFACE(c)) && wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->toplevel->parent){
      PRINT_FUNCTION;
      return client_from_wlr_surface(wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->toplevel->parent->surface);
    }

	return NULL;
}

int
client_is_float_type(Client *c)
{
	struct wlr_box min = {0}, max = {0};
	client_get_size_hints(c, &max, &min);

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

int
client_is_mapped(Client *c)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		return wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->mapped;
#endif
	return wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->mapped;
}

int
client_wants_fullscreen(Client *c)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		return wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c))->fullscreen;
#endif
	return wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c))->toplevel->requested.fullscreen;
}

int
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

void
client_send_close(Client *c)
{
  REF_CALL_1("gwwm client","client-send-close" ,WRAP_CLIENT(c));
}

void
client_set_fullscreen(Client *c, int fullscreen)
{
  scm_call_2(REFP("gwwm client","client-do-set-fullscreen"),
             WRAP_CLIENT(c),
             scm_from_bool(fullscreen));
}

uint32_t
client_set_size(Client *c, uint32_t width, uint32_t height)
{
#ifdef XWAYLAND
	if (client_is_x11(c)) {
		wlr_xwayland_surface_configure(wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c)),
				c->geom.x, c->geom.y, width, height);
		return 0;
	}
#endif
	return wlr_xdg_toplevel_set_size(wlr_xdg_surface_from_wlr_surface(CLIENT_SURFACE(c)), width, height);
}

void
client_set_tiled(Client *c, uint32_t edges)
{
  PRINT_FUNCTION;
  REF_CALL_2("gwwm client","client-set-tiled" ,WRAP_CLIENT(c), scm_from_uint32(edges));
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

void
client_restack_surface(Client *c)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		wlr_xwayland_surface_restack(wlr_xwayland_surface_from_wlr_surface(CLIENT_SURFACE(c)), NULL,
				XCB_STACK_MODE_ABOVE);
#endif
	return;
}
void
client_set_resizing(Client *c,int resizing)
{
  REF_CALL_2("gwwm client","client-set-resizing!" ,WRAP_CLIENT(c), scm_from_bool(resizing));
}

void *
toplevel_from_popup(struct wlr_xdg_popup *popup)
{
	struct wlr_xdg_surface *surface = popup->base;

	while (1) {
		switch (surface->role) {
		case WLR_XDG_SURFACE_ROLE_POPUP:
			if (wlr_surface_is_layer_surface(surface->popup->parent))
				return wlr_layer_surface_v1_from_wlr_surface(surface->popup->parent)->data;
			else if (!wlr_surface_is_xdg_surface(surface->popup->parent))
				return NULL;

			surface = wlr_xdg_surface_from_wlr_surface(surface->popup->parent);
			break;
		case WLR_XDG_SURFACE_ROLE_TOPLEVEL:
				return surface->data;
		case WLR_XDG_SURFACE_ROLE_NONE:
			return NULL;
		}
	}
}

SCM_DEFINE (gwwm_client_get_parent, "client-get-parent" ,1,0,0,
            (SCM c), "")
#define FUNC_NAME s_gwwm_client_get_parent
{
  GWWM_ASSERT_CLIENT_OR_FALSE(c ,1);
  Client *cl = UNWRAP_CLIENT(c);
  Client *p = client_get_parent(cl);
  if (p) {
    return WRAP_CLIENT(p);
  };
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_client_wants_fullscreen_p , "client-wants-fullscreen?",1,0,0,
            (SCM client), "")
#define FUNC_NAME s_gwwm_client_wants_fullscreen_p
{
  GWWM_ASSERT_CLIENT_OR_FALSE(client ,1);
  return scm_from_bool(client_wants_fullscreen(UNWRAP_CLIENT(client)));
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_setfullscreen, "%setfullscreen",2,0,0,(SCM c,SCM yes),"")
#define FUNC_NAME s_gwwm_setfullscreen
{
  GWWM_ASSERT_CLIENT_OR_FALSE(c ,1);
  setfullscreen(UNWRAP_CLIENT(c),scm_to_bool(yes));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

void
scm_init_gwwm_client(void)
{
#ifndef SCM_MAGIC_SNARFER
#include "client.x"
#endif
}

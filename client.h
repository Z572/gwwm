#include <libguile.h>
/*
 * Attempt to consolidate unavoidable suck into one file, away from dwl.c.  This
 * file is not meant to be pretty.  We use a .h file with static inline
 * functions instead of a separate .c module, or function pointers like sway, so
 * that they will simply compile out if the chosen #defines leave them unused.
 */

/* Leave these functions first; they're used in the others */

#define WRAP_CLIENT(o) (scm_call_3(REF("oop goops","make"), \
                                  REF("gwwm client","<gwwm-client>"), \
                                  scm_from_utf8_keyword("data"), \
                                  FROM_P(o)))
#define UNWRAP_CLIENT(o) (Client *)(TO_P(scm_call_1(REFP("gwwm client",".data"),o)))


static inline int
client_is_x11(Client *c)
{
  return (scm_to_bool(REF_CALL_1("gwwm client","client-is-x11?", WRAP_CLIENT(c))));
}

static inline struct wlr_surface *
client_surface(Client *c)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		return c->surface.xwayland->surface;
#endif
	return c->surface.xdg->surface;
}

static inline Client *
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
static inline void
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

static inline void
client_for_each_surface(Client *c, wlr_surface_iterator_func_t fn, void *data)
{
	wlr_surface_for_each_surface(client_surface(c), fn, data);
#ifdef XWAYLAND
	if (client_is_x11(c))
		return;
#endif
	wlr_xdg_surface_for_each_popup_surface(c->surface.xdg, fn, data);
}

static inline const char *
client_get_appid(Client *c)
{
  return scm_to_utf8_string(REF_CALL_1("gwwm client", "client-get-appid",(WRAP_CLIENT(c))));
}

static inline void
client_get_geometry(Client *c, struct wlr_box *geom)
{
#ifdef XWAYLAND
	if (client_is_x11(c)) {
		geom->x = c->surface.xwayland->x;
		geom->y = c->surface.xwayland->y;
		geom->width = c->surface.xwayland->width;
		geom->height = c->surface.xwayland->height;
		return;
	}
#endif
	wlr_xdg_surface_get_geometry(c->surface.xdg, geom);
}

static inline void
client_get_size_hints(Client *c, struct wlr_box *max, struct wlr_box *min)
{
	struct wlr_xdg_toplevel *toplevel;
	struct wlr_xdg_toplevel_state *state;
#ifdef XWAYLAND
	if (client_is_x11(c)) {
		struct wlr_xwayland_surface_size_hints *size_hints;
		size_hints = c->surface.xwayland->size_hints;
		if (size_hints) {
			max->width = size_hints->max_width;
			max->height = size_hints->max_height;
			min->width = size_hints->min_width;
			min->height = size_hints->min_height;
		}
		return;
	}
#endif
	toplevel = c->surface.xdg->toplevel;
	state = &toplevel->current;
	max->width = state->max_width;
	max->height = state->max_height;
	min->width = state->min_width;
	min->height = state->min_height;
}

static inline const char *
client_get_title(Client *c)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		return c->surface.xwayland->title;
#endif
	return c->surface.xdg->toplevel->title;
}

static inline Client *
client_get_parent(Client *c)
{
	Client *p;
#ifdef XWAYLAND
	if (client_is_x11(c) && c->surface.xwayland->parent)
		return client_from_wlr_surface(c->surface.xwayland->parent->surface);
#endif
	if (c->surface.xdg->toplevel->parent)
		return client_from_wlr_surface(c->surface.xdg->toplevel->parent->surface);

	return NULL;
}

static inline int
client_is_float_type(Client *c)
{
	struct wlr_box min = {0}, max = {0};
	client_get_size_hints(c, &max, &min);

#ifdef XWAYLAND
	if (client_is_x11(c)) {
		struct wlr_xwayland_surface *surface = c->surface.xwayland;
		if (surface->modal)
			return 1;

		for (size_t i = 0; i < surface->window_type_len; i++)
			if (surface->window_type[i] == netatom[NetWMWindowTypeDialog]
					|| surface->window_type[i] == netatom[NetWMWindowTypeSplash]
					|| surface->window_type[i] == netatom[NetWMWindowTypeToolbar]
					|| surface->window_type[i] == netatom[NetWMWindowTypeUtility])
				return 1;

		return ((min.width > 0 || min.height > 0 || max.width > 0 || max.height > 0)
			&& (min.width == max.width || min.height == max.height))
			|| c->surface.xwayland->parent;
	}
#endif

	return ((min.width > 0 || min.height > 0 || max.width > 0 || max.height > 0)
		&& (min.width == max.width || min.height == max.height))
		|| c->surface.xdg->toplevel->parent;
}

static inline int
client_is_mapped(Client *c)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		return c->surface.xwayland->mapped;
#endif
	return c->surface.xdg->mapped;
}

static inline int
client_wants_fullscreen(Client *c)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		return c->surface.xwayland->fullscreen;
#endif
	return c->surface.xdg->toplevel->requested.fullscreen;
}

static inline int
client_is_unmanaged(Client *c)
{
#ifdef XWAYLAND
	return c->type == X11Unmanaged;
#endif
	return 0;
}

static inline void
client_send_close(Client *c)
{
  REF_CALL_1("gwwm client","client-send-close" ,WRAP_CLIENT(c));
}

static inline void
client_set_fullscreen(Client *c, int fullscreen)
{
#ifdef XWAYLAND
	if (client_is_x11(c)) {
		wlr_xwayland_surface_set_fullscreen(c->surface.xwayland, fullscreen);
		return;
	}
#endif
	wlr_xdg_toplevel_set_fullscreen(c->surface.xdg, fullscreen);
}

SCM_DEFINE (gwwm_client_xdg_surface ,"client-xdg-surface",1,0,0,(SCM c),"")
#define FUNC_NAME s_gwwm_client_xdg_surface
{
  Client *cl= (UNWRAP_CLIENT(c));
  return WRAP_WLR_XDG_SURFACE(cl->surface.xdg);
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_client_xwayland_surface ,"client-xwayland-surface",1,0,0,(SCM c),"")
#define FUNC_NAME s_gwwm_client_xwayland_surface
{ Client *cl=(UNWRAP_CLIENT(c));
  return WRAP_WLR_XWAYLAND_SURFACE(cl->surface.xwayland);
}
#undef FUNC_NAME



static inline uint32_t
client_set_size(Client *c, uint32_t width, uint32_t height)
{
#ifdef XWAYLAND
	if (client_is_x11(c)) {
		wlr_xwayland_surface_configure(c->surface.xwayland,
				c->geom.x, c->geom.y, width, height);
		return 0;
	}
#endif
	return wlr_xdg_toplevel_set_size(c->surface.xdg, width, height);
}

static inline void
client_set_tiled(Client *c, uint32_t edges)
{
  REF_CALL_2("gwwm client","client-set-tiled" ,WRAP_CLIENT(c), scm_from_uint32(edges));
}

static inline struct wlr_surface *
client_surface_at(Client *c, double cx, double cy, double *sx, double *sy)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		return wlr_surface_surface_at(c->surface.xwayland->surface,
				cx, cy, sx, sy);
#endif
	return wlr_xdg_surface_surface_at(c->surface.xdg, cx, cy, sx, sy);
}

static inline void
client_restack_surface(Client *c)
{
#ifdef XWAYLAND
	if (client_is_x11(c))
		wlr_xwayland_surface_restack(c->surface.xwayland, NULL,
				XCB_STACK_MODE_ABOVE);
#endif
	return;
}
static inline void
client_set_resizing(Client *c,int resizing)
{
  REF_CALL_2("gwwm client","client-set-resizing!" ,WRAP_CLIENT(c), scm_from_bool(resizing));
}

static inline void *
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
SCM_DEFINE (gwwm_client_is_fullscreen_p, "client-is-fullscreen?" ,1,0,0,
            (SCM c), "")
#define FUNC_NAME s_gwwm_client_is_fullscreen_p
{
  Client *cl = UNWRAP_CLIENT(c);
  return scm_from_bool(cl->isfullscreen);
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_client_is_floating_p, "client-is-floating?" ,1,0,0,
            (SCM c), "")
#define FUNC_NAME s_gwwm_client_is_floating_p
{
  Client *cl = UNWRAP_CLIENT(c);
  return scm_from_bool(cl->isfloating);
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_client_border_width, "client-border-width" , 1,0,0,
            (SCM c), "")
#define FUNC_NAME s_gwwm_client_border_width
{
    Client *cl = UNWRAP_CLIENT(c);
  return scm_from_unsigned_integer(cl->bw);
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_client_get_title, "client-get-title" ,1,0,0,
            (SCM c), "")
#define FUNC_NAME s_gwwm_client_get_title
{
  Client *cl = UNWRAP_CLIENT(c);
  return scm_from_locale_string(client_get_title(cl));
}
#undef FUNC_NAME


SCM_DEFINE (gwwm_client_get_appid, "client-get-appid" ,1,0,0,
            (SCM c), "")
#define FUNC_NAME s_gwwm_client_get_appid
{
  Client *cl = UNWRAP_CLIENT(c);
  return scm_from_locale_string(client_get_appid(cl));
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_client_get_parent, "client-get-parent" ,1,0,0,
            (SCM c), "")
#define FUNC_NAME s_gwwm_client_get_parent
{
  Client *cl = UNWRAP_CLIENT(c);
  Client *p = client_get_parent(cl);
  if (p) {
    return WRAP_CLIENT(p);
  };
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_set_client_border_width, "client-set-border-width" , 2,0,0,
            (SCM c ,SCM w), "")
#define FUNC_NAME s_gwwm_client_border_width_set
{
    Client *cl = UNWRAP_CLIENT(c);
    cl->bw=scm_to_signed_integer(w ,0, 1240);
    arrange(current_monitor);
  return WRAP_CLIENT(cl);
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_client_type, "client-type" , 1,0,0,
            (SCM c), "")
{
  Client *cl = UNWRAP_CLIENT(c);
  char* t;
  switch (cl->type)
    {
    case XDGShell:
      t="XDGShell";
      break;
    case LayerShell:
      t="LayerShell";
      break;
    case X11Managed:
      t="X11Managed";
      break;
    case X11Unmanaged:
      t="X11Unmanaged";
      break;
    }
  return scm_from_utf8_string(t);
}

SCM_DEFINE (gwwm_client_wants_fullscreen_p , "client-wants-fullscreen?",1,0,0,
            (SCM client), "")
#define FUNC_NAME s_gwwm_client_wants_fullscreen
{
  return scm_from_bool(client_wants_fullscreen(UNWRAP_CLIENT(client)));
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_client_list , "client-list",0,0,0,(),"")
#define FUNC_NAME s_gwwm_client_list
{
  SCM a=scm_make_list(scm_from_int(0), SCM_UNSPECIFIED);
  	Client *c;
	wl_list_for_each(c, &clients, link) {
      a=scm_cons(WRAP_CLIENT(c), a);
    }
    return a;
}
#undef FUNC_NAME


SCM_DEFINE (gwwm_client_set_floating, "client-set-floating!" ,2,0,0,
            (SCM c ,SCM floating), "")
#define FUNC_NAME s_gwwm_client_set_floating
{
  Client *cl = UNWRAP_CLIENT(c);
  setfloating(cl , scm_to_bool(floating));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (gwwm_setfullscreen, "client-set-fullscreen!",2,0,0,(SCM c,SCM yes),"")
#define FUNC_NAME s_gwwm_setfullscreen
{
  setfullscreen(UNWRAP_CLIENT(c),scm_to_bool(yes));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

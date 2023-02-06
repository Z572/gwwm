#include "libguile/eval.h"
#include "libguile/foreign-object.h"
#include "libguile/foreign.h"
#include "libguile/goops.h"
#include "libguile/numbers.h"
#include "libguile/scm.h"
#include "libguile/symbols.h"
#include "util.h"
#include "wayland-util.h"
#include <drm_fourcc.h>
#include <libguile.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <wlr/interfaces/wlr_buffer.h>

struct gwwm_buffer {
  struct wlr_buffer base;
  SCM scm;
};

static void gwwm_buffer_destroy(struct wlr_buffer *wlr_buffer) {
  struct gwwm_buffer *buffer = wl_container_of(wlr_buffer, buffer, base);
  buffer->scm = NULL;
  free(buffer);
}
static bool gwwm_buffer_begin_data_ptr_access(struct wlr_buffer *wlr_buffer,
                                              uint32_t flags, void **data,
                                              uint32_t *format,
                                              size_t *stride) {
  struct gwwm_buffer *buffer = wl_container_of(wlr_buffer, buffer, base);
  SCM ssurface =
      (scm_slot_ref(buffer->scm, scm_from_utf8_symbol("image-surface")));

  *data = (TO_P(REF_CALL_1(
      "system foreign", "bytevector->pointer",
      (REF_CALL_1("cairo", "cairo-image-surface-get-data", ssurface)))));

  *format = (scm_to_uint32(
      scm_slot_ref(buffer->scm, scm_from_utf8_symbol("format"))));
  *stride = (scm_to_double(
      REF_CALL_1("cairo", "cairo-image-surface-get-stride", ssurface)));
  return true;
}

static void gwwm_buffer_end_data_ptr_access(struct wlr_buffer *wlr_buffer) {}
static const struct wlr_buffer_impl gwwm_buffer_impl = {
    .destroy = gwwm_buffer_destroy,
    .begin_data_ptr_access = gwwm_buffer_begin_data_ptr_access,
    .end_data_ptr_access = gwwm_buffer_end_data_ptr_access};

static struct gwwm_buffer *gwwm_buffer_create(int width, int height) {
  struct gwwm_buffer *buffer = scm_calloc(sizeof(*buffer));
  if (buffer == NULL) {
    return NULL;
  }

  wlr_buffer_init(&buffer->base, &gwwm_buffer_impl, width, height);
  SCM ssurface = REF_CALL_3("cairo", "cairo-image-surface-create",
                            scm_from_utf8_symbol("argb32"), scm_from_int(width),
                            scm_from_int(height));
  buffer->scm = (scm_call_3(
      REF("oop goops", "make"), REFP("gwwm buffer", "<gwwm-cairo-buffer>"),
      scm_from_utf8_keyword("data"), scm_pointer_address(FROM_P(buffer))));
  scm_slot_set_x(buffer->scm, scm_from_utf8_symbol("cairo"),
                 REF_CALL_1("cairo", "cairo-create", ssurface));
  scm_slot_set_x(buffer->scm, scm_from_utf8_symbol("image-surface"), ssurface);
  scm_slot_set_x(buffer->scm, scm_from_utf8_symbol("format"),
                 scm_from_unsigned_integer(DRM_FORMAT_ARGB8888));
  wlr_buffer_lock(&buffer->base);
  wlr_buffer_drop(&buffer->base)
  return buffer;
}
SCM_DEFINE(cairo_buffer_create, "cairo-buffer-create", 2, 0, 0,
           (SCM width, SCM height), "") {
  return (gwwm_buffer_create(scm_to_int(width), scm_to_int(height)))->scm;
}

SCM_DEFINE(cairo_buffer_base, "cairo-buffer-base", 1, 0, 0, (SCM buffer), "") {
  return WRAP_WLR_BUFFER(&((struct gwwm_buffer *)(TO_P(MAKE_P(scm_slot_ref(
                               buffer, scm_from_utf8_symbol("%data"))))))
                              ->base);
}
SCM_DEFINE(set_cairo_buffer_base, "set-cairo-buffer-base!", 2, 0, 0,
           (SCM buffer, SCM wlr_buffer), "") {
  ((struct gwwm_buffer *)(TO_P(
       MAKE_P(scm_slot_ref(buffer, scm_from_utf8_symbol("%data"))))))
      ->base = *((struct wlr_buffer *)(UNWRAP_WLR_BUFFER(wlr_buffer)));
  return SCM_UNSPECIFIED;
}

void scm_init_gwwm_buffer(void) {
#ifndef SCM_MAGIC_SNARFER
#include "buffer.x"
#endif
}
